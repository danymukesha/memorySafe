#' Memory-safe mode
#'
#' When memory-safe mode is active, operations that would load a large amount
#' of data into R's memory (like [collect()], [as.data.frame()], or
#' [as_tibble()]) will warn or error before proceeding.
#'
#' The limits are controlled by two options:
#' - `memorySafe.limit_rows` (default 100,000): maximum rows allowed
#' - `memorySafe.limit_bytes` (default 100 MB): maximum estimated size
#' - `memorySafe.action` (default `"warning"`): one of `"warning"` or
#'   `"error"`
#'
#' @param activate `TRUE` to enable memory-safe mode, `FALSE` to disable.
#'   If called without arguments, toggles the current state.
#' @returns The previous state of memory-safe mode (invisibly).
#' @export
#' @examples
#' memory_safe_mode(TRUE)
#' df <- disk_df(mtcars)
#' # This would warn: collect(df)
#' memory_safe_mode(FALSE)
memory_safe_mode <- function(activate) {
  current <- getOption("memorySafe.active", FALSE)
  if (missing(activate)) {
    activate <- !current
  }
  options(memorySafe.active = isTRUE(activate))
  if (activate) {
    message("memorySafe: memory-safe mode is now ON.")
    message("  Rows limit: ", format(
      getOption("memorySafe.limit_rows", 100000), big.mark = ","))
    message("  Bytes limit: ", format(
      getOption("memorySafe.limit_bytes", 100e6), big.mark = ","), " (",
      format(getOption("memorySafe.limit_bytes", 100e6) / 1e6, big.mark = ","), " MB)")
    message("  Action: ", getOption("memorySafe.action", "warning"))
  } else {
    message("memorySafe: memory-safe mode is OFF.")
  }
  invisible(current)
}

#' Check and warn if a collect operation would load too much data
#'
#' @param x A disk_df.
#' @param context A string describing the calling function.
#' @param ... Not used.
#' @export
memory_safe_check <- function(x, context = "operation", ...) {
  if (!getOption("memorySafe.active", FALSE)) {
    return(invisible(TRUE))
  }

  n_rows <- nrow(x)
  n_cols <- length(names(x))
  limit_rows <- getOption("memorySafe.limit_rows", 100000)
  limit_bytes <- getOption("memorySafe.limit_bytes", 100e6)
  action <- getOption("memorySafe.action", "warning")

  # Rough size estimate: 8 bytes per numeric, but be conservative
  est_bytes <- n_rows * n_cols * 16

  ok <- TRUE
  msgs <- character(0)

  if (n_rows > limit_rows) {
    msgs <- c(msgs, paste0(
      "Memory-safe check: ", context, " would load ",
      format(n_rows, big.mark = ","), " rows (limit: ",
      format(limit_rows, big.mark = ","), ")."
    ))
    ok <- FALSE
  }

  if (est_bytes > limit_bytes) {
    msgs <- c(msgs, paste0(
      "Memory-safe check: ", context, " would load ~",
      format(round(est_bytes / 1e6), big.mark = ","), " MB (limit: ",
      format(round(limit_bytes / 1e6), big.mark = ","), " MB)."
    ))
    ok <- FALSE
  }

  if (!ok) {
    msg <- paste(msgs, collapse = "\n")
    if (action == "error") {
      stop(msg, "\nUse chunk_map() to process this data in pieces, ",
           "or increase the limit with memory_safe_set_limit().",
           call. = FALSE)
    } else {
      warning(msg, "\nProceeding anyway. Set ",
              "options(memorySafe.action = \"error\") to stop instead.",
              call. = FALSE, immediate. = TRUE)
    }
  }

  invisible(ok)
}

#' Set memory-safe limits
#'
#' @param limit_rows Maximum number of rows to allow in memory (default 100k).
#'   Set to `Inf` for no row limit.
#' @param limit_bytes Maximum number of bytes to allow (default 100 MB).
#'   Set to `Inf` for no byte limit.
#' @param action What to do when a limit is exceeded: `"warning"` (default)
#'   or `"error"`.
#' @returns Invisibly returns the previous limits as a list.
#' @export
#' @examples
#' memory_safe_set_limit(limit_rows = 50000, action = "error")
memory_safe_set_limit <- function(limit_rows = NULL, limit_bytes = NULL,
                                   action = NULL) {
  old <- list(
    limit_rows = getOption("memorySafe.limit_rows", 100000),
    limit_bytes = getOption("memorySafe.limit_bytes", 100e6),
    action = getOption("memorySafe.action", "warning")
  )

  if (!is.null(limit_rows)) {
    options(memorySafe.limit_rows = limit_rows)
  }
  if (!is.null(limit_bytes)) {
    options(memorySafe.limit_bytes = limit_bytes)
  }
  if (!is.null(action)) {
    action <- match.arg(action, c("warning", "error"))
    options(memorySafe.action = action)
  }

  invisible(old)
}

#' Get current memory-safe limits
#' @returns A list with the current `limit_rows`, `limit_bytes`, and `action`.
#' @export
memory_safe_get_limit <- function() {
  list(
    limit_rows = getOption("memorySafe.limit_rows", 100000),
    limit_bytes = getOption("memorySafe.limit_bytes", 100e6),
    action = getOption("memorySafe.action", "warning"),
    active = getOption("memorySafe.active", FALSE)
  )
}
