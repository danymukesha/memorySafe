#' Apply a function to a disk_df in chunks
#'
#' Processes a [disk_df] in row-wise chunks, applying a function to each
#' chunk and collecting the results. This is useful for operations that
#' can't be expressed as SQL (e.g., complex statistical models) but that
#' can operate on subsets of the data.
#'
#' @param .data A disk_df.
#' @param .f A function that takes a data.frame (or tibble) and returns
#'   another data.frame (or any object that can be combined with
#'   [rbind()]/[vctrs::vec_rbind()]).
#' @param .chunk_size Number of rows per chunk. Default 10,000.
#' @param .progress Show a progress bar? Default `FALSE`.
#' @param .combine How to combine results. `"rbind"` (default) uses
#'   [data.table::rbindlist()] or [rbind()]; `"list"` returns a list.
#' @param ... Additional arguments passed to `.f`.
#' @returns If `.combine = "rbind"`, a data.frame. If `.combine = "list"`,
#'   a list.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' result <- chunk_map(df, function(chunk) {
#'   data.frame(mean_mpg = mean(chunk$mpg), n = nrow(chunk))
#' }, .chunk_size = 10)
#' result
chunk_map <- function(.data, .f, .chunk_size = 10000,
                       .progress = FALSE, .combine = c("rbind", "list"), ...) {
  .combine <- match.arg(.combine)
  n_total <- nrow(.data)
  n_chunks <- ceiling(n_total / .chunk_size)

  if (.progress) {
    pb <- utils::txtProgressBar(min = 0, max = n_chunks, style = 3)
  }

  results <- vector("list", n_chunks)

  for (i in seq_len(n_chunks)) {
    offset <- (i - 1) * .chunk_size
    chunk_data <- head(.data, n = .chunk_size, offset = offset)

    if (nrow(chunk_data) > 0) {
      results[[i]] <- .f(chunk_data, ...)
    }

    if (.progress) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  if (.progress) close(pb)

  # Remove NULL / empty results
  results <- results[!vapply(results, is.null, logical(1))]

  if (.combine == "list") {
    return(results)
  }

  # rbind
  if (length(results) == 0) {
    return(data.frame())
  }

  do.call(rbind, results)
}

#' Split a disk_df into chunked disk_df objects
#'
#' Creates a list of [disk_df] objects, each pointing to a different slice of
#' the same underlying table. Useful for parallel processing.
#'
#' @param .data A disk_df.
#' @param .chunk_size Number of rows per chunk.
#' @returns A list of disk_df objects, each limited to a subset of rows.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' chunks <- chunk_split(df, .chunk_size = 10)
#' length(chunks)
#' collect(chunks[[1]])
chunk_split <- function(.data, .chunk_size = 10000) {
  n_total <- nrow(.data)
  n_chunks <- ceiling(n_total / .chunk_size)

  chunks <- vector("list", n_chunks)

  for (i in seq_len(n_chunks)) {
    offset <- (i - 1) * .chunk_size
    this_n <- min(.chunk_size, n_total - offset)

    # Create a new disk_df that wraps a subquery limiting to this chunk
    # We'll use a view or just store the offset info
    chunk <- structure(
      list(
        con      = .data$con,
        table    = .data$table,
        ops      = .data$ops,
        .dims    = c(this_n, length(.data$names)),
        .names   = .data$.names,
        .offset  = offset,
        .limit   = this_n
      ),
      class = "disk_df_chunk"
    )

    chunks[[i]] <- chunk
  }

  # Wrap chunks so collect/head works with the offset
  lapply(chunks, function(ch) {
    structure(ch, class = c("disk_df_chunk", "disk_df"))
  })
}

#' @export
collect.disk_df_chunk <- function(x, ...) {
  sql <- build_query(x, limit = x$.limit, offset = x$.offset)
  out <- DBI::dbGetQuery(x$con, sql)
  tibble::as_tibble(out)
}

#' @export
head.disk_df_chunk <- function(x, n = 6L, ...) {
  collect(x)
}

#' @export
dim.disk_df_chunk <- function(x) {
  x$.dims
}

#' @export
print.disk_df_chunk <- function(x, ...) {
  cat("# A disk_df chunk: ", x$.dims[1], " rows x ", x$.dims[2], " cols\n", sep = "")
  cat("# (rows ", x$.offset + 1, "-", x$.offset + x$.limit, " of parent)\n", sep = "")
  head(x) |> tibble::as_tibble() |> print(...)
  invisible(x)
}
