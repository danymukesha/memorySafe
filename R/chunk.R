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

  results <- results[!vapply(results, is.null, logical(1))]

  if (.combine == "list") {
    return(results)
  }

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

  con  <- .subset2(.data, "con")
  tbl  <- .subset2(.data, "table")
  ops  <- .subset2(.data, "ops")
  nms  <- .subset2(.data, ".names")

  chunks <- vector("list", n_chunks)

  for (i in seq_len(n_chunks)) {
    offset <- (i - 1) * .chunk_size
    this_n <- min(.chunk_size, n_total - offset)

    chunk <- structure(
      list(
        con      = con,
        table    = tbl,
        ops      = ops,
        .dims    = c(this_n, length(nms)),
        .names   = nms,
        .offset  = offset,
        .limit   = this_n
      ),
      class = "disk_df_chunk"
    )

    chunks[[i]] <- chunk
  }

  lapply(chunks, function(ch) {
    structure(ch, class = c("disk_df_chunk", "disk_df"))
  })
}

#' @export
collect.disk_df_chunk <- function(x, ...) {
  sql <- build_query(x, limit = .subset2(x, ".limit"),
                     offset = .subset2(x, ".offset"))
  out <- DBI::dbGetQuery(.subset2(x, "con"), sql)
  tibble::as_tibble(out)
}

#' @export
head.disk_df_chunk <- function(x, n = 6L, ...) {
  collect(x)
}

#' @export
dim.disk_df_chunk <- function(x) {
  .subset2(x, ".dims")
}

#' @export
print.disk_df_chunk <- function(x, ...) {
  dims <- .subset2(x, ".dims")
  offset <- .subset2(x, ".offset")
  limit <- .subset2(x, ".limit")
  cat("# A disk_df chunk: ", dims[1], " rows x ", dims[2], " cols\n", sep = "")
  cat("# (rows ", offset + 1, "-", offset + limit, " of parent)\n", sep = "")
  h <- head(x)
  print(tibble::as_tibble(h), ...)
  invisible(x)
}
