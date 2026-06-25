#' Read a CSV file as a disk_df
#'
#' A convenience wrapper that reads a CSV file into a [disk_df] without
#' ever storing the full data in R's memory. The CSV is read line-by-line
#' using [utils::read.csv()] in chunks and written directly to SQLite.
#'
#' @param file Path to a CSV file.
#' @param name Optional table name. Defaults to the file name (without .csv).
#' @param chunk_size Number of rows to read per chunk when loading into
#'   SQLite. Default 5000. Lower values use less RAM during loading.
#' @param ... Additional arguments passed to [utils::read.csv()] for the
#'   first chunk (e.g. `sep`, `quote`, `stringsAsFactors`). These must be
#'   compatible with all chunks.
#' @returns A disk_df.
#' @export
#' @examples
#' \dontrun{
#' df <- read_disk_csv("very_large_file.csv")
#' dim(df)
#' }
read_disk_csv <- function(file, name = NULL, chunk_size = 5000, ...) {
  if (!file.exists(file)) {
    stop("File not found: ", file)
  }
  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(file))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  nm <- make_table_name(name)

  # Read first chunk to get column names and types
  first_chunk <- utils::read.csv(file, nrows = chunk_size, ...)
  DBI::dbWriteTable(con, nm, first_chunk[0, ], temporary = TRUE)
  DBI::dbAppendTable(con, nm, first_chunk)

  total_rows <- nrow(first_chunk)

  # Read remaining chunks
  repeat {
    chunk <- tryCatch(
      utils::read.csv(file, skip = total_rows + 1, nrows = chunk_size,
                       header = FALSE, ...),
      error = function(e) NULL
    )
    if (is.null(chunk) || nrow(chunk) == 0) break
    names(chunk) <- names(first_chunk)
    DBI::dbAppendTable(con, nm, chunk)
    total_rows <- total_rows + nrow(chunk)
  }

  structure(
    list(
      con      = con,
      table    = nm,
      ops      = list(),
      .dims    = c(total_rows, ncol(first_chunk)),
      .names   = names(first_chunk)
    ),
    class = "disk_df"
  )
}
