#' Create a disk-backed data frame
#'
#' `disk_df()` stores data in a local SQLite database so operations happen
#' out-of-memory. The result looks and feels like a tibble, but the full
#' dataset is never loaded into RAM unless you explicitly request it with
#' [collect()].
#'
#' @param x A data.frame, a character path to a CSV file, or an existing
#'   SQLite connection (via DBI). The most common usage is passing a
#'   data.frame.
#' @param name Optional table name inside SQLite. If `NULL` (the default),
#'   the name is inferred from the object.
#' @param ... Additional arguments. For character input, these are passed to
#'   [read.csv()] (e.g. `sep`, `stringsAsFactors`).
#' @returns An object of class `disk_df`.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' df
#' dim(df)
#' names(df)
disk_df <- function(x, name = NULL, ...) {
  UseMethod("disk_df")
}

#' @export
disk_df.default <- function(x, name = NULL, ...) {
  if (is.data.frame(x)) {
    disk_df.data.frame(x, name = name, ...)
  } else {
    stop("Don't know how to make a disk_df from ", class(x)[1])
  }
}

#' @export
disk_df.data.frame <- function(x, name = NULL, ...) {
  if (is.null(name)) name <- deparse(substitute(x))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  nm <- make_table_name(name)
  DBI::dbWriteTable(con, nm, x, temporary = TRUE)
  structure(
    list(
      con      = con,
      table    = nm,
      ops      = list(),
      .dims    = dim(x),
      .names   = names(x)
    ),
    class = "disk_df"
  )
}

#' @export
disk_df.character <- function(x, name = NULL, ...) {
  if (file.exists(x)) {
    if (is.null(name)) name <- tools::file_path_sans_ext(basename(x))
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    nm <- make_table_name(name)
    dat <- utils::read.csv(x, ...)
    DBI::dbWriteTable(con, nm, dat, temporary = TRUE)
    structure(
      list(
        con      = con,
        table    = nm,
        ops      = list(),
        .dims    = dim(dat),
        .names   = names(dat)
      ),
      class = "disk_df"
    )
  } else {
    stop("File not found: ", x)
  }
}

#' Coerce to a disk_df
#'
#' @param x An object to coerce.
#' @param ... Additional arguments passed to methods.
#' @returns A `disk_df`.
#' @export
as.disk_df <- function(x, ...) {
  UseMethod("as.disk_df")
}

#' @export
as.disk_df.default <- function(x, ...) {
  disk_df(x, ...)
}

#' Test if an object is a disk_df
#'
#' @param x An object to test.
#' @returns `TRUE` if the object inherits from `disk_df`.
#' @export
is.disk_df <- function(x) {
  inherits(x, "disk_df")
}

# -------------------------------------------------------------------------
# S3 generics we need before their methods
# -------------------------------------------------------------------------

#' @export
collect <- function(x, ...) {
  UseMethod("collect")
}

#' @export
filter <- function(.data, ...) {
  UseMethod("filter")
}

#' @export
select <- function(.data, ...) {
  UseMethod("select")
}

#' @export
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @export
summarise <- function(.data, ...) {
  UseMethod("summarise")
}

#' @export
group_by <- function(.data, ...) {
  UseMethod("group_by")
}

#' @export
ungroup <- function(.data, ...) {
  UseMethod("ungroup")
}

#' @export
arrange <- function(.data, ...) {
  UseMethod("arrange")
}

#' @export
pull <- function(.data, ...) {
  UseMethod("pull")
}

#' @export
glimpse <- function(x, ...) {
  UseMethod("glimpse")
}

# -------------------------------------------------------------------------
# Print / summary
# -------------------------------------------------------------------------

#' @export
print.disk_df <- function(x, ...) {
  n <- nrow(x)
  k <- length(.subset2(x, "ops"))
  cat("# A disk_df: ", n, " rows x ", length(.subset2(x, ".names")),
      " cols\n", sep = "")
  if (k > 0) {
    cat("# Pending operations: ", k, "\n", sep = "")
  }
  memsafe <- getOption("memorySafe.active", FALSE)
  cat("# Memory-safe mode: ", if (memsafe) "ON" else "OFF", "\n", sep = "")
  cat("#\n")
  h <- head(x, n = 10)
  print(tibble::as_tibble(h), ...)
  invisible(x)
}

#' @export
str.disk_df <- function(object, ...) {
  cat("disk_df [", nrow(object), " x ",
      length(.subset2(object, ".names")), "]\n", sep = "")
  cat("Table: ", .subset2(object, "table"), "\n")
  cat("Columns: ", paste(.subset2(object, ".names"), collapse = ", "), "\n")
  cat("Pending ops: ", length(.subset2(object, "ops")), "\n")
  invisible(object)
}

# -------------------------------------------------------------------------
# Dimension / names
# -------------------------------------------------------------------------

#' @export
dim.disk_df <- function(x) {
  ops <- .subset2(x, "ops")
  dims <- .subset2(x, ".dims")
  if (length(ops) == 0 && !is.null(dims)) {
    return(dims)
  }
  sql <- build_query(x, count = TRUE)
  n <- DBI::dbGetQuery(.subset2(x, "con"), sql)[[1]]
  c(n, length(.subset2(x, ".names")))
}

#' @export
dimnames.disk_df <- function(x) {
  list(NULL, disk_names(x))
}

disk_names <- function(x) {
  nms <- .subset2(x, ".names")
  if (is.null(nms)) character(0) else nms
}

#' @export
names.disk_df <- function(x) {
  disk_names(x)
}

#' @export
`names<-.disk_df` <- function(x, value) {
  x[[".names"]] <- value
  x
}

#' @export
nrow.disk_df <- function(x) {
  dim(x)[1]
}

# -------------------------------------------------------------------------
# Convert back to in-memory
# -------------------------------------------------------------------------

#' @export
as.data.frame.disk_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  memory_safe_check(x, "as.data.frame")
  out <- collect(x)
  as.data.frame(out)
}

#' @export
as_tibble.disk_df <- function(x, ...) {
  memory_safe_check(x, "as_tibble")
  collect(x)
}

# -------------------------------------------------------------------------
# head / tail
# -------------------------------------------------------------------------

#' @export
head.disk_df <- function(x, n = 6L, offset = NULL, ...) {
  sql <- build_query(x, limit = n, offset = offset)
  DBI::dbGetQuery(.subset2(x, "con"), sql)
}

#' @export
tail.disk_df <- function(x, n = 6L, ...) {
  sql <- build_query(x, limit = n, offset = max(0, nrow(x) - n))
  DBI::dbGetQuery(.subset2(x, "con"), sql)
}

# -------------------------------------------------------------------------
# Subsetting / extracting
# -------------------------------------------------------------------------

#' @export
`$.disk_df` <- function(x, name) {
  nm <- as.character(substitute(name))
  nms <- disk_names(x)
  if (!nm %in% nms) stop("Unknown column: ", nm)
  sql <- build_query(x, select_cols = nm)
  DBI::dbGetQuery(.subset2(x, "con"), sql)[[1]]
}

#' @export
`[[.disk_df` <- function(x, i, ...) {
  nms <- disk_names(x)
  if (is.character(i)) {
    idx <- which(nms == i)
    if (length(idx) == 0) stop("Unknown column: ", i)
    nm <- nms[idx]
  } else if (is.numeric(i)) {
    nm <- nms[i]
  } else {
    stop("Invalid subscript type")
  }
  sql <- build_query(x, select_cols = nm)
  DBI::dbGetQuery(.subset2(x, "con"), sql)[[1]]
}

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

make_table_name <- function(name) {
  name <- gsub("[^A-Za-z0-9_]", "_", name)
  paste0("_disk_df_", name, "_", as.integer(Sys.time()))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @importFrom rlang :=
NULL
