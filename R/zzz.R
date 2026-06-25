#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom tibble as_tibble
#' @importFrom rlang enquo enquos quo_name quo_get_expr quo_get_env
#' @importFrom utils read.csv head tail txtProgressBar setTxtProgressBar
NULL

.onLoad <- function(libname, pkgname) {
  # Set default options if not already set
  if (is.null(getOption("memorySafe.active"))) {
    options(memorySafe.active = FALSE)
  }
  if (is.null(getOption("memorySafe.limit_rows"))) {
    options(memorySafe.limit_rows = 100000)
  }
  if (is.null(getOption("memorySafe.limit_bytes"))) {
    options(memorySafe.limit_bytes = 100e6)
  }
  if (is.null(getOption("memorySafe.action"))) {
    options(memorySafe.action = "warning")
  }
}

.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "memorySafe ", utils::packageVersion("memorySafe"),
    " — a disk-backed data.frame with memory-safe mode.\n",
    "See ?disk_df to get started, or memory_safe_mode() to enable protections."
  )
  packageStartupMessage(msg)
}

# Register finalizer to clean up SQLite connections
reg.finalizer <- function(e, x) {
  if (is.environment(e)) {
    reg.finalizer(e, function(...) {
      if (inherits(x, "disk_df") && DBI::dbIsValid(x$con)) {
        DBI::dbDisconnect(x$con)
      }
    })
  }
}
