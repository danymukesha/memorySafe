#' dplyr verbs for disk_df
#'
#' These methods let you use familiar dplyr verbs on a [disk_df] without
#' pulling data into memory. Each verb records the operation and returns a
#' new `disk_df` — execution is lazy and happens only when you call
#' [collect()].

#' Subset rows with a SQL WHERE clause
#' @param .data A disk_df.
#' @param ... Logical predicates expressed in R syntax. They are translated
#'   to SQL, so only simple expressions work (comparisons, `&`, `|`, `!`,
#'   `is.na()`, `%in%`).
#' @returns A disk_df with the filter operation queued.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(filter(df, mpg > 20))
filter.disk_df <- function(.data, ...) {
  op_filter(.data, ...)
}

#' Choose columns
#' @param .data A disk_df.
#' @param ... One or more column names (unquoted).
#' @returns A disk_df with the select operation queued.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(select(df, mpg, wt))
select.disk_df <- function(.data, ...) {
  op_select(.data, ...)
}

#' Add new columns
#'
#' New columns are computed via SQL expressions. Only simple arithmetic,
#' string, and conditional expressions are supported. For complex operations,
#' use [collect()] first.
#'
#' @param .data A disk_df.
#' @param ... Name-value pairs of expressions, like `newcol = expr`.
#' @returns A disk_df with the mutate operation queued.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(mutate(df, wt_kg = wt * 453.592 / 1000))
mutate.disk_df <- function(.data, ...) {
  op_mutate(.data, ...)
}

#' Summarise / aggregate data
#'
#' @param .data A disk_df.
#' @param ... Name-value pairs of summary functions. Supported: `n()`, `sum`,
#'   `mean`, `sd`, `min`, `max`, `median`, `n_distinct`, `first`, `last`.
#' @returns A disk_df with the summarise operation queued.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(summarise(df, avg_mpg = mean(mpg), n = n()))
summarise.disk_df <- function(.data, ...) {
  op_summarise(.data, ...)
}

#' @export
summarize <- function(.data, ...) {
  UseMethod("summarize")
}

#' @export
summarize.disk_df <- summarise.disk_df

#' Group data for subsequent summarise
#' @param .data A disk_df.
#' @param ... Column names to group by.
#' @returns A disk_df with grouping recorded.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(df |> group_by(cyl) |> summarise(avg_mpg = mean(mpg)))
group_by.disk_df <- function(.data, ...) {
  op_group_by(.data, ...)
}

#' Remove grouping
#' @param .data A disk_df.
#' @returns A disk_df with grouping removed.
#' @export
ungroup.disk_df <- function(.data, ...) {
  op_ungroup(.data)
}

#' Arrange rows
#' @param .data A disk_df.
#' @param ... Columns to order by. Use [desc()] for descending order.
#' @returns A disk_df with the arrange operation queued.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(arrange(df, desc(mpg)))
arrange.disk_df <- function(.data, ...) {
  op_arrange(.data, ...)
}

#' Execute the lazy operations and return a tibble
#'
#' This is where the SQL queries are actually sent to the database and the
#' result is materialized into an in-memory tibble. If memory-safe mode is
#' active and the result exceeds the configured limit, a warning is issued.
#'
#' @param x A disk_df.
#' @param ... Not used.
#' @returns A tibble (or data.frame) with the query results.
#' @export
#' @examples
#' df <- disk_df(mtcars)
#' collect(filter(df, mpg > 20))
collect.disk_df <- function(x, ...) {
  sql <- build_query(x)
  out <- DBI::dbGetQuery(.subset2(x, "con"), sql)
  tibble::as_tibble(out)
}

#' Pull a single column
#' @param .data A disk_df.
#' @param var The column name or index.
#' @param ... Not used.
#' @returns A vector.
#' @export
pull.disk_df <- function(.data, var = -1, ...) {
  nms <- .subset2(.data, ".names")
  var_expr <- substitute(var)
  if (is.numeric(var_expr)) {
    idx <- as.integer(var_expr)
    if (idx < 0) idx <- length(nms) + idx + 1
    nm <- nms[idx]
  } else if (is.character(var_expr)) {
    nm <- var_expr
  } else {
    nm <- as.character(var_expr)
  }
  .data[[nm]]
}

#' Glimpse at the structure
#' @param x A disk_df.
#' @param ... Not used.
#' @export
glimpse.disk_df <- function(x, ...) {
  nms <- .subset2(x, ".names")
  con <- .subset2(x, "con")
  tbl <- .subset2(x, "table")
  cat("disk_df: ", nrow(x), " rows x ", length(nms), " columns\n", sep = "")
  for (nm in nms) {
    sql <- paste0("SELECT \"", nm, "\" FROM \"", tbl, "\" LIMIT 5")
    sample <- DBI::dbGetQuery(con, sql)[[1]]
    typ <- class(sample)[1]
    preview <- paste(head(sample, 3), collapse = ", ")
    cat(" $ ", nm, " <", typ, "> ", preview, if (length(sample) > 3) " ...", "\n", sep = "")
  }
  invisible(x)
}
