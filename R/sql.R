# SQL expression translator and query builder for disk_df
#
# The disk_df stores a list of "operations" in its `ops` field. Each operation
# captures a dplyr verb and its arguments. When collect() or a query is needed,
# these ops are compiled into a single SQL statement.

# -------------------------------------------------------------------------
# SQL expression translation
# -------------------------------------------------------------------------

expr_to_sql <- function(expr, env = parent.frame()) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (exists(nm, envir = env, inherits = TRUE)) {
      val <- get(nm, envir = env)
      if (is.character(val)) DBI::dbQuoteString(DBI::ANSI(), val)
      else if (is.numeric(val)) as.character(val)
      else if (is.logical(val)) if (val) "1" else "0"
      else if (is.na(val)) "NULL"
      else DBI::dbQuoteString(DBI::ANSI(), as.character(val))
    } else {
      # Column reference - quote it
      paste0("\"", nm, "\"")
    }
  } else if (is.atomic(expr) && length(expr) == 1) {
    if (is.character(expr)) DBI::dbQuoteString(DBI::ANSI(), expr)
    else if (is.numeric(expr)) as.character(expr)
    else if (is.logical(expr)) if (expr) "1" else "0"
    else if (is.na(expr)) "NULL"
    else as.character(expr)
  } else if (is.call(expr)) {
    op <- as.character(expr[[1]])
    args <- lapply(expr[-1], expr_to_sql, env = env)

    switch(op,
      "+"  = glue_op("+", args, 2),
      "-"  = if (length(args) == 1) paste0("-", args[[1]]) else glue_op("-", args, 2),
      "*"  = glue_op("*", args, 2),
      "/"  = glue_op("/", args, 2),
      "^"  = paste0("POWER(", args[[1]], ", ", args[[2]], ")"),
      "%%" = glue_op("%", args, 2),

      ">"   = glue_op(">", args, 2),
      ">="  = glue_op(">=", args, 2),
      "<"   = glue_op("<", args, 2),
      "<="  = glue_op("<=", args, 2),
      "=="  = glue_op("=", args, 2),
      "!="  = glue_op("!=", args, 2),

      "&"  = glue_op("AND", args, 2),
      "&&" = glue_op("AND", args, 2),
      "|"  = glue_op("OR", args, 2),
      "||" = glue_op("OR", args, 2),
      "!"  = paste0("NOT (", args[[1]], ")"),

      "("     = paste0("(", args[[1]], ")"),
      "{"     = args[[1]],

      "%in%" = paste0(args[[1]], " IN (", paste(args[-1], collapse = ", "), ")"),

      "is.na"    = paste0(args[[1]], " IS NULL"),
      "!is.na"   = paste0(args[[1]], " IS NOT NULL"),

      "n"    = "COUNT(*)",
      "n()"  = "COUNT(*)",

      "mean" = paste0("AVG(", args[[1]], ")"),
      "sum"  = paste0("SUM(", args[[1]], ")"),
      "sd"   = paste0("STDEV(", args[[1]], ")"),
      "var"  = paste0("VARIANCE(", args[[1]], ")"),
      "min"  = paste0("MIN(", args[[1]], ")"),
      "max"  = paste0("MAX(", args[[1]], ")"),
      "median" = paste0("MEDIAN(", args[[1]], ")"),
      "n_distinct" = paste0("COUNT(DISTINCT ", args[[1]], ")"),
      "first" = paste0("FIRST_VALUE(", args[[1]], ")"),
      "last"  = paste0("LAST_VALUE(", args[[1]], ")"),
      "abs"   = paste0("ABS(", args[[1]], ")"),
      "round" = if (length(args) >= 2) paste0("ROUND(", args[[1]], ", ", args[[2]], ")") else paste0("ROUND(", args[[1]], ")"),
      "sqrt"  = paste0("SQRT(", args[[1]], ")"),
      "log"   = if (length(args) >= 2) paste0("LOG(", args[[2]], ", ", args[[1]], ")") else paste0("LOG(", args[[1]], ")"),
      "log10" = paste0("LOG10(", args[[1]], ")"),
      "log2"  = paste0("LOG2(", args[[1]], ")"),
      "exp"   = paste0("EXP(", args[[1]], ")"),
      "sign"  = paste0("SIGN(", args[[1]], ")"),
      "ceiling" = paste0("CEIL(", args[[1]], ")"),
      "floor"   = paste0("FLOOR(", args[[1]], ")"),
      "trimws"  = paste0("TRIM(", args[[1]], ")"),
      "tolower" = paste0("LOWER(", args[[1]], ")"),
      "toupper" = paste0("UPPER(", args[[1]], ")"),
      "nchar"   = paste0("LENGTH(", args[[1]], ")"),
      "substr"  = paste0("SUBSTR(", args[[1]], ", ", args[[2]], ", ", args[[3]], ")"),
      "paste"   = paste0(lapply(args, function(a) paste0("COALESCE(CAST(", a, " AS TEXT), '')")), collapse = " || ' ' || "),
      "paste0"  = paste0(lapply(args, function(a) paste0("COALESCE(CAST(", a, " AS TEXT), '')")), collapse = " || "),
      "ifelse"  = paste0("CASE WHEN ", args[[1]], " THEN ", args[[2]], " ELSE ", args[[3]], " END"),
      "coalesce" = paste0("COALESCE(", paste(args, collapse = ", "), ")"),
      "case_when" = case_when_to_sql(expr, env),

      "as.numeric" = paste0("CAST(", args[[1]], " AS REAL)"),
      "as.integer" = paste0("CAST(", args[[1]], " AS INTEGER)"),
      "as.character" = paste0("CAST(", args[[1]], " AS TEXT)"),

      "row_number" = "ROW_NUMBER() OVER (ORDER BY (SELECT 1))",
      "lag"  = paste0("LAG(", args[[1]], ", ", if (length(args) >= 2) args[[2]] else "1", ") OVER (ORDER BY (SELECT 1))"),
      "lead" = paste0("LEAD(", args[[1]], ", ", if (length(args) >= 2) args[[2]] else "1", ") OVER (ORDER BY (SELECT 1))"),

      stop("Unsupported operation in memorySafe: ", op, "\n",
           "This function can't be translated to SQL. Try doing this ",
           "operation in R after collect().")
    )
  } else if (is.atomic(expr)) {
    vals <- vapply(expr, function(v) {
      if (is.character(v)) DBI::dbQuoteString(DBI::ANSI(), v)
      else if (is.numeric(v)) as.character(v)
      else if (is.logical(v)) if (v) "1" else "0"
      else "NULL"
    }, character(1))
    paste(vals, collapse = ", ")
  } else {
    stop("Can't translate expression to SQL")
  }
}

glue_op <- function(op, args, n) {
  if (length(args) != n) {
    stop("Operator ", op, " expects ", n, " arguments, got ", length(args))
  }
  paste0("(", args[[1]], " ", op, " ", args[[2]], ")")
}

case_when_to_sql <- function(expr, env) {
  args <- as.list(expr[-1])
  parts <- character(length(args))
  for (i in seq_along(args)) {
    cond <- args[[i]][[2]]
    val  <- args[[i]][[3]]
    sql_cond <- expr_to_sql(cond, env)
    sql_val  <- expr_to_sql(val, env)
    parts[i] <- paste0("WHEN ", sql_cond, " THEN ", sql_val)
  }
  paste0("CASE ", paste(parts, collapse = " "), " END")
}

# -------------------------------------------------------------------------
# Operation helpers
#
# These functions modify .data by adding operation records. They access
# internal list elements directly to avoid S3 dispatch on disk_df.
# -------------------------------------------------------------------------

op_filter <- function(.data, ...) {
  dots <- rlang::enquos(...)
  curr_ops <- .subset2(.data, "ops")
  for (quo in dots) {
    expr <- rlang::quo_get_expr(quo)
    env  <- rlang::quo_get_env(quo)
    curr_ops <- append(curr_ops, list(list(
      type = "filter",
      expr = expr,
      env  = env
    )))
  }
  .data[["ops"]] <- curr_ops
  .data
}

op_select <- function(.data, ...) {
  dots <- rlang::enquos(...)
  cols <- character(0)
  for (quo in dots) {
    expr <- rlang::quo_get_expr(quo)
    if (is.call(expr) && expr[[1]] == "c") {
      cols <- c(cols, as.character(expr[-1]))
    } else {
      cols <- c(cols, as.character(expr))
    }
  }
  nms <- .subset2(.data, ".names")
  missing <- setdiff(cols, nms)
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "))
  }
  curr_ops <- .subset2(.data, "ops")
  curr_ops <- append(curr_ops, list(list(
    type = "select",
    cols = cols
  )))
  .data[["ops"]] <- curr_ops
  .data[[".names"]] <- cols
  .data
}

op_mutate <- function(.data, ...) {
  dots <- rlang::enquos(...)
  curr_ops <- .subset2(.data, "ops")
  curr_ops <- append(curr_ops, list(list(
    type = "mutate",
    dots = dots
  )))
  .data[["ops"]] <- curr_ops
  nms <- .subset2(.data, ".names")
  dot_names <- names(dots)
  for (i in seq_along(dots)) {
    nm <- dot_names[i]
    if (is.null(nm) || nm == "") nm <- rlang::quo_name(dots[[i]])
    if (!nm %in% nms) {
      nms <- c(nms, nm)
    }
  }
  .data[[".names"]] <- nms
  .data
}

op_summarise <- function(.data, ...) {
  dots <- rlang::enquos(...)
  new_names <- names(dots)
  if (is.null(new_names)) new_names <- rep("", length(dots))
  unnamed <- new_names == ""
  if (any(unnamed)) {
    for (i in which(unnamed)) {
      new_names[i] <- rlang::quo_name(dots[[i]])
    }
  }
  curr_ops <- .subset2(.data, "ops")
  curr_ops <- append(curr_ops, list(list(
    type = "summarise",
    dots = dots,
    new_names = new_names
  )))
  .data[["ops"]] <- curr_ops
  .data[[".names"]] <- new_names
  .data
}

op_group_by <- function(.data, ...) {
  dots <- rlang::enquos(...)
  grp_cols <- character(0)
  for (quo in dots) {
    expr <- rlang::quo_get_expr(quo)
    grp_cols <- c(grp_cols, as.character(expr))
  }
  curr_ops <- .subset2(.data, "ops")
  curr_ops <- append(curr_ops, list(list(
    type = "group_by",
    cols = grp_cols
  )))
  .data[["ops"]] <- curr_ops
  .data
}

op_ungroup <- function(.data) {
  curr_ops <- .subset2(.data, "ops")
  curr_ops <- append(curr_ops, list(list(
    type = "ungroup"
  )))
  .data[["ops"]] <- curr_ops
  .data
}

op_arrange <- function(.data, ...) {
  dots <- rlang::enquos(...)
  cols <- list()
  for (quo in dots) {
    expr <- rlang::quo_get_expr(quo)
    env  <- rlang::quo_get_env(quo)
    desc <- FALSE
    if (is.call(expr) && expr[[1]] == "desc") {
      desc <- TRUE
      expr <- expr[[2]]
    }
    cols <- c(cols, list(list(
      col = as.character(expr),
      desc = desc
    )))
  }
  curr_ops <- .subset2(.data, "ops")
  curr_ops <- append(curr_ops, list(list(
    type = "arrange",
    cols = cols
  )))
  .data[["ops"]] <- curr_ops
  .data
}

# -------------------------------------------------------------------------
# Query builder
# -------------------------------------------------------------------------

build_query <- function(x, count = FALSE, limit = NULL, offset = NULL,
                        select_cols = NULL) {
  table <- .subset2(x, "table")
  names <- .subset2(x, ".names")
  ops   <- .subset2(x, "ops")

  current_cols <- names
  pre_summary_cols <- NULL
  pre_summary_exprs <- NULL
  group_cols <- character(0)
  is_grouped <- FALSE
  is_summary <- FALSE

  where_clauses  <- character(0)
  order_clauses  <- character(0)
  having_clauses <- character(0)
  select_exprs   <- NULL
  is_distinct    <- FALSE

  for (op in ops) {
    switch(op$type,
      filter = {
        sql <- expr_to_sql(op$expr, op$env)
        where_clauses <- c(where_clauses, sql)
      },

      select = {
        current_cols <- op$cols
        select_exprs <- NULL
      },

      mutate = {
        dot_names <- names(op$dots)
        for (i in seq_along(op$dots)) {
          quo <- op$dots[[i]]
          nm <- dot_names[i]
          if (is.null(nm) || nm == "") nm <- rlang::quo_name(quo)
          expr <- rlang::quo_get_expr(quo)
          env  <- rlang::quo_get_env(quo)
          sql  <- expr_to_sql(expr, env)
          if (is.null(select_exprs)) select_exprs <- list()
          select_exprs <- c(select_exprs, list(list(
            name = nm,
            sql  = sql,
            type = "mutate"
          )))
          current_cols <- unique(c(current_cols, nm))
        }
      },

      summarise = {
        is_summary <- TRUE
        pre_summary_cols <- current_cols
        pre_summary_exprs <- select_exprs
        if (is.null(select_exprs)) select_exprs <- list()
        summary_names <- op$new_names
        for (i in seq_along(op$dots)) {
          quo <- op$dots[[i]]
          nm  <- summary_names[i]
          if (nm == "") nm <- paste0("col_", i)
          expr <- rlang::quo_get_expr(quo)
          env  <- rlang::quo_get_env(quo)
          sql  <- expr_to_sql(expr, env)
          select_exprs <- c(select_exprs, list(list(
            name = nm,
            sql  = sql,
            type = "summarise"
          )))
        }
        current_cols <- summary_names
      },

      group_by = {
        group_cols <- op$cols
        is_grouped <- TRUE
      },

      ungroup = {
        group_cols <- character(0)
        is_grouped <- FALSE
      },

      arrange = {
        for (col_info in op$cols) {
          dir <- if (col_info$desc) "DESC" else "ASC"
          order_clauses <- c(order_clauses, paste0(col_info$col, " ", dir))
        }
      }
    )
  }

  # Build the inner query (before summarise wrapping)
  if (is_summary && !is.null(select_exprs)) {
    # Summarise with existing select_exprs: wrap in subquery
    # First build the inner SELECT with mutate columns
    has_mutate <- any(vapply(ops, function(o) identical(o$type, "mutate"), logical(1)))
    has_select <- any(vapply(ops, function(o) identical(o$type, "select"), logical(1)))

    if (has_mutate && !is.null(pre_summary_exprs)) {
      # Wrap in a subquery so computed columns are available
      # For columns that came from mutate, use the expression; otherwise quote the name
      expr_names <- vapply(pre_summary_exprs, `[[`, character(1), "name")
      inner_cols <- character(0)
      for (col in pre_summary_cols) {
        if (col %in% expr_names) {
          idx <- which(expr_names == col)
          inner_cols <- c(inner_cols, paste0(pre_summary_exprs[[idx]]$sql, " AS \"", col, "\""))
        } else {
          inner_cols <- c(inner_cols, paste0("\"", col, "\""))
        }
      }
      inner_select <- paste(inner_cols, collapse = ", ")

      inner_sql <- paste0("SELECT ", inner_select, " FROM \"", table, "\"")
      if (length(where_clauses) > 0) {
        inner_sql <- paste0(inner_sql, " WHERE ",
                            paste(where_clauses, collapse = " AND "))
      }

      # Now build summary SELECT on top
      all_cols <- character(0)
      for (gc in group_cols) {
        all_cols <- c(all_cols, paste0("\"", gc, "\""))
      }
      for (e in select_exprs) {
        all_cols <- c(all_cols, paste0(e$sql, " AS \"", e$name, "\""))
      }
      select_part <- paste(all_cols, collapse = ", ")

      sql <- paste0("SELECT ", select_part, " FROM (", inner_sql, ") AS _inner")

      if (length(group_cols) > 0) {
        sql <- paste0(sql, " GROUP BY ",
                      paste0("\"", group_cols, "\"", collapse = ", "))
      }
    } else {
      # Simple summarise without preceding mutate
      all_cols <- character(0)
      for (gc in group_cols) {
        all_cols <- c(all_cols, paste0("\"", gc, "\""))
      }
      for (e in select_exprs) {
        all_cols <- c(all_cols, paste0(e$sql, " AS \"", e$name, "\""))
      }
      select_part <- paste(all_cols, collapse = ", ")
      sql <- paste0("SELECT ", select_part, " FROM \"", table, "\"")
      if (length(where_clauses) > 0) {
        sql <- paste0(sql, " WHERE ", paste(where_clauses, collapse = " AND "))
      }
      if (length(group_cols) > 0) {
        sql <- paste0(sql, " GROUP BY ",
                      paste0("\"", group_cols, "\"", collapse = ", "))
      }
    }
  } else if (!is.null(select_exprs)) {
    # Mutate: include existing columns + computed ones
    existing_cols <- setdiff(current_cols,
                             vapply(select_exprs, `[[`, character(1), "name"))
    all_cols <- character(0)
    for (col in existing_cols) {
      all_cols <- c(all_cols, paste0("\"", col, "\""))
    }
    for (e in select_exprs) {
      all_cols <- c(all_cols, paste0(e$sql, " AS \"", e$name, "\""))
    }
    select_part <- paste(all_cols, collapse = ", ")
    sql <- paste0("SELECT ", select_part, " FROM \"", table, "\"")
    if (length(where_clauses) > 0) {
      sql <- paste0(sql, " WHERE ", paste(where_clauses, collapse = " AND "))
    }
  } else {
    if (count) {
      select_part <- "COUNT(*) AS n"
    } else if (!is.null(select_cols)) {
      cols <- paste0("\"", select_cols, "\"")
      select_part <- paste(cols, collapse = ", ")
    } else {
      qnames <- paste0("\"", current_cols, "\"")
      select_part <- paste(qnames, collapse = ", ")
    }

    sql <- paste0("SELECT ", select_part, " FROM \"", table, "\"")

    if (length(where_clauses) > 0) {
      sql <- paste0(sql, " WHERE ", paste(where_clauses, collapse = " AND "))
    }

    if (length(group_cols) > 0) {
      sql <- paste0(sql, " GROUP BY ",
                    paste0("\"", group_cols, "\"", collapse = ", "))
    }
  }

  if (length(order_clauses) > 0) {
    sql <- paste0(sql, " ORDER BY ", paste(order_clauses, collapse = ", "))
  }

  if (!is.null(limit)) {
    sql <- paste0(sql, " LIMIT ", limit)
    if (!is.null(offset)) {
      sql <- paste0(sql, " OFFSET ", offset)
    }
  }

  sql
}
