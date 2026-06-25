# ============================================================================
# Benchmark: memorySafe vs. In-Memory (base R / dplyr)
# ============================================================================
#
# Demonstrates why memorySafe matters:
#   1. CSV LOADING  — read_disk_csv() streams; read.csv() loads everything
#   2. PIPELINES    — disk_df ops are SQL (no intermediate copies)
#   3. CHUNK MODELS — chunk_map() applies R functions piecewise
#   4. MEMORY SAFE  — memory_safe_mode catches accidental big pulls
# ============================================================================

library(memorySafe)

fmt_n  <- function(x) format(round(x), big.mark = ",")

set.seed(42)

# ---------------------------------------------------------------------------
# Generate synthetic datasets (as CSV files)
# ---------------------------------------------------------------------------
sizes <- c("S" = 1e4, "M" = 1e5, "L" = 5e5, "XL" = 1e7)

dir <- file.path(tempdir(), "memorySafe_bench")
dir.create(dir, showWarnings = FALSE)

csv_files <- list()
for (label in names(sizes)) {
  n <- sizes[[label]]
  path <- file.path(dir, sprintf("data_%s.csv", label))
  if (!file.exists(path)) {
    cat(sprintf("Generating %s (%s rows)...", label, fmt_n(n)))
    df <- data.frame(
      id       = 1:n,
      group    = sample(letters[1:20], n, replace = TRUE),
      category = sample(c("A","B","C","D","E"), n, replace = TRUE),
      x1       = rnorm(n),
      x2       = rnorm(n, mean = 50, sd = 10),
      x3       = runif(n, 0, 100),
      y        = rnorm(n, mean = 10, sd = 3)
    )
    write.csv(df, path, row.names = FALSE)
    cat(sprintf("  (%s)\n", format(utils::object.size(df), units = "auto")))
  }
  csv_files[[label]] <- path
}

# ---------------------------------------------------------------------------
# 1. CSV LOADING BENCHMARK
# ---------------------------------------------------------------------------
cat("\n══════════════════════════════════════════════════════════\n")
cat("BENCHMARK 1: CSV LOADING — memorySafe vs base R\n")
cat("══════════════════════════════════════════════════════════\n")

for (label in names(sizes)) {
  csv <- csv_files[[label]]
  n   <- sizes[[label]]

  # --- memorySafe: streaming load ---
  t <- system.time({
    df_disk <- suppressMessages(read_disk_csv(csv, chunk_size = 5000))
  })
  disk_mem <- utils::object.size(df_disk)

  # --- base R: load all into memory ---
  t2 <- system.time({
    df_ram <- read.csv(csv)
  })
  ram_mem <- utils::object.size(df_ram)

  cat(sprintf("  %s (%7s rows):\n", label, fmt_n(n)))
  cat(sprintf("    read_disk_csv  %5.2f sec  disk_df size: %s\n",
              t["elapsed"], format(disk_mem, units = "auto")))
  cat(sprintf("    read.csv       %5.2f sec  data.frame:    %s\n",
              t2["elapsed"], format(ram_mem, units = "auto")))
}

# ---------------------------------------------------------------------------
# 2. dplyr PIPELINE BENCHMARK (disk_df vs in-memory)
# ---------------------------------------------------------------------------
cat("\n══════════════════════════════════════════════════════════\n")
cat("BENCHMARK 2: dplyr PIPELINE (filter → group → summarise)\n")
cat("══════════════════════════════════════════════════════════\n")

pipeline <- function(.data) {
  .data |>
    filter(x1 > 0, category %in% c("A", "B")) |>
    group_by(group) |>
    summarise(avg_x1 = mean(x1), avg_y = mean(y), count = n()) |>
    arrange(desc(avg_x1))
}

has_dplyr <- requireNamespace("dplyr", quietly = TRUE)

for (label in names(sizes)) {
  csv <- csv_files[[label]]
  n   <- sizes[[label]]

  df_disk <- suppressMessages(read_disk_csv(csv, chunk_size = 5000))
  df_ram  <- read.csv(csv)

  # --- memorySafe pipeline ---
  t_disk <- system.time({
    res_disk <- pipeline(df_disk) |> collect()
  })

  # --- In-memory dplyr pipeline ---
  if (has_dplyr) {
    t_ram <- system.time({
      res_ram <- df_ram |>
        dplyr::filter(x1 > 0, category %in% c("A", "B")) |>
        dplyr::group_by(group) |>
        dplyr::summarise(avg_x1 = mean(x1), avg_y = mean(y), count = dplyr::n()) |>
        dplyr::arrange(dplyr::desc(avg_x1))
    })
  }

  cat(sprintf("  %s (%7s rows):\n", label, fmt_n(n)))
  cat(sprintf("    memorySafe pipeline:  %5.2f sec  (SQL — zero copies)\n",
              t_disk["elapsed"]))
  if (has_dplyr) {
    cat(sprintf("    In-memory dplyr:      %5.2f sec  (R — copies each step)\n",
                t_ram["elapsed"]))
  }
}

cat("\n  → With larger data, in-memory dplyr creates 2-3 intermediate\n")
cat("    data.frame copies, multiplying memory pressure.\n")

# ---------------------------------------------------------------------------
# 3. CHUNK PROCESSING BENCHMARK
# ---------------------------------------------------------------------------
cat("\n══════════════════════════════════════════════════════════\n")
cat("BENCHMARK 3: CHUNK PROCESSING (per-chunk linear models)\n")
cat("══════════════════════════════════════════════════════════\n")

for (label in names(sizes)) {
  csv <- csv_files[[label]]
  n   <- sizes[[label]]

  df_disk <- suppressMessages(read_disk_csv(csv, chunk_size = 5000))

  t <- system.time({
    res <- chunk_map(df_disk, function(chunk) {
      m <- lm(y ~ x1 + x2 + x3, data = chunk)
      data.frame(n_rows = nrow(chunk), rsq = summary(m)$r.squared,
                 coef_x1 = coef(m)[["x1"]])
    }, .chunk_size = 50000)
  })

  cat(sprintf("  %s (%7s rows):  chunk_map → %d chunks  %5.2f sec\n",
              label, fmt_n(n), nrow(res), t["elapsed"]))
}

cat("\n  → chunk_map never holds more than .chunk_size rows in RAM.\n")
cat("    A 1B-row dataset works the same way as 1K rows.\n")

# ---------------------------------------------------------------------------
# 4. MEMORY-SAFE MODE DEMO
# ---------------------------------------------------------------------------
cat("\n══════════════════════════════════════════════════════════\n")
cat("BENCHMARK 4: MEMORY-SAFE MODE PREVENTS ACCIDENTS\n")
cat("══════════════════════════════════════════════════════════\n")

# Create a medium-sized disk_df for the demo
big <- disk_df(data.frame(id = 1:5e5, value = rnorm(5e5)))

memory_safe_mode(TRUE)

cat('  as.data.frame() on 500k rows with default limits (100k rows):\n')
tryCatch(
  invisible(as.data.frame(big)),
  warning = function(w) cat("    ⚠  ", conditionMessage(w), "\n")
)

memory_safe_set_limit(limit_rows = 1000, action = "error")
cat('  as.data.frame() with limit=1000, action="error":\n')
tryCatch(
  as.data.frame(big),
  error = function(e) cat("    ⛔  ", conditionMessage(e), "\n")
)

memory_safe_mode(FALSE)
memory_safe_set_limit(limit_rows = 100000, action = "warning")

# ---------------------------------------------------------------------------
# SUMMARY
# ---------------------------------------------------------------------------
cat("\n══════════════════════════════════════════════════════════\n")
cat("WHY USE memorySafe?\n")
cat("══════════════════════════════════════════════════════════\n")
cat("
  FEATURE              memorySafe              In-memory
  ───────────────────────────────────────────────────────────
  CSV loading          Streams (chunked)       Entire file in RAM
  dplyr pipelines      SQL (no copies)         Intermediate copies
  Data > RAM           Works                   Crashes / swaps
  Complex models       chunk_map() piecewise   Must fit in RAM
  Safety net           memory_safe_mode()      No protection

  Bottom line: memorySafe lets you work with datasets that
  would otherwise crash R — without changing your workflow.\n")
