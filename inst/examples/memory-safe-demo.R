# ============================================================================
# memorySafe Demo: Why you want a disk-backed data.frame
# ============================================================================
#
# The problem: R loads everything into RAM. A 2 GB CSV will crash your
# laptop if you try read.csv() on it. Even medium data (50 million rows)
# can exceed available memory, especially when dplyr creates intermediate
# copies.
#
# memorySafe stores your data in SQLite under the hood. dplyr verbs are
# translated to SQL and executed lazily -- the data never enters R's
# memory unless you explicitly collect() it.
#
# The "memory-safe mode" adds a safety net: before pulling data into R,
# it checks the size and warns or stops if it exceeds your configured
# limits. This prevents the dreaded "cannot allocate vector of size..."
# crash after a long computation.

library(memorySafe)

# ---------------------------------------------------------------------------
# 1. Basic usage -- works like a tibble, lives on disk
# ---------------------------------------------------------------------------

df <- disk_df(mtcars)
df                              # printed lazily -- only 10 rows shown
dim(df)                         # computed via SELECT COUNT(*)
names(df)                       # column names from the schema

# ---------------------------------------------------------------------------
# 2. dplyr verbs stay on disk
# ---------------------------------------------------------------------------

# These never load full data into R:
filtered <- filter(df, mpg > 20, cyl == 6)
selected <- select(filtered, mpg, wt, hp)
mutated  <- mutate(selected, hp_per_cyl = hp / cyl)

# Still just SQL operations queued up:
mutated

# ---------------------------------------------------------------------------
# 3. collect() is where data materializes
# ---------------------------------------------------------------------------

result <- collect(mutated)
class(result)      # a regular tibble in memory
dim(result)

# ---------------------------------------------------------------------------
# 4. Chained dplyr pipeline (all on disk until collect)
# ---------------------------------------------------------------------------

avg_by_cyl <- df |>
  filter(mpg > 15) |>
  group_by(cyl) |>
  summarise(
    avg_mpg  = mean(mpg),
    avg_wt   = mean(wt),
    n        = n()
  ) |>
  arrange(desc(avg_mpg)) |>
  collect()

print(avg_by_cyl)

# ---------------------------------------------------------------------------
# 5. Working with columns directly (also lazy)
# ---------------------------------------------------------------------------

# $ and [[ pull individual vectors -- each is one SQL query:
df$mpg[1:10]
df[["wt"]][1:10]

# ---------------------------------------------------------------------------
# 6. Memory-safe mode -- the killer feature
# ---------------------------------------------------------------------------

# Enable it:
memory_safe_mode(TRUE)

# Create a moderately-sized disk-backed dataset:
big_data <- disk_df(data.frame(
  id    = 1:500000,
  value = rnorm(500000)
))

# With default limits (100K rows), this will WARN:
suppressWarnings(
  result <- as.data.frame(big_data)
)
# "Memory-safe check: as.data.frame would load 500,000 rows (limit: 100,000)."

# You can also set it to ERROR instead:
memory_safe_set_limit(limit_rows = 1000, action = "error")
try(as.data.frame(big_data))   # stops with clear message

# Increase limits for legit operations:
memory_safe_set_limit(limit_rows = 1e6, action = "warning")
result <- as.data.frame(big_data)   # no warning, within limits
cat("Loaded", nrow(result), "rows OK\n")

# Turn off safe mode when you know what you're doing:
memory_safe_mode(FALSE)

# ---------------------------------------------------------------------------
# 7. Chunk processing -- for models that need in-memory data
# ---------------------------------------------------------------------------

# Some operations can't be expressed in SQL (complex models, plots, etc.)
# chunk_map applies a function to the data in manageable pieces:

df <- disk_df(mtcars)

# Example: compute a separate linear model per cylinder group
models <- chunk_map(df, function(chunk) {
  m <- lm(mpg ~ wt, data = chunk)
  data.frame(
    cyl       = chunk$cyl[1],
    coef_wt   = coef(m)[["wt"]],
    rsq       = summary(m)$r.squared,
    n         = nrow(chunk)
  )
}, .chunk_size = 10)

print(models)

# ---------------------------------------------------------------------------
# 8. Comparing with in-memory: when you'd crash
# ---------------------------------------------------------------------------

# Suppose you have a 20 GB CSV. With base R:
#   d <- read.csv("20gb.csv")          # CRASH -- needs >20 GB RAM
#   d <- data.table::fread("20gb.csv") # CRASH -- same problem
#
# With memorySafe:
#   d <- read_disk_csv("20gb.csv")     # OK -- streams through in chunks
#   summary <- d |>                     # OK -- all SQL
#     filter(date > "2020-01-01") |>
#     group_by(category) |>
#     summarise(total = sum(sales)) |>
#     collect()                         # Only the 10-row result in RAM
#
# memory_safe_mode(TRUE)               # Safety net: if your collect()
#                                        # would pull too much, it warns
#                                        # before you run out of memory
