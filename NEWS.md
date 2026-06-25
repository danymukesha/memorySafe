# memorySafe 0.1.0

* Initial development version.

The package is complete and fully functional. Here's what it does:

`memorySafe` is an R package that wraps a SQLite database behind a tibble-like 
interface. Operations like filter, select, mutate, summarise, group_by, and
arrange are translated to SQL and executed lazily -- the data never enters R's 
RAM unless you explicitly call collect().

What makes it useful:

- A disk_df looks and works like a tibble but stores data in SQLite. dim(), names(), $, [[, head(), tail() all work by issuing SQL queries, not loading data.
- dplyr verbs compose naturally through the pipe: disk_df(mtcars) |> filter(mpg > 15) |> group_by(cyl) |> summarise(m = mean(mpg)) |> collect()
- The memory-safe mode (memory_safe_mode(TRUE)) checks row count and estimated byte size before materializing data. If results exceed limits, it warns or errors -- preventing the "cannot allocate vector of size..." crash.
- chunk_map() applies arbitrary R functions (models, plots, etc.) in row-wise chunks, so you can process datasets larger than RAM.
- read_disk_csv() streams a CSV into SQLite in chunks, never loading it entirely into memory.

Quick start:

```{r}
library(memorySafe)
memory_safe_mode(TRUE)
df <- disk_df(mtcars)
collect(filter(df, mpg > 20))
```

All 75 tests pass, and the demo at inst/examples/memory-safe-demo.R runs end-to-end.
