# [memorySafe:] Why you need a disk-backed data frame

R usually loads everything in RAM. This is a problem, for example,
a 2 GB CSV will crash your laptop if you try `read.csv()` on it.
Even medium data (50 million rows) can exceed available memory, 
especially when `dplyr` creates intermediate copies. 

`memorySafe` stores your data in SQLite under the hood. 
`dplyr` verbs are trasnlated to SQL and execulted lazily,
i.e. the data never enteres R´s memory unless you explicitly `collect()` it.

The "memory-safe mode" adds a safety net: before pulling data into R,
it checks the size and warns or stops if it exceeds your configured
limits. This prevents the dreaded "cannot allocate vector of size..."
crash after a long computation. 

## Current status: 

under-development..

## Benchmark: memorySafe vs. In-Memory (base R / dplyr)

```r
> source("./inst/examples/benchmark.R")
Generating S (10,000 rows)...  (510.6 Kb)
Generating M (1e+05 rows)...  (5 Mb)
Generating L (5e+05 rows)...  (24.8 Mb)

══════════════════════════════════════════════════════════
BENCHMARK 1: CSV LOADING — memorySafe vs base R
══════════════════════════════════════════════════════════
  S ( 10,000 rows):
    read_disk_csv   0.11 sec  disk_df size: 3.5 Kb
    read.csv        0.03 sec  data.frame:    510.6 Kb
  M (  1e+05 rows):
    read_disk_csv   1.91 sec  disk_df size: 3.5 Kb
    read.csv        0.46 sec  data.frame:    5 Mb
  L (  5e+05 rows):
    read_disk_csv  30.61 sec  disk_df size: 3.5 Kb
    read.csv        5.33 sec  data.frame:    24.8 Mb

══════════════════════════════════════════════════════════
BENCHMARK 2: dplyr PIPELINE (filter → group → summarise)
══════════════════════════════════════════════════════════
  S ( 10,000 rows):
    memorySafe pipeline:   0.02 sec  (SQL — zero copies)
    In-memory dplyr:       0.01 sec  (R — copies each step)
  M (  1e+05 rows):
    memorySafe pipeline:   0.13 sec  (SQL — zero copies)
    In-memory dplyr:       0.02 sec  (R — copies each step)
  L (  5e+05 rows):
    memorySafe pipeline:   0.13 sec  (SQL — zero copies)
    In-memory dplyr:       0.03 sec  (R — copies each step)

  → With larger data, in-memory dplyr creates 2-3 intermediate
    data.frame copies, multiplying memory pressure.

══════════════════════════════════════════════════════════
BENCHMARK 3: CHUNK PROCESSING (per-chunk linear models)
══════════════════════════════════════════════════════════
  S ( 10,000 rows):  chunk_map → 1 chunks   0.06 sec
  M (  1e+05 rows):  chunk_map → 2 chunks   0.18 sec
  L (  5e+05 rows):  chunk_map → 10 chunks   1.14 sec

  → chunk_map never holds more than .chunk_size rows in RAM.
    A 1B-row dataset works the same way as 1K rows.

══════════════════════════════════════════════════════════
BENCHMARK 4: MEMORY-SAFE MODE PREVENTS ACCIDENTS
══════════════════════════════════════════════════════════
memorySafe: memory-safe mode is now ON.
  Rows limit: 1e+05
  Bytes limit: 1e+08 (100 MB)
  Action: warning
  as.data.frame() on 500k rows with default limits (100k rows):
    ⚠   Memory-safe check: as.data.frame would load 500,000 rows (limit: 1e+05).
Proceeding anyway. Set options(memorySafe.action = "error") to stop instead. 
  as.data.frame() with limit=1000, action="error":
    ⛔   Memory-safe check: as.data.frame would load 500,000 rows (limit: 1,000).
Use chunk_map() to process this data in pieces, or increase the limit with memory_safe_set_limit(). 
memorySafe: memory-safe mode is OFF.

══════════════════════════════════════════════════════════
WHY USE memorySafe?
══════════════════════════════════════════════════════════

  FEATURE              memorySafe              In-memory
  ───────────────────────────────────────────────────────────
  CSV loading          Streams (chunked)       Entire file in RAM
  dplyr pipelines      SQL (no copies)         Intermediate copies
  Data > RAM           Works                   Crashes / swaps
  Complex models       chunk_map() piecewise   Must fit in RAM
  Safety net           memory_safe_mode()      No protection

  Bottom line: memorySafe lets you work with datasets that
  would otherwise crash R — without changing your workflow.
Warnmeldung:
call dbDisconnect() when finished working with a connection 
```
