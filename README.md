# [memorySafe:] Why you need a disk-backed data frame

R usually loads everything in RAM. This is a problem, for example,
A 2 GB CSV will crash your laptop if you try read.csv() on it.
Even medum data (50 million rows) can exceed available memory, 
especially when `dplyr` creates intermediate copies. 

`memerySafe` stores your data in SQLite under the hood. 
`dplyr` verbs are trasnlated to SQL and execulted lazily,
i.e. the data never enteres R´s memoer unless you explcitly `collect()` it.

The "memory-safe mode" adds a safety net: before pulling data into R,
it checks the size and warns or stops if it exceeds your configured
limits. This prevents the dreaded "cannot allocate vector of size..."
crash after a long computation. 

## Current status: 

under-development..
