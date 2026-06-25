test_that("chunk_map processes data correctly", {
  df <- disk_df(mtcars)
  result <- chunk_map(df, function(chunk) {
    data.frame(
      mean_mpg = mean(chunk$mpg),
      n = nrow(chunk)
    )
  }, .chunk_size = 10)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)  # 32 rows / 10 = 4 chunks
  expect_equal(sum(result$n), 32)
  expect_equal(mean(result$mean_mpg), mean(mtcars$mpg), tolerance = 0.02)
})

test_that("chunk_map with .combine = 'list' returns list", {
  df <- disk_df(mtcars)
  result <- chunk_map(df, function(chunk) {
    mean(chunk$mpg)
  }, .chunk_size = 10, .combine = "list")
  expect_type(result, "list")
  expect_equal(length(result), 4)
})

test_that("chunk_split returns list of disk_dfs", {
  df <- disk_df(mtcars)
  chunks <- chunk_split(df, .chunk_size = 10)
  expect_type(chunks, "list")
  expect_equal(length(chunks), 4)
  expect_s3_class(chunks[[1]], "disk_df")
})

test_that("chunk_split chunks have correct sizes", {
  df <- disk_df(mtcars)
  chunks <- chunk_split(df, .chunk_size = 10)
  sizes <- sapply(chunks, nrow)
  expect_equal(sizes, c(10, 10, 10, 2))
})

test_that("chunk_split chunks can be collected", {
  df <- disk_df(mtcars)
  chunks <- chunk_split(df, .chunk_size = 10)
  first <- collect(chunks[[1]])
  expect_equal(nrow(first), 10)
  expect_equal(first$mpg, mtcars$mpg[1:10])
})
