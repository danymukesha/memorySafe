test_that("disk_df creation and basic properties", {
  df <- disk_df(mtcars)
  expect_s3_class(df, "disk_df")
  expect_equal(nrow(df), 32)
  expect_equal(ncol(df), 11)
  expect_equal(names(df), names(mtcars))
})

test_that("print.disk_df works without error", {
  df <- disk_df(mtcars)
  expect_output(print(df), "A disk_df")
  expect_output(print(df), "32 rows")
})

test_that("as.data.frame.disk_df returns original data", {
  df <- disk_df(mtcars)
  out <- as.data.frame(df)
  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), dim(mtcars))
  expect_equal(out$mpg, mtcars$mpg)
})

test_that("as_tibble.disk_df returns a tibble", {
  df <- disk_df(mtcars)
  out <- tibble::as_tibble(df)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 32)
})

test_that("head and tail work", {
  df <- disk_df(mtcars)
  h <- head(df, 5)
  expect_equal(nrow(h), 5)
  t <- tail(df, 3)
  expect_equal(nrow(t), 3)
})

test_that("subsetting with $ and [[ works", {
  df <- disk_df(mtcars)
  expect_equal(df$mpg, mtcars$mpg)
  expect_equal(df[["mpg"]], mtcars$mpg)
  expect_equal(df[[1]], mtcars[[1]])
})

test_that("is.disk_df works", {
  expect_true(is.disk_df(disk_df(mtcars)))
  expect_false(is.disk_df(mtcars))
})

test_that("dim.disk_df without pending ops uses cached dims", {
  df <- disk_df(mtcars)
  expect_equal(dim(df), c(32, 11))
})

test_that("str.disk_df works", {
  df <- disk_df(mtcars)
  expect_output(str(df), "disk_df")
})

test_that("nrow S3 method works", {
  df <- disk_df(mtcars)
  expect_equal(nrow(df), 32)
})

test_that("as.disk_df dispatches correctly", {
  df <- as.disk_df(mtcars)
  expect_s3_class(df, "disk_df")
})
