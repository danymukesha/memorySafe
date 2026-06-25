test_that("memory_safe_mode toggles on and off", {
  old <- memory_safe_mode(FALSE)
  expect_false(getOption("memorySafe.active", FALSE))

  memory_safe_mode(TRUE)
  expect_true(getOption("memorySafe.active", FALSE))

  memory_safe_mode(FALSE)
  expect_false(getOption("memorySafe.active", FALSE))
})

test_that("memory_safe_set_limit works", {
  old <- memory_safe_set_limit(limit_rows = 5000, action = "error")
  expect_equal(getOption("memorySafe.limit_rows"), 5000)
  expect_equal(getOption("memorySafe.action"), "error")

  # Restore
  memory_safe_set_limit(limit_rows = old$limit_rows, action = old$action)
})

test_that("memory_safe_get_limit returns current limits", {
  limits <- memory_safe_get_limit()
  expect_type(limits, "list")
  expect_true("limit_rows" %in% names(limits))
  expect_true("limit_bytes" %in% names(limits))
  expect_true("action" %in% names(limits))
})

test_that("memory_safe_check warns when data is large (but still returns data)", {
  memory_safe_mode(TRUE)
  memory_safe_set_limit(limit_rows = 10, action = "warning")

  df <- disk_df(iris)  # 150 rows, > 10

  expect_warning(
    out <- as.data.frame(df),
    "Memory-safe check"
  )
  expect_equal(nrow(out), 150)

  memory_safe_mode(FALSE)
  memory_safe_set_limit(limit_rows = 100000, action = "warning")
})

test_that("memory_safe_check errors when action is error", {
  memory_safe_mode(TRUE)
  memory_safe_set_limit(limit_rows = 10, action = "error")

  df <- disk_df(iris)

  expect_error(
    as.data.frame(df),
    "Memory-safe check"
  )

  memory_safe_mode(FALSE)
  memory_safe_set_limit(limit_rows = 100000, action = "warning")
})

test_that("memory_safe_check does not warn when limits are high enough", {
  memory_safe_mode(TRUE)
  memory_safe_set_limit(limit_rows = 1000000, action = "warning")

  df <- disk_df(iris)
  expect_no_warning(
    out <- as.data.frame(df)
  )

  memory_safe_mode(FALSE)
})

test_that("memory_safe_mode toggles when called without args", {
  memory_safe_mode(FALSE)
  old <- memory_safe_mode()
  expect_true(getOption("memorySafe.active"))
  memory_safe_mode()
  expect_false(getOption("memorySafe.active"))
})
