test_that("filter works", {
  df <- disk_df(mtcars)
  out <- collect(filter(df, mpg > 20))
  expect_equal(nrow(out), sum(mtcars$mpg > 20))
  expect_true(all(out$mpg > 20))

  out2 <- collect(filter(df, mpg > 20, wt < 3))
  expect_equal(nrow(out2), sum(mtcars$mpg > 20 & mtcars$wt < 3))
})

test_that("select works", {
  df <- disk_df(mtcars)
  out <- collect(select(df, mpg, wt))
  expect_equal(names(out), c("mpg", "wt"))
  expect_equal(ncol(out), 2)
  expect_equal(nrow(out), 32)
})

test_that("mutate works", {
  df <- disk_df(mtcars)
  out <- collect(mutate(df, wt_kg = wt * 453.592 / 1000))
  expect_true("wt_kg" %in% names(out))
  expect_equal(out$wt_kg[1], mtcars$wt[1] * 453.592 / 1000)

  out2 <- collect(mutate(df, wt_kg = wt * 453.592 / 1000, mpg2 = mpg^2))
  expect_true(all(c("wt_kg", "mpg2") %in% names(out2)))
})

test_that("summarise works", {
  df <- disk_df(mtcars)
  out <- collect(summarise(df, avg_mpg = mean(mpg), n = n()))
  expect_equal(nrow(out), 1)
  expect_equal(out$avg_mpg, mean(mtcars$mpg))
  expect_equal(out$n, 32)
})

test_that("group_by + summarise works", {
  df <- disk_df(mtcars)
  out <- collect(summarise(group_by(df, cyl), avg_mpg = mean(mpg), n = n()))
  expected <- aggregate(mpg ~ cyl, data = mtcars, FUN = function(x) c(mean = mean(x), n = length(x)))
  expect_equal(nrow(out), 3)  # 4, 6, 8 cylinders
  expect_equal(sort(out$cyl), c(4, 6, 8))
})

test_that("arrange works", {
  df <- disk_df(mtcars)
  out <- collect(arrange(df, mpg))
  expect_true(all(diff(out$mpg) >= 0))
})

test_that("pull works", {
  df <- disk_df(mtcars)
  expect_equal(pull(df, mpg), mtcars$mpg)
})

test_that("glimpse works", {
  df <- disk_df(mtcars)
  expect_output(glimpse(df), "disk_df")
})

test_that("chained operations work", {
  data("iris")
  df <- disk_df(iris)
  result <- df |>
    filter(Species == "setosa") |>
    select(Sepal.Length, Sepal.Width) |>
    mutate(ratio = Sepal.Length / Sepal.Width) |>
    summarise(avg_ratio = mean(ratio))

  out <- collect(result)
  expected <- mean(iris$Sepal.Length[iris$Species == "setosa"] /
                   iris$Sepal.Width[iris$Species == "setosa"])
  expect_equal(out$avg_ratio, expected)
})

test_that("summarize (with z) is an alias for summarise", {
  df <- disk_df(mtcars)
  out1 <- collect(summarise(df, m = mean(mpg)))
  out2 <- collect(summarize(df, m = mean(mpg)))
  expect_equal(out1$m, out2$m)
})
