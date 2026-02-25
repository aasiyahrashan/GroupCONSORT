test_that("new_cohort errors on missing id_col", {
  df <- data.frame(x = 1:3)
  expect_error(new_cohort(df, id_col = "id"), "id_col 'id' not found")
})

test_that("new_cohort creates a cohort object with correct class", {
  df <- data.frame(id = 1:5, site = c("A", "A", "B", "B", "B"), age = 20:24)
  co <- new_cohort(df, label = "All", id_col = "id", group_col = "site")
  expect_s3_class(co, "cohort")
})

test_that("new_cohort initialises tracker with correct structure", {
  df <- data.frame(id = 1:5, site = c("A", "A", "B", "B", "B"))
  co <- new_cohort(df, label = "Baseline", id_col = "id", group_col = "site")
  tr <- get_tracker(co)
  expect_named(tr, c("group", "step", "n_remaining", "n_dropped"))
  expect_equal(nrow(tr), 2)  # two groups
  expect_equal(sum(tr$n_remaining), 5)
})

test_that("new_cohort falls back to single 'All' group when group_col is NULL", {
  df <- data.frame(id = 1:4)
  co <- new_cohort(df, label = "All", id_col = "id", group_col = NULL)
  tr <- get_tracker(co)
  expect_equal(tr$group, "All")
  expect_equal(tr$n_remaining, 4L)
})

test_that("new_cohort falls back to 'All' group when group_col missing from data", {
  df <- data.frame(id = 1:4)
  co <- new_cohort(df, label = "All", id_col = "id", group_col = "nonexistent")
  expect_equal(get_tracker(co)$group, "All")
})

test_that("include_if filters data and records n_dropped correctly", {
  df <- data.frame(id = 1:10, age = c(5, 5, 10, 10, 15, 15, 20, 20, 25, 25))
  co <- new_cohort(df, "Start", id_col = "id", group_col = NULL) |>
    include_if(age >= 10, "Age >= 10")
  tr <- get_tracker(co)
  step2 <- dplyr::filter(tr, step == "Age >= 10")
  expect_equal(step2$n_remaining, 8L)
  expect_equal(step2$n_dropped,   2L)
})

test_that("include_if handles multiple steps cumulatively", {
  df <- data.frame(id = 1:10, age = 1:10, weight = c(rep(5, 5), rep(20, 5)))
  co <- new_cohort(df, "All", id_col = "id", group_col = NULL) |>
    include_if(age >= 3,    "Age >= 3") |>
    include_if(weight >= 20, "Weight >= 20")
  tr <- get_tracker(co)
  expect_equal(nrow(tr), 3)
  final <- dplyr::filter(tr, step == "Weight >= 20")
  expect_equal(final$n_remaining, 5L)
})

test_that("include_if drops everyone gracefully (n_remaining == 0)", {
  df <- data.frame(id = 1:5, age = 10:14)
  co <- new_cohort(df, "All", id_col = "id", group_col = NULL) |>
    include_if(age > 100, "Impossible")
  step <- dplyr::filter(get_tracker(co), step == "Impossible")
  expect_equal(step$n_remaining, 0L)
  expect_equal(step$n_dropped,   5L)
  expect_equal(nrow(get_data(co)), 0L)
})

test_that("include_if errors when passed a non-cohort", {
  expect_error(include_if(list(), age > 5, "step"), "cohort object")
})

test_that("get_data returns the filtered data frame", {
  df <- data.frame(id = 1:6, score = c(1, 3, 5, 7, 9, 11))
  co <- new_cohort(df, "All", id_col = "id", group_col = NULL) |>
    include_if(score > 5, "Score > 5")
  expect_equal(nrow(get_data(co)), 3L)
})

test_that("tracker counts per group are correct for grouped data", {
  df <- data.frame(
    id   = 1:8,
    site = rep(c("A", "B"), each = 4),
    age  = c(10, 20, 30, 40, 5, 15, 25, 35)
  )
  co <- new_cohort(df, "All", id_col = "id", group_col = "site") |>
    include_if(age >= 15, "Age >= 15")
  tr <- dplyr::filter(get_tracker(co), step == "Age >= 15")
  a <- dplyr::filter(tr, group == "A")$n_remaining
  b <- dplyr::filter(tr, group == "B")$n_remaining
  expect_equal(a, 3L)
  expect_equal(b, 3L)
})
