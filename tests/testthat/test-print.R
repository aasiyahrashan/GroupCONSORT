test_that("print.cohort produces output without error", {
  df <- data.frame(id = 1:10, site = rep(c("A", "B"), 5), age = 10:19)
  co <- new_cohort(df, "All", id_col = "id", group_col = "site") |>
    include_if(age >= 15, "Age >= 15")
  out <- capture.output(print(co))
  expect_true(any(grepl("<cohort>", out)))
  expect_true(any(grepl("step", out)))
})
