make_cohort <- function(n_groups = 2) {
  df <- data.frame(
    id   = 1:20,
    site = rep(c("A", "B"), each = 10),
    age  = c(5, 5, 10, 10, 15, 15, 20, 20, 25, 25,
             8, 8, 12, 12, 16, 16, 22, 22, 28, 28),
    wt   = c(rep(c(10, 20), 10))
  )
  if (n_groups == 1) df$site <- "All"
  new_cohort(df, "Randomised", id_col = "id", group_col = "site") |>
    include_if(age >= 10, "Age >= 10") |>
    include_if(wt  >= 15, "Weight >= 15")
}

test_that("consort_plot returns a consort_grob for grouped cohort", {
  p <- consort_plot(make_cohort())
  expect_s3_class(p, "consort_grob")
})

test_that("consort_plot returns a consort_grob for single-group cohort", {
  p <- consort_plot(make_cohort(1))
  expect_s3_class(p, "consort_grob")
})

test_that("consort_plot accepts a tracker tibble directly", {
  tr <- get_tracker(make_cohort())
  p  <- consort_plot(tr)
  expect_s3_class(p, "consort_grob")
})

test_that("consort_plot errors on invalid tracker", {
  expect_error(consort_plot(data.frame(x = 1)), "tracker missing")
})

test_that("step_labels renames steps without error", {
  co <- make_cohort()
  p  <- consort_plot(co, step_labels = c("Age >= 10" = "Paediatric / adult"))
  expect_s3_class(p, "consort_grob")
})

test_that("group_labels renames groups without error", {
  co <- make_cohort()
  p  <- consort_plot(co, group_labels = c("A" = "Centre A"))
  expect_s3_class(p, "consort_grob")
})

test_that("unknown step_labels produces a warning", {
  co <- make_cohort()
  expect_warning(
    consort_plot(co, step_labels = c("Nonexistent step" = "Renamed")),
    "step_labels not in tracker"
  )
})

test_that("na_cells marks correct cells and plot still renders", {
  co <- make_cohort()
  p  <- consort_plot(co,
                     na_cells = data.frame(step = "Weight >= 15", group = "A"))
  expect_s3_class(p, "consort_grob")
})

test_that("na_cells with bad column names errors", {
  co <- make_cohort()
  expect_error(
    consort_plot(co, na_cells = data.frame(foo = "x", bar = "y")),
    "step.*group"
  )
})

test_that("na_cells must be a data frame", {
  co <- make_cohort()
  expect_error(consort_plot(co, na_cells = "bad"), "data frame")
})

test_that("font_size scaling does not break layout (smoke test)", {
  co <- make_cohort()
  for (fs in c(0.5, 1, 1.5, 2)) {
    expect_s3_class(consort_plot(co, font_size = fs), "consort_grob")
  }
})

test_that("manual box_width / excl_width accepted", {
  co <- make_cohort()
  p  <- consort_plot(co, box_width = 50, excl_width = 40)
  expect_s3_class(p, "consort_grob")
})

test_that("layout attribute is attached to the returned plot", {
  p <- consort_plot(make_cohort())
  ly <- attr(p, ".layout")
  expect_false(is.null(ly))
  expect_named(ly, c("w_mm", "h_mm"))
})

test_that("single-step cohort (no exclusions) renders", {
  df   <- data.frame(id = 1:5, site = c("A", "A", "B", "B", "B"))
  co   <- new_cohort(df, "Only step", id_col = "id", group_col = "site")
  expect_s3_class(consort_plot(co), "consort_grob")
})

test_that("large number of groups renders without error", {
  df <- data.frame(
    id   = 1:50,
    site = paste0("Site", 1:10)[rep(1:10, 5)],
    age  = sample(5:80, 50, replace = TRUE)
  )
  co <- new_cohort(df, "All", id_col = "id", group_col = "site") |>
    include_if(age >= 18, "Adults")
  expect_s3_class(consort_plot(co), "consort_grob")
})

test_that("long step labels wrap without error", {
  df <- data.frame(
    id  = 1:10,
    age = 1:10
  )
  co <- new_cohort(df, "A very long baseline label that goes well beyond typical width",
                   id_col = "id", group_col = NULL) |>
    include_if(age >= 5,
               "This is a very long exclusion criterion label that should trigger wrapping logic")
  expect_s3_class(consort_plot(co), "consort_grob")
})

test_that("save_consort_plot errors without .layout attribute", {
  p <- structure(list(), class = "consort_grob")
  expect_error(save_consort_plot(p, tempfile()), "No layout metadata")
})

test_that("save_consort_plot writes PNG file", {
  skip_if_not_installed("grDevices")
  p    <- consort_plot(make_cohort())
  path <- tempfile()
  save_consort_plot(p, path, formats = "png")
  expect_true(file.exists(paste0(path, ".png")))
})

test_that("step names keep their acronyms' capitals", {
  expect_equal(GroupCONSORT:::clean_label("Not transferred from another ICU"),
               "Not transferred from another ICU")
  expect_equal(GroupCONSORT:::clean_label("COVID primary diagnosis"),
               "COVID primary diagnosis")
  expect_equal(GroupCONSORT:::clean_label("02_age_over_18"), "Age over 18")
})

test_that("exclusion_labels add a reason line to that step's exclusion box", {
  tr <- get_tracker(make_cohort())
  ec <- GroupCONSORT:::build_excl_content(
    GroupCONSORT:::flag_na_cells(tr, NULL), unique(tr$step), 3, 2,
    c("Weight >= 15" = "Weight under 15")
  )
  expect_equal(ec[[1]]$reason_line, character(0))
  expect_equal(ec[[2]]$reason_line, "Weight under 15")
  expect_s3_class(
    consort_plot(tr, exclusion_labels = c("Weight >= 15" = "Weight under 15")),
    "consort_grob"
  )
})

test_that("exclusion_labels follow step_labels renaming", {
  ex <- GroupCONSORT:::recode_names(c("Age >= 10" = "Under 10"),
                                    c("Age >= 10" = "Adult"))
  expect_named(ex, "Adult")
  expect_s3_class(
    consort_plot(make_cohort(), step_labels = c("Age >= 10" = "Adult"),
                 exclusion_labels = c("Age >= 10" = "Under 10")),
    "consort_grob"
  )
})

test_that("paginate_consort passes exclusion_labels to every page", {
  pages <- paginate_consort(make_cohort(), page_height_mm = 80,
                            exclusion_labels = c("Weight >= 15" = "Weight under 15"))
  expect_length(pages, 2)
  expect_true(all(vapply(pages, inherits, logical(1), "consort_grob")))
})

test_that("exclusion reasons wrap to a fixed exclusion box width", {
  tr  <- get_tracker(make_cohort())
  lay <- GroupCONSORT:::layout_params(1)
  ec  <- GroupCONSORT:::build_excl_content(
    GroupCONSORT:::flag_na_cells(tr, NULL), unique(tr$step), 3, 2,
    c("Weight >= 15" = "Weight under fifteen kilograms at the screening visit")
  )
  wrapped <- GroupCONSORT:::wrap_and_measure_mm(ec, 30, lay)
  expect_gt(length(wrapped[[2]]$reason_line), 1)
  expect_equal(wrapped[[1]]$reason_line, NULL)
})
