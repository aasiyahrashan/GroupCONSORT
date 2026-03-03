#' Merge tracker tibbles from independently-processed datasets
#'
#' Combines two or more tracker tibbles produced in separate environments or
#' from separate datasets into a single tracker ready for [consort_plot()].
#' Groups that are missing a step (because that criterion did not apply to
#' their dataset) are carried forward at their last known count and flagged
#' automatically in the returned `na_cells` data frame.
#'
#' @param ... Two or more tracker tibbles from [get_tracker()]. Each must have
#'   columns `group`, `step`, `n_remaining`, `n_dropped`.
#' @param step_order Optional character vector giving the desired display order
#'   of steps. Defaults to the order steps are first encountered across all
#'   trackers.
#' @param group_order Optional character vector giving the desired display order
#'   of groups. Defaults to the order groups are first encountered.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`tracker`}{A single tracker tibble with columns `group`, `step`,
#'       `n_remaining`, `n_dropped`, suitable for passing to [consort_plot()].}
#'     \item{`na_cells`}{A data frame with columns `step` and `group`
#'       identifying step–group combinations that did not apply to a group's
#'       source dataset. Pass this directly to the `na_cells` argument of
#'       [consort_plot()].}
#'   }
#' @export
#'
#' @examples
#' cgd <- prep_cgd_example()
#'
#' # Two separate datasets processed independently
#' tracker_eu <- cgd |>
#'   dplyr::filter(region == "Europe") |>
#'   new_cohort("Randomised", id_col = "id", group_col = "region") |>
#'   include_if(age >= 5,     "Age >= 5 years") |>
#'   include_if(weight >= 15, "Weight >= 15 kg") |>
#'   get_tracker()
#'
#' tracker_na <- cgd |>
#'   dplyr::filter(region == "North America") |>
#'   new_cohort("Randomised", id_col = "id", group_col = "region") |>
#'   include_if(age >= 5, "Age >= 5 years") |>
#'   get_tracker()  # weight step not run for this dataset
#'
#' result <- merge_trackers(tracker_eu, tracker_na)
#' \dontrun{
#' consort_plot(result$tracker, na_cells = result$na_cells)
#' }
merge_trackers <- function(..., step_order = NULL, group_order = NULL) {
  trackers <- list(...)

  if (length(trackers) < 2)
    stop("Provide at least two tracker tibbles to merge.")

  for (i in seq_along(trackers))
    validate_tracker(trackers[[i]])

  # Preserve encounter order for steps and groups
  all_steps  <- unique(unlist(lapply(trackers, `[[`, "step")))
  all_groups <- unique(unlist(lapply(trackers, `[[`, "group")))

  if (!is.null(step_order)) {
    bad <- setdiff(step_order, all_steps)
    if (length(bad))
      warning("step_order contains steps not in any tracker: ",
              paste(bad, collapse = ", "))
    all_steps <- c(step_order, setdiff(all_steps, step_order))
  }

  if (!is.null(group_order)) {
    bad <- setdiff(group_order, all_groups)
    if (length(bad))
      warning("group_order contains groups not in any tracker: ",
              paste(bad, collapse = ", "))
    all_groups <- c(group_order, setdiff(all_groups, group_order))
  }

  combined <- dplyr::bind_rows(trackers)

  # Full grid of all group x step combinations
  full_grid <- tidyr::expand_grid(
    group = all_groups,
    step  = all_steps
  )

  # Find which combinations are genuinely missing
  present <- dplyr::select(combined, "group", "step")
  missing <- dplyr::anti_join(full_grid, present, by = c("group", "step"))

  na_cells <- missing  # these will be flagged as N/A in the plot

  # For missing cells, carry forward the last known n_remaining for that group
  # so the totals remain correct. n_dropped = 0 (criterion didn't apply).
  filled <- if (nrow(missing) > 0) {
    # Last known n_remaining per group from the combined data
    last_n <- combined |>
      dplyr::group_by(.data$group) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select("group", last_n = "n_remaining")

    missing |>
      dplyr::left_join(last_n, by = "group") |>
      dplyr::mutate(
        n_remaining = .data$last_n,
        n_dropped   = 0L
      ) |>
      dplyr::select("group", "step", "n_remaining", "n_dropped")
  } else {
    NULL
  }

  tracker <- dplyr::bind_rows(combined, filled) |>
    # Restore canonical step order
    dplyr::mutate(step  = factor(.data$step,  levels = all_steps),
                  group = factor(.data$group, levels = all_groups)) |>
    dplyr::arrange(.data$step, .data$group) |>
    dplyr::mutate(step  = as.character(.data$step),
                  group = as.character(.data$group))

  list(tracker = tracker, na_cells = na_cells)
}
