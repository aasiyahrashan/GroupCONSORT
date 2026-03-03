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
#'       identifying step-group combinations that did not apply to a group's
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

  # For missing cells, carry forward step-by-step through the ordered steps
  # so that intermediate missing steps get the correct last-known count.
  #
  # The old code used slice_tail() on the combined data, which gave every

  # missing step the *final* observed n for that group.  That is wrong when
  # the gap is in the middle: e.g. if group A has steps 1,2,4 but not 3,
  # step 3 should carry forward from step 2's count, not step 4's.
  filled <- if (nrow(missing) > 0) {
    observed <- dplyr::select(combined, "group", "step", "n_remaining")

    fill_rows <- list()
    for (g in all_groups) {
      g_observed <- dplyr::filter(observed, .data$group == g)
      g_missing  <- dplyr::filter(missing,  .data$group == g)
      if (nrow(g_missing) == 0) next

      # Walk through steps in canonical order, carrying forward
      last_known <- NA_integer_
      step_n <- stats::setNames(rep(NA_integer_, length(all_steps)), all_steps)
      for (s in all_steps) {
        obs_row <- g_observed[g_observed$step == s, , drop = FALSE]
        if (nrow(obs_row) > 0) {
          last_known <- obs_row$n_remaining[1]
          step_n[s]  <- last_known
        } else {
          step_n[s] <- last_known
        }
      }

      for (j in seq_len(nrow(g_missing))) {
        s <- g_missing$step[j]
        fill_rows[[length(fill_rows) + 1L]] <- tibble::tibble(
          group       = g,
          step        = s,
          n_remaining = step_n[s],
          n_dropped   = 0L
        )
      }
    }

    dplyr::bind_rows(fill_rows)
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
