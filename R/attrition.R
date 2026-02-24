# Pipe-friendly cohort attrition tracking.
#
# A `cohort` object is a list with four elements:
#   $data      — the filtered data frame at the current step
#   $tracker   — tibble(group, step, n_remaining, n_dropped)
#   $id_col    — name of the unique patient ID column
#   $group_col — name of the site/country grouping column

#' Create a cohort object and initialise the attrition tracker
#'
#' @param data Data frame with one row per observation and a unique patient ID
#'   column.
#' @param label Label for the baseline step, e.g. `"Base cohort"`.
#' @param id_col Name of the unique patient identifier column.
#' @param group_col Name of the site or country grouping column. `NULL` or a
#'   missing column both produce a single `"All"` group.
#'
#' @return A `cohort` object.
#'
#' @examples
#' cgd <- prep_cgd_example()
#' cohort <- new_cohort(cgd, label = "Randomised", id_col = "id",
#'                      group_col = "region")
#'
#' @export
new_cohort <- function(data, label = "Base cohort",
                       id_col = "person_id",
                       group_col = "country") {
  if (!id_col %in% colnames(data))
    stop("id_col '", id_col, "' not found in data.")

  if (is.null(group_col) || !group_col %in% colnames(data)) {
    data[["._group"]] <- "All"
    group_col <- "._group"
  }

  structure(
    list(
      data      = data,
      tracker   = count_step(data, label, id_col, group_col, n_dropped = 0L),
      id_col    = id_col,
      group_col = group_col
    ),
    class = "cohort"
  )
}

#' Apply an inclusion filter and record the attrition step
#'
#' @param cohort A `cohort` object from [new_cohort()].
#' @param condition An unquoted filter expression, e.g. `age >= 18`.
#' @param label Display label for this step.
#'
#' @return The updated `cohort` object.
#'
#' @examples
#' cgd <- prep_cgd_example()
#' cohort <- new_cohort(cgd, label = "Randomised", id_col = "id",
#'                      group_col = "region") |>
#'   include_if(age >= 5, "Age >= 5 years") |>
#'   include_if(steroids == 0, "Not on corticosteroids")
#'
#' @export
include_if <- function(cohort, condition, label) {
  check_cohort(cohort)
  filtered <- dplyr::filter(cohort$data, {{ condition }})
  cohort$tracker <- append_step(cohort, filtered, label)
  cohort$data    <- filtered
  cohort
}

#' Extract the data frame from a cohort object
#' @param cohort A `cohort` object.
#' @return The filtered data frame at the current stage.
#' @export
get_data <- function(cohort) cohort$data

#' Extract the attrition tracker from a cohort object
#' @param cohort A `cohort` object.
#' @return A tibble with columns `group`, `step`, `n_remaining`, `n_dropped`.
#' @export
get_tracker <- function(cohort) cohort$tracker

#' Print method for cohort objects
#' @param x A `cohort` object.
#' @param ... Ignored.
#' @export
print.cohort <- function(x, ...) {
  tr     <- x$tracker
  steps  <- unique(tr$step)
  groups <- setdiff(unique(tr$group), "All")
  n_start <- sum(dplyr::filter(tr, .data$step == steps[1])$n_remaining)
  n_end   <- sum(dplyr::filter(tr, .data$step == steps[length(steps)])$n_remaining)
  cat(sprintf(
    "<cohort>  %d step%s | %d group%s\n  Start: n = %s  |  End: n = %s\n",
    length(steps),  if (length(steps)  != 1) "s" else "",
    max(length(groups), 1), if (max(length(groups), 1) != 1) "s" else "",
    format(n_start, big.mark = ","),
    format(n_end,   big.mark = ",")
  ))
  invisible(x)
}


check_cohort <- function(x) {
  if (!inherits(x, "cohort")) stop("Expected a cohort object from new_cohort().")
}

count_step <- function(data, label, id_col, group_col, n_dropped = NA_integer_) {
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(
      step        = label,
      n_remaining = dplyr::n_distinct(.data[[id_col]]),
      n_dropped   = n_dropped,
      .groups     = "drop"
    ) |>
    dplyr::rename(group = dplyr::all_of(group_col))
}

append_step <- function(cohort, filtered_data, label) {
  prev <- cohort$tracker |>
    dplyr::group_by(.data$group) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::select("group", last_n = "n_remaining")

  new_row <- count_step(filtered_data, label, cohort$id_col, cohort$group_col) |>
    dplyr::right_join(prev, by = "group") |>
    dplyr::mutate(
      step        = label,
      n_remaining = dplyr::coalesce(.data$n_remaining, 0L),
      n_dropped   = .data$last_n - .data$n_remaining
    ) |>
    dplyr::select("group", "step", "n_remaining", "n_dropped")

  dplyr::bind_rows(cohort$tracker, new_row)
}
