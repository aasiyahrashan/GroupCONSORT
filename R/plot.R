# Publication-quality CONSORT flowcharts from a consortr tracker tibble.

#' Draw a publication-quality CONSORT flowchart
#'
#' @param tracker A `cohort` object or a tracker tibble with columns `group`,
#'   `step`, `n_remaining`, `n_dropped`.
#' @param na_cells A data frame with columns `step` and `group` identifying
#'   step-group combinations where the step did not apply. These cells show
#'   "N/A" in the exclusion box; counts in the main boxes are always shown.
#'   `NULL` (default) means no N/A cells.
#' @param step_labels Named character vector renaming steps for display.
#' @param group_labels Named character vector renaming groups for display.
#' @param font_size Multiplicative scaling factor for all text. Default `1`.
#' @param box_width Width of main boxes in plot units. `NULL` = auto.
#' @param excl_width Width of exclusion boxes in plot units. `NULL` = auto.
#'
#' @return A `ggplot2` object.
#' @export
consort_plot <- function(tracker,
                         na_cells = NULL,
                         step_labels = NULL,
                         group_labels = NULL,
                         font_size = 1,
                         box_width = NULL,
                         excl_width = NULL) {

  if (inherits(tracker, "cohort")) tracker <- get_tracker(tracker)
  validate_tracker(tracker)
  tracker <- recode_tracker(tracker, step_labels, group_labels)

  steps    <- unique(tracker$step)
  n_steps  <- length(steps)
  groups   <- unique(tracker$group)
  n_groups <- length(groups)

  # Validate and attach NA flags
  tracker <- flag_na_cells(tracker, na_cells)

  lay <- layout_params(font_size)

  mc <- build_main_content(tracker, steps, n_groups)
  ec <- build_excl_content(tracker, steps, n_steps, n_groups)

  cpu <- lay$chars_per_unit
  bw  <- box_width  %||% auto_width(mc, cpu, lay$pad_x)
  ew  <- excl_width %||% auto_width(ec, cpu, lay$pad_x)

  mc <- wrap_and_measure(mc, bw, lay)
  ec <- wrap_and_measure(ec, ew, lay)

  main <- position_main(mc, lay)
  excl <- position_excl(ec, main, lay)

  x0       <- 0
  x_excl   <- x0 + bw / 2 + lay$h_gap + ew / 2
  lx_main  <- x0     - bw / 2 + lay$pad_x
  lx_excl  <- x_excl - ew / 2 + lay$pad_x

  col_line <- "grey50"

  p <- ggplot2::ggplot()

  # Main boxes
  p <- p + ggplot2::geom_rect(
    data = main, ggplot2::aes(
      xmin = x0 - bw / 2, xmax = x0 + bw / 2,
      ymin = .data$y - .data$bh / 2, ymax = .data$y + .data$bh / 2
    ), fill = NA, colour = col_line, linewidth = 0.4)

  # Vertical arrows
  if (n_steps > 1) {
    segs <- tibble::tibble(
      y_from = main$y[-n_steps] - main$bh[-n_steps] / 2,
      y_to   = main$y[-1]       + main$bh[-1]       / 2
    )
    p <- p + ggplot2::geom_segment(
      data = segs, ggplot2::aes(
        x = x0, xend = x0, y = .data$y_from, yend = .data$y_to
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(1.5, "mm"), type = "closed"),
      colour = col_line, linewidth = 0.4)
  }

  # Exclusion boxes + horizontal connectors
  if (nrow(excl) > 0) {
    el <- x_excl - ew / 2
    p <- p +
      ggplot2::geom_rect(
        data = excl, ggplot2::aes(
          xmin = el, xmax = x_excl + ew / 2,
          ymin = .data$y - .data$bh / 2, ymax = .data$y + .data$bh / 2
        ), fill = NA, colour = col_line, linewidth = 0.35) +
      ggplot2::geom_segment(
        data = excl, ggplot2::aes(
          x = x0, xend = el, y = .data$y, yend = .data$y
        ),
        arrow = ggplot2::arrow(length = ggplot2::unit(1.3, "mm"), type = "closed"),
        colour = col_line, linewidth = 0.35)
  }

  # Text labels
  labels <- build_labels(main, excl, lx_main, lx_excl, lay, n_groups)
  bold_l  <- dplyr::filter(labels, .data$bold)
  plain_l <- dplyr::filter(labels, !.data$bold)

  if (nrow(bold_l) > 0)
    p <- p + ggplot2::geom_text(
      data = bold_l,
      ggplot2::aes(x = .data$tx, y = .data$ty, label = .data$label),
      fontface = "bold", size = bold_l$fsize,
      hjust = 0, vjust = 1, lineheight = 1.05, colour = col_line)
  if (nrow(plain_l) > 0)
    p <- p + ggplot2::geom_text(
      data = plain_l,
      ggplot2::aes(x = .data$tx, y = .data$ty, label = .data$label),
      fontface = "plain", size = plain_l$fsize,
      hjust = 0, vjust = 1, lineheight = 1.05, colour = col_line)

  m  <- 0.06
  xl <- x0 - bw / 2
  xr <- if (nrow(excl) > 0) x_excl + ew / 2 else x0 + bw / 2
  yt <- main$y[1]       + main$bh[1]       / 2
  yb <- main$y[n_steps] - main$bh[n_steps] / 2

  p +
    ggplot2::coord_cartesian(
      xlim = c(xl - m, xr + m), ylim = c(yb - m, yt + m), expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(2, 2, 2, 2),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA)
    ) -> p

  attr(p, ".layout") <- list(x = c(xl, xr), y = c(yb, yt), m = m)
  p
}


#' Save a CONSORT flowchart at content-fitting dimensions
#' @param plot A `ggplot2` object from [consort_plot()].
#' @param path Output path without extension.
#' @param formats `"png"`, `"pdf"`, or both.
#' @param scale Inches per plot unit.
#' @param dpi PNG resolution.
#' @return `plot`, invisibly.
#' @export
save_consort_plot <- function(plot, path, formats = c("png", "pdf"),
                              scale = 0.75, dpi = 300) {
  ly <- attr(plot, ".layout")
  if (is.null(ly)) stop("No layout metadata.")
  w <- (diff(ly$x) + 2 * ly$m) * scale
  h <- (diff(ly$y) + 2 * ly$m) * scale
  for (fmt in formats) {
    f <- paste0(path, ".", fmt)
    args <- list(filename = f, plot = plot, width = w, height = h,
                 bg = "transparent")
    if (fmt == "pdf") args$device <- grDevices::cairo_pdf
    if (fmt == "png") args$dpi    <- dpi
    do.call(ggplot2::ggsave, args)
    message("Saved: ", f)
  }
  invisible(plot)
}


# =========================================================================
# Layout
# =========================================================================

layout_params <- function(fs) {
  list(
    fs_title       = 2.7 * fs,
    fs_body        = 2.5 * fs,
    line_h         = 0.24 * fs,
    pad_x          = 0.08 * fs,   # scale so boxes don't feel cramped at large fs
    pad_y          = 0.06 * fs,
    gap            = 0.38 * fs,   # inter-box gap must grow with text size
    h_gap          = 0.22 * fs,
    chars_per_unit = 15 / fs
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# =========================================================================
# NA cell flagging — explicit only
# =========================================================================

flag_na_cells <- function(tracker, na_cells) {
  if (is.null(na_cells)) {
    tracker$is_na <- FALSE
    return(tracker)
  }
  if (!is.data.frame(na_cells))
    stop("na_cells must be NULL or a data frame with 'step' and 'group' columns.")
  if (!all(c("step", "group") %in% colnames(na_cells)))
    stop("na_cells data frame must have 'step' and 'group' columns.")

  na_cells$.na_flag <- TRUE
  tracker <- dplyr::left_join(tracker,
                              dplyr::select(na_cells, "step", "group", ".na_flag"),
                              by = c("step", "group"))
  tracker$is_na <- tidyr::replace_na(tracker$.na_flag, FALSE)
  tracker$.na_flag <- NULL
  tracker
}


# =========================================================================
# Content builders
# =========================================================================

#' Main boxes always show counts for all groups — no N/A here.
build_main_content <- function(tracker, steps, n_groups) {
  purrr::map(steps, function(s) {
    rows  <- dplyr::filter(tracker, .data$step == s)
    total <- sum(rows$n_remaining, na.rm = TRUE)
    title <- clean_label(s)
    n_line <- if (n_groups > 1) {
      paste0("Total: n = ", format(total, big.mark = ","))
    } else {
      paste0("n = ", format(total, big.mark = ","))
    }
    group_lines <- if (n_groups > 1) {
      rows |>
        dplyr::mutate(line = dplyr::case_when(
          is.na(.data$n_remaining) ~ paste0(.data$group, ": \u2014"),
          TRUE ~ paste0(.data$group, ": n = ",
                        format(.data$n_remaining, big.mark = ","))
        )) |>
        dplyr::pull(.data$line)
    } else character(0)

    list(title = title, n_line = n_line, group_lines = group_lines,
         raw_lines = c(title, n_line, group_lines), type = "main")
  })
}

#' Exclusion boxes: groups flagged is_na show "N/A"; others show drop count.
#' Box is shown at every transition (even if all drops are 0).
build_excl_content <- function(tracker, steps, n_steps, n_groups) {
  if (n_steps < 2) return(list())
  purrr::map(seq(2, n_steps), function(i) {
    prev <- dplyr::filter(tracker, .data$step == steps[i - 1])
    curr <- dplyr::filter(tracker, .data$step == steps[i])

    joined <- dplyr::left_join(
      dplyr::select(prev, "group", prev_n = "n_remaining"),
      dplyr::select(curr, "group", curr_n = "n_remaining", "is_na"),
      by = "group"
    ) |>
      dplyr::mutate(
        d = tidyr::replace_na(.data$prev_n, 0L) -
          tidyr::replace_na(.data$curr_n, 0L)
      )

    # Total excludes NA groups
    applicable <- dplyr::filter(joined, !.data$is_na)
    total_d <- sum(applicable$d, na.rm = TRUE)

    title <- paste0("Excluded (n = ", format(total_d, big.mark = ","), ")")

    group_lines <- if (n_groups > 1) {
      joined |>
        dplyr::mutate(line = dplyr::if_else(
          .data$is_na,
          paste0("  ", .data$group, ": N/A"),
          paste0("  ", .data$group, ": n = ",
                 format(.data$d, big.mark = ","))
        )) |>
        dplyr::pull(.data$line)
    } else character(0)

    list(title = title, n_line = NULL, group_lines = group_lines,
         raw_lines = c(title, group_lines), type = "excl", step_idx = i)
  })
}


# =========================================================================
# Auto-width, wrapping, positioning
# =========================================================================

auto_width <- function(content_list, cpu, pad_x) {
  if (length(content_list) == 0) return(1)
  max_ch <- max(purrr::map_int(content_list, function(x)
    max(nchar(x$raw_lines), na.rm = TRUE)))
  w <- max_ch / cpu + 2 * pad_x + 0.12
  max(min(w, 5.5), 0.8)
}

wrap_and_measure <- function(content_list, box_w, lay) {
  wc <- floor((box_w - 2 * lay$pad_x) * lay$chars_per_unit)
  purrr::map(content_list, function(item) {
    item$title_wrapped <- stringr::str_wrap(item$title, width = max(wc, 10))
    nt <- count_lines(item$title_wrapped)
    nn <- if (!is.null(item$n_line)) 1L else 0L
    ng <- length(item$group_lines)
    item$n_title <- nt
    total_lines <- nt + nn + ng
    # N lines need (N-1) inter-line gaps + top/bottom padding + last-line cap
    item$bh <- (total_lines - 1) * lay$line_h + lay$line_h * 0.6 + 2 * lay$pad_y
    item
  })
}

position_main <- function(content_list, lay) {
  n  <- length(content_list)
  bh <- purrr::map_dbl(content_list, "bh")
  y  <- numeric(n)
  for (i in seq_along(content_list)[-1])
    y[i] <- y[i - 1] - bh[i - 1] / 2 - lay$gap - bh[i] / 2
  tibble::tibble(
    title_wrapped = purrr::map_chr(content_list, "title_wrapped"),
    n_line  = purrr::map_chr(content_list, function(x) x$n_line %||% ""),
    group_text = purrr::map_chr(content_list, function(x)
      paste(x$group_lines, collapse = "\n")),
    n_title = purrr::map_int(content_list, "n_title"),
    bh = bh, y = y
  )
}

position_excl <- function(content_list, main, lay) {
  if (length(content_list) == 0)
    return(tibble::tibble(title_wrapped = character(), group_text = character(),
                          n_title = integer(), bh = numeric(), y = numeric()))
  purrr::map(content_list, function(item) {
    i <- item$step_idx
    # Midpoint of the vertical gap between consecutive main boxes
    top_gap    <- main$y[i - 1] - main$bh[i - 1] / 2
    bottom_gap <- main$y[i]     + main$bh[i]     / 2
    y_mid      <- (top_gap + bottom_gap) / 2
    # If the exclusion box is taller than the gap, expand the gap by pushing the
    # lower main box down.  We don't mutate `main` here — just record the y so
    # the box is at least fully contained in the available space.
    available  <- top_gap - bottom_gap   # positive value
    half_excl  <- item$bh / 2
    if (half_excl > available / 2) {
      # Centre on gap midpoint; caller already uses bh for drawing so the box
      # will simply overlap slightly — better than clipping silently.
      y_mid <- (top_gap + bottom_gap) / 2
    }
    tibble::tibble(
      title_wrapped = item$title_wrapped,
      group_text = paste(item$group_lines, collapse = "\n"),
      n_title = item$n_title, bh = item$bh, y = y_mid
    )
  }) |> dplyr::bind_rows()
}


# =========================================================================
# Text labels
# =========================================================================

build_labels <- function(main, excl, lx_main, lx_excl, lay, n_groups) {
  rows <- list()
  n_bold <- n_groups > 1  # only bold the n-line when there are groups below it
  for (i in seq_len(nrow(main))) {
    top <- main$y[i] + main$bh[i] / 2 - lay$pad_y
    rows[[length(rows) + 1L]] <- lbl(lx_main, top, main$title_wrapped[i],
                                     TRUE, lay$fs_title)
    if (nchar(main$n_line[i]) > 0)
      rows[[length(rows) + 1L]] <- lbl(
        lx_main, top - main$n_title[i] * lay$line_h,
        main$n_line[i], n_bold, lay$fs_body)
    if (nchar(main$group_text[i]) > 0)
      rows[[length(rows) + 1L]] <- lbl(
        lx_main, top - (main$n_title[i] + 1L) * lay$line_h,
        main$group_text[i], FALSE, lay$fs_body)
  }
  for (i in seq_len(nrow(excl))) {
    top <- excl$y[i] + excl$bh[i] / 2 - lay$pad_y
    rows[[length(rows) + 1L]] <- lbl(lx_excl, top, excl$title_wrapped[i],
                                     TRUE, lay$fs_body)
    if (nchar(excl$group_text[i]) > 0)
      rows[[length(rows) + 1L]] <- lbl(
        lx_excl, top - excl$n_title[i] * lay$line_h,
        excl$group_text[i], FALSE, lay$fs_body)
  }
  dplyr::bind_rows(rows)
}

lbl <- function(tx, ty, label, bold, fsize)
  tibble::tibble(tx = tx, ty = ty, label = label, bold = bold, fsize = fsize)


# =========================================================================
# Utilities
# =========================================================================

validate_tracker <- function(tracker) {
  need <- c("group", "step", "n_remaining", "n_dropped")
  miss <- setdiff(need, colnames(tracker))
  if (length(miss)) stop("tracker missing: ", paste(miss, collapse = ", "))
}

recode_tracker <- function(tracker, step_labels, group_labels) {
  recode_vec <- function(x, lookup) {
    idx <- match(x, names(lookup))
    ifelse(is.na(idx), x, unname(lookup[idx]))
  }
  if (!is.null(step_labels)) {
    bad <- setdiff(names(step_labels), tracker$step)
    if (length(bad)) warning("step_labels not in tracker: ", paste(bad, collapse = ", "))
    tracker$step <- recode_vec(tracker$step, step_labels)
  }
  if (!is.null(group_labels)) {
    bad <- setdiff(names(group_labels), tracker$group)
    if (length(bad)) warning("group_labels not in tracker: ", paste(bad, collapse = ", "))
    tracker$group <- recode_vec(tracker$group, group_labels)
  }
  tracker
}

count_lines <- function(x) lengths(regmatches(x, gregexpr("\n", x))) + 1L

clean_label <- function(x) {
  x |>
    stringr::str_remove("^\\d+[_.\\-]\\s*") |>
    stringr::str_replace_all("[_.]", " ") |>
    stringr::str_to_sentence()
}
