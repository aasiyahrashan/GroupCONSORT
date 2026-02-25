# Publication-quality CONSORT flowcharts rendered with grid.
#
# Coordinate convention (grid defaults):
#   x = 0 at LEFT,   increasing rightward, unit = mm
#   y = 0 at BOTTOM, increasing UPWARD,    unit = mm
#
# One line_height_mm for every line step.
# One section_gap_mm inserted once between title block and body.

#' Draw a publication-quality CONSORT flowchart
#'
#' @param tracker A `cohort` object or a tracker tibble with columns `group`,
#'   `step`, `n_remaining`, `n_dropped`.
#' @param na_cells A data frame with columns `step` and `group` identifying
#'   step–group combinations where the step did not apply (shown as "N/A").
#'   `NULL` (default) means no N/A cells.
#' @param step_labels Named character vector renaming steps for display.
#' @param group_labels Named character vector renaming groups for display.
#' @param font_size Multiplicative scaling factor for all text. Default `1`.
#' @param box_width Width of main boxes in mm. `NULL` = auto.
#' @param excl_width Width of exclusion boxes in mm. `NULL` = auto.
#'
#' @return A `consort_grob` (a `gTree`). Print it or pass to
#'   [save_consort_plot()].
#' @export
consort_plot <- function(tracker,
                         na_cells     = NULL,
                         step_labels  = NULL,
                         group_labels = NULL,
                         font_size    = 1,
                         box_width    = NULL,
                         excl_width   = NULL) {

  if (inherits(tracker, "cohort")) tracker <- get_tracker(tracker)
  validate_tracker(tracker)
  tracker <- recode_tracker(tracker, step_labels, group_labels)

  steps    <- unique(tracker$step)
  n_steps  <- length(steps)
  groups   <- unique(tracker$group)
  n_groups <- length(groups)

  tracker <- flag_na_cells(tracker, na_cells)
  lay     <- layout_params(font_size)

  mc <- build_main_content(tracker, steps, n_groups)
  ec <- build_excl_content(tracker, steps, n_steps, n_groups)

  bw <- box_width  %||% auto_width_mm(mc, lay)
  ew <- excl_width %||% auto_width_mm(ec, lay)

  mc <- wrap_and_measure_mm(mc, bw, lay)
  ec <- wrap_and_measure_mm(ec, ew, lay)

  excl_bh <- vapply(ec, `[[`, numeric(1), "bh_mm")
  total_h  <- canvas_height_mm(mc, ec, lay)
  main     <- position_main_mm(mc, lay, excl_bh, total_h)
  excl     <- position_excl_mm(ec, main, lay, total_h)

  gtree <- assemble_grob(main, excl, bw, ew, lay, n_groups, total_h)

  w_mm <- canvas_width_mm(excl, bw, ew, lay)
  attr(gtree, ".layout") <- list(w_mm = w_mm, h_mm = total_h)
  class(gtree) <- c("consort_grob", class(gtree))
  gtree
}

#' @export
print.consort_grob <- function(x, ...) {
  ly <- attr(x, ".layout")
  grid::grid.newpage()
  vp <- grid::viewport(
    width  = grid::unit(ly$w_mm, "mm"),
    height = grid::unit(ly$h_mm, "mm"),
    x = 0.5, y = 0.5
  )
  grid::pushViewport(vp)
  grid::grid.draw(x)
  grid::popViewport()
  invisible(x)
}

#' Save a CONSORT flowchart at content-fitting dimensions
#'
#' @param plot A `consort_grob` from [consort_plot()].
#' @param path Output path without extension.
#' @param formats `"png"`, `"pdf"`, or both.
#' @param scale Multiplier on natural mm dimensions. Default `1`.
#' @param dpi PNG resolution.
#' @return `plot`, invisibly.
#' @export
save_consort_plot <- function(plot, path, formats = c("png", "pdf"),
                              scale = 1, dpi = 300) {
  ly <- attr(plot, ".layout")
  if (is.null(ly)) stop("No layout metadata. Was this made by consort_plot()?")

  w_in <- ly$w_mm * scale / 25.4
  h_in <- ly$h_mm * scale / 25.4

  draw_fn <- function() {
    grid::grid.newpage()
    vp <- grid::viewport(
      width  = grid::unit(ly$w_mm * scale, "mm"),
      height = grid::unit(ly$h_mm * scale, "mm"),
      x = 0.5, y = 0.5
    )
    grid::pushViewport(vp)
    grid::grid.draw(plot)
    grid::popViewport()
  }

  for (fmt in formats) {
    f <- paste0(path, ".", fmt)
    if (fmt == "png") {
      grDevices::png(f, width = w_in, height = h_in,
                     units = "in", res = dpi, bg = "transparent")
      draw_fn()
      grDevices::dev.off()
    } else if (fmt == "pdf") {
      grDevices::cairo_pdf(f, width = w_in, height = h_in,
                           bg = "transparent")
      draw_fn()
      grDevices::dev.off()
    }
    message("Saved: ", f)
  }
  invisible(plot)
}


# =========================================================================
# Layout params
#
# line_height_mm: measured from a 2-line plain reference grob (baseline to
# baseline). Multiplied by 1.5 for comfortable leading that accommodates
# bold lines without them appearing cramped against the following plain line.
# =========================================================================

layout_params <- function(fs) {
  fs_pt <- 8 * fs

  # Baseline-to-baseline distance at this font size with lineheight=1.3
  g2 <- grid::textGrob(
    "Ag\nAg",
    gp = grid::gpar(fontsize = fs_pt, fontface = "plain", lineheight = 1.3)
  )
  lh <- grid::convertHeight(grid::grobHeight(g2), "mm", valueOnly = TRUE) / 2

  list(
    fs_pt          = fs_pt,
    lineheight     = 1.3,         # must match the reference grob
    line_height_mm = lh,
    section_gap_mm = 0,
    pad_x_mm       = 2.5 * fs,
    pad_y_mm       = 2.0 * fs,
    gap_mm         = 4   * fs,
    h_gap_mm       = 5   * fs,
    col_line       = "grey20",
    col_fill       = "white",
    lwd_main       = 0.6,
    lwd_excl       = 0.5,
    lwd_arrow      = 0.4,
    arrow_len_mm   = 1.5,
    margin_mm      = 4
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# =========================================================================
# NA cell flagging
# =========================================================================

flag_na_cells <- function(tracker, na_cells) {
  if (is.null(na_cells)) { tracker$is_na <- FALSE; return(tracker) }
  if (!is.data.frame(na_cells) ||
      !all(c("step", "group") %in% colnames(na_cells)))
    stop("na_cells must be NULL or a data frame with 'step' and 'group' columns.")
  na_cells$.na_flag <- TRUE
  tracker <- dplyr::left_join(
    tracker,
    dplyr::select(na_cells, "step", "group", ".na_flag"),
    by = c("step", "group")
  )
  tracker$is_na    <- tidyr::replace_na(tracker$.na_flag, FALSE)
  tracker$.na_flag <- NULL
  tracker
}


# =========================================================================
# Content builders
# =========================================================================

build_main_content <- function(tracker, steps, n_groups) {
  purrr::map(steps, function(s) {
    rows  <- dplyr::filter(tracker, .data$step == s)
    total <- sum(rows$n_remaining, na.rm = TRUE)
    title <- clean_label(s)
    n_line <- if (n_groups > 1)
      paste0("Total: n = ", format(total, big.mark = ","))
    else
      paste0("n = ", format(total, big.mark = ","))
    group_lines <- if (n_groups > 1)
      dplyr::mutate(rows, line = dplyr::case_when(
        is.na(.data$n_remaining) ~ paste0(.data$group, ": \u2014"),
        TRUE ~ paste0(.data$group, ": n = ",
                      format(.data$n_remaining, big.mark = ","))
      ))$line
    else character(0)

    list(title = title, n_line = n_line, group_lines = group_lines,
         raw_lines = c(title, n_line, group_lines), type = "main")
  })
}

build_excl_content <- function(tracker, steps, n_steps, n_groups) {
  if (n_steps < 2) return(list())
  purrr::map(seq(2, n_steps), function(i) {
    prev <- dplyr::filter(tracker, .data$step == steps[i - 1])
    curr <- dplyr::filter(tracker, .data$step == steps[i])
    joined <- dplyr::left_join(
      dplyr::select(prev, "group", prev_n = "n_remaining"),
      dplyr::select(curr, "group", curr_n = "n_remaining", "is_na"),
      by = "group"
    ) |> dplyr::mutate(
      d = tidyr::replace_na(.data$prev_n, 0L) -
        tidyr::replace_na(.data$curr_n, 0L)
    )
    total_d <- sum(dplyr::filter(joined, !.data$is_na)$d, na.rm = TRUE)
    title   <- paste0("Excluded: n = ", format(total_d, big.mark = ","))
    group_lines <- if (n_groups > 1)
      dplyr::mutate(joined, line = dplyr::if_else(
        .data$is_na,
        paste0("  ", .data$group, ": N/A"),
        paste0("  ", .data$group, ": n = ", format(.data$d, big.mark = ","))
      ))$line
    else character(0)

    list(title = title, n_line = NULL, group_lines = group_lines,
         raw_lines = c(title, group_lines), type = "excl", step_idx = i)
  })
}


# =========================================================================
# Auto-width
# =========================================================================

auto_width_mm <- function(content_list, lay) {
  if (length(content_list) == 0) return(40)
  widths <- numeric(0)
  for (item in content_list) {
    widths <- c(widths, grob_width_mm(item$title,    "bold",  lay))
    if (!is.null(item$n_line))
      widths <- c(widths, grob_width_mm(item$n_line, "bold",  lay))
    for (gl in item$group_lines)
      widths <- c(widths, grob_width_mm(gl,          "plain", lay))
  }
  w <- max(widths, na.rm = TRUE) + 2 * lay$pad_x_mm + 2
  min(max(w, 25), 150)
}

grob_width_mm <- function(txt, fontface, lay) {
  g <- grid::textGrob(txt, gp = grid::gpar(fontsize = lay$fs_pt,
                                           fontface = fontface))
  grid::convertWidth(grid::grobWidth(g), "mm", valueOnly = TRUE)
}


# =========================================================================
# Wrap titles; compute box heights
# =========================================================================

wrap_and_measure_mm <- function(content_list, box_w_mm, lay) {
  avail_mm <- box_w_mm - 2 * lay$pad_x_mm
  purrr::map(content_list, function(item) {
    tw <- grob_width_mm(item$title, "bold", lay)
    item$title_wrapped <- if (tw > avail_mm + 0.5) {
      mpc     <- tw / max(nchar(item$title), 1L)
      wrap_at <- max(floor(avail_mm / mpc), 6L)
      stringr::str_wrap(item$title, width = wrap_at)
    } else {
      item$title
    }
    item$n_title_lines <- length(strsplit(item$title_wrapped, "\n",
                                          fixed = TRUE)[[1]])
    item$bh_mm <- box_height_mm(item, lay)
    item
  })
}

box_height_mm <- function(item, lay) {
  lh      <- lay$line_height_mm
  n_title <- item$n_title_lines %||%
    length(strsplit(item$title_wrapped %||% item$title, "\n",
                    fixed = TRUE)[[1]])
  has_body <- (!is.null(item$n_line) && nchar(item$n_line) > 0) ||
    length(item$group_lines) > 0
  n_body <- (if (!is.null(item$n_line) && nchar(item$n_line) > 0) 1L else 0L) +
    length(item$group_lines)
  sg <- if (has_body) lay$section_gap_mm else 0
  n_title * lh + sg + n_body * lh + 2 * lay$pad_y_mm
}


# =========================================================================
# Canvas dimensions
# =========================================================================

canvas_height_mm <- function(mc, ec, lay) {
  bh      <- vapply(mc, `[[`, numeric(1), "bh_mm")
  excl_bh <- if (length(ec) > 0)
    vapply(ec, `[[`, numeric(1), "bh_mm") else numeric(0)
  n     <- length(mc)
  total <- sum(bh)
  for (i in seq_len(n - 1)) {
    ebox_h <- if (i <= length(excl_bh)) excl_bh[i] else 0
    gap    <- max(lay$gap_mm, ebox_h + lay$gap_mm * 0.4)
    total  <- total + gap
  }
  total + 2 * lay$margin_mm
}

canvas_width_mm <- function(excl, bw, ew, lay) {
  m <- lay$margin_mm
  if (length(excl) > 0) bw + lay$h_gap_mm + ew + 2 * m
  else bw + 2 * m
}


# =========================================================================
# Position boxes
# =========================================================================

position_main_mm <- function(content_list, lay, excl_bh, total_h) {
  n  <- length(content_list)
  bh <- vapply(content_list, `[[`, numeric(1), "bh_mm")
  m  <- lay$margin_mm

  y_top <- numeric(n)
  y_top[1] <- m
  for (i in seq_len(n - 1)) {
    ebox_h <- if (i <= length(excl_bh)) excl_bh[i] else 0
    gap    <- max(lay$gap_mm, ebox_h + lay$gap_mm * 0.4)
    y_top[i + 1] <- y_top[i] + bh[i] + gap
  }

  lapply(seq_len(n), function(i) {
    c(content_list[[i]], list(
      y_bot_mm = total_h - y_top[i] - bh[i],
      y_top_tt = y_top[i]
    ))
  })
}

position_excl_mm <- function(content_list, main, lay, total_h) {
  if (length(content_list) == 0) return(list())
  lapply(content_list, function(item) {
    i      <- item$step_idx
    top_tt <- main[[i - 1]]$y_top_tt + main[[i - 1]]$bh_mm
    bot_tt <- main[[i]]$y_top_tt
    mid_tt <- (top_tt + bot_tt) / 2
    y_bot  <- total_h - mid_tt - item$bh_mm / 2
    c(item, list(y_bot_mm = y_bot))
  })
}


# =========================================================================
# Assemble gTree
# =========================================================================

assemble_grob <- function(main, excl, bw, ew, lay, n_groups, total_h) {
  m            <- lay$margin_mm
  x_main_left  <- m
  x_main_cx    <- m + bw / 2
  x_excl_left  <- m + bw + lay$h_gap_mm

  grobs <- list()
  add   <- function(g) grobs[[length(grobs) + 1L]] <<- g

  for (item in main) {
    add(rect_grob(x_main_left, item$y_bot_mm, bw, item$bh_mm,
                  lay$col_line, lay$col_fill, lay$lwd_main))
    add(text_block_grob(item,
                        x_left  = x_main_left + lay$pad_x_mm,
                        y_start = item$y_bot_mm + item$bh_mm - lay$pad_y_mm,
                        lay, n_groups))
  }

  n_main <- length(main)
  if (n_main > 1) {
    for (i in seq_len(n_main - 1)) {
      y_from <- main[[i]]$y_bot_mm
      y_to   <- main[[i + 1]]$y_bot_mm + main[[i + 1]]$bh_mm
      add(arrow_grob(x_main_cx, y_from, x_main_cx, y_to, lay))
    }
  }

  if (length(excl) > 0) {
    for (item in excl) {
      add(rect_grob(x_excl_left, item$y_bot_mm, ew, item$bh_mm,
                    lay$col_line, lay$col_fill, lay$lwd_excl))
      add(text_block_grob(item,
                          x_left  = x_excl_left + lay$pad_x_mm,
                          y_start = item$y_bot_mm + item$bh_mm - lay$pad_y_mm,
                          lay, n_groups))
      yc <- item$y_bot_mm + item$bh_mm / 2
      add(arrow_grob(x_main_cx, yc, x_excl_left, yc, lay))
    }
  }

  grid::gTree(children = do.call(grid::gList, grobs))
}


# =========================================================================
# Grob helpers
# =========================================================================

u <- function(x) grid::unit(x, "mm")

rect_grob <- function(x_left, y_bot, w, h, col, fill, lwd) {
  grid::rectGrob(
    x = u(x_left), y = u(y_bot),
    width = u(w),   height = u(h),
    just  = c("left", "bottom"),
    gp    = grid::gpar(col = col, fill = fill, lwd = lwd)
  )
}

arrow_grob <- function(x0, y0, x1, y1, lay) {
  grid::segmentsGrob(
    x0 = u(x0), y0 = u(y0), x1 = u(x1), y1 = u(y1),
    arrow = grid::arrow(length = u(lay$arrow_len_mm), type = "closed"),
    gp    = grid::gpar(col = lay$col_line, fill = lay$col_line,
                       lwd = lay$lwd_arrow)
  )
}

text_block_grob <- function(item, x_left, y_start, lay, n_groups) {
  grobs  <- list()
  cursor <- y_start
  lh     <- lay$line_height_mm
  n_bold <- n_groups > 1

  gp_bold  <- grid::gpar(fontsize = lay$fs_pt, fontface = "bold",
                         col = lay$col_line, lineheight = lay$lineheight)
  gp_plain <- grid::gpar(fontsize = lay$fs_pt, fontface = "plain",
                         col = lay$col_line, lineheight = lay$lineheight)
  add <- function(g) grobs[[length(grobs) + 1L]] <<- g

  # Title lines (bold) — uniform lh step each
  title_lines <- strsplit(item$title_wrapped %||% item$title,
                          "\n", fixed = TRUE)[[1]]
  for (tl in title_lines) {
    add(grid::textGrob(label = tl,
                       x = u(x_left), y = u(cursor),
                       just = c("left", "top"), gp = gp_bold))
    cursor <- cursor - lh
  }

  # Section gap once, only when body follows
  has_body <- (!is.null(item$n_line) && nchar(item$n_line) > 0) ||
    length(item$group_lines) > 0
  if (has_body) cursor <- cursor - lay$section_gap_mm

  # n_line
  if (!is.null(item$n_line) && nchar(item$n_line) > 0) {
    add(grid::textGrob(label = item$n_line,
                       x = u(x_left), y = u(cursor),
                       just = c("left", "top"),
                       gp = if (n_bold) gp_bold else gp_plain))
    cursor <- cursor - lh
  }

  # Group lines — uniform lh step each
  for (gl in item$group_lines) {
    add(grid::textGrob(label = gl,
                       x = u(x_left), y = u(cursor),
                       just = c("left", "top"), gp = gp_plain))
    cursor <- cursor - lh
  }

  grid::gTree(children = do.call(grid::gList, grobs))
}


# =========================================================================
# Validators, recoders, utilities
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
    if (length(bad)) warning("step_labels not in tracker: ",
                             paste(bad, collapse = ", "))
    tracker$step <- recode_vec(tracker$step, step_labels)
  }
  if (!is.null(group_labels)) {
    bad <- setdiff(names(group_labels), tracker$group)
    if (length(bad)) warning("group_labels not in tracker: ",
                             paste(bad, collapse = ", "))
    tracker$group <- recode_vec(tracker$group, group_labels)
  }
  tracker
}

clean_label <- function(x) {
  x |>
    stringr::str_remove("^\\d+[_.\\-]\\s*") |>
    stringr::str_replace_all("[_.]", " ") |>
    stringr::str_to_sentence()
}
