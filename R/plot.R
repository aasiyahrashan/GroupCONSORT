# Publication-quality CONSORT flowcharts rendered with grid.
#

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
  main    <- position_main_mm(mc, lay, excl_bh)
  excl    <- position_excl_mm(ec, main, lay)

  gtree <- assemble_grob(main, excl, bw, ew, lay, n_groups)

  w_mm <- canvas_width_mm(excl, bw, ew, lay)
  h_mm <- canvas_height_mm(main, lay)
  attr(gtree, ".layout") <- list(w_mm = w_mm, h_mm = h_mm)
  class(gtree) <- c("consort_grob", class(gtree))
  gtree
}

#' @export
print.consort_grob <- function(x, ...) {
  ly <- attr(x, ".layout")
  grid::grid.newpage()
  if (!is.null(ly)) {
    vp <- grid::viewport(
      width  = grid::unit(ly$w_mm, "mm"),
      height = grid::unit(ly$h_mm, "mm")
    )
    grid::pushViewport(vp)
    grid::grid.draw(x)
    grid::popViewport()
  } else {
    grid::grid.draw(x)
  }
  invisible(x)
}

#' Save a CONSORT flowchart at content-fitting dimensions
#'
#' @param plot A `consort_grob` from [consort_plot()].
#' @param path Output path without extension.
#' @param formats `"png"`, `"pdf"`, or both.
#' @param scale Multiplier on the natural mm dimensions. Default `1`.
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
      height = grid::unit(ly$h_mm * scale, "mm")
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
# Layout params — everything in mm or pt
# =========================================================================

layout_params <- function(fs) {
  list(
    fs_title_pt    = 8   * fs,
    fs_body_pt     = 7.5 * fs,
    pad_x_mm       = 3   * fs,
    pad_y_mm       = 2.5 * fs,
    gap_mm         = 8   * fs,
    h_gap_mm       = 8   * fs,
    section_gap_mm = 1.5 * fs,
    line_gap_frac  = 0.2,
    col_line       = "grey20",
    col_fill       = "white",
    lwd_main       = 0.6,
    lwd_excl       = 0.5,
    lwd_arrow      = 0.5,
    arrow_len_mm   = 1.8,
    margin_mm      = 5
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
# Auto-width in mm
# =========================================================================

auto_width_mm <- function(content_list, lay) {
  if (length(content_list) == 0) return(40)
  all_lines <- unlist(lapply(content_list, `[[`, "raw_lines"))
  all_lines <- all_lines[nchar(all_lines) > 0]
  widths_mm <- vapply(all_lines, function(txt)
    mm_width(txt, lay$fs_body_pt, bold = FALSE), numeric(1))
  w <- max(widths_mm, na.rm = TRUE) + 2 * lay$pad_x_mm + 4
  min(max(w, 28), 120)
}


# =========================================================================
# Wrap text then measure exact grob heights
# =========================================================================

wrap_and_measure_mm <- function(content_list, box_w_mm, lay) {
  avail_mm <- box_w_mm - 2 * lay$pad_x_mm
  # Calibrate chars-per-mm from a reference grob
  ref_g  <- grid::textGrob("MMMM", gp = grid::gpar(fontsize = lay$fs_body_pt))
  ref_mm <- grid::convertWidth(grid::grobWidth(ref_g), "mm", valueOnly = TRUE)
  cpm    <- 4 / ref_mm
  wrap_w <- max(floor(avail_mm * cpm), 10L)

  purrr::map(content_list, function(item) {
    item$title_wrapped <- stringr::str_wrap(item$title, width = wrap_w)
    item$bh_mm <- measure_box_height_mm(item, lay)
    item
  })
}

measure_box_height_mm <- function(item, lay) {
  title_h <- mm_height(item$title_wrapped, lay$fs_title_pt,
                       bold = TRUE, lay = lay)
  n_h <- if (!is.null(item$n_line) && nchar(item$n_line) > 0)
    mm_height(item$n_line, lay$fs_body_pt, bold = FALSE, lay = lay)
  else 0
  gl_h <- if (length(item$group_lines) > 0)
    sum(vapply(item$group_lines, function(l)
      mm_height(l, lay$fs_body_pt, bold = FALSE, lay = lay), numeric(1)))
  else 0
  sg <- if (n_h > 0 || gl_h > 0) lay$section_gap_mm else 0
  title_h + sg + n_h + gl_h + 2 * lay$pad_y_mm
}

mm_height <- function(txt, fontsize_pt, bold, lay) {
  face <- if (bold) "bold" else "plain"
  g <- grid::textGrob(
    txt,
    gp = grid::gpar(fontsize = fontsize_pt, fontface = face,
                    lineheight = 1 + lay$line_gap_frac)
  )
  grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE)
}

mm_width <- function(txt, fontsize_pt, bold = FALSE) {
  face <- if (bold) "bold" else "plain"
  g <- grid::textGrob(txt, gp = grid::gpar(fontsize = fontsize_pt,
                                           fontface = face))
  grid::convertWidth(grid::grobWidth(g), "mm", valueOnly = TRUE)
}


# =========================================================================
# Position boxes (y_top_mm from top of canvas, increasing downward)
# =========================================================================

position_main_mm <- function(content_list, lay, excl_bh) {
  n     <- length(content_list)
  bh    <- vapply(content_list, `[[`, numeric(1), "bh_mm")
  y_top <- numeric(n)
  for (i in seq_len(n)[-1]) {
    ebox_h <- if ((i - 1) <= length(excl_bh)) excl_bh[i - 1] else 0
    gap    <- max(lay$gap_mm, ebox_h + lay$gap_mm * 0.35)
    y_top[i] <- y_top[i - 1] + bh[i - 1] + gap
  }
  lapply(seq_len(n), function(i)
    c(content_list[[i]], list(y_top_mm = y_top[i]))
  )
}

position_excl_mm <- function(content_list, main, lay) {
  if (length(content_list) == 0) return(list())
  lapply(content_list, function(item) {
    i          <- item$step_idx
    top_gap    <- main[[i - 1]]$y_top_mm + main[[i - 1]]$bh_mm
    bottom_gap <- main[[i]]$y_top_mm
    y_mid      <- (top_gap + bottom_gap) / 2
    c(item, list(y_top_mm = y_mid - item$bh_mm / 2))
  })
}


# =========================================================================
# Canvas size
# =========================================================================

canvas_width_mm <- function(excl, bw, ew, lay) {
  m <- lay$margin_mm
  if (length(excl) > 0) bw + lay$h_gap_mm + ew + 2 * m
  else bw + 2 * m
}

canvas_height_mm <- function(main, lay) {
  last <- main[[length(main)]]
  last$y_top_mm + last$bh_mm + 2 * lay$margin_mm
}


# =========================================================================
# Assemble gTree
# x=0 left of canvas, y=0 TOP of canvas, increasing downward.
# All coordinates passed as grid::unit(value, "mm").
# =========================================================================

assemble_grob <- function(main, excl, bw, ew, lay, n_groups) {
  m      <- lay$margin_mm
  x_main <- m + bw / 2
  x_excl <- m + bw + lay$h_gap_mm + ew / 2

  grobs <- list()
  add   <- function(g) grobs[[length(grobs) + 1L]] <<- g

  # Main boxes
  for (item in main) {
    xl <- x_main - bw / 2
    yt <- item$y_top_mm + m
    add(rect_grob(xl, yt, bw, item$bh_mm, lay$col_line, lay$col_fill,
                  lay$lwd_main))
    add(text_block_grob(item, xl + lay$pad_x_mm, yt + lay$pad_y_mm,
                        lay, n_groups))
  }

  # Vertical arrows
  n_main <- length(main)
  if (n_main > 1) {
    for (i in seq_len(n_main - 1)) {
      y_from <- main[[i]]$y_top_mm + main[[i]]$bh_mm + m
      y_to   <- main[[i + 1]]$y_top_mm + m
      add(arrow_grob(x_main, y_from, x_main, y_to, lay))
    }
  }

  # Exclusion boxes + horizontal arrows
  if (length(excl) > 0) {
    for (item in excl) {
      xl_e <- x_excl - ew / 2
      yt_e <- item$y_top_mm + m
      yc_e <- yt_e + item$bh_mm / 2

      add(rect_grob(xl_e, yt_e, ew, item$bh_mm, lay$col_line, lay$col_fill,
                    lay$lwd_excl))
      add(text_block_grob(item, xl_e + lay$pad_x_mm, yt_e + lay$pad_y_mm,
                          lay, n_groups))
      # Horizontal: from main column right edge to excl box left edge
      add(arrow_grob(x_main + bw / 2, yc_e, xl_e, yc_e, lay))
    }
  }

  grid::gTree(children = do.call(grid::gList, grobs))
}


# =========================================================================
# Grob helpers
# =========================================================================

u <- function(x) grid::unit(x, "mm")

rect_grob <- function(x_left, y_top, w, h, col, fill, lwd) {
  grid::rectGrob(
    x      = u(x_left),
    y      = u(y_top),
    width  = u(w),
    height = u(h),
    just   = c("left", "top"),
    gp     = grid::gpar(col = col, fill = fill, lwd = lwd)
  )
}

arrow_grob <- function(x0, y0, x1, y1, lay) {
  grid::segmentsGrob(
    x0    = u(x0), y0 = u(y0),
    x1    = u(x1), y1 = u(y1),
    arrow = grid::arrow(length = u(lay$arrow_len_mm), type = "closed"),
    gp    = grid::gpar(col  = lay$col_line, fill = lay$col_line,
                       lwd  = lay$lwd_arrow)
  )
}

# Stack text lines downward from (x_left, y_top), measuring each line exactly.
text_block_grob <- function(item, x_left, y_top, lay, n_groups) {
  grobs  <- list()
  cursor <- y_top
  n_bold <- n_groups > 1
  add    <- function(g) grobs[[length(grobs) + 1L]] <<- g

  # Title
  title_txt <- item$title_wrapped %||% item$title
  add(grid::textGrob(
    label = title_txt,
    x     = u(x_left), y = u(cursor),
    just  = c("left", "top"),
    gp    = grid::gpar(fontsize   = lay$fs_title_pt,
                       fontface   = "bold",
                       col        = lay$col_line,
                       lineheight = 1 + lay$line_gap_frac)
  ))
  cursor <- cursor +
    mm_height(title_txt, lay$fs_title_pt, bold = TRUE, lay = lay) +
    lay$section_gap_mm

  # n_line
  if (!is.null(item$n_line) && nchar(item$n_line) > 0) {
    face <- if (n_bold) "bold" else "plain"
    add(grid::textGrob(
      label = item$n_line,
      x     = u(x_left), y = u(cursor),
      just  = c("left", "top"),
      gp    = grid::gpar(fontsize   = lay$fs_body_pt,
                         fontface   = face,
                         col        = lay$col_line,
                         lineheight = 1 + lay$line_gap_frac)
    ))
    cursor <- cursor +
      mm_height(item$n_line, lay$fs_body_pt, bold = n_bold, lay = lay)
  }

  # Group lines
  for (gl in item$group_lines) {
    add(grid::textGrob(
      label = gl,
      x     = u(x_left), y = u(cursor),
      just  = c("left", "top"),
      gp    = grid::gpar(fontsize   = lay$fs_body_pt,
                         fontface   = "plain",
                         col        = lay$col_line,
                         lineheight = 1 + lay$line_gap_frac)
    ))
    cursor <- cursor +
      mm_height(gl, lay$fs_body_pt, bold = FALSE, lay = lay)
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
