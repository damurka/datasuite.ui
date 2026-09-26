# Custom charts and checking what is saved.
#
# A custom chart (report kind "custom_chart") is data, never code: where its table comes from (the app resolves
# `data`), a few fixed transforms, and a plot description. So a dataset holding one never runs anything when it is
# opened, and the chart redraws from the loaded data like every other kind. The shape is in
# countdown-analytics/docs/AI-PLAN.md (section 13) and docs/AI-BRIDGE.md:
#
#   list(title, data = list(member, args), transform = list(list(filter = ...), ...),
#        plot = list(geom, x, y, colour, fill, facet, position, labels = list(x, y, title), percent, flip))
#
# This file is generic: the transforms work on any data frame and report_plot_spec() draws any plot description. An
# app says where `data` comes from (cd2030.core: a CacheConnection member).

# The transforms a spec may use, and the geoms a plot may draw.
.spec_transforms <- c("filter", "select", "rename", "mutate_ratio", "aggregate", "pivot_longer", "arrange", "top_n")
.spec_geoms <- c("line", "col", "point", "area", "tile")
.spec_funs <- c("sum", "mean", "median", "min", "max")

.spec_fail <- function(msg, call = rlang::caller_env()) .ds_abort(c(x = "{msg}"), call = call)

.is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
.is_strings <- function(x) is.character(x) && length(x) >= 1 && !anyNA(x) && all(nzchar(x))
.as_strings <- function(x) if (is.list(x)) unlist(x, use.names = FALSE) else x

#' Check a custom chart description
#'
#' A custom chart (report kind `"custom_chart"`) is described by data, never code: where its table comes from, a few
#' fixed transforms and a plot description. `report_validate_spec()` checks one before it is saved, and returns it
#' tidied (lists from JSON turned into vectors where a vector is meant).
#'
#' @param spec The description: `list(title, data = list(member, args), transform, plot)`. `transform` is a list of
#'   one-entry lists, each one of `filter` (`list(column = values)`), `select` (column names), `rename`
#'   (`list(new = "old")`), `mutate_ratio` (`list(name, numerator, denominator, percent)`), `aggregate`
#'   (`list(by, fun, cols)`; `fun` one of sum, mean, median, min, max), `pivot_longer`
#'   (`list(cols, names_to, values_to)`), `arrange` (`list(by, desc)`), `top_n` (`list(n, by, desc)`). `plot` is
#'   `list(geom, x, y, colour, fill, facet, position, labels, percent, flip)` with `geom` one of line, col, point,
#'   area, tile.
#' @param members The names `data$member` may take, or `NULL` not to check it.
#' @return The spec, tidied. Stops with a message saying what is wrong.
#' @examples
#' report_validate_spec(list(
#'   title = "Coverage by region",
#'   data = list(member = "calculate_coverage", args = list(admin_level = "adminlevel_1")),
#'   transform = list(list(filter = list(year = 2023))),
#'   plot = list(geom = "col", x = "adminlevel_1", y = "value")
#' ))
#' @export
report_validate_spec <- function(spec, members = NULL) {
  if (!is.list(spec)) .spec_fail("A custom chart must be a list (title, data, transform, plot).")
  if (!is.null(spec$title) && !.is_string(spec$title)) .spec_fail("The chart's title must be one piece of text.")

  data <- spec$data
  if (!is.list(data) || !.is_string(data$member)) .spec_fail("Say where the chart's data comes from: data = list(member = <name>, args = list(...)).")
  if (!is.null(members) && !data$member %in% members) {
    .spec_fail(sprintf("\"%s\" can't be charted. Members that can: %s.", data$member, paste(members, collapse = ", ")))
  }
  if (!is.null(data$args) && !is.list(data$args)) .spec_fail("data$args must be a named list of arguments.")
  if (length(data$args) && (is.null(names(data$args)) || any(!nzchar(names(data$args))))) .spec_fail("Every argument in data$args needs a name.")

  steps <- spec$transform %||% list()
  if (!is.list(steps)) .spec_fail("transform must be a list of steps, each list(<step> = ...).")
  spec$transform <- lapply(seq_along(steps), function(i) .spec_check_step(steps[[i]], i))

  spec$plot <- .spec_check_plot(spec$plot)
  spec
}

.spec_check_step <- function(step, i) {
  if (!is.list(step) || length(step) != 1 || is.null(names(step)) || !names(step) %in% .spec_transforms) {
    .spec_fail(sprintf("Step %d of transform must be one of %s, as list(<step> = ...).", i, paste(.spec_transforms, collapse = ", ")))
  }
  op <- names(step)
  x <- step[[1]]
  bad <- function(what) .spec_fail(sprintf("Step %d (%s): %s.", i, op, what))
  switch(op,
    filter = {
      if (!is.list(x) || !length(x) || is.null(names(x))) bad("give list(column = values to keep)")
      x <- lapply(x, .as_strings_keep)
    },
    select = {
      x <- .as_strings(x)
      if (!.is_strings(x)) bad("give the column names to keep")
    },
    rename = {
      if (!is.list(x) && !is.character(x)) bad("give list(new_name = \"old_name\")")
      x <- as.list(x)
      if (is.null(names(x)) || !all(vapply(x, .is_string, NA))) bad("give list(new_name = \"old_name\")")
    },
    mutate_ratio = {
      if (!is.list(x) || !.is_string(x$name) || !.is_string(x$numerator) || !.is_string(x$denominator)) bad("give list(name, numerator, denominator, percent)")
      x$percent <- isTRUE(x$percent)
    },
    aggregate = {
      if (!is.list(x)) bad("give list(by, fun, cols)")
      x$by <- .as_strings(x$by %||% character())
      x$cols <- .as_strings(x$cols)
      x$fun <- x$fun %||% "sum"
      if (!.is_strings(x$cols)) bad("say which columns to aggregate (cols)")
      if (!.is_string(x$fun) || !x$fun %in% .spec_funs) bad(sprintf("fun must be one of %s", paste(.spec_funs, collapse = ", ")))
    },
    pivot_longer = {
      if (!is.list(x)) bad("give list(cols, names_to, values_to)")
      x$cols <- .as_strings(x$cols)
      x$names_to <- x$names_to %||% "name"
      x$values_to <- x$values_to %||% "value"
      if (!.is_strings(x$cols) || !.is_string(x$names_to) || !.is_string(x$values_to)) bad("give list(cols, names_to, values_to)")
    },
    arrange = {
      if (is.character(x)) x <- list(by = x)
      if (!is.list(x)) bad("give list(by, desc)")
      x$by <- .as_strings(x$by)
      x$desc <- isTRUE(x$desc)
      if (!.is_strings(x$by)) bad("say which columns to sort by (by)")
    },
    top_n = {
      if (!is.list(x) || !is.numeric(x$n) || length(x$n) != 1 || x$n < 1 || !.is_string(x$by)) bad("give list(n, by, desc)")
      x$desc <- if (is.null(x$desc)) TRUE else isTRUE(x$desc)
    }
  )
  stats::setNames(list(x), op)
}

.as_strings_keep <- function(v) if (is.list(v)) unlist(v, use.names = FALSE) else v

.spec_check_plot <- function(plot) {
  if (!is.list(plot)) .spec_fail("Describe the plot: plot = list(geom, x, y, ...).")
  if (!.is_string(plot$geom) || !plot$geom %in% .spec_geoms) .spec_fail(sprintf("plot$geom must be one of %s.", paste(.spec_geoms, collapse = ", ")))
  if (!.is_string(plot$x) || !.is_string(plot$y)) .spec_fail("Say which columns go on the axes: plot$x and plot$y.")
  for (f in c("colour", "fill", "facet")) {
    if (!is.null(plot[[f]]) && !.is_string(plot[[f]])) .spec_fail(sprintf("plot$%s must be a column name.", f))
  }
  if (identical(plot$geom, "tile") && is.null(plot$fill)) .spec_fail("A tile plot needs plot$fill (the column that colours the tiles).")
  if (!is.null(plot$position) && !plot$position %in% c("dodge", "stack", "fill", "identity")) .spec_fail("plot$position must be dodge, stack, fill or identity.")
  if (!is.null(plot$labels) && !is.list(plot$labels)) .spec_fail("plot$labels must be list(x, y, title, ...).")
  plot$percent <- isTRUE(plot$percent)
  plot$flip <- isTRUE(plot$flip)
  plot
}

#' Apply a custom chart's transforms to its data
#'
#' @param data A data frame.
#' @param transform The spec's `transform` steps (see [report_validate_spec()]).
#' @return The data frame, transformed. Stops with a message naming a missing column.
#' @examples
#' d <- data.frame(region = c("A", "B", "A"), year = c(2022, 2022, 2023), anc4 = c(50, 60, 55))
#' report_apply_transforms(d, list(list(filter = list(year = 2022)),
#'                                list(select = c("region", "anc4"))))
#' @export
report_apply_transforms <- function(data, transform = list()) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  need <- function(cols, op) {
    missing <- setdiff(cols, names(data))
    if (length(missing)) .spec_fail(sprintf("%s: no column %s. The columns are: %s.", op, paste(missing, collapse = ", "), paste(names(data), collapse = ", ")))
  }
  for (step in transform) {
    op <- names(step)
    x <- step[[1]]
    data <- switch(op,
      filter = {
        need(names(x), "filter")
        keep <- rep(TRUE, nrow(data))
        for (col in names(x)) keep <- keep & as.character(data[[col]]) %in% as.character(x[[col]])
        data[keep, , drop = FALSE]
      },
      select = {
        need(x, "select")
        data[, x, drop = FALSE]
      },
      rename = {
        need(unlist(x), "rename")
        for (new in names(x)) names(data)[names(data) == x[[new]]] <- new
        data
      },
      mutate_ratio = {
        need(c(x$numerator, x$denominator), "mutate_ratio")
        ratio <- data[[x$numerator]] / data[[x$denominator]]
        ratio[!is.finite(ratio)] <- NA
        data[[x$name]] <- if (isTRUE(x$percent)) ratio * 100 else ratio
        data
      },
      aggregate = {
        need(c(x$by, x$cols), "aggregate")
        fun <- switch(x$fun, sum = function(v) sum(v, na.rm = TRUE), mean = function(v) mean(v, na.rm = TRUE),
                      median = function(v) stats::median(v, na.rm = TRUE), min = function(v) suppressWarnings(min(v, na.rm = TRUE)),
                      max = function(v) suppressWarnings(max(v, na.rm = TRUE)))
        if (length(x$by)) {
          stats::aggregate(data[x$cols], by = data[x$by], FUN = fun)
        } else {
          as.data.frame(lapply(stats::setNames(x$cols, x$cols), function(col) fun(data[[col]])))
        }
      },
      pivot_longer = {
        need(x$cols, "pivot_longer")
        ids <- setdiff(names(data), x$cols)
        parts <- lapply(x$cols, function(col) {
          part <- data[, ids, drop = FALSE]
          part[[x$names_to]] <- rep(col, nrow(data))
          part[[x$values_to]] <- data[[col]]
          part
        })
        out <- do.call(rbind, parts)
        rownames(out) <- NULL
        out
      },
      arrange = {
        need(x$by, "arrange")
        ord <- do.call(order, c(unname(lapply(data[x$by], function(v) if (is.numeric(v)) v else xtfrm(v))), list(decreasing = isTRUE(x$desc))))
        data[ord, , drop = FALSE]
      },
      top_n = {
        need(x$by, "top_n")
        ord <- order(data[[x$by]], decreasing = isTRUE(x$desc))
        data[utils::head(ord, x$n), , drop = FALSE]
      }
    )
  }
  rownames(data) <- NULL
  data
}

#' Draw a custom chart's plot description
#'
#' Generic: a table in, a ggplot out. Chart options (the customize panel) apply to the result like to any other
#' chart.
#'
#' @param data The (transformed) data frame.
#' @param plot The plot description (see [report_validate_spec()]): `geom` (line, col, point, area, tile), `x`, `y`,
#'   and optionally `colour`, `fill`, `facet`, `position` (dodge, stack, fill), `labels` (`list(x, y, title,
#'   colour, fill)`), `percent` (the y values are percentages, 0 to 100) and `flip` (bars go across).
#' @param title A title, used when `plot$labels$title` isn't given.
#' @return A ggplot.
#' @examples
#' d <- data.frame(region = c("A", "B"), value = c(55, 61))
#' report_plot_spec(d, list(geom = "col", x = "region", y = "value", percent = TRUE))
#' @export
report_plot_spec <- function(data, plot, title = NULL) {
  plot <- .spec_check_plot(plot)
  cols <- c(plot$x, plot$y, plot$colour, plot$fill, plot$facet)
  missing <- setdiff(cols, names(data))
  if (length(missing)) .spec_fail(sprintf("The plot uses %s, which the data doesn't have. The columns are: %s.", paste(missing, collapse = ", "), paste(names(data), collapse = ", ")))

  .data <- rlang::.data
  mapping <- ggplot2::aes(x = .data[[plot$x]], y = .data[[plot$y]])
  if (!is.null(plot$colour)) mapping$colour <- ggplot2::aes(colour = .data[[plot$colour]])$colour
  if (!is.null(plot$fill)) mapping$fill <- ggplot2::aes(fill = .data[[plot$fill]])$fill
  if (identical(plot$geom, "line")) {
    mapping$group <- if (is.null(plot$colour)) ggplot2::aes(group = 1)$group else ggplot2::aes(group = .data[[plot$colour]])$group
  }

  position <- plot$position %||% (if (plot$geom %in% c("col", "area")) "stack" else "identity")
  geom <- switch(plot$geom,
    line = ggplot2::geom_line(linewidth = 1),
    col = ggplot2::geom_col(position = if (identical(position, "dodge")) ggplot2::position_dodge2(preserve = "single") else position),
    point = ggplot2::geom_point(size = 2.5),
    area = ggplot2::geom_area(position = position, alpha = 0.8),
    tile = ggplot2::geom_tile(colour = "white")
  )

  p <- ggplot2::ggplot(data, mapping) + geom
  if (identical(plot$geom, "line")) p <- p + ggplot2::geom_point(size = 1.8)
  if (plot$percent && !identical(plot$geom, "tile")) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%"), expand = ggplot2::expansion(mult = c(0, 0.05)))
  }
  if (!is.null(plot$facet)) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[plot$facet]]))
  if (plot$flip) p <- p + ggplot2::coord_flip()

  labels <- plot$labels %||% list()
  p + ggplot2::labs(
    title = labels$title %||% title,
    x = labels$x %||% plot$x,
    y = labels$y %||% plot$y,
    colour = labels$colour %||% plot$colour,
    fill = labels$fill %||% plot$fill
  ) + ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}

# The block types a report may have.
.report_block_types <- c("heading", "paragraph", "note", "pagebreak", "chart", "table", "image", "canvas")

#' Check a report before it is saved
#'
#' Checks the shape of a report project (see [export_report()]) -- one the AI built, say -- before an app saves it:
#' each block's type, and for charts and tables their kind. A `custom_chart` block may carry its description in
#' `spec` or, as the AI bridge sends it, in `options`; either way it is checked with [report_validate_spec()] and kept
#' in `spec`.
#'
#' @param project `list(name, design, cover, blocks)`.
#' @param kinds The report kinds charts and tables may use (names), or `NULL` not to check them.
#' @param members Passed to [report_validate_spec()] for custom charts.
#' @return The project, tidied (each block has an id). Stops with a message saying what is wrong.
#' @examples
#' report_validate_project(list(name = "Notes", blocks = list(
#'   list(type = "heading", text = "Summary"),
#'   list(type = "paragraph", text = "Coverage rose.")
#' )))
#' @export
report_validate_project <- function(project, kinds = NULL, members = NULL) {
  if (!is.list(project)) .spec_fail("A report must be a list: name, blocks (and optionally design, cover).")
  if (!is.null(project$name) && !.is_string(project$name)) .spec_fail("The report's name must be one piece of text.")
  blocks <- project$blocks
  if (!is.list(blocks) || !length(blocks)) .spec_fail("A report needs blocks: a list of headings, paragraphs, charts, tables...")
  ids <- character()
  project$blocks <- lapply(seq_along(blocks), function(i) {
    b <- blocks[[i]]
    where <- sprintf("Block %d", i)
    if (!is.list(b) || !.is_string(b$type) || !b$type %in% .report_block_types) {
      .spec_fail(sprintf("%s: its type must be one of %s.", where, paste(.report_block_types, collapse = ", ")))
    }
    if (is.null(b$id) || !.is_string(b$id) || b$id %in% ids) b$id <- paste0("ai", i)
    while (b$id %in% ids) b$id <- paste0(b$id, "x")
    ids <<- c(ids, b$id)
    if (b$type %in% c("heading", "paragraph", "note") && !is.character(b$text %||% "")) .spec_fail(sprintf("%s: its text must be text.", where))
    if (b$type %in% c("chart", "table")) {
      if (!.is_string(b$kind)) .spec_fail(sprintf("%s: a %s needs a kind (see the report kinds).", where, b$type))
      if (identical(b$kind, "custom_chart")) {
        spec <- b$spec
        if (is.null(spec) && is.list(b$options) && !is.null(b$options$plot)) {
          spec <- b$options
          b$options <- NULL
        }
        b$spec <- report_validate_spec(spec, members = members)
      } else if (!is.null(kinds) && !b$kind %in% kinds) {
        .spec_fail(sprintf("%s: there is no kind \"%s\". Kinds: %s.", where, b$kind, paste(kinds, collapse = ", ")))
      }
    }
    b
  })
  project
}
