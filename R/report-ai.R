# What the AI reads and changes of a saved report (the apps' AI bridge actions listReports, readReport and
# updateBlocks): the reports listed, a report read compactly -- its text, and for each chart and table what it is and
# a short table of the data it shows, so a narrative can be written from it -- and targeted changes to its blocks
# (text, a chart's kind and settings, its chart options, its layout; insert, delete, move), each by block id, applied
# in order and checked as a whole. Nothing else in the report changes: its design, cover and the blocks not named.

.rb_ai_fail <- function(...) stop(sprintf(...), call. = FALSE)

.rb_ai_text_types <- c("heading", "paragraph", "note")

# ---- listing -------------------------------------------------------------------------------------------------------

#' The saved reports, as the AI lists them
#'
#' @param projects The saved reports, a named list by id (a dataset's `report_projects`).
#' @return A list, the last edited first, of `list(id, name, kind, lang, blocks, updated)`; `kind` is `"report"` (a
#'   document) or `"deck"` (a slide deck), `blocks` the number of blocks (a deck's: of its slides' items).
#' @examples
#' report_list(list(r1 = list(name = "Coverage", blocks = list(report_paragraph("Hello")))))
#' @export
report_list <- function(projects) {
  if (!is.list(projects) || !length(projects)) return(list())
  rows <- lapply(names(projects), function(id) {
    p <- projects[[id]]
    if (!is.list(p)) return(NULL)
    Filter(Negate(is.null), list(
      id = id, name = p$name %||% "", kind = if (identical(p$kind, "deck")) "deck" else "report",
      lang = if (.is_string(p$lang)) p$lang, blocks = length(report_project_blocks(p)), updated = p$updated %||% ""
    ))
  })
  rows <- Filter(Negate(is.null), rows)
  rows[order(vapply(rows, function(r) r$updated, character(1)), decreasing = TRUE)]
}

# ---- reading -------------------------------------------------------------------------------------------------------

#' A saved report, as the AI reads it
#'
#' Every block in order, compactly: its id and type; a heading's, paragraph's or note's text (plain, the formatting
#' left out); a chart's or table's kind, settings, chart options and layout, and -- with `data = TRUE` -- a short table
#' of what it shows (drawn as the report draws it: the chart's title, subtitle and caption, and the values it plots, or
#' the table's rows), so the AI can write about it from its numbers.
#'
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param project The report.
#' @param id The report's id (by default its own).
#' @param i18n A translator (`t(key)`) the charts are drawn with. By default the app's, in the report's language.
#' @param lang The language a report without one of its own is written in (it follows the app's).
#' @param data Whether to draw the charts and tables for their data.
#' @param max_rows,max_cols The most rows and columns of each chart's or table's data.
#' @param max_chars The most characters of each text.
#' @return `list(id, name, kind, lang, region, blocks)`. A slide deck's blocks are its slides' items (each with its
#'   `slide`); a document's free page (`canvas`) lists its items' blocks in `items`.
#' @export
report_read <- function(context, project, id = project$id, i18n = NULL, lang = "en", data = TRUE, max_rows = 25,
                        max_cols = 8, max_chars = 2000) {
  if (!is.list(project)) .rb_ai_fail("There is no such report.")
  report_lang <- if (.is_string(project$lang)) project$lang else lang
  if (is.null(i18n)) i18n <- tryCatch(cd_report_translator(cd_i18n(), report_lang), error = function(e) NULL)
  kinds <- .ds_report_kinds()
  one <- function(b) .rb_ai_read_block(b, context, project, i18n, kinds, data, max_rows, max_cols, max_chars)
  blocks <- if (identical(project$kind, "deck")) {
    out <- list()
    for (s in seq_along(project$slides %||% list())) {
      for (item in project$slides[[s]]$items %||% list()) {
        b <- item$block %||% list()
        b$id <- item$id %||% b$id
        b$box <- c(as.numeric(item$w %||% 1), as.numeric(item$h %||% 1))
        out[[length(out) + 1]] <- c(one(b), list(slide = s))
      }
    }
    out
  } else {
    lapply(project$blocks %||% list(), function(b) {
      if (!identical(b$type, "canvas")) return(one(b))
      items <- lapply(b$items %||% list(), function(item) {
        ib <- item$block %||% list()
        ib$id <- item$id %||% ib$id
        ib$box <- c(as.numeric(item$w %||% 1), as.numeric(item$h %||% 1))
        one(ib)
      })
      list(id = b$id, type = "canvas", items = items)
    })
  }
  Filter(Negate(is.null), list(
    id = id %||% "", name = project$name %||% "", kind = if (identical(project$kind, "deck")) "deck" else "report",
    lang = report_lang, region = if (.is_string(project$region)) project$region, blocks = blocks
  ))
}

# One block, compactly
.rb_ai_read_block <- function(b, context, project, i18n, kinds, data, max_rows, max_cols, max_chars) {
  out <- list(id = b$id %||% "", type = b$type %||% "")
  type <- out$type
  if (type %in% c(.rb_ai_text_types, "list", "quote", "pre")) {
    text <- .rb_ai_plain(b$text)
    if (nchar(text) > max_chars) text <- paste0(substr(text, 1, max_chars), "\u2026")
    out$text <- text
    if (identical(type, "heading")) out$level <- as.integer(b$level %||% 1)
    if (.is_string(b$list)) out$list <- b$list
  } else if (type %in% c("chart", "table")) {
    out$kind <- b$kind %||% ""
    label <- kinds[[out$kind]]$label
    if (.is_string(label)) out$label <- label
    settings <- Filter(Negate(is.null), b[c("indicator", "admin_level", "region", "year", "variant")])
    if (length(settings)) out$settings <- settings
    if (length(b$options)) out$options <- b$options
    format <- Filter(Negate(is.null), b[c("size", "title", "caption")])
    if (length(format)) out$format <- format
    if (isTRUE(data)) {
      out$data <- tryCatch(.rb_ai_block_data(context, report_resolve_block(b, project), i18n, max_rows, max_cols),
                           error = function(e) list(error = conditionMessage(e)))
    }
  } else if (identical(type, "image")) {
    format <- Filter(Negate(is.null), b[c("caption", "alt", "size", "width", "align")])
    if (length(format)) out$format <- format
  }
  out
}

# A block's text without its formatting: lines, fields ({country}) kept
.rb_ai_plain <- function(html) {
  x <- paste(as.character(html %||% ""), collapse = "\n")
  x <- gsub("<br\\s*/?>", "\n", x, ignore.case = TRUE)
  x <- gsub("</(p|li|div|h[1-6])>", "\n", x, ignore.case = TRUE)
  x <- gsub("<[^>]*>", "", x)
  entities <- c("&nbsp;" = " ", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#39;" = "'", "&#x27;" = "'", "&amp;" = "&")
  for (e in names(entities)) x <- gsub(e, entities[[e]], x, fixed = TRUE)
  x <- gsub("[ \t]+\n", "\n", x)
  trimws(gsub("\n{2,}", "\n", x))
}

# The text the AI wrote, as a block keeps it: special characters escaped, **bold** in bold, lines as line breaks (a
# heading's on one line)
.rb_ai_html <- function(text, type) {
  x <- paste(as.character(unlist(text)), collapse = "\n")
  x <- gsub("\r\n?", "\n", x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\\*\\*([^*\n]+)\\*\\*", "<b>\\1</b>", x)
  x <- trimws(x)
  if (identical(type, "heading")) gsub("\\s*\n\\s*", " ", x) else gsub("\n", "<br>", x, fixed = TRUE)
}

# What a chart or table shows, as a short table: drawn as the report draws it
.rb_ai_block_data <- function(context, b, i18n, max_rows, max_cols) {
  r <- render_report_block(context, b, i18n)
  if (identical(r$type, "plot")) return(.rb_ai_ggplot(r$value, max_rows, max_cols))
  if (identical(r$type, "table")) return(.rb_ai_flextable(r$value, max_rows, max_cols))
  list(error = r$message %||% "It cannot be drawn with this dataset.")
}

# A data frame as a few rows: list(columns, rows (each a list of values), totalRows, truncated)
.rb_ai_rows <- function(df, max_rows, max_cols) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  keep <- !vapply(df, is.list, logical(1))
  df <- df[, keep, drop = FALSE]
  if (ncol(df) > max_cols) df <- df[, seq_len(max_cols), drop = FALSE]
  df <- unique(df)
  total <- nrow(df)
  shown <- df[seq_len(min(total, max_rows)), , drop = FALSE]
  shown[] <- lapply(shown, function(col) {
    if (is.factor(col) || inherits(col, c("Date", "POSIXt", "difftime"))) as.character(col)
    else if (is.double(col)) round(col, 2)
    else col
  })
  rows <- lapply(seq_len(nrow(shown)), function(i) unname(lapply(shown[i, , drop = FALSE], function(v) if (is.na(v[[1]])) NULL else v[[1]])))
  list(columns = names(df), rows = rows, totalRows = total, truncated = total > nrow(shown))
}

# A ggplot's titles and the values it plots: each layer's mapped aesthetics (named by the chart's labels; a series
# given as a constant, e.g. colour = "Survey", is a column too) and the panels' variables, evaluated on its data. The
# layers are one table (their columns together), without the rows that plot nothing, in the order of the first column.
.rb_ai_ggplot <- function(p, max_rows, max_cols) {
  labs <- tryCatch(ggplot2::get_labs(p), error = function(e) tryCatch(p$labels, error = function(e) list()))
  text_of <- function(x) if (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)) x
  out <- Filter(Negate(is.null), list(title = text_of(labs$title), subtitle = text_of(labs$subtitle),
                                      caption = text_of(labs$caption)))
  aes_names <- c("x", "y", "xmin", "xmax", "ymin", "ymax", "xend", "yend", "fill", "colour", "label", "size", "shape",
                 "linetype")
  unnamed <- c(fill = "series", colour = "series", shape = "series", linetype = "series")
  facets <- tryCatch({
    fp <- p$facet$params
    c(fp$facets, fp$rows, fp$cols)
  }, error = function(e) NULL)
  frames <- list()
  for (layer in p$layers) {
    data <- layer$data
    if (is.function(data)) data <- tryCatch(data(p$data), error = function(e) NULL)
    if (!is.data.frame(data)) data <- p$data
    if (!is.data.frame(data) || !nrow(data)) next
    mapping <- as.list(layer$mapping %||% list())
    if (!isFALSE(layer$inherit.aes)) {
      for (n in names(p$mapping)) if (is.null(mapping[[n]])) mapping[[n]] <- p$mapping[[n]]
    }
    names(mapping) <- sub("^color$", "colour", names(mapping))
    cols <- list()
    add <- function(name, quo) {
      value <- tryCatch(rlang::eval_tidy(quo, data), error = function(e) NULL)
      if (is.null(value) || is.list(value) || inherits(value, "sfc")) return()
      if (length(value) == 1 && nrow(data) > 1) value <- rep(value, nrow(data))
      if (length(value) != nrow(data)) return()
      if (name %in% names(cols) || any(vapply(cols, function(v) identical(v, value), logical(1)))) return()
      cols[[name]] <<- value
    }
    for (a in intersect(aes_names, names(mapping))) add(text_of(labs[[a]]) %||% (if (a %in% names(unnamed)) unnamed[[a]] else a), mapping[[a]])
    for (f in names(facets)) add(f, facets[[f]])
    if (!length(cols)) next
    frames[[length(frames) + 1]] <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!length(frames)) return(c(out, list(note = "The chart plots no values that can be read.")))
  columns <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(f) {
    for (n in setdiff(columns, names(f))) f[[n]] <- NA
    f[columns]
  })
  df <- do.call(rbind, frames)
  values <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], names(df)[1])
  if (length(values)) df <- df[rowSums(!is.na(df[, values, drop = FALSE])) > 0, , drop = FALSE]
  if (nrow(df)) df <- df[order(df[[1]]), , drop = FALSE]
  out$data <- .rb_ai_rows(df, max_rows, max_cols)
  out
}

# A flextable's rows, its columns named by its (last) header row
.rb_ai_flextable <- function(ft, max_rows, max_cols) {
  body <- ft$body$dataset
  keys <- intersect(ft$col_keys %||% names(body), names(body))
  body <- body[, keys, drop = FALSE]
  header <- tryCatch(ft$header$dataset, error = function(e) NULL)
  if (is.data.frame(header) && nrow(header) && all(keys %in% names(header))) {
    labels <- as.character(unlist(header[nrow(header), keys]))
    if (!anyNA(labels) && all(nzchar(labels)) && !anyDuplicated(labels)) names(body) <- labels
  }
  list(data = .rb_ai_rows(body, max_rows, max_cols))
}

# ---- changing -------------------------------------------------------------------------------------------------------

#' Change blocks of a saved report
#'
#' Applies targeted changes, in order, to a report's blocks, and checks the result as a whole
#' ([report_validate_project()]). Only the blocks named change; the design, the cover and every other block stay as
#' they are. Each change is one of:
#'
#' * `list(blockId, <fields>)`: changes that block.
#'   - A heading, paragraph or note: `text` (plain text; a line break starts a new line, `**bold**` is bold), `type`
#'     (`"heading"`, `"paragraph"` or `"note"`), `level` (a heading's, 1 to 6), `align`, `space_before`,
#'     `space_after` (pt).
#'   - A chart or table: `kind` (another kind; the settings the new kind does not take are left out, those it needs
#'     get its defaults), its settings `indicator`, `admin_level`, `region`, `year`, `variant` (as the kind allows;
#'     `NULL` removes one), `options` (chart options, see [cd_chart_options()]; merged into the chart's own, `NULL`
#'     removes one), `size` (`"full"`, `"half"`, `"third"`), `title` (`NULL`: the chart's own), `caption` (`FALSE`
#'     hides the chart's caption).
#'   - A picture: `caption`, `alt`, `size`, `width` (percent of its column), `align`.
#'   - Any block of a document: `pageBreakBefore` (`TRUE` starts a new page before it, `FALSE` removes that).
#' * `list(afterBlockId, insert = <block>)`: a new heading, paragraph, note, page break, chart or table after that
#'   block (`"@start"`: at the start). Several inserted after the same block keep their order.
#' * `list(blockId, delete = TRUE)`: removes the block.
#' * `list(blockId, moveAfter = <id>)`: moves the block after another (`"@start"`: to the start).
#'
#' A slide deck's items can be changed, but not added, removed or moved.
#'
#' @param project The report.
#' @param changes A list of changes.
#' @param kinds The kinds of chart and table (a named list of [report_kind()]); by default the app's.
#' @param members Passed to [report_validate_project()] for custom charts.
#' @return `list(project, changes)`: the changed report (without its `sig`s on changed charts: the builder signs them
#'   again) and one sentence per change, saying what it did.
#' @examples
#' p <- list(name = "Notes", blocks = list(list(id = "b1", type = "paragraph", text = "")))
#' report_update_blocks(p, list(list(blockId = "b1", text = "Coverage rose."),
#'                              list(afterBlockId = "b1", insert = list(type = "heading", text = "Next steps"))))$changes
#' @export
report_update_blocks <- function(project, changes, kinds = .ds_report_kinds(), members = NULL) {
  if (!is.list(project)) .rb_ai_fail("There is no such report.")
  if (is.data.frame(changes)) changes <- lapply(seq_len(nrow(changes)), function(i) as.list(changes[i, , drop = FALSE]))
  if (!is.list(changes) || !length(changes)) .rb_ai_fail("Give the changes: a list of { blockId, ... } or { afterBlockId, insert }.")
  deck <- identical(project$kind, "deck")
  said <- character()
  # where each block inserted after a block went, so the next one goes after it
  inserted_after <- list()
  for (i in seq_along(changes)) {
    ch <- changes[[i]]
    where <- sprintf("Change %d", i)
    if (!is.list(ch)) .rb_ai_fail("%s: a change must be an object.", where)
    if (!is.null(ch$insert)) {
      if (deck) .rb_ai_fail("%s: a slide deck's items can be changed, but not added, removed or moved (do that in the Reports page).", where)
      anchor <- .rb_ai_id(ch$afterBlockId %||% ch$after, where, "afterBlockId")
      at <- if (identical(anchor, "@start")) 0L else .rb_ai_top(project, anchor, where)
      last <- inserted_after[[anchor]]
      if (!is.null(last)) at <- .rb_ai_top(project, last, where)
      b <- .rb_ai_new_block(ch$insert, project, kinds, where)
      project$blocks <- append(project$blocks, list(b), after = at)
      inserted_after[[anchor]] <- b$id
      after_label <- if (identical(anchor, "@start")) "at the start" else paste("after", .rb_ai_label(project$blocks[[at]], kinds))
      said <- c(said, sprintf("Add %s %s", .rb_ai_label(b, kinds, article = TRUE), after_label))
      next
    }
    id <- .rb_ai_id(ch$blockId %||% ch$id, where, "blockId")
    if (isTRUE(ch$delete)) {
      if (deck) .rb_ai_fail("%s: a slide deck's items can be changed, but not added, removed or moved (do that in the Reports page).", where)
      at <- .rb_ai_top(project, id, where)
      said <- c(said, sprintf("Remove %s", .rb_ai_label(project$blocks[[at]], kinds)))
      project$blocks <- project$blocks[-at]
      next
    }
    if (!is.null(ch$moveAfter)) {
      if (deck) .rb_ai_fail("%s: a slide deck's items can be changed, but not added, removed or moved (do that in the Reports page).", where)
      target <- .rb_ai_id(ch$moveAfter, where, "moveAfter")
      if (identical(target, id)) .rb_ai_fail("%s: a block can't be moved after itself.", where)
      at <- .rb_ai_top(project, id, where)
      b <- project$blocks[[at]]
      project$blocks <- project$blocks[-at]
      to <- if (identical(target, "@start")) 0L else .rb_ai_top(project, target, where)
      project$blocks <- append(project$blocks, list(b), after = to)
      said <- c(said, sprintf("Move %s %s", .rb_ai_label(b, kinds),
                              if (to == 0) "to the start" else paste("after", .rb_ai_label(project$blocks[[to]], kinds))))
      next
    }
    loc <- .rb_ai_find(project, id, where)
    fields <- setdiff(names(ch), c("blockId", "id"))
    if (!length(fields)) .rb_ai_fail("%s: say what to change in block %s.", where, id)
    old <- .rb_ai_get(project, loc)
    if ("pageBreakBefore" %in% fields) {
      if (!identical(loc$where, "doc")) .rb_ai_fail("%s: only a document's blocks can start a new page.", where)
      at <- loc$i
      before <- at > 1 && identical(project$blocks[[at - 1]]$type, "pagebreak")
      if (isTRUE(ch$pageBreakBefore) && !before) {
        project$blocks <- append(project$blocks, list(list(id = .rb_ai_new_id(project), type = "pagebreak")), after = at - 1)
        loc$i <- at + 1L
      } else if (isFALSE(ch$pageBreakBefore) && before) {
        project$blocks <- project$blocks[-(at - 1)]
        loc$i <- at - 1L
      }
      fields <- setdiff(fields, "pageBreakBefore")
    }
    new <- if (length(fields)) .rb_ai_change_block(old, ch[fields], kinds, where) else old
    project <- .rb_ai_set(project, loc, new)
    parts <- .rb_ai_what_changed(old, new, kinds)
    if ("pageBreakBefore" %in% names(ch)) parts <- c(parts, if (isTRUE(ch$pageBreakBefore)) "new page before it" else "no new page before it")
    said <- c(said, sprintf("%s: %s", .rb_ai_label(old, kinds, capital = TRUE), if (length(parts)) paste(parts, collapse = "; ") else "unchanged"))
  }
  # the report as a whole
  check <- if (deck) list(name = project$name, blocks = report_project_blocks(project)) else project
  if (!deck && !length(project$blocks)) .rb_ai_fail("A report needs at least one block.")
  if (length(check$blocks)) report_validate_project(check, kinds = names(kinds) %||% NULL, members = members)
  list(project = project, changes = said)
}

# An id the AI gave
.rb_ai_id <- function(x, where, what) {
  x <- unlist(x)
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) .rb_ai_fail("%s: %s must be a block id (from readReport).", where, what)
  x
}

# A document's top-level block's index
.rb_ai_top <- function(project, id, where) {
  ids <- vapply(project$blocks %||% list(), function(b) as.character(b$id %||% ""), character(1))
  at <- match(id, ids)
  if (is.na(at)) {
    loc <- tryCatch(.rb_ai_find(project, id, where), error = function(e) NULL)
    if (!is.null(loc)) .rb_ai_fail("%s: block %s is on a free page; only the report's own blocks can be added after, removed or moved.", where, id)
    .rb_ai_fail("%s: there is no block %s in this report (readReport gives the block ids).", where, id)
  }
  at
}

# Where block `id` is: list(where = "doc", i), list(where = "canvas", i, j) or list(where = "deck", s, j)
.rb_ai_find <- function(project, id, where) {
  if (identical(project$kind, "deck")) {
    for (s in seq_along(project$slides %||% list())) {
      items <- project$slides[[s]]$items %||% list()
      for (j in seq_along(items)) if (identical(as.character(items[[j]]$id %||% ""), id)) return(list(where = "deck", s = s, j = j))
    }
  } else {
    blocks <- project$blocks %||% list()
    for (i in seq_along(blocks)) {
      if (identical(as.character(blocks[[i]]$id %||% ""), id)) return(list(where = "doc", i = i))
      if (identical(blocks[[i]]$type, "canvas")) {
        items <- blocks[[i]]$items %||% list()
        for (j in seq_along(items)) if (identical(as.character(items[[j]]$id %||% ""), id)) return(list(where = "canvas", i = i, j = j))
      }
    }
  }
  .rb_ai_fail("%s: there is no block %s in this report (readReport gives the block ids).", where, id)
}

.rb_ai_get <- function(project, loc) {
  item <- switch(loc$where, doc = return(project$blocks[[loc$i]]), canvas = project$blocks[[loc$i]]$items[[loc$j]],
                 deck = project$slides[[loc$s]]$items[[loc$j]])
  b <- item$block %||% list()
  b$id <- item$id
  b
}

.rb_ai_set <- function(project, loc, b) {
  if (identical(loc$where, "doc")) {
    project$blocks[[loc$i]] <- b
  } else if (identical(loc$where, "canvas")) {
    b$id <- project$blocks[[loc$i]]$items[[loc$j]]$id
    project$blocks[[loc$i]]$items[[loc$j]]$block <- b
  } else {
    b$id <- project$slides[[loc$s]]$items[[loc$j]]$id
    project$slides[[loc$s]]$items[[loc$j]]$block <- b
  }
  project
}

# An id no block of the report has
.rb_ai_new_id <- function(project) {
  ids <- unlist(lapply(project$blocks %||% list(), function(b) c(b$id, vapply(b$items %||% list(), function(it) as.character(it$id %||% ""), ""))))
  i <- length(ids) + 1L
  while (paste0("ai", i) %in% ids) i <- i + 1L
  paste0("ai", i)
}

# A block the AI adds
.rb_ai_new_block <- function(insert, project, kinds, where) {
  if (!is.list(insert)) .rb_ai_fail("%s: insert must be a block, e.g. { type: \"paragraph\", text: \"...\" }.", where)
  type <- unlist(insert$type)
  allowed <- c(.rb_ai_text_types, "pagebreak", "chart", "table")
  if (!is.character(type) || length(type) != 1 || !type %in% allowed) {
    .rb_ai_fail("%s: an inserted block's type must be one of %s.", where, paste(allowed, collapse = ", "))
  }
  b <- list(id = .rb_ai_new_id(project), type = type)
  if (type %in% .rb_ai_text_types) {
    if (is.null(insert$text)) .rb_ai_fail("%s: a new %s needs its text.", where, type)
    if (identical(type, "heading")) b$level <- 2L
  }
  if (type %in% c("chart", "table")) {
    if (is.null(insert$kind)) .rb_ai_fail("%s: a new %s needs its kind.", where, type)
    b$size <- "full"
    b$caption <- TRUE
  }
  fields <- setdiff(names(insert), c("type", "id"))
  if (type %in% c("chart", "table") && !"kind" %in% fields) fields <- c("kind", fields)
  if (length(fields)) b <- .rb_ai_change_block(b, insert[fields], kinds, where, new = TRUE)
  b
}

.rb_ai_block_fields <- list(
  text = c("text", "type", "level", "align", "space_before", "space_after"),
  data = c("kind", "indicator", "admin_level", "region", "year", "variant", "options", "size", "title", "caption"),
  image = c("caption", "alt", "size", "width", "align")
)

# One block with `fields` (a named list) changed; stops, for the AI, at the first that is wrong
.rb_ai_change_block <- function(b, fields, kinds, where, new = FALSE) {
  type <- b$type %||% ""
  group <- if (type %in% .rb_ai_text_types) "text" else if (type %in% c("chart", "table")) "data" else if (identical(type, "image")) "image" else NULL
  label <- sprintf("%s: block %s (%s)", where, b$id %||% "", type)
  if (is.null(group)) {
    .rb_ai_fail("%s can't be changed by the AI (it can be moved, removed or given a new page before it).", label)
  }
  unknown <- setdiff(names(fields), .rb_ai_block_fields[[group]])
  if (length(unknown)) {
    .rb_ai_fail("%s has no %s to change. What can be changed: %s.", label, paste(unknown, collapse = ", "),
                paste(.rb_ai_block_fields[[group]], collapse = ", "))
  }
  get <- function(name) fields[[name]]
  one <- function(name) {
    v <- unlist(fields[[name]])
    if (length(v) != 1 || is.na(v)) .rb_ai_fail("%s: %s must be one value.", label, name)
    v
  }
  if (group == "text") {
    if ("type" %in% names(fields)) {
      to <- one("type")
      if (!to %in% .rb_ai_text_types) .rb_ai_fail("%s: a text block's type must be heading, paragraph or note.", label)
      if (!identical(to, type)) {
        b$type <- to
        if (identical(to, "heading")) b$level <- as.integer(b$level %||% 2) else b$level <- NULL
        b$list <- NULL
      }
    }
    if ("text" %in% names(fields)) {
      text <- get("text")
      if (!is.null(text) && !is.character(unlist(text))) .rb_ai_fail("%s: text must be text.", label)
      b$text <- .rb_ai_html(text %||% "", b$type)
    }
    if ("level" %in% names(fields)) {
      if (!identical(b$type, "heading")) .rb_ai_fail("%s: only a heading has a level (make it one with type = \"heading\").", label)
      level <- suppressWarnings(as.integer(one("level")))
      if (is.na(level) || level < 1 || level > 6) .rb_ai_fail("%s: a heading's level is 1 to 6.", label)
      b$level <- level
    }
    if ("align" %in% names(fields)) b$align <- .rb_ai_choice(get("align"), c("left", "center", "right", "justify"), label, "align")
    for (s in intersect(c("space_before", "space_after"), names(fields))) {
      v <- get(s)
      if (is.null(v)) { b[[s]] <- NULL; next }
      v <- suppressWarnings(as.numeric(unlist(v)))
      if (length(v) != 1 || is.na(v) || v < 0 || v > 200) .rb_ai_fail("%s: %s is a space in points, 0 to 200.", label, s)
      b[[s]] <- v
    }
    return(b)
  }
  if (group == "image") {
    for (name in intersect(c("caption", "alt"), names(fields))) {
      v <- get(name)
      b[[name]] <- if (is.null(v)) NULL else as.character(one(name))
    }
    if ("size" %in% names(fields)) b$size <- .rb_ai_choice(get("size"), c("full", "half", "third"), label, "size")
    if ("align" %in% names(fields)) b$align <- .rb_ai_choice(get("align"), c("left", "center", "right"), label, "align")
    if ("width" %in% names(fields)) {
      w <- suppressWarnings(as.numeric(one("width")))
      if (is.na(w) || w < 5 || w > 100) .rb_ai_fail("%s: width is a percent of its column, 5 to 100.", label)
      b$width <- w
    }
    return(b)
  }
  # a chart or table: its kind first, then its settings as the kind allows
  given <- names(fields)
  if ("kind" %in% given) {
    kind <- as.character(one("kind"))
    if (!identical(kind, "custom_chart") && length(kinds) && !kind %in% names(kinds)) {
      .rb_ai_fail("%s: there is no kind \"%s\". Kinds: %s.", label, kind, paste(names(kinds), collapse = ", "))
    }
    b$kind <- kind
    spec <- kinds[[kind]]
    if (!is.null(spec$type) && !identical(spec$type, b$type)) {
      b$type <- spec$type
      # a table has no chart options
      if (identical(b$type, "table")) b$options <- NULL
    }
    # what the new kind does not take is left out, what it needs gets its default
    if (!is.null(spec)) b <- .rb_ai_fit_kind(b, spec, keep = given)
  }
  spec <- kinds[[b$kind %||% ""]]
  settings <- intersect(c("indicator", "admin_level", "region", "year", "variant"), given)
  for (s in settings) {
    v <- get(s)
    if (is.null(v)) { b[s] <- list(NULL); next }
    b[[s]] <- .rb_ai_setting(s, v, spec, b$kind, label)
  }
  if ("options" %in% given) {
    if (identical(b$type, "table")) .rb_ai_fail("%s: a table has no chart options.", label)
    b$options <- .rb_ai_options(b$options, get("options"), label)
  }
  if ("size" %in% given) b$size <- .rb_ai_choice(get("size"), c("full", "half", "third"), label, "size")
  if ("title" %in% given) {
    v <- get("title")
    b$title <- if (is.null(v) || !nzchar(paste(unlist(v), collapse = ""))) NULL else as.character(one("title"))
  }
  if ("caption" %in% given) {
    v <- unlist(get("caption"))
    if (!is.null(v) && !(is.logical(v) && length(v) == 1 && !is.na(v))) .rb_ai_fail("%s: caption is true (shown) or false (hidden).", label)
    b$caption <- if (isFALSE(v)) FALSE else TRUE
  }
  if (!new && any(given %in% c("kind", settings, "options", "size", "title", "caption"))) b$sig <- NULL
  b
}

# One of `allowed`, or an error that lists them
.rb_ai_choice <- function(v, allowed, label, name) {
  v <- unlist(v)
  if (!is.character(v) || length(v) != 1 || !v %in% allowed) .rb_ai_fail("%s: %s must be one of %s.", label, name, paste(allowed, collapse = ", "))
  v
}

# A block after a change of kind: settings the kind does not take dropped (unless given in the change, which are then
# checked), those it needs set to its defaults
.rb_ai_fit_kind <- function(b, spec, keep) {
  drop <- function(s) if (!s %in% keep) b[s] <<- list(NULL)
  indicators <- unlist(spec$indicators)
  if (!length(indicators)) drop("indicator")
  else if (!isTRUE(b$indicator %in% indicators)) b$indicator <- if ("anc4" %in% indicators) "anc4" else indicators[[1]]
  levels <- unlist(spec$levels)
  if (!length(levels)) drop("admin_level")
  else if (!isTRUE(b$admin_level %in% levels)) b$admin_level <- levels[[1]]
  variants <- names(spec$variants)
  if (!length(variants)) drop("variant")
  else if (!isTRUE(b$variant %in% variants)) b$variant <- variants[[1]]
  if (!isTRUE(spec$year)) drop("year")
  if (!isTRUE(spec$regional) && !any(setdiff(levels, "national") %in% c("adminlevel_1", "district"))) drop("region")
  else if (isTRUE(spec$regional) && is.null(b$region)) b$region <- "@report"
  b
}

# One setting of a chart or table, checked against its kind (the allowed values in the error)
.rb_ai_setting <- function(name, v, spec, kind, label) {
  v <- unlist(v)
  if (length(v) != 1 || is.na(v)) .rb_ai_fail("%s: %s must be one value.", label, name)
  if (is.null(spec)) return(v)
  takes <- function() {
    s <- c(indicator = length(unlist(spec$indicators)) > 0, admin_level = length(unlist(spec$levels)) > 0,
           variant = length(spec$variants) > 0, year = isTRUE(spec$year),
           region = isTRUE(spec$regional) || any(unlist(spec$levels) %in% c("adminlevel_1", "district")))
    names(s)[s]
  }
  refuse <- function(allowed) {
    .rb_ai_fail("%s: the kind %s can't show %s = %s. Allowed %s: %s.", label, kind, name, v, name, paste(allowed, collapse = ", "))
  }
  if (!name %in% takes()) {
    .rb_ai_fail("%s: the kind %s has no %s. Its settings: %s.", label, kind, name,
                if (length(takes())) paste(takes(), collapse = ", ") else "none")
  }
  switch(name,
    indicator = if (!v %in% unlist(spec$indicators)) refuse(unlist(spec$indicators)) else v,
    admin_level = if (!v %in% unlist(spec$levels)) refuse(unlist(spec$levels)) else v,
    variant = if (!v %in% names(spec$variants)) refuse(names(spec$variants)) else v,
    year = {
      y <- suppressWarnings(as.integer(v))
      if (is.na(y)) .rb_ai_fail("%s: year must be a year, e.g. 2023.", label)
      y
    },
    region = as.character(v)
  )
}

# A chart's own options with `changes` merged in (NULL removes one), checked with cd_chart_options()
.rb_ai_options <- function(current, changes, label) {
  if (is.null(changes)) return(NULL)
  if (!is.list(changes) || is.null(names(changes))) .rb_ai_fail("%s: options must be an object of chart options, e.g. { \"show_labels\": true }.", label)
  unknown <- setdiff(names(changes), chart_option_fields())
  if (length(unknown)) {
    .rb_ai_fail("%s: there is no chart option %s. The chart options are: %s.", label, paste(unknown, collapse = ", "),
                paste(chart_option_fields(), collapse = ", "))
  }
  out <- if (is.list(current)) current else list()
  for (name in names(changes)) {
    v <- changes[[name]]
    # JSON arrays of numbers come as lists
    if (is.list(v) && is.null(names(v))) v <- unlist(v)
    if (name %in% c("x_limits", "y_limits") && is.logical(v)) v <- as.numeric(v)
    out[name] <- list(v)
  }
  out <- Filter(Negate(is.null), out)
  check <- lapply(out, function(v) if (is.list(v) && !is.null(names(v))) unlist(v) else v)
  tryCatch(do.call(cd_chart_options, check), error = function(e) {
    .rb_ai_fail("%s: %s", label, sub("^[^A-Za-z`]+", "", gsub("\\s+", " ", cli::ansi_strip(conditionMessage(e)))))
  })
  if (length(out)) out else NULL
}

# What a block is, for a sentence: `Chart "Coverage trend" (anc4)`, `the paragraph "Coverage rose..."`
.rb_ai_label <- function(b, kinds, article = FALSE, capital = FALSE) {
  type <- b$type %||% "block"
  noun <- switch(type, pagebreak = "page break", image = "picture", canvas = "free page", type)
  name <- if (type %in% c("chart", "table")) {
    what <- if (.is_string(b$title)) b$title else kinds[[b$kind %||% ""]]$label %||% b$kind %||% ""
    detail <- Filter(Negate(is.null), list(b$indicator, if (!is.null(b$year)) as.character(b$year)))
    sprintf("\"%s\"%s", what, if (length(detail)) sprintf(" (%s)", paste(unlist(detail), collapse = ", ")) else "")
  } else if (type %in% c(.rb_ai_text_types, "list", "quote")) {
    text <- .rb_ai_plain(b$text)
    if (!nzchar(text)) "(empty)" else sprintf("\"%s\"", if (nchar(text) > 40) paste0(substr(text, 1, 40), "\u2026") else text)
  } else if (type == "image" && .is_string(b$caption)) {
    sprintf("\"%s\"", b$caption)
  } else {
    ""
  }
  out <- trimws(paste(if (article) (if (grepl("^[aeiou]", noun)) "an" else "a") else "the", noun, name))
  if (capital) out <- sub("^the ", "", out)
  if (capital) paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out))) else out
}

# What changed in a block, in a few words each
.rb_ai_what_changed <- function(old, new, kinds) {
  parts <- character()
  shown <- function(x) if (is.null(x)) "none" else paste(unlist(x), collapse = ", ")
  if (!identical(old$type, new$type)) parts <- c(parts, sprintf("%s -> %s", old$type, new$type))
  if (!identical(old$text, new$text)) {
    parts <- c(parts, if (nzchar(.rb_ai_plain(old$text))) "text rewritten" else "text written")
  }
  if (!identical(old$level, new$level) && identical(new$type, "heading")) parts <- c(parts, sprintf("level %s", new$level))
  if (!identical(old$kind, new$kind)) {
    lab <- function(k) kinds[[k %||% ""]]$label %||% k %||% "none"
    parts <- c(parts, sprintf("%s -> %s", lab(old$kind), lab(new$kind)))
  }
  for (s in c("indicator", "admin_level", "region", "year", "variant", "size", "title", "caption", "align", "alt",
              "width", "space_before", "space_after")) {
    if (!identical(old[[s]], new[[s]])) parts <- c(parts, sprintf("%s %s -> %s", s, shown(old[[s]]), shown(new[[s]])))
  }
  if (!identical(old$options, new$options)) {
    o <- old$options %||% list()
    n <- new$options %||% list()
    changed <- unique(c(setdiff(names(o), names(n)), names(n)[!vapply(names(n), function(k) identical(o[[k]], n[[k]]), logical(1))]))
    if (length(changed)) parts <- c(parts, sprintf("options %s", paste(vapply(changed, function(k) sprintf("%s = %s", k, shown(n[[k]])), ""), collapse = ", ")))
  }
  parts
}
