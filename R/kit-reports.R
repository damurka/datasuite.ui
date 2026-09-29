# The Reports page: the report builder, Quire's (the quire package; this module is its host, reports_server()). A report is
# a list of blocks plus a design (theme, page) and a cover, kept in the dataset (cache$set_report_project()). The builder
# edits it and writes its Word, PowerPoint and PDF files in the browser; this module answers what it asks:
#   - the charts and tables it can draw (kinds) and each one drawn (render: cd_report_render(), as the files draw them),
#   - the fields' values ({country}, {anc4_latest}...), the standard reports, themes, years, regions and the flag,
#   - the reports kept in the dataset (list, get, save, delete) and their pictures; a file it wrote is downloaded
#     through the page's hidden link,
#   - a page's "Generate report" button (cd_request_report()) asks for a new report from that page's standard report,
#   - a report has its own language (project$lang), chosen when it is created (the app's language by default): the
#     standard report's text is written in it, and its charts, tables, dates and file are always drawn in it
#     (cd_report_translator()), whatever language the app is in. A report saved before this has none and follows the app.
#   - the AI changes saved reports too (report_update_blocks(), through the app's AI bridge): a change to the open
#     report is sent to the builder ("report.changed"), which opens it again as saved; the builder saves every edit,
#     so what it had is in the saved report already,
#   - the builder's AI buttons (write a paragraph, change a block, write the narrative) open DataSuite's chat with a
#     prompt about the report (.rb_ai_ask_prompt()), for the user to send.
# From R (export_report(), export_deck()) the same writers run through quire::quire_export() (R/report-quire.R).

reports_ui <- function(id, i18n) {
  ns <- NS(id)
  cd_page_ui(
    id, i18n,
    # the report builder is Quire's (the quire package): this module is its host (reports_server())
    quire::quire_ui(ns("studio"), aiEnabled = .cd_in_datasuite()),
    # the dataset's reference documents: context the AI reads for reports (kit-documents.R)
    documents_card_ui(ns("documents"), i18n),
    # a file the builder writes is served through this (hidden) link's handler
    div(style = "display: none", downloadLink(ns("file"), ""))
  )
}

# ---- what the component shows ------------------------------------------------------------------------------------

# The fields a report's text can contain, labelled in every language
cd_report_field_catalog <- function(i18n) {
  langs <- setdiff(i18n$get_languages(), "key")
  lapply(report_field_catalog(), function(f) {
    label <- if (!is.null(f$indicator)) {
      stats::setNames(lapply(langs, function(l) {
        paste(cd_plain_text(i18n, paste0("opt_", f$indicator), l), cd_plain_text(i18n, paste0("lbl_rb_field_", f$what), l), sep = " \u00b7 ")
      }), langs)
    } else {
      cd_text(i18n, paste0("lbl_rb_field_", f$key))
    }
    list(key = f$key, group = f$group, label = label)
  })
}

# A country's flag as a data URL, for the cover (NULL when it cannot be found)
cd_report_flag <- function(cache) {
  path <- tryCatch(as_report_context(cache)$flag(), error = function(e) NULL)
  if (is.null(path)) return(NULL)
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)))
}

# The kinds of chart and table, with their choices translated
cd_report_kinds <- function(i18n, cache) {
  opt <- function(values, key) lapply(values, function(v) list(value = v, label = cd_text(i18n, paste0(key, v))))
  kinds <- .ds_report_kinds()
  lapply(names(kinds), function(k) {
    spec <- kinds[[k]]
    indicators <- spec$indicators
    defaults <- list()
    if (length(indicators)) defaults$indicator <- if ("anc4" %in% indicators) "anc4" else indicators[[1]]
    if (length(spec$levels)) defaults$admin_level <- spec$levels[[1]]
    if (length(spec$variants)) defaults$variant <- names(spec$variants)[[1]]
    if (isTRUE(spec$regional)) defaults$region <- "@report"
    list(
      kind = k, type = spec$type, group = spec$group,
      groupLabel = cd_text(i18n, paste0("lbl_rb_group_", spec$group)),
      label = cd_text(i18n, paste0("lbl_rb_kind_", k)),
      indicators = if (length(indicators)) opt(indicators, "opt_"),
      levels = if (length(spec$levels)) opt(spec$levels, "lbl_rb_level_"),
      variants = if (length(spec$variants)) opt(names(spec$variants), "lbl_rb_var_"),
      year = isTRUE(spec$year),
      regional = isTRUE(spec$regional),
      tall = isTRUE(spec$tall),
      defaults = defaults
    )
  })
}

cd_report_new_id <- function() paste0("r", format(Sys.time(), "%Y%m%d%H%M%S"), sample.int(999, 1))

# The app's indicator group (rmncah, vaccine): which standard reports and kinds it has

# The translator cd2030.core draws a report with: t(key) in one language (the user's), whatever the app's translator is on
cd_report_translator <- function(i18n, lang) {
  lang <- if (lang %in% c("en", "fr", "pt")) lang else "en"
  list(t = function(key, ...) cd_plain_text(i18n, key, lang), lang = lang)
}

# Every block of a report: a document's blocks, or each item of a deck's slides (its block, with the item's id and its
# box, the size it is drawn at, in inches)
cd_report_blocks <- function(p) report_project_blocks(p)

cd_report_is_deck <- function(p) identical(p$kind, "deck")

cd_report_summary <- function(projects) {
  if (!length(projects)) return(list())
  rows <- lapply(names(projects), function(id) {
    p <- projects[[id]]
    blocks <- cd_report_blocks(p)
    list(
      id = id, name = p$name %||% "",
      kind = if (cd_report_is_deck(p)) "deck" else "document",
      updated = p$updated %||% "",
      charts = sum(vapply(blocks, function(b) isTRUE(b$type %in% c("chart", "table")), logical(1))),
      blocks = if (cd_report_is_deck(p)) length(p$slides %||% list()) else length(blocks),
      order = p$updated %||% ""
    )
  })
  rows[order(vapply(rows, function(r) r$order, character(1)), decreasing = TRUE)]
}

# ---- server ------------------------------------------------------------------------------------------------------

# The Reports page as Quire's host (quire::quire_host()): the builder asks, this answers from the dataset. It asks
# what can be drawn (cd_report_kinds()), draws each chart and table (render_report_block(), in the report's own
# language), gives the text fields, the standard reports, the themes, the years and regions, and keeps the reports
# and their pictures in the dataset. The builder does the rest (editing, pages, the Word, PowerPoint and PDF files,
# the AI's reading and editing). This module also tells the builder what changes here:
#   - a new dataset: its charts are drawn again ("data.changed"),
#   - the dataset's reports changed elsewhere (the AI saved or changed one): the list follows, and the open report,
#     when it is not what the builder last saved, is sent to open again ("report.changed"),
#   - a page's "Generate report" (cd_request_report()): the report is made from that page's standard report and
#     opened ("report.open"),
#   - the app's language ("lang").
# The builder's AI buttons open DataSuite's chat with a prompt about the report (its "ai.ask").
reports_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    documents_card_server("documents", i18n)
    state <- reactiveValues(open = NULL, project = NULL, file = NULL, file_name = NULL)

    # a report saved without its own id (reports the AI saved before cd2030.core 1.3.4) takes the key it is stored under
    projects <- function() {
      ps <- tryCatch(isolate(cache())$report_projects, error = function(e) list()) %||% list()
      for (key in names(ps)) {
        if (is.list(ps[[key]]) && !(is.character(ps[[key]]$id) && length(ps[[key]]$id) == 1 && nzchar(ps[[key]]$id))) ps[[key]]$id <- key
      }
      ps
    }
    lang <- reactive(if (isTruthy(cache())) cache()$language %||% "en" else "en")
    tr <- function(l) cd_report_translator(i18n, l %||% isolate(lang()))
    save <- function(project) {
      project$updated <- format(Sys.time(), "%Y-%m-%d %H:%M")
      isolate(cache())$set_report_project(project$id, project)
      project
    }
    # a new report from a standard report, with new ids for its blocks (and slides)
    from_preset <- function(key, l, name = NULL) {
      preset <- .ds_report_presets(l)[[key]]
      if (is.null(preset)) stop(sprintf("There is no standard report %s.", key))
      suffix <- function(x) paste0(x, "_", sample.int(1e6, 1))
      p <- list(id = cd_report_new_id(), name = if (is.character(name) && nzchar(trimws(name))) trimws(name) else preset$name,
                lang = l, design = preset$design, cover = preset$cover,
                region = if (any(vapply(cd_report_blocks(preset), function(b) identical(b$region, "@report"), logical(1)))) cd_report_regions(isolate(cache()))[1],
                blocks = lapply(preset$blocks %||% list(), function(b) { b$id <- suffix(b$id); b }))
      if (identical(preset$kind, "deck")) {
        p$kind <- "deck"
        p$slides <- lapply(preset$slides %||% list(), function(s) {
          s$id <- suffix(s$id %||% "s")
          s$items <- lapply(s$items %||% list(), function(it) {
            it$id <- suffix(it$id %||% "i")
            it$block$id <- it$id
            it
          })
          s
        })
      }
      p
    }
    data_url_asset <- function(url) {
      if (!is.character(url) || !startsWith(url, "data:")) return(NULL)
      list(type = sub("^data:([^;,]+).*$", "\\1", url), data = sub("^data:[^,]*,", "", url))
    }
    chart_keys <- c(
      search = "lbl_cc_search", noResults = "lbl_cc_no_results", reset = "lbl_chart_reset", resetGroup = "lbl_cc_reset_group",
      changed = "lbl_cc_changed", asDrawn = "lbl_chart_style_as_drawn", yes = "lbl_chart_style_yes", no = "lbl_chart_style_no",
      min = "lbl_chart_style_min", max = "lbl_chart_style_max", entriesLegend = "lbl_chart_style_legend_entries",
      entriesCategories = "lbl_chart_style_category_entries", entryText = "lbl_chart_style_entry_text",
      entryColor = "lbl_chart_style_entry_color", show = "lbl_cc_show", hidden = "lbl_cc_hidden"
    )

    host <- quire::quire_host(
      kinds = function() cd_report_kinds(i18n, isolate(cache())),
      render = function(request) cd_report_render(isolate(cache()), request, i18n),
      fields = function(project, lang) report_fields(isolate(cache()), project, lang = lang %||% isolate(lang())),
      fieldCatalog = function() cd_report_field_catalog(i18n),
      presets = function(lang) {
        ps <- .ds_report_presets(lang %||% isolate(lang()))
        lapply(names(ps), function(k) list(
          id = k, name = ps[[k]]$name, description = ps[[k]]$description, kind = ps[[k]]$kind %||% "document",
          charts = sum(vapply(cd_report_blocks(ps[[k]]), function(b) isTRUE(b$type %in% c("chart", "table")), logical(1)))
        ))
      },
      preset = function(id, lang, name) from_preset(id, lang %||% isolate(lang()), name),
      themes = function() {
        c(
          unname(lapply(report_themes(), function(th) {
            th$name <- cd_text(i18n, paste0("lbl_rb_theme_", th$theme))
            th$palette <- as.list(th$palette)
            th
          })),
          unname(lapply(tryCatch(isolate(cache())$report_themes, error = function(e) list()) %||% list(), function(th) {
            th$palette <- as.list(th$palette)
            th
          }))
        )
      },
      years = function() isolate(cache())$data_years,
      regions = function() cd_report_regions(isolate(cache())),
      flag = function() cd_report_flag(isolate(cache())),
      chartSchema = function() {
        if (is.null(.chart_schema_cache$schema)) .chart_schema_cache$schema <- cd_chart_schema(i18n)
        list(tabs = .chart_schema_cache$schema$tabs, fields = .chart_schema_cache$schema$fields,
             texts = lapply(chart_keys, function(k) cd_text(i18n, k)))
      },
      listReports = function() {
        rows <- cd_report_summary(projects())
        lapply(rows, function(r) { r$order <- NULL; r })
      },
      getReport = function(id) projects()[[id]],
      saveReport = function(project) {
        if (!is.list(project) || !is.character(project$id)) stop("A report to save has an id.")
        state$project <- save(project)
        invisible(NULL)
      },
      deleteReport = function(id) {
        isolate(cache())$set_report_project(id, NULL)
        invisible(NULL)
      },
      assetGet = function(id) data_url_asset(report_asset_data_url(isolate(cache()), id)),
      assetSet = function(id, asset) {
        report_store_asset(isolate(cache()), id, paste0("data:", asset$type %||% "image/png", ";base64,", asset$data))
        invisible(NULL)
      },
      # a PowerPoint or Word file (template or document) made into a theme; the file is kept, and exports are written
      # from it
      themeFromFile = function(name, data) {
        ext <- tolower(tools::file_ext(name %||% ""))
        if (!ext %in% c("potx", "pptx", "dotx", "docx")) stop(cd_plain_text(i18n, "lbl_rb_themeFileKinds", isolate(lang())))
        bytes <- jsonlite::base64_dec(sub("^data:[^,]*,", "", data %||% ""))
        path <- tempfile(fileext = paste0(".", ext))
        writeBin(bytes, path)
        th <- report_theme_from_file(path, name = tools::file_path_sans_ext(basename(name)))
        asset <- paste0("tpl_", th$theme)
        isolate(cache())$set_report_asset(asset, list(type = "application/octet-stream", data = bytes))
        th$template <- paste0("asset:", asset)
        th$template_ext <- ext
        th$slide_designs <- cd_report_designs_store(isolate(cache()), th$slide_designs, asset)
        isolate(cache())$set_report_theme(th$theme, th)
        th$palette <- as.list(th$palette)
        th
      },
      # a Word, PowerPoint or PDF file the builder wrote: kept here and downloaded through the page's (hidden) link, which
      # reaches the reader in DataSuite's window as in a browser
      saveFile = function(file) {
        ext <- tolower(tools::file_ext(file$name %||% "report.docx"))
        path <- tempfile(fileext = paste0(".", ext))
        writeBin(jsonlite::base64_dec(file$data %||% ""), path)
        country <- tryCatch(isolate(cache())$country, error = function(e) "") %||% ""
        base <- gsub("[^A-Za-z0-9]+", "_", paste(country, tools::file_path_sans_ext(file$name %||% "report")))
        state$file <- path
        state$file_name <- paste0(gsub("^_|_$", "", base), "_", format(Sys.Date()), ".", ext)
        list(path = state$file_name, url = paste0("session/", session$token, "/download/", session$ns("file"), "?w="))
      }
    )

    studio <- quire::quire_server("studio", host, on_event = function(name, data) {
      if (identical(name, "report.opened")) {
        id <- data$project
        state$open <- if (is.character(id) && length(id) == 1) id else NULL
        state$project <- if (!is.null(state$open)) projects()[[state$open]]
      } else if (identical(name, "ai.ask")) {
        # an AI button of the builder: DataSuite's chat opens with a prompt about the report, in the app's language
        p <- isolate(state$project) %||% projects()[[isolate(state$open) %||% ""]]
        prompt <- if (is.character(data$prompt) && nzchar(data$prompt)) data$prompt else if (!is.null(p)) .rb_ai_ask_prompt(data, p, i18n, isolate(lang()))
        if (!is.null(prompt)) .ai_host_request("openChat", query = prompt)
      }
    })

    # a new dataset: its charts are drawn again, its reports listed
    observeEvent(cache(), {
      state$open <- NULL
      studio$send("data.changed")
      studio$send("reports.changed")
    }, ignoreInit = TRUE)

    # the app's language: the builder's follows
    observeEvent(lang(), studio$send("lang", list(lang = lang())), ignoreInit = TRUE)

    # the dataset's reports changed elsewhere (the AI saved or changed one): the list follows, and the open report,
    # when it is not what the builder last saved, is sent to it to open again (the builder saves every edit, so nothing
    # of the user's is lost)
    observeEvent(tryCatch(cache()$report_projects, error = function(e) NULL), {
      req(cache(), active())
      studio$send("reports.changed")
      open <- isolate(state$open)
      if (is.null(open)) return()
      stored <- projects()[[open]]
      if (is.null(stored) || identical(stored, isolate(state$project))) return()
      state$project <- stored
      studio$send("report.changed", list(project = stored))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # a page's "Generate report" (cd_request_report()): list(preset, nonce); the report is made and opened
    request <- reactiveVal(NULL)
    session$userData$cd_report_request <- request
    observeEvent(request(), {
      req(cache())
      a <- request()
      p <- tryCatch(save(from_preset(a$preset, isolate(lang()), a$name)), error = function(e) NULL)
      if (!is.null(p)) studio$send("report.open", list(id = p$id))
    }, ignoreInit = TRUE)

    output$file <- downloadHandler(
      filename = function() state$file_name %||% "report.docx",
      content = function(file) file.copy(state$file, file, overwrite = TRUE)
    )
    # its link is hidden; a hidden output is suspended and its download never registered
    outputOptions(output, "file", suspendWhenHidden = FALSE)
  })
}

# The prompt an AI button of the builder opens DataSuite's chat with (`a`: list(scope, block, after)), in `lang`:
#   narrative  the whole report's text: an introduction, a paragraph after each chart and table, a conclusion
#   write      one paragraph (`block`), after the chart or table `after` when there is one
#   change     one block (`block`): the user says what to change
# The report and the blocks are named with their ids, so the AI reads the report and changes only what is asked.
.rb_ai_ask_prompt <- function(a, project, i18n, lang) {
  text <- function(key, fallback) {
    value <- tryCatch(cd_plain_text(i18n, key, lang), error = function(e) key)
    if (identical(value, key)) fallback else value
  }
  fill <- function(template, values) {
    for (k in names(values)) {
      slot <- paste0("{", k, "}")
      at <- regexpr(slot, template, fixed = TRUE)
      if (at > 0) template <- paste0(substr(template, 1, at - 1), values[[k]], substr(template, at + nchar(slot), nchar(template)))
    }
    template
  }
  blocks <- report_project_blocks(project)
  find <- function(id) if (.is_string(id)) Filter(function(b) identical(b$id, id), blocks)[1][[1]]
  label <- function(b) {
    if (is.null(b)) return("")
    if (isTRUE(b$type %in% c("chart", "table"))) {
      what <- if (.is_string(b$title)) b$title else text(paste0("lbl_rb_kind_", b$kind %||% ""), b$kind %||% "")
      ind <- if (.is_string(b$indicator)) text(paste0("opt_", b$indicator), b$indicator)
      return(sprintf("\"%s\"%s", what, if (!is.null(ind)) sprintf(" (%s)", ind) else ""))
    }
    plain <- .rb_ai_plain(b$text)
    if (nzchar(plain)) sprintf("\"%s\"", if (nchar(plain) > 40) paste0(substr(plain, 1, 40), "...") else plain) else ""
  }
  values <- list(name = project$name %||% "", id = project$id %||% "", block = if (.is_string(a$block)) a$block else "")
  scope <- a$scope %||% "narrative"
  prompt <- if (identical(scope, "change")) {
    fill(text("lbl_rb_aiPromptChange", "Change the block {label} ({block}) in the report \"{name}\" (id {id}): "),
         c(values, label = label(find(a$block))))
  } else if (identical(scope, "write") && !is.null(find(a$after))) {
    fill(text("lbl_rb_aiPromptWriteAfter", "In the report \"{name}\" (id {id}), write the paragraph {block} after the chart {after}, from its data."),
         c(values, after = label(find(a$after))))
  } else if (identical(scope, "write")) {
    fill(text("lbl_rb_aiPromptWrite", "In the report \"{name}\" (id {id}), write the paragraph {block}: "), values)
  } else {
    fill(text("lbl_rb_aiPromptNarrative", "Fill in the narrative of the report \"{name}\" (id {id}): write a short introduction, a paragraph after each chart and table, and a conclusion, from the data in the report."), values)
  }
  gsub("[ \t\n]+", " ", prompt)
}

# A PowerPoint file's slide designs with their pictures (logos, a background photo) kept once in the dataset, as the
# report's pictures are ("asset:<prefix>_<n>" in `src`): the builder and the writers draw them from there. A picture over
# 5 MB is left out.
cd_report_designs_store <- function(cache, designs, prefix) {
  if (!is.list(designs)) return(NULL)
  n <- 0
  lapply(designs, function(d) {
    if (!is.list(d)) return(d)
    d$decor <- Filter(Negate(is.null), lapply(d$decor %||% list(), function(item) {
      if (!identical(item$type, "image")) return(item)
      f <- item$file
      if (!is.character(f) || !file.exists(f) || file.info(f)$size > 5e6) return(NULL)
      type <- switch(tolower(tools::file_ext(f)), png = "image/png", gif = "image/gif", "image/jpeg")
      n <<- n + 1
      id <- paste0(prefix, "_", n)
      cache$set_report_asset(id, list(type = type, data = readBin(f, "raw", file.info(f)$size)))
      item$src <- paste0("asset:", id)
      item$file <- NULL
      item
    }))
    # what a file does not set is left out (NULL would reach the editor as {})
    d$decor <- lapply(d$decor, function(item) Filter(Negate(is.null), item))
    for (k in c("title", "subtitle", "body")) if (is.list(d[[k]])) d[[k]] <- Filter(Negate(is.null), d[[k]])
    Filter(Negate(is.null), d)
  })
}

# The dataset's regions (admin level 1), sorted
cd_report_regions <- function(cache) {
  tryCatch(sort(unique(cache$subnational_regions$adminlevel_1)), error = function(e) character())
}

# One chart or table for the builder (a quire render request: the block, the report's design and language, its
# region, the size in inches), drawn as the exported file draws it (the theme and the report's saved chart styling).
# A chart comes as a picture with its legend entries and panels (the builder recolours, renames, re-arranges them); a
# table as its cells (flextable's text as formatted), which the builder draws in its Table Design style.
cd_report_render <- function(cache, request, i18n) {
  .rb_render_request(cache, request, cd_report_translator(i18n, request$lang %||% "en"), cd_report_regions(cache))
}

# A flextable as the builder's table: its header rows (merged cells spanning), its body rows (each cell's text as
# flextable formats it, and its number when the column holds numbers) and its footer as the note under it
cd_flextable_render <- function(ft) {
  chunks <- flextable::information_data_chunk(ft)
  keys <- ft$col_keys
  part <- function(name) {
    d <- chunks[chunks$.part == name, , drop = FALSE]
    if (!nrow(d)) return(list())
    spans <- tryCatch(ft[[name]]$spans$rows, error = function(e) NULL)
    data <- ft[[name]]$dataset
    lapply(sort(unique(d$.row_id)), function(i) {
      cells <- list()
      for (j in seq_along(keys)) {
        span <- if (!is.null(spans) && nrow(spans) >= i) spans[i, j] else 1
        if (identical(span, 0L) || identical(span, 0)) next
        cell <- list(text = paste(d$txt[d$.row_id == i & d$.col_id == keys[j]], collapse = ""))
        if (span > 1) cell$span <- as.integer(span)
        v <- if (identical(name, "body") && !is.null(data[[keys[j]]])) data[[keys[j]]][i]
        if (is.numeric(v) && length(v) == 1 && !is.na(v)) cell$value <- v
        cells[[length(cells) + 1]] <- cell
      }
      cells
    })
  }
  footer <- part("footer")
  note <- if (length(footer)) paste(unique(unlist(lapply(footer, function(r) vapply(r, function(c) c$text, character(1))))), collapse = " ")
  out <- list(kind = "table", header = part("header"), rows = part("body"))
  if (is.character(note) && nzchar(note)) out$footer <- note
  out
}
