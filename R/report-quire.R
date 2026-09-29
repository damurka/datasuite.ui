# Writing a report with Quire's writers (quire::quire_export()): the builder's own, run in R, so a report written here
# is the file the builder downloads. The host is the report context's side of it: drawing a chart or table, the text
# fields, the pictures kept with the data and the flag.

# The Quire host for a report context. `i18n`: an object with a `t(key)` method (the report's language), or NULL.
.rb_quire_host <- function(context, i18n = NULL) {
  regions <- .rb_regions(context)
  quire::quire_host(
    # the kinds with their indicators' names, for {chart_indicator} and {chart_indicators}
    kinds = function() {
      kinds <- .ds_report_kinds()
      unname(lapply(names(kinds), function(k) {
        ind <- kinds[[k]]$indicators
        list(kind = k, type = kinds[[k]]$type, label = k,
             indicators = if (is.character(ind) && length(ind) > 1) lapply(ind, function(v) list(value = v, label = .ds_indicator_name(i18n, v))))
      }))
    },
    years = function() tryCatch(sort(as.numeric(as_report_context(context)$years())), error = function(e) numeric()),
    render = function(request) .rb_render_request(context, request, i18n, regions),
    fields = function(project, lang) report_fields(context, project, lang = lang %||% "en"),
    flag = function() cd_report_flag(context),
    assetGet = function(id) {
      url <- report_asset_data_url(context, id)
      if (is.character(url) && startsWith(url, "data:")) list(type = sub("^data:([^;,]+).*$", "\\1", url), data = sub("^data:[^,]*,", "", url))
    }
  )
}

# One report written by Quire: `format` "docx", "pptx" or "html" (the printable page)
.rb_quire_write <- function(context, project, file, format, i18n) {
  lang <- project$lang %||% (if (is.list(i18n) || is.environment(i18n)) i18n$lang) %||% "en"
  project$design$slide_designs <- .rb_quire_designs(project$design$slide_designs)
  quire::quire_export(project, .rb_quire_host(context, i18n), file, format = format, lang = lang)
}

# Slide designs as Quire reads them: a picture read from a PowerPoint file (report_theme_from_file(): its `file` on
# disk; the app keeps it as "asset:<id>" instead) given as a data URL
.rb_quire_designs <- function(designs) {
  if (!is.list(designs)) return(designs)
  lapply(designs, function(d) {
    if (!is.list(d) || !is.list(d$decor)) return(d)
    d$decor <- lapply(d$decor, function(item) {
      f <- item$file
      if (identical(item$type, "image") && is.null(item$src) && is.character(f) && length(f) == 1 && file.exists(f)) {
        type <- switch(tolower(tools::file_ext(f)), png = "image/png", gif = "image/gif", svg = "image/svg+xml", "image/jpeg")
        item$src <- paste0("data:", type, ";base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size)))
        item$file <- NULL
      }
      item
    })
    d
  })
}

# One chart or table as Quire's render answer (request: the block, the report's design and language, its region):
# drawn as the builder shows it (the theme and the report's saved chart styling). A chart comes as a picture with its
# legend entries and panels; a table as its cells (flextable's text as formatted).
.rb_render_request <- function(context, request, i18n, regions) {
  b <- request$block
  if (!is.list(b)) return(quire::quire_error("No block to draw."))
  design <- request$design
  b <- report_resolve_block(b, list(region = request$region), regions)
  r <- tryCatch(with_report_chart_options(context, render_report_block(context, b, i18n, design), design = design),
                error = function(e) list(type = "error", message = conditionMessage(e)))
  size <- report_block_size(b, design)
  if (identical(r$type, "plot")) {
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    dpi <- if (identical(request$purpose, "export")) 300 else 200
    ok <- tryCatch({ save_report_chart(r, b, f, dpi = dpi, design = design); TRUE }, error = function(e) conditionMessage(e))
    if (!isTRUE(ok)) return(quire::quire_error(ok))
    list(kind = "image", src = paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))),
         w = size[1], h = size[2],
         entries = tryCatch(cd_chart_entries(r$value), error = function(e) NULL),
         # a chart drawn as panels (by year, district...): how, so the builder can change it
         facets = tryCatch(chart_facet_info(r$value), error = function(e) NULL))
  } else if (identical(r$type, "table")) {
    cd_flextable_render(r$value)
  } else {
    quire::quire_error(r$message %||% "")
  }
}

# The printable page printed to a PDF by a browser (Chrome, Edge or Chromium): when there is no Word or LibreOffice
.rb_browser_pdf <- function(html, file) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    .ds_abort(c("x" = "PDF export needs Microsoft Word, LibreOffice, or the {.pkg chromote} package with Chrome.", "i" = "Download as Word instead."))
  }
  .rb_find_browser()
  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file:///", normalizePath(html, winslash = "/")), wait_ = FALSE)
  b$wait_for(loaded)
  pdf <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE)
  writeBin(jsonlite::base64_dec(pdf$data), file)
}
