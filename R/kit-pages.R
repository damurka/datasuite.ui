# The page registry machinery. An app lists its analysis pages once, with cd_page_def() (apps/<app>/pages.R), and
# registers the list with cd_use_pages(). From then on:
#   - a page module's UI is  cd_page_ui(id, i18n, filters = ..., <cards>)  -- title, section, subtitle, denominator
#     row and report button all come from its registry entry, never from the module;
#   - app.R builds every page container with cd_pages_ui() and starts every server with cd_pages_server().

# One page. `ui`/`server` are the page module's functions (ui(id, i18n, ...), server(id, cache, i18n, ..., active)).
# title/section/subtitle are translation keys. `help`: c(<docs chapter>, <section>) for the Get help button.
# `report`: the page's standard report (one the app registered, see report_register()), started in the report builder by the page's
#   "Generate report" button; NULL when the page has none. The button is shown only in an app with the Reports page.
# `server_args`: extra arguments after (id, cache, i18n). `active = FALSE`: the server takes no `active` argument.
cd_page_def <- function(id, ui, server, title, section, subtitle, help = NULL, denominator = FALSE, report = NULL,
                        server_args = list(), active = TRUE) {
  list(id = id, ui = ui, server = server, title = title, section = section, subtitle = subtitle, help = help,
       denominator = denominator, report = report, server_args = server_args, active = active)
}

cd_use_pages <- function(pages) {
  assign("pages", stats::setNames(pages, vapply(pages, function(p) p$id, character(1))), envir = .cd_state)
}

cd_page_meta <- function(id) {
  page <- get0("pages", envir = .cd_state, inherits = FALSE)[[id]]
  if (is.null(page)) stop("Page '", id, "' is not in the page registry (pages.R).", call. = FALSE)
  page
}

# The whole page for a module: `id` is the module's own id (= the page id). `filters`: cd_filter_bar(...).
cd_page_ui <- function(id, i18n, ..., filters = NULL) {
  page <- cd_page_meta(id)
  cd_page_body(
    dashboardId = NS(id)("page"),
    dashboardTitle = i18n$t(page$title),
    eyebrow = page$section,
    subtitle = page$subtitle,
    include_denominator = isTRUE(page$denominator),
    include_report = !is.null(page$report) && cd_has_reports(),
    i18n = i18n,
    filters = filters,
    ...
  )
}

# The page containers for every registered page, ready for cd_screens().
cd_pages_ui <- function(pages, i18n) {
  lapply(pages, function(page) cd_screen(tabName = page$id, page$ui(page$id, i18n)))
}

# Starts a page's server plus its header (help, report, notes) -- so a module never wires its own header -- the first
# time the page is opened. Starting all of them when the session began kept a 31-page app deaf for 12 s (measured:
# 3 s creating the servers, 5 s in the first flush over their 262 outputs, 24 of the 30 outputs drawn at startup on
# pages nobody had opened). `options(datasuite.ui.lazy_pages = FALSE)` starts them all at once, as before.
# `page_is(id)`: app.R's reactive "is this page open with data loaded".
# `header_extra`: what the app adds to every page header's server (see cd_page_header_server()).
# `current`: reactive, the id of the page on screen.
cd_pages_server <- function(pages, cache, i18n, page_is, header_extra = NULL, current = NULL,
                            session = getDefaultReactiveDomain()) {
  ids <- vapply(pages, function(p) p$id, character(1))
  started <- new.env(parent = emptyenv())
  start <- function(id) {
    if (!is.character(id) || length(id) != 1 || !id %in% ids || isTRUE(started[[id]])) return(invisible(FALSE))
    started[[id]] <- TRUE
    p <- pages[[match(id, ids)]]
    # in the session's own domain and outside whatever reactive asked: an observer opening a page must not come to
    # depend on what the page reads while it starts
    withReactiveDomain(session, isolate({
      args <- c(list(p$id, cache, i18n), p$server_args, if (isTRUE(p$active)) list(active = page_is(p$id)))
      do.call(p$server, args)
      moduleServer(p$id, function(input, output, session) {
        cd_page_header_server("page", cache = cache, path = p$help[1], section = if (length(p$help) > 1) p$help[2],
                              i18n = i18n, key = p$report %||% p$id, extra = header_extra)
      })
    }))
    invisible(TRUE)
  }
  # what needs a page that may not have been opened (the AI reading a chart elsewhere) starts it: cd_start_pages()
  if (!is.null(session)) session$userData$cd_start_pages <- function(which = ids) for (id in intersect(which, ids)) start(id)
  if (is.null(current) || !isTRUE(getOption("datasuite.ui.lazy_pages", TRUE))) {
    for (id in ids) start(id)
  } else {
    # before anything else that follows the page on screen, so the page's own observers see this same change
    observe(start(current()), priority = 1000)
  }
  invisible(NULL)
}

#' Start the servers of pages not opened yet
#'
#' An app starts a page's server the first time the page is opened (`cd_pages_server()`). What needs a page that may
#' not have been opened (the AI reading a chart on another page) starts it with this. Nothing happens for a page
#' already started, or in an app whose pages are all started with the session.
#'
#' @param session The Shiny session.
#' @param which The ids of the pages to start; every page when `NULL`.
#'
#' @return `NULL`, invisibly.
#' @export
cd_start_pages <- function(session = getDefaultReactiveDomain(), which = NULL) {
  f <- session$userData$cd_start_pages
  if (is.function(f)) if (is.null(which)) f() else f(which)
  invisible(NULL)
}
