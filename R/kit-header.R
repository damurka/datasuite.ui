cd_page_header <- function(id, title, i18n, include_report = FALSE, include_notes = FALSE, include_help = TRUE,
                            eyebrow = NULL, subtitle = NULL, include_denominator = FALSE) {
  ns <- NS(id)
  tagList(div(
    class = 'cd-page-header',
    div(
      class = 'cd-page-heading',
      if (!is.null(eyebrow)) div(class = 'cd-page-eyebrow', i18n$t(eyebrow)),
      h1(title),
      if (!is.null(subtitle)) p(class = 'cd-page-subtitle', i18n$t(subtitle))
    ),
    div(
      class = 'right-buttons',
      if (include_report) cd_button(ns('report'), "btn_report_generate", i18n, icon = 'file-lines', size = 'sm'),
      if (include_notes) cd_notes_button_ui(ns('add_notes'), i18n),
      if (include_help) cd_help_button_ui(ns('get_help'), name = i18n$t('btn_global_help'))
    )
  ),
  # Analysis pages: which denominators the numbers on the page were divided by -- project/NationalCoverage.dc.html's
  # own "Denominator" row under the page header. Filled by cd_page_header_server() below.
  if (include_denominator) uiOutput(ns('denominator'), class = 'cd-denominator-slot'))
}

# "Denominator  (o Vaccination DHIS2)  (o Maternal DHIS2)  Set on the Denominator Selection page."

# `key`: the page's standard report (one the app registered) and its id in the notes store.
# `extra`: a function(input, output, session, cache, i18n) run inside the header's module, for what an app adds to
# every page header (Countdown: its denominator row).
cd_page_header_server <- function(id, cache, path, section = NULL, i18n, key = id, extra = NULL) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      if (is.function(extra)) extra(input, output, session, cache, i18n)

      # the page's standard report opens in the report builder (modules/reports.R), which asks for its name
      observeEvent(input$report, cd_request_report(session, key))

      cd_help_button_server(
        id = 'get_help',
        path = path,
        section = section,
        cache = cache
      )

      cd_notes_button_server(
        id = 'add_notes',
        cache = cache,
        document_objects = NULL,
        page_id = key,
        page_name = key,
        i18n = i18n
      )
    }
  )
}
