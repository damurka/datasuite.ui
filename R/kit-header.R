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
