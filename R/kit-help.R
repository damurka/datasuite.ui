cd_help_button_ui <- function(id, name, i18n = cd_i18n()) {
  ns <- NS(id)
  cd_button(ns('help'), name, i18n, icon = 'circle-question', size = 'sm')
}

# The Get help button opens the page's section of the DataSuite docs, in the dataset's language. `path` is the docs
# page under the language (e.g. "apps/countdown/data-quality") and `section` a heading id on it; the headings the apps
# open carry the same id in every language. The site is getOption("datasuite.docs_url"), https://datasuite.damurka.com
# by default.
cd_help_button_server <- function(id, path, section = NULL, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      observeEvent(input$help, {
        utils::browseURL(cd_help_url(path, section, cd_help_language(cache)))
      })
    }
  )
}

# The dataset's language; English before a dataset is loaded (the Load Data page) or for any other language.
cd_help_language <- function(cache) {
  lang <- tryCatch(cache()$language, error = function(e) NULL)
  if (length(lang) == 1 && lang %in% c('en', 'fr', 'pt')) lang else 'en'
}

cd_help_url <- function(path = NULL, section = NULL, lang = 'en') {
  base <- sub('/+$', '', getOption('datasuite.docs_url', 'https://datasuite.damurka.com'))
  url <- paste0(base, '/', lang, '/')
  if (length(path) == 1 && nzchar(path)) url <- paste0(url, gsub('^/+|/+$', '', path), '/')
  if (length(section) == 1 && nzchar(section)) url <- paste0(url, '#', section)
  url
}
