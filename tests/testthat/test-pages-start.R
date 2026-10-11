test_that("the Reports page's server is started with the session, the other pages' when they are opened", {
  started <- character()
  page <- function(id, server = NULL, ...) {
    cd_page_def(id, ui = function(...) NULL, server = server %||% function(id, cache, i18n, ...) started <<- c(started, id),
                title = "t", section = "s", subtitle = "u", active = FALSE, ...)
  }
  testthat::local_mocked_bindings(
    reports_server = function(id, cache, i18n, ...) started <<- c(started, id),
    cd_page_header_server = function(...) NULL
  )
  pages <- list(page("coverage"), page("reports", server = reports_server), page("asks_at_load", lazy = FALSE), page("equity"))
  shiny::testServer(function(input, output, session) {
    current <- shiny::reactiveVal("coverage")
    cd_pages_server(pages, cache = NULL, i18n = NULL, page_is = function(id) shiny::reactive(TRUE), current = current)
  }, {
    session$flushReact()
    # the builder asks the Reports server what it offers as the app loads: a server started later answered one
    # message of ten, and the page listed no reports
    expect_setequal(started, c("reports", "asks_at_load", "coverage"))
    current("equity"); session$flushReact()
    expect_setequal(started, c("reports", "asks_at_load", "coverage", "equity"))
    current("reports"); session$flushReact()
    expect_identical(sum(started == "reports"), 1L)
  })
})
