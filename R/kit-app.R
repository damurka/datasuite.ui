# The whole shell of an app -- page, sidebar and header, server -- so an app only says what is particular to it: its
# pages (the page registry), its nav sections, its start screens and how its data is loaded.
#
#   app_frame(app_name, app_version, theme, nav_sections, registry, i18n, language, selected_file,
#             start_screens = list(cd_screen(tabName = "welcome", welcome_ui("welcome"))),
#             data = function(input, output, session) list(dataset = <reactive>, ready = <reactive>, ...))
#
# `data` is called once per session and gives back:
#   dataset    a reactive: the loaded dataset (NULL before one is loaded). It is the object every page receives; it
#              keeps the language (`language`, `set_language()`), the country shown in the header (`country`) and,
#              for the kit's tools, notes, chart options and reports (see the README's "dataset contract").
#   ready      a reactive: TRUE once the dataset can be used (pages and the Reports button wait for it)
#   analysis_ready  (optional) a stricter reactive for nav items marked requires_adjustment = TRUE
#   adopt_language  (optional) a reactive: TRUE when a new dataset should take the language on screen rather than
#              show its own (a fresh upload, as opposed to a resumed file)
# `start_tab`: the screen shown first and the one a locked page falls back to. `open_tabs`: tabs that never lock.
# `theme`: a theme name of cd-ui.css (NULL for the default). `page_header_extra`: what the app adds to every page
# header's server (see cd_page_header_server()).
app_frame <- function(app_name, app_version, theme, nav_sections, registry, i18n, language, selected_file = NA,
                      start_screens = list(), data, start_tab = "upload_data", open_tabs = start_tab,
                      page_header_extra = NULL) {
  ui <- cd_app_ui(
    theme = theme,
    title = app_name,
    header = cd_app_bar(app_name, app_version),
    sidebar = cd_sidebar(),
    body = cd_app_body(
      shiny.i18n::usei18n(i18n),
      cd_head_assets(),
      do.call(cd_screens, c(start_screens, list(cd_pages_ui(registry, i18n))))
    )
  )

  server <- function(input, output, session) {
    # React components render their own text, in all languages, so a language change is one message to the browser
    # rather than an update to each component.
    active_language <- reactiveVal(language)
    show_language <- function(lang) {
      shiny.i18n::update_lang(lang)
      cd_set_language(session, lang)
      active_language(lang)
    }

    loaded <- data(input, output, session)
    cache <- loaded$dataset
    # Charts keep what the user changed about their look in the dataset (cd_plot_server() reads this).
    session$userData$cd_cache <- cache
    data_ready <- loaded$ready
    analysis_ready <- loaded$analysis_ready %||% data_ready
    adopt_language <- loaded$adopt_language %||% reactive(FALSE)

    cd_shell_server(output, nav_sections, initial_tab = start_tab, data_ready = data_ready, analysis_ready = analysis_ready, i18n = i18n)

    # A click on a locked item of the sidebar says why it is locked
    observeEvent(input$cd_locked_nav_click, {
      showNotification(i18n$t("err_nav_locked"), type = "warning")
    })

    # A locked page reached another way than the sidebar (stale client state) goes back to the start screen
    observeEvent(input$tabs, {
      req(input$tabs)
      if (!input$tabs %in% open_tabs && !isTRUE(data_ready())) cd_navigate_to(session, start_tab)
    }, ignoreInit = TRUE)

    # Every page server is created at startup. A page's work waits until it is opened with data loaded, and stays done
    # when the user goes elsewhere (a new dataset starts every page over): pass `active = page_is("<tab name>")`.
    page_is <- function(tab) {
      force(tab)
      visited <- reactiveVal(FALSE)
      observe({
        if (identical(input$tabs, tab) && isTRUE(data_ready())) visited(TRUE)
      })
      observeEvent(cache(), visited(FALSE), ignoreInit = TRUE)
      reactive(visited() && isTRUE(data_ready()))
    }

    # A new dataset: a fresh one takes the language on screen, a resumed one shows its own. Then its language changes
    # show, and the language picker sets the dataset's (or the screen's, before a dataset is loaded).
    observeEvent(cache(), {
      req(cache())
      if (isTRUE(isolate(adopt_language()))) {
        if (!identical(cache()$language, isolate(active_language()))) cache()$set_language(isolate(active_language()))
      } else {
        show_language(cache()$language)
      }
    })
    observeEvent(cache()$language, {
      req(cache())
      show_language(cache()$language)
    }, ignoreInit = TRUE)
    observeEvent(input$selected_language, {
      if (!isTruthy(cache())) {
        show_language(input$selected_language)
      } else {
        cache()$set_language(input$selected_language)
      }
      session$sendCustomMessage("reinit-tooltips", TRUE)
    })

    cd_pages_server(registry, cache, i18n, page_is, header_extra = page_header_extra)
    observeEvent(input$open_reports, cd_request_report(session))

    # The dataset's country in the header (debounced: it can be set several times while a dataset is built)
    header_country <- shiny::debounce(reactive({ req(cache()); cache()$country }), millis = 300)
    output$header_pill <- renderUI({
      req(header_country())
      tags$span(
        class = "cd-dataset-pill",
        tags$span(class = "cd-dataset-pill__dot"),
        tags$span(class = "cd-dataset-pill__country", header_country()),
        if (!is.na(selected_file)) tags$span(class = "cd-dataset-pill__file", basename(selected_file))
      )
    })

    # The header's report button opens the Reports page
    output$download_buttons <- renderUI({
      req(data_ready(), cd_has_reports())
      cd_button("open_reports", "btn_report_download", i18n, icon = "file-lines", variant = "bare", class = "cd-header-download")
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
