# datasuite.ui 0.2.0

* The AI bridge, protocol 2 (`docs/AI-BRIDGE.md`): every app built with `app_frame()` tells DataSuite's chat where
  the user is -- the page, its cards and tabs, which tab each card shows, what is in view (measured in the browser),
  which charts and tables have been drawn, what the app adds -- and answers its requests: the pages, the data behind a
  chart or table, a card's position for a screenshot, showing a tab, opening a page. Apps add their own state and
  actions with `app_frame(ai_state =, ai_actions =)` and `ai_action()`; `ai_reply_when()` answers once the browser has
  caught up. `cd_plot_server(about =)` and `ai_register_component()` say what a chart or table is.
* Custom charts: `report_validate_spec()`, `report_apply_transforms()` and `report_plot_spec()` check, reshape and
  draw a chart described as data (report kind `custom_chart`); `report_validate_project()` checks a report before it
  is saved.

# datasuite.ui 0.1.1

* fontawesome and jquerylib, which the kit uses on every page, are now imported rather than suggested, so they
  install with the package. shinyjs and shinycssloaders are no longer suggested: the kit replaced both with its own
  code.
* `report_fonts()` gains `installed = TRUE`; `installed = FALSE` gives every font a report may name, installed on
  this computer or not.

# datasuite.ui 0.1.0

First release: the shared interface of DataSuite's Shiny apps, taken out of cd2030.core and the Countdown apps.

* Chart options: `cd_chart_options()` and `apply_chart_options()` restyle any ggplot2 chart (texts, fonts, colours,
  axes, legend, grid, facets) and show or hide each element (`show_title`, `show_legend`, ...).
* The report builder: Word, PowerPoint and PDF export, themes (built-in and from Office files, with slide designs),
  fields, slide decks and free-layout pages. An app plugs in with `report_register()` (its themes, kinds of chart,
  standard reports, fields) and `report_context()` (one dataset's drawing, pictures, saved options, field values).
* The Shiny and React kit: components, page frame (`app_frame()`), sidebar and header, chart cards with download
  tools and the Customize panel, the Reports page, and the kit's translations (packages add theirs with
  `cd_register_translations()`).
