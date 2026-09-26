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
