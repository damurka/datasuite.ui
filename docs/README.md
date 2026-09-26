# How an app is put together

The architecture behind the package README: how an app is built on datasuite.ui, what the Countdown layer
(cd2030.core) adds on top, and where each piece lives. The Countdown apps (cd2030.rmncah, cd2030.vaxx, cd2030.pooled)
are the working examples.

| Read this | For |
| --- | --- |
| [`../README.md`](../README.md) | what the package is, a minimal app, the dataset contract, developing and releasing |
| this file | an app's anatomy, the page registry, a page module, the Countdown layer, themes, translations |
| [`COMPONENTS.md`](COMPONENTS.md) | reference: every component, its arguments, an example, related pieces and a screenshot (images in `img/`) |
| [`gallery`](gallery/app.R) | a live page showing every component: `shiny::runApp("docs/gallery")` from the package folder (`GALLERY_THEME=vaccine\|pooled` for the other themes) |
| [`HOWTO.md`](HOWTO.md) | recipes (add a page, a chart, an app, a component, a translation key) and the gotchas we hit |
| [`CONVENTIONS.md`](CONVENTIONS.md) | naming rules (snake_case, `cd_` prefix, `<stem>_ui/_server` pairs) |

## The idea in one paragraph

An app is an R package whose `run_app()` (1) sets what is particular to it, (2) builds a translator, (3) lists its
pages in a *page registry*, (4) builds its nav tree and (5) returns the app from `app_frame()` -- for a Countdown app,
from cd2030.core's `cd_app()`, which is `app_frame()` with the Introduction and Load Data screens. Everything visible
-- header, sidebar, cards, buttons, charts with their download tools, the Reports page -- comes from datasuite.ui, as
R functions that render React components (`js/src`) or plain `cd-` styled HTML. There is no Bootstrap, AdminLTE or
shinydashboard anywhere.

## Three layers

| Layer | Package | Holds |
| --- | --- | --- |
| the kit | **datasuite.ui** | `app_frame()`, the page registry, components, cards, charts with their tools, chart options, the Customize panel, the report engine and the Reports page, translations machinery (`R/kit-*.R`, `R/chart-options*.R`, `R/report-*.R`, `inst/www`, `js/src`) |
| the Countdown layer | **cd2030.core** | the analysis, and every page and piece the Countdown apps share: `cd_app()`, the Load Data wizard, the filters (admin level, indicator, denominator, palette, population, years), the nav sections, the scoped and tabbed pages, the data quality / denominator / coverage / equity pages, `cd_cfg()`, the report content (kinds, standard reports, fields) registered with `report_register()` (`R/ui-*.R`, `R/report-countdown.R`, `R/report-preset-*.R`) |
| an app | **cd2030.rmncah**, **cd2030.vaxx**, **cd2030.pooled** | `run_app()`, the app's own pages (`R/page-*.R`), its registry (`R/pages.R`), its Load Data screen, its translations (`inst/translation/translation.json`) and introduction (`inst/intro`) |

The rule: datasuite.ui never calls cd2030.core, and cd2030.core never calls an app. Something Countdown-specific
reaches the kit only through an argument or a registration (`page_header_extra`, `report_register()`,
`cd_register_translations()`, a dataset member).

## This package's files

```
R/
  kit-app.R               app_frame(): the whole app
  kit-page.R  kit-shell.R kit-header.R    page, screens, sidebar, app bar, page header
  kit-pages.R             the page registry (cd_page_def, cd_use_pages, cd_page_ui, cd_pages_ui, cd_pages_server)
  kit-card.R  kit-tab-panes.R             cards, tab strips and panes
  kit-buttons.R kit-inputs.R kit-files.R kit-dialogs.R kit-feedback.R kit-message-box.R    components
  kit-plot.R  kit-plot-downloads.R  kit-download-button.R  kit-excel.R                     charts, downloads, Excel
  kit-chart-schema.R kit-chart-state.R kit-chart-tools.R kit-chart-layout.R                the Customize panel
  kit-help.R  kit-notes.R                 Get help, Add notes
  kit-reports.R           the Reports page
  kit-wizard.R kit-startup-loader.R       a wizard's step rail, the loading screen
  kit-i18n.R  kit-shiny.R  kit-assets.R   translations, Shiny helpers, CSS/JS dependencies
  kit-docs.R              the help topic that lists the kit's functions (?`interface-kit`)
  chart-options*.R        chart options: build, merge, apply to a ggplot
  report-*.R              the report engine: blocks, themes, fields, Word/PowerPoint/PDF export, templates
inst/
  www/                    cd-ui.css, fonts, the built React bundle (cd-react/), logo, i18n-fix.js; served at cd-ui/
  translation/ui.json     the kit's translation keys
  rmd/report-template.docx  the Word template reports are written into
js/                       React/TypeScript source of the components; `npm run build` writes inst/www/cd-react
tests/testthat/           chart options and the report builder
docs/                     this folder (not part of the built package)
```

## An app's anatomy (`run_app()`)

The order matters. This is what cd2030.rmncah's `R/run_app.R` does:

```r
run_app <- function(selected_file = Sys.getenv("CDSUITE_SHINY_SELECTED_FILE", unset = NA),
                    language = Sys.getenv("CDSUITE_SHINY_LOCALE", unset = "en"),
                    app_name = Sys.getenv("CDSUITE_SHINY_NAME", unset = "RMNCAH"), app_version = ..., ...) {
  options(shiny.maxRequestSize = ..., future.globals.maxSize = ...)

  options(cd2030.selected_group = "rmncah", cd2030.app_group = "rmncah")   # 1. pin the indicator group (below)
  set_selected_group("rmncah")
  options(cd2030.config = list(...))                  #    per-app settings for the shared pages (see "Config")
  rmncah_wizard_options()                             #    the Load Data wizard's fields
  options(cd2030.help_dir = system.file("intro", package = "cd2030.rmncah"))

  i18n <- shiny.i18n::init_i18n(translation_json_path =                     # 2. the translator
    cd_translations(system.file("translation", "translation.json", package = "cd2030.rmncah")))
  i18n$set_translation_language(language); cd_use_i18n(i18n)

  pages <- rmncah_pages(); cd_use_pages(pages)        # 3. the page registry (R/pages.R)

  nav <- list(cd_nav_start(), cd_nav_quality(), cd_nav_denominators(),     # 4. the nav tree
              cd_nav_section("lbl_nav_section_analysis", cd_nav_national(), cd_nav_subnational(), ...),
              cd_nav_section("lbl_nav_section_output", cd_nav_item("title_reports", tabName = "reports", ...)))

  cd_app(app_name = app_name, app_version = app_version, theme = "rmncah",  # 5. build and return the app
         nav_sections = nav, registry = pages, i18n = i18n, language = language, selected_file = selected_file,
         upload_ui = upload_data_ui, upload_server = upload_data_server)
}
```

`run_app()` returns the `shinyApp` object; printing it (or `shiny::runApp()`) runs it. DataSuite runs an app from a
folder whose `app.R` is one line, `cd2030.rmncah::run_app()`, and passes its settings in environment variables:
`CDSUITE_SHINY_NAME`, `_VERSION`, `_LOCALE`, and `CDSUITE_SHINY_SELECTED_FILE` (a dataset for rmncah/vaxx, a folder of
`.rds` files for pooled). Given as arguments instead, they work from an R console.

### The page registry (`R/pages.R` of an app)

One `cd_page_def()` per analysis page; the registry is a list of them and `cd_use_pages()` makes it available to
`cd_page_ui()`. From it `app_frame()` builds the page containers (`cd_pages_ui()`) and starts every page server
(`cd_pages_server()`), including each page's header (title, help, report button). A page module never repeats its
title, section or help chapter.

```r
cd_page_def(
  id = "national_target",                 # the tab name; also the module id
  ui = national_target_ui, server = national_target_server,
  title = "title_nav_global_coverage",    # translation keys
  section = "title_nav_national_analysis",
  subtitle = "sub_target_national",
  help = c("national-global-coverage"),   # c(help chapter, optional section) for the Get help button
  denominator = TRUE,                     # show the "Denominator" row under the header (cd2030.core's header extra)
  report = NULL,                          # key the report/notes buttons use, if the page has one
  server_args = list(),                   # extra arguments after (id, cache, i18n)
  active = TRUE                           # FALSE: the server is not given `active` (see below)
)
```

The shared Countdown pages (`reporting_rate_ui`, `national_coverage_ui`, ...) are exported by cd2030.core; an app's
registry names them next to its own pages.

### A page module

```r
my_page_ui <- function(id, i18n) {
  ns <- NS(id)
  cd_page_ui(id, i18n,                                  # title/section/subtitle come from the registry
    filters = cd_filter_bar(cd_chip_select(ns("x"), "title_x", choices = c(opt_a = "a"), i18n = i18n)),
    cd_chart_card(...), cd_table_card(...)
  )
}

my_page_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  stopifnot(is.reactive(cache)); stopifnot(is.reactive(active))
  moduleServer(id, function(input, output, session) {
    data <- reactive({ req(cache(), active()); cache()$some_data })   # never compute before the page is opened
    ...
  })
}
```

* `cache` is a reactive holding the dataset (for Countdown apps the cd2030.core `CacheConnection`; `NULL` until one
  is loaded). What the kit itself needs from it is the [dataset contract](../README.md#the-dataset-contract).
* `active` is TRUE once the page has been opened *with data loaded* and **stays TRUE** afterwards (`page_is()` in
  `app_frame()`); a new dataset resets it. Gate any real computation on `req(active())`, or every page computes on
  start-up.

In an app package a page module is a file `R/page-<n>_<name>.R`; it is not exported (only `run_app()` is).

## The Countdown layer (cd2030.core)

### Config: what differs per app (`cd_cfg`)

The shared Countdown pages hold nothing specific to one indicator group. Each app sets one list and the pages read it
with `cd_cfg("key", default)` (cd2030.core `R/ui-core-config.R` documents the keys):

| Key | Meaning |
| --- | --- |
| `nat_cov_indicators` | tabs of national/sub-national coverage (may include `"fpet"`, which adds the family-planning chart) |
| `target_indicators` | tabs of the coverage-target pages |
| `equity_indicators`, `equity_custom_exclude` | tabs of Equity Assessment; indicators its Custom picker leaves out |
| `cov_trend_indicators`, `sub_derived_indicators`, `survey_comp_indicators` | tabs of the three Denominator Selection cards |
| `adjustment_indicators` | tabs of Data Adjustment Changes |
| `k_factors` | the adjustment factors: `list(name = list(id = "k_anc", label = "<translation key>"))` |
| `reporting_rate_indicators`, `reporting_rate_facet_ncol` | Reporting Rate's service chips and national plot layout |
| `consistency_pairs` | `list(c("anc1","penta1"), ...)`: one Consistency Checks tab per pair |
| `has_maternal` | does the group have a maternal denominator |

Other app-level options read by the shared code: `cd2030.default_indicators` (tabs of pages that pass no list; a
vector or a function such as `get_analysis_indicators`), `cd2030.denominator_choices` (denominator chip options),
`cd2030.wizard` (the Load Data wizard, cd2030.core `R/ui-wizard-wizard-config.R`), `cd2030.help_dir` (the folder of
the Introduction page's markdown, one file per language).

### The indicator group -- read this before touching loading code

`cd2030.core` keeps **one indicator group for the whole R session** (`set_selected_group()`), and that value wins over
`options(cd2030.selected_group)`. It is changed by loading any dataset (each remembers the group it was built for). So
an app started after another in the same R session, or a saved dataset built for another group, would silently run on
the wrong group (e.g. vaxx showing OPD). Therefore:

* `run_app()` calls `set_selected_group()` explicitly and stores `options(cd2030.app_group)`, which loading cannot
  change.
* The wizard reads `cd_wizard_indicator_group()` (the app's group), re-asserts it before every load, and refuses a
  dataset built for a different group (`cd_wizard_check_group()`).
* Saved copies are `<file>_<group>.rds` (`cd_saved_copy_name()`), so rmncah and vaxx can open the same source file.

### Shared pages (cd2030.core `R/ui-page-*.R`)

Page modules identical in rmncah and vaxx, or differing only by an indicator list: outlier detection, reporting rate,
completeness, consistency checks, overall score, remove years, adjustment, denominator assessment/selection, coverage,
target, inequality (+ map), equity, and the national/sub-national wrappers. An app's registry just names them.
Anything genuinely group-specific (rmncah's mortality, utilization, health-system and Bayesian pages, each app's
upload-data screen) stays in that app's package.

## Themes

`app_frame(theme = ...)` puts `cd-theme-<name>` on `<body>`; `inst/www/cd-ui.css` (last block, "App themes")
re-declares the five primary-colour tokens (`--cd-primary`, `-hover`, `-rgb`, `-ink`, `-tint`). Default (no class) is
rmncah maroon; `"vaccine"` is blue, `"pooled"` is green. Components only read the tokens, so a new theme is one CSS
block.

## Translations

Text is never written into a component: it is a translation key. Translations are layered, and `cd_translations()`
merges them into one temporary file for `shiny.i18n::init_i18n()`:

1. `inst/translation/ui.json` of this package: the kit's own keys;
2. files registered with `cd_register_translations()` -- cd2030.core registers `inst/translation/cd2030.json` (keys
   the Countdown pages and apps have in common) when it loads;
3. the app's own `inst/translation/translation.json` (its extras; it may override any key).

Add a key to the layer that uses it: a kit component's text to `ui.json`, a shared Countdown page's to cd2030.core's
`cd2030.json`, an app-only key to that app's file. React components get every language at once (`cd_text()` ->
`{en, fr, pt}`) and re-render themselves on a language change (one `cd-lang` message, `cd_set_language()`); plain HTML
text uses `i18n$t()` and the `usei18n()` DOM rescan.

## Building the React components

Source is `js/src`; `npm run build` in `js/` (type-check + webpack) writes `inst/www/cd-react/cd-react.js`, which is
committed so running an app needs no Node. See `HOWTO.md` -> "Add a React component".

## Checking a change

`devtools::test()` and `devtools::check()` cover chart options and the report builder. The rest of the UI is checked
by building and running an app (see `HOWTO.md` -> "Verifying a change"): build every registry page's UI and start its
server with `testServer()`, start the app and fetch `/`, and click through the affected pages in a browser with a real
dataset.
