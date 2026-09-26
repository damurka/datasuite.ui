# datasuite.ui

The interface every DataSuite Shiny app is built from: the page frame, sidebar and header, cards, inputs, dialogs and
charts (R functions that render React components), chart options that restyle any ggplot2 chart, and a Word-like report
builder that writes Word, PowerPoint and PDF files.

It is **generic**: it knows nothing about any app's data. An app gives it its pages, its charts, its data and its
report content through small interfaces (a page registry, a `data_server`, `report_register()` / `report_context()`).
It never calls cd2030.core; cd2030.core calls it.

## Where it fits

```
DataSuite (the editor)                 launches an app and keeps its R packages installed and up to date
  an extension (countdown-analytics)   says which apps exist: contributes.shinyApps[].package, e.g. cd2030.rmncah
    an app package (cd2030.rmncah)     run_app(): its own pages, nav, translations, Load Data screen
      cd2030.core                      Countdown analysis + the pages every Countdown app shares + report content
        datasuite.ui  <- this          the frame, components, chart options, report engine
```

All of them are published on [damurka.r-universe.dev](https://damurka.r-universe.dev), which builds each package from
its GitHub repo's `main`. DataSuite installs datasuite.ui as a dependency of the app an extension names, updates it
when the extension is updated, and installs the packages it suggests (PDF export, pictures, editable PowerPoint charts)
at the same time.

## Install

```r
install.packages("datasuite.ui", repos = c("https://damurka.r-universe.dev", "https://cloud.r-project.org"))
```

The suggested packages turn on parts of the report builder: `chromote` (PDF without Word or LibreOffice), `pdftools`
(showing the final pages), `rvg` (editable charts in PowerPoint), `magick`, `rsvg`, `svglite`, `ragg`, `png` (pictures),
`zip` (finishing Word and PowerPoint files), `systemfonts` (only offering installed fonts), `plotly`, `dplyr`,
`countrycode`. Without them those features do less or say what is missing.

## A minimal app

A complete app: one page with one chart. It runs as it is, next to a `translation.json` holding the three keys it
uses (see [Translations](#translations)).

```r
library(shiny)
library(datasuite.ui)

# The dataset every page receives. Any object will do: the kit only reads the members it uses
# (see "The dataset contract"). Here: the data, the language and the country shown in the header.
new_dataset <- function(data) {
  ds <- new.env()
  ds$data <- data
  ds$country <- "Demo"
  ds$language <- "en"
  ds$set_language <- function(lang) ds$language <- lang
  ds
}

# A page: a UI and a server with the same stem. Its title and section come from the registry.
cars_ui <- function(id, i18n) {
  ns <- NS(id)
  cd_page_ui(id, i18n,
    cd_chart_card("title_cars", chart_toolbar = cd_plot_toolbar_ui(ns("plot")),
                  cd_plot_ui(ns("plot"), toolbar_inline = TRUE), i18n = i18n)
  )
}
cars_server <- function(id, cache, i18n, active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    cd_plot_server("plot", i18n,
      plot_data = reactive({ req(active()); cache()$data }),   # nothing is computed before the page is opened
      plot_fun = function(d) ggplot2::ggplot(d, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
    )
  })
}

# The page registry: one entry per page
pages <- list(cd_page_def(id = "cars", ui = cars_ui, server = cars_server,
                          title = "title_cars", section = "lbl_section_demo", subtitle = "sub_cars"))
cd_use_pages(pages)

i18n <- shiny.i18n::init_i18n(translation_json_path = cd_translations("translation.json"))
i18n$set_translation_language("en")
cd_use_i18n(i18n)

app_frame(
  app_name = "Demo", app_version = "0.1.0", theme = NULL,
  nav_sections = list(cd_nav_section("lbl_section_demo", cd_nav_item("title_cars", tabName = "cars", icon = "car"))),
  registry = pages, i18n = i18n, language = "en",
  start_tab = "cars", open_tabs = "cars",
  data_server = function(input, output, session) {
    dataset <- reactiveVal(new_dataset(mtcars))
    list(dataset = dataset, ready = reactive(!is.null(dataset())))
  }
)
```

The chart card comes with its download tools (PNG, Excel) and the Customize panel. A real app puts this in a package
whose `run_app()` builds the same pieces and returns the app: see cd2030.rmncah's `R/run_app.R`, which goes through
cd2030.core's `cd_app()` (an `app_frame()` with the Countdown Introduction and Load Data screens).

### `app_frame()` in short

| Argument | What it is |
| --- | --- |
| `nav_sections` | the sidebar: `cd_nav_section(label_key, cd_nav_item(label_key, tabName, icon, children, requires_adjustment))` |
| `registry` | the pages: a list of `cd_page_def()`; `app_frame()` builds their containers and starts their servers |
| `start_screens` | screens that are not pages (an upload screen, an introduction): `cd_screen(tabName, ui)` |
| `data_server` | `function(input, output, session)`, called once per session, returning `dataset` (a reactive), `ready` (a reactive: pages unlock when TRUE) and optionally `analysis_ready` (for nav items with `requires_adjustment = TRUE`) and `adopt_language` |
| `start_tab`, `open_tabs` | the screen shown first; the tabs that never lock |
| `theme` | `NULL` (maroon), `"vaccine"` (blue), `"pooled"` (green): a block of `inst/www/cd-ui.css` |
| `page_header_extra` | what an app adds to every page header's server (cd2030.core adds the denominators row) |

A page server is called as `server(id, cache, i18n, <server_args>, active = <reactive>)`. `active` becomes TRUE once
the page has been opened with data loaded and stays TRUE; gate every real computation on it.

### The dataset contract

The dataset is whatever `data_server()` returns in `dataset`. The kit reads these members when they exist, and does
without the feature when they don't:

| Member | Used by |
| --- | --- |
| `language`, `set_language(lang)` | the language picker; a resumed dataset opens in its own language |
| `country` | the header pill, report fields |
| `chart_options` (a list by chart id), `set_chart_options(id, options)` | the Customize panel: a chart keeps its look |
| `get_notes(page_id, object_id, params)`, `append_page_note(...)` | the Add notes button |
| `report_projects`, `set_report_project(id, project)`, `report_assets`, `set_report_asset(id, value)`, `report_themes`, `set_report_theme(id, theme)` | the Reports page: saved reports, their pictures, saved themes |
| `data_years`, `subnational_regions` | report fields and region pickers |

cd2030.core's `CacheConnection` has all of them; it is the reference implementation.

## What is in it

### 1. Chart options -- `R/chart-options*.R`

Restyle a finished ggplot2 chart without touching the code that drew it: texts, fonts, sizes, colours, axes, legend,
grid, facets, marks, and show or hide any element.

```r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
apply_chart_options(p, cd_chart_options(title = "Fuel economy", title_size = 18, show_legend = FALSE))
```

`cd_chart_options()` builds a set (class `cd_chart_options`), `merge_chart_options()` layers several,
`resolve_chart_options()` picks the ones stored for a chart id, `chart_option_fields()` lists every option (the
Customize panel is built from it), `cd_chart_type()` names a chart by its geometry, `chart_facet_info()` tells the
editor how a chart is faceted. The class keeps its old `cd_` name so datasets saved before the split still load.

### 2. The Shiny and React kit -- `R/kit-*.R`, `js/src`, `inst/www`

Every visible piece of an app. Each R function either writes plain HTML with `cd-` classes (styled by
`inst/www/cd-ui.css`) or renders a React component from `js/src/components` with `cd_react_element()`.

| File | What |
| --- | --- |
| `kit-app.R` | `app_frame()`: the whole app |
| `kit-page.R`, `kit-shell.R`, `kit-header.R` | `cd_app_ui()`, `cd_screens()`, `cd_page_content()`; sidebar and app bar (`cd_sidebar()`, `cd_app_bar()`, `cd_nav_section()`, `cd_nav_item()`); the page header |
| `kit-pages.R` | the page registry: `cd_page_def()`, `cd_use_pages()`, `cd_page_ui()`, `cd_pages_ui()`, `cd_pages_server()` |
| `kit-card.R`, `kit-tab-panes.R` | cards (`cd_card()`, `cd_chart_card()`, `cd_table_card()`), tab strips and panes |
| `kit-buttons.R`, `kit-inputs.R`, `kit-files.R`, `kit-dialogs.R`, `kit-feedback.R`, `kit-message-box.R` | buttons, chips and fields, file and folder uploads, dialogs, banners, spinners, empty states, messages |
| `kit-plot.R`, `kit-plot-downloads.R`, `kit-download-button.R`, `kit-excel.R` | a chart with its toolbar and downloads (`cd_plot_ui()` / `cd_plot_server()`), Excel sheets |
| `kit-chart-*.R` | the Customize panel: its fields, and turning its values into chart options and back |
| `kit-help.R`, `kit-notes.R` | the Get help and Add notes buttons of a page header |
| `kit-reports.R` | the Reports page (`reports_ui()` / `reports_server()`) around the React report editor |
| `kit-wizard.R`, `kit-startup-loader.R` | the step rail of a wizard, the loading screen |
| `kit-i18n.R`, `kit-shiny.R`, `kit-assets.R` | translations; Shiny helpers (`cd_update_input()`, `cd_mounted()`, `cd_navigate_to()`); CSS/JS dependencies |

`docs/COMPONENTS.md` describes each one with an example and a screenshot; `docs/gallery` shows them all live.

### 3. The report builder -- `R/report-*.R`, `inst/rmd`, `js/src/components/report`

Reports are made of blocks (headings, text, charts, tables, questions, pictures, page breaks) on pages with a theme
and a cover, edited in the browser and exported with `export_report()` (Word, and PDF made from the Word file) or
`export_deck()` (PowerPoint, PDF). Themes: `report_themes()`, `report_default_design()`, `report_theme_from_file()` (a
theme taken from an Office file). Fields such as `{country}` or `{latest_year}`: `report_field_catalog()`,
`report_fields()`.

The engine draws nothing itself. **An app plugs its content in, in two parts:**

* **The registry, once per app** (usually in its package's `.onLoad()`): `report_register(themes, default_theme,
  cover, kinds, presets, indicator_name, fields, chart_id)`. `kinds` returns the kinds of chart and table a block
  can be (each a `report_kind()`), `presets` the standard reports, `chart_id` the id a chart's options are kept under.
* **The context, per dataset**: `report_context(draw, asset_get, asset_set, chart_options, fields, years, regions,
  flag)` -- how to draw a block from this dataset, where its pictures are kept, its values for the fields. Every report
  function takes a context, or anything `as_report_context()` turns into one: an app adds an `as_report_context()`
  method for its own dataset class (cd2030.core does for `CacheConnection`), so `export_report(cache, project, file)`
  works.

cd2030.core's `R/report-countdown.R` and `R/report-preset-*.R` are a full example: Countdown's kinds, standard reports
and drawing code.

## Translations

No text is written into a component: it is a key, looked up in a `shiny.i18n` translation file with English, French
and Portuguese. `cd_translations(app_file)` merges three layers into one temporary file for
`shiny.i18n::init_i18n()`:

1. the kit's keys, `inst/translation/ui.json` (buttons, dialogs, the Reports page, the Customize panel ...);
2. files packages registered with `cd_register_translations(path)`, usually in their `.onLoad()` (cd2030.core
   registers its `inst/translation/cd2030.json`);
3. the app's own file (`app_file`), which may override any key.

Then `cd_use_i18n(i18n)` makes it the translator the components use. A file looks like:

```json
{
  "languages": ["key", "en", "fr", "pt"],
  "translation": [
    {"key": "title_cars", "en": "Cars", "fr": "Voitures", "pt": "Carros"}
  ]
}
```

A missing key shows as the key itself, with a warning in R. React components receive every language at once and
switch without a round trip; plain HTML uses `i18n$t()`.

## The React bundle

The components' source is in `js/` (TypeScript, React, webpack). The build writes `inst/www/cd-react/cd-react.js`,
which **is committed**, so installing or running the package needs no Node:

```sh
cd js
npm ci
npm run build     # type-check, then webpack --mode production -> inst/www/cd-react/
```

Rebuild and commit the bundle whenever `js/src` changes. The package serves `inst/www` at the URL prefix `cd-ui/`
(`.onLoad()`), and `cd_head_assets()` adds the stylesheet, fonts and bundle to a page.

## Developing

```r
devtools::load_all()              # try changes without installing
devtools::test()                  # tests/testthat: chart options, report builder, Word/PowerPoint export, templates
devtools::document()              # after changing roxygen comments
devtools::check()                 # must stay 0 errors, 0 warnings, 0 notes
shiny::runApp("docs/gallery")     # every component on one page (GALLERY_THEME=vaccine or pooled)
```

Naming rules are in `docs/CONVENTIONS.md` (snake_case, `cd_` prefix for the kit, `<stem>_ui()` / `<stem>_server()`
pairs). R sources must be ASCII (write `\u00e9` in strings).

Most of the kit is only exercised by a running app. After changing it, also run an app on top of it (cd2030.rmncah,
with cd2030.core installed) and click through the pages you touched.

## Releasing

1. Bump `Version:` in `DESCRIPTION` and add a section to `NEWS.md`.
2. `devtools::check()`: 0 errors, 0 warnings, 0 notes.
3. Commit, tag `vX.Y.Z`, push `main` and the tag.
4. r-universe builds it, usually within the hour (<https://damurka.r-universe.dev/builds>). DataSuite users get it the
   next time an extension whose app depends on it is installed or updated. If a package needs the new version, raise
   its `datasuite.ui (>= X.Y.Z)` in that package's `DESCRIPTION`.

## Documentation map

| Read | For |
| --- | --- |
| this file | what the package is, how an app uses it, how to develop and release it |
| [`docs/README.md`](docs/README.md) | how an app is put together on top of it: anatomy, page registry, themes, translations, the Countdown layer |
| [`docs/COMPONENTS.md`](docs/COMPONENTS.md) | every component: arguments, an example, a screenshot; the report builder in detail |
| [`docs/HOWTO.md`](docs/HOWTO.md) | recipes (a page, a chart, a component, a translation key, a new app) and the gotchas we hit |
| [`docs/CONVENTIONS.md`](docs/CONVENTIONS.md) | naming rules and the history of the renames |
| [`docs/gallery/app.R`](docs/gallery/app.R) | a live page with every component |
| [`NEWS.md`](NEWS.md) | what changed in each version |
| `?datasuite.ui` | the package overview in R's help |

## License

AGPL (>= 3). Copyright African Population and Health Research Center (APHRC).
