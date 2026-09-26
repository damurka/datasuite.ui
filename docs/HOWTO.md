# How to ... (recipes and gotchas)

Companion to `README.md` (how an app is put together), `../README.md` (the package) and `COMPONENTS.md` (what each
function does). The apps are R packages (cd2030.rmncah, cd2030.vaxx, cd2030.pooled); the Countdown pages they share are
in cd2030.core; the components are here.

## Add a page to an existing app

1. Write the module in the app package's `R/page-<n>_<name>.R` (or, if it is group-agnostic and several Countdown apps
   need it, in cd2030.core's `R/ui-page-<n>_<name>.R`, exported through `R/ui-exports.R`): `<name>_ui(id, i18n)`
   returning `cd_page_ui(id, i18n, filters = cd_filter_bar(...), <cards>)` and
   `<name>_server(id, cache, i18n, active = reactive(TRUE))`. Gate real work on `req(cache(), active())`.
2. Add a `cd_page_def(id = "<tab>", ui = ..., server = ..., title = "<key>", section = "<key>", subtitle = "<key>", help = ...)`
   to the app's registry (`R/pages.R`, e.g. `rmncah_pages()`).
3. Add a `cd_nav_item("<title key>", tabName = "<tab>", icon = "...")` to the app's nav tree in `run_app()`.
4. Add the translation keys (`title_*`, `sub_*`, the nav label) to the app's `inst/translation/translation.json`, or to
   cd2030.core's `inst/translation/cd2030.json` if several apps use them.
5. `devtools::document()` and `devtools::check()` in the package you changed (new column names used unquoted in dplyr
   code go in its `R/globals.R`).

A page that is the same analysis at national and sub-national level: use `cd_scoped_page_ui/_server` (README/COMPONENTS -> scoped pages).

## Add a chart (or a tabbed set of charts) to a page

One chart: `cd_chart_card(title, chart_toolbar = cd_plot_toolbar_ui(ns("c")), cd_plot_ui(ns("c"), toolbar_inline = TRUE), i18n = i18n)`
in the UI, `cd_plot_server("c", i18n, plot_data = <reactive>, plot_fun = function(d) plot(d, ...), plot_filename = reactive("c"),
excel_sheet = "<key>")` in the server. Tabs by indicator: `cd_tabbed_charts_ui/_server` (COMPONENTS.md section 3 has a full example).
A table: `cd_table_ui/_server`.

## Add or change what differs per app (a custom indicator group)

* An indicator list, a set of tabs, the adjustment factors, Reporting Rate's services, Consistency Checks' pairs: add or edit the key in
  the app's `options(cd2030.config = list(...))` in `run_app()`. If a shared page needs a *new* per-app setting, read it there with
  `cd_cfg("new_key", default)` and document the key in cd2030.core's `R/ui-core-config.R`.
* The Load Data national-rate fields and reference uploads: `options(cd2030.wizard = ...)`, set by the app's
  `<app>_wizard_options()` (in its `R/page-0_upload_data.R`).
* Denominator options / whether there is a maternal denominator: `options(cd2030.denominator_choices = ...)`,
  `cd_cfg("has_maternal")`.
* Tabs of the pages that pass no list: `options(cd2030.default_indicators = c(...))` or a function (e.g. `get_analysis_indicators`).

## Add a new app (for example a custom indicator group)

1. Copy the cd2030.vaxx repo (the smallest app) to `cd2030.<name>`; rename the package in `DESCRIPTION` (`Package:`,
   `Title:`, `URL:`), the `.Rproj`, and `R/package.R`'s help topic. Keep `Imports: cd2030.core, datasuite.ui` with
   their minimum versions, and `Remotes:`.
2. In `R/run_app.R`: `options(cd2030.selected_group = "<group>", cd2030.app_group = "<group>")` and
   `set_selected_group("<group>")` (the group must exist in cd2030.core: `register_indicator_group()`); set
   `cd2030.config`; pick `theme = ` (or add a colour block to datasuite.ui's `inst/www/cd-ui.css`, "App themes", and
   use its name); adjust the nav tree; point `system.file()` calls at the new package name.
3. `R/pages.R`: drop the pages the app does not have, add its own.
4. `inst/translation/translation.json`: only the keys specific to it (it inherits the kit's and cd2030.core's). It may
   override any key.
5. `R/page-0_upload_data.R`: copy from vaxx and set `cd2030.wizard` for the group's national-rate fields.
6. `inst/intro/0_intro_<lang>.md` for the Introduction page (`options(cd2030.help_dir = ...)`).
7. Publish it: add it to the r-universe registry (`damurka/damurka.r-universe.dev`, `packages.json`), then add a
   `contributes.shinyApps` entry to the DataSuite extension (countdown-analytics) with `"package": {"name":
   "cd2030.<name>", "version": "...", "repos": [...]}` and an `apps/<name>/app.R` of one line,
   `cd2030.<name>::run_app()`. The extension also holds the app's `data-reference.json` / `docs-index.json`, which
   DataSuite's AI tools read.

## Add a translation key

`i18n$t("key")` for plain text, `cd_text(i18n, "key")` (done for you by every `cd_*` component) for React. Add
`{"key": ..., "en": ..., "fr": ..., "pt": ...}` to the layer that uses it: datasuite.ui's `inst/translation/ui.json` (a kit
component), cd2030.core's `inst/translation/cd2030.json` (the shared Countdown pages) or the app's own
`inst/translation/translation.json`. A
placeholder is written `{name}` and filled with `str_glue(...)` or by `cd_message_server()`'s `parameters`. A key that is missing
shows as the key itself (in R also a warning "translation does not exist"). Keys built at run time (`paste0("opt_", indicator)`)
are easy to miss: add `opt_<indicator>` for every indicator an app offers.

## Add a React component

1. `js/src/components/MyThing.tsx`. For an input, wrap with `InputAdapter` (`@/shiny.react`) so it has a value/`updateReactInput`;
   send `input$<id>__mounted` on mount (copy `FieldSelect.tsx`) if R will push values to it. Take every piece of text from props as
   `{en, fr, pt}` and show it with `tr(text, useLang())`.
2. Register it in `js/src/index.ts` (`window.jsmodule["@/countdown"]`).
3. R wrapper in the matching `R/kit-*.R` file (inputs in `kit-inputs.R`, feedback in `kit-feedback.R`, ...):
   `cd_my_thing <- function(inputId, ..., i18n = cd_i18n()) cd_react_element("MyThing", shiny.react::asProps(inputId = inputId, ...))`.
   Every name that does not start with a dot is exported (`exportPattern`); add it to the aliases in `R/kit-docs.R` so
   it has a help topic, and describe it in `docs/COMPONENTS.md`.
4. Style with `cd-` classes in `inst/www/cd-ui.css` and colours from the `--cd-*` tokens (never a hard-coded brand colour: themes change them).
5. `cd js && npm run build` (type-checks, then writes `inst/www/cd-react/cd-react.js`); commit the bundle. Add it to
   `docs/gallery/app.R` so it can be seen and screenshotted.

## Verifying a change

`devtools::test()` covers chart options and the report builder (here) and the analysis (cd2030.core); there is no
test suite for pages. `devtools::check()` must stay clean in every package you touched. For the UI, from the app
package's folder (the app must not be already running on the port):

* **Parse** every touched file: `Rscript -e "invisible(parse('file.R'))"`.
* **Build every page and start every server**, offline: `pkgload::load_all()`, build the translator as `run_app()` does,
  then for each `p` in the registry (e.g. `rmncah_pages()`): `p$ui(p$id, i18n)` and
  `testServer(p$server, args = c(list(cache = reactive(NULL), i18n = i18n, active = reactive(FALSE)), p$server_args), session$flushReact())`.
  (Pages registered with `active = FALSE` do not take `active`; the error for those is expected.)
* **Undefined functions:** `devtools::check()` reports them ("no visible global function definition").
* **Start it** (`shiny::runApp(run_app(), port = N, launch.browser = FALSE)`, or `shiny::runApp()` in the package folder,
  whose `app.R` loads the source with pkgload) and fetch `/` -- checks the theme class and that it serves.
* **With data:** run with `CDSUITE_SHINY_SELECTED_FILE` set to a dataset (an `.rds` resumes straight to the app; an `.xlsx` starts
  the wizard) and click through the pages you touched. Errors in a chart show in red inside the card; the R console has the rest.
* For R-side logic against a real cache: `cache <- init_CacheConnection(rds_path = "<file>.rds")` after `set_selected_group("<group>")`.

## Gotchas we hit (read before changing loading, filters or charts)

* **The indicator group is session-global in `cd2030.core`.** Loading a dataset (any `init_CacheConnection()`) sets it to the dataset's own
  group, and it beats `options(cd2030.selected_group)`. Apps pin their own (`cd2030.app_group`, `set_selected_group()` at start, re-asserted by the
  wizard before each load). Never trust `get_selected_group()` in the app after a load; use `cd_wizard_indicator_group()`.
* **Per-group things must be read late.** A top-level `x <- getOption(...)` in a module runs at `source()` time, before the app has set
  its options. Read inside functions (`cd_cfg("key")` at call time), or make the option a function.
* **`showCustom` must match on both sides** (`cd_tabbed_charts_ui` and `_server`).
* **Hidden outputs are suspended.** Shiny does not compute an output whose element is hidden (`display: none`), and reports
  its hidden state only on a resize; a filter whose value the whole page waits for (the region chip) is set
  `suspendWhenHidden = FALSE`, and `nav.ts` fires a `resize` after each tab switch.
* **Empty chip means "all".** `cd_chip_multi` sends `""`. `as.integer("")` is `NA`, and `NA` years plot nothing: use `cd_years_input()`.
* **Push to a component only after it mounted** (`cd_mounted()` / `cd_remounted()`): messages to an unmounted React component are dropped.
  Use `cd_remounted()` for anything that can be torn down and rebuilt (wizard steps).
* **`str_glue()` reads the calling function's variables.** A translation like "Relationship between {vacc1} and {vacc2}" needs `vacc1`/`vacc2`
  defined where `str_glue()` runs.
* **`active` is latched.** It stays TRUE after the first visit so returning to a page does not recompute; a new dataset resets it. Do not
  compare against `input$tabs` yourself.
* **`plot()` on core objects.** The map plots read their `palette` from the data object's attributes; asking for another palette is a
  `palette =` argument to the data function (`get_filtered_mapping_data`, `filter_mortality_summary`, `prepare_mapping_service_utlization`).
* **Printing `run_app()`'s result starts the app** (at top level R auto-prints it). In a script, assign it (`app <- run_app()`)
  or wrap it in `invisible()`.
* **Line endings are mixed** (older files CRLF, newer LF). A script that edits a file should read and write it with `newline=''` and keep what it finds.
* **R sources must be ASCII** (`R CMD check` warns otherwise): write `"\u00e9"` in strings, plain ASCII in comments.
* **A rename must be a whole-word replace over datasuite.ui, cd2030.core, every app package and `js/src`;** see `CONVENTIONS.md`
  and the rename map approach used for the snake_case pass.
* **An installed package is what an app runs.** After changing datasuite.ui or cd2030.core, reinstall it before running an
  app (`devtools::install()`; for cd2030.core add `dependencies = FALSE`, or its `Remotes:` reinstalls datasuite.ui from
  GitHub over your local one). Stop a running app first: Windows locks a loaded package's files.
