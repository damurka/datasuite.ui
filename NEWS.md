# datasuite.ui 0.4.4

* The Ask AI button's hint is its text. Hovering it showed the markup around it
  (`<span class="i18n" data-key="lbl_ask_ai_hint">Ask DataSuite's AI about this</span>`): the hint is an attribute,
  and was given what `i18n$t()` returns for text to be translated in place.

# datasuite.ui 0.4.3

* The React components (sidebar, header, inputs, chips, wizard, chart options...) now come from `@quire/components`
  (datasuite-ui-kit's `packages/components`), so other hosts than Shiny can use them; the apps see the same components
  under `@/countdown` as before. `js/` keeps the Shiny glue (the AI bridge, spinners, tab switches, dialogs).
* Charts and tables already on screen are drawn again in the new language when the language changes. The session's
  language now exists from the start (shiny.i18n made it on the first change, so what was drawn before the first change
  never followed it), and `cd_plain_text()` uses it. `i18n$t()` outside a reactive context (a module server's body
  building a card) reads it without a dependency: with shiny.i18n's own reactive value it failed with "Operation not
  allowed without an active reactive context".
* Requests to DataSuite on Jovian's host channel find the kernel's `host_ask()` / `host_notify()` in Jovian 0.2.6's
  layout too (`.elara.host_ask()` in `tools:jovian`), as well as in the `hera` namespace of earlier kernels.

# datasuite.ui 0.4.2

* Requires quire 0.2.17, as the apps do (a data table's alignment in the builder, Word, PowerPoint and print).
* Reports written as Word or PowerPoint have their charts again where the rsvg package is not installed: it is suggested,
  so it is installed with the app, and writing a report without it says so. The SVG charts are made pictures with rsvg
  (magick's `image_read_svg()` uses it too), and without it every chart was left out of the document.
* `ds_host_request()`: what an app asks of DataSuite (open the chat, print, install packages), in one place. When
  DataSuite starts the app with `CDSUITE_HOST_UI=1` in a Jovian R kernel, the requests go on Jovian's host channel
  (`hera::host_notify()`, `hera::host_ask()`, methods `datasuite.<action>`): printing waits for DataSuite's answer
  without watching a file. Otherwise the `DATASUITE_HOST_REQUEST` line on stderr as before (`docs/HOST-REQUESTS.md`).
* Inside DataSuite, `export_report(format = "pdf")` on a computer without Word or LibreOffice has DataSuite print the
  report, as the Reports page's PDF does, instead of needing chromote.
* Charts: a ggplot is built once for its layout, its legend entries, the chart options, its panels and its drawing
  (it was built three to six times for one chart on the screen or in a report). The image download is the chart on
  the screen, not drawn again; a chart drawn with base graphics is recorded, so the download is that chart (it was
  whatever ggplot2 drew last). A chart's errors are logged with `message()` rather than printed, and a plotly chart
  given to `cd_render_plot()` says it cannot be drawn there.
* Reports: charts come to Quire as SVG when svglite is installed (sharp on screen, in print and in Word, which gets a
  PNG beside it; PowerPoint gets the PNG). A PNG chart is drawn with the cairo device when any of its text -- an axis,
  the legend, the title, labels on the chart -- is in Calibri, Cambria or another font ragg draws blank at some sizes,
  not only when the theme's base font is.
* The Get help button shows the docs in the session's host when it can (`rstudioapi::viewer()`: DataSuite's R kernel,
  RStudio), else in the browser.
* V8 and magick move to Suggests (Quire's writers need them, and DataSuite installs them with the app; writing a
  report without them says so). No longer suggests plotly. Suggested packages have minimum versions; suggests
  rstudioapi.

# datasuite.ui 0.4.1

* Inside DataSuite, DataSuite prints the Reports page's printable page (the PDF) and draws the pictures of its pages
  for Print Preview and the final pages, so the app needs neither chromote and a Chrome browser nor pdftools there
  (DESCRIPTION's `Config/datasuite/onDemand` keeps DataSuite from installing them). Outside DataSuite, or if it
  cannot, printing is as before.
* No longer suggests chromote or rsvg.

# datasuite.ui 0.4.0

* The Reports page's builder is Quire (the quire package). `reports_ui()` and `reports_server()` keep their names and
  arguments; the module is Quire's host: the kinds, drawing (charts as pictures with their legend entries and panels,
  flextables as cells, keeping merged cells, fills, bold, alignment and widths), fields, standard reports, themes
  (from Office files too), years, regions, the flag, the chart options, and the reports and pictures kept in the
  dataset. A page's Generate report opens the new report, the AI's changes reopen the open report, and the AI buttons
  (the narrative, Write this section) open DataSuite's chat.
* Reports written from R (`export_report()`, `export_deck()`, same arguments) are written by Quire's own Word and
  PowerPoint writers, so a file written from R is the one the builder downloads. The PDF is made by Word, PowerPoint
  or LibreOffice, else printed from Quire's printable page. The officer writers and the old ReportStudio builder are
  gone (officer moves to Suggests; V8 and magick are imported).
* Chart options, printing and Office themes come from quire: `cd_chart_options()` reads `quire::quire_chart_options()`,
  the builder's own file; printing and converting are quire's; `report_theme_from_file()` reads with
  `quire::quire_theme_from_file()`. Legend and panel-heading borders (`legend_border`, `strip_border` and their colours).
* `cd_adjustment_editor()` and `cd_update_adjustment_editor()`: the Data Adjustment page's editor -- the years removed
  everywhere and an area's data removed for some or every year; completeness (each indicator group's k, an
  indicator's own), outliers and missing values set everywhere or with a region's or district's own rules (a
  two-pane picker for regions and their districts); beside each setting what the check pages found; read-only
  after Adjust data, until Edit.
* Tables get their own loader: `cd_table_spinner()` and `cd_loading_skeleton(variant = "table")`.
* `cd_nav_item(hidden = TRUE)`: a page left out of the sidebar, reached from elsewhere (the Reports page, from the
  header's button).
* Requires quire 0.2.16.

# datasuite.ui 0.3.4

* The AI can read a saved report and change parts of it, without replacing it (what the apps' AI bridge actions
  `listReports`, `readReport` and `updateBlocks` use):
  - `report_list()`: the saved reports (id, name, report or deck, language, blocks, last edited).
  - `report_read()`: a report's blocks in order, compactly -- each text's words, and for each chart and table its kind,
    settings, options and a short table of what it shows (its title, subtitle, caption and the values it plots, or
    the table's rows; capped), drawn as the report draws it -- and the report's language, so the AI writes from the
    report's own numbers, in its language.
  - `report_update_blocks()`: targeted changes by block id, applied in order and checked as a whole -- a text's words,
    type or heading level; a chart's or table's kind (the settings the new kind does not take are left out), its
    settings (checked against the kind; the allowed values in the error), its chart options (checked with
    `cd_chart_options()`; the options in the error), its size, title and caption; a picture's caption and size; a new
    page before a block; and new blocks inserted after one, blocks removed or moved. The design, the cover and every
    other block stay as they are. It returns one sentence per change, for the user to confirm.
* The Reports page: an **AI** group on the document builder's Home tab -- **Write the narrative** (an introduction, a
  paragraph after each chart and table, a conclusion), **Write with AI** (the paragraph the caret is in, from the chart
  or table before it) and **Change with AI** (the block the caret is in) -- and **Change with AI** on a selected chart
  or table. Each opens DataSuite's chat with a prompt naming the report and the block, for the user to finish or send
  (English, French and Portuguese). Outside DataSuite they are disabled, with a hint.
* The Reports page follows the dataset's reports: a report the AI saved or changed shows in the list at once, and a
  change to the report that is open is opened again in the builder (it saves every edit, so nothing is lost).
* A report saved without its own id (reports the AI saved before cd2030.core 1.3.4) takes the key it is stored under,
  so the builder's edits to it are saved again (they were not).
* Every package in Imports has a minimum version.

# datasuite.ui 0.3.3

* Portuguese: the interface reads as Portuguese is written in Mozambique and Angola (European norm) instead of Brazilian Portuguese: "ficheiro", "Transferir", "A carregar", "Repor predefinição" and so on.

# datasuite.ui 0.3.2

* Reference documents for a dataset: the Reports page has a "Reference documents" card to add, list and remove the
  files the AI reads as context (reports, strategies, survey reports, notes), kept in the dataset's analysis folder
  `documents/` (`ds_documents_dir()`, `ds_documents_list()`, `ds_documents_add()`, `ds_documents_remove()`,
  `documents_card_ui()` / `documents_card_server()`). PDF, Word, PowerPoint, Excel, CSV and text files; a name
  already there becomes "name (2)". Translations in English, French and Portuguese.

# datasuite.ui 0.3.1

* The "soft" picture style on a circle or rounded picture draws its smaller shape directly instead of shrinking the
  shape's mask: with the ImageMagick of Ubuntu 26.04 (R-devel on Linux at r-universe) the shrunk mask made the whole
  picture nearly transparent.

# datasuite.ui 0.3.0

* The "Ask AI" buttons work: the header's and each chart card's open DataSuite's chat with a prompt about the page or
  that card, ready to edit (the card becomes `askedAbout` in the bridge state, so "this" means it). Outside DataSuite
  they are disabled, with a hint.
* `ai_action()` levels: `read`, `view` (changes only what is shown), `add` (adds something removable) and `replace`
  (overwrites or deletes), with optional `classify(args, session)` for a level that depends on the call and
  `summary(args, session)` for a readable description. The built-in `describeAction` returns both, so DataSuite
  asks the user only before a replacement, in the chat. `kind` stays `read`/`change` for older DataSuite builds.

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
