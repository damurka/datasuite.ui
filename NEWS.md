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
