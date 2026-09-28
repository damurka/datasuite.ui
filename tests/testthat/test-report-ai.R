ai_kinds <- list(
  coverage = report_kind("chart", "national", "Coverage trend", c("anc4", "penta3"), c("national", "adminlevel_1")),
  map = report_kind("chart", "subnational", "Coverage map", c("anc4", "penta3"), variants = c(Blues = "Blue", Greens = "Green"),
                    year = TRUE),
  coverage_table = report_kind("table", "national", "Coverage table")
)

ai_project <- function() {
  list(id = "r1", name = "Benin National Coverage", lang = "fr", updated = "2026-09-01 10:00",
       design = list(theme = "countdown"), cover = list(title = "Cover"),
       blocks = list(
         list(id = "h1", type = "heading", level = 1, text = "Introduction"),
         list(id = "p1", type = "paragraph", text = "Old <b>text</b> &amp; more"),
         list(id = "c1", type = "chart", kind = "coverage", indicator = "anc4", admin_level = "national", size = "full",
              options = list(show_labels = FALSE), sig = "sig-c1"),
         list(id = "p2", type = "paragraph", text = ""),
         list(id = "t1", type = "table", kind = "coverage_table", size = "full"),
         list(id = "img", type = "image", src = "asset:x", caption = "A picture")
       ))
}

ai_context <- function() {
  report_context(draw = function(block, i18n) {
    if (identical(block$type, "table")) {
      return(flextable::flextable(data.frame(region = paste("Region", 1:40), coverage = seq(50, 89, length.out = 40))))
    }
    if (identical(block$kind, "broken")) stop("No data for this chart.")
    data <- data.frame(year = rep(2019:2023, 2), value = c(60, 62.346, 64, 66, 70, 58, 59, 61, 60, 63),
                       source = rep(c("DHIS2", "Survey"), each = 5))
    ggplot2::ggplot(data, ggplot2::aes(year, value, colour = source)) + ggplot2::geom_line() +
      ggplot2::labs(title = "ANC4 coverage", caption = "Denominator: penta1", x = "Year", y = "Coverage (%)", colour = "Source")
  })
}

# ---- listing ---------------------------------------------------------------------------------------------------------

test_that("report_list lists the reports, the last edited first, with their kind and number of blocks", {
  deck <- list(name = "Slides", kind = "deck", updated = "2026-09-02 08:00",
               slides = list(list(items = list(list(id = "i1", block = list(type = "paragraph", text = "x")),
                                               list(id = "i2", block = list(type = "chart", kind = "coverage"))))))
  out <- report_list(list(r1 = ai_project(), d1 = deck))
  expect_identical(vapply(out, `[[`, "", "id"), c("d1", "r1"))
  expect_identical(out[[1]]$kind, "deck")
  expect_identical(out[[1]]$blocks, 2L)
  expect_identical(out[[2]][c("name", "kind", "lang", "blocks")], list(name = "Benin National Coverage", kind = "report", lang = "fr", blocks = 6L))
  expect_identical(report_list(list()), list())
  expect_identical(report_list(NULL), list())
})

# ---- reading ---------------------------------------------------------------------------------------------------------

test_that("report_read gives every block compactly: plain text, and each chart's and table's data", {
  r <- report_read(ai_context(), ai_project(), max_rows = 5)
  expect_identical(r$id, "r1")
  expect_identical(r$lang, "fr")
  expect_identical(vapply(r$blocks, `[[`, "", "id"), c("h1", "p1", "c1", "p2", "t1", "img"))
  expect_identical(r$blocks[[1]][c("type", "text", "level")], list(type = "heading", text = "Introduction", level = 1L))
  expect_identical(r$blocks[[2]]$text, "Old text & more")
  expect_identical(r$blocks[[4]]$text, "")
  chart <- r$blocks[[3]]
  expect_identical(chart$kind, "coverage")
  expect_identical(chart$settings, list(indicator = "anc4", admin_level = "national"))
  expect_identical(chart$options, list(show_labels = FALSE))
  expect_identical(chart$data$title, "ANC4 coverage")
  expect_identical(chart$data$caption, "Denominator: penta1")
  expect_identical(chart$data$data$columns, c("Year", "Coverage (%)", "Source"))
  # capped, the rows in the order of the first column, numbers rounded
  expect_length(chart$data$data$rows, 5)
  expect_identical(chart$data$data$totalRows, 10L)
  expect_true(chart$data$data$truncated)
  expect_identical(chart$data$data$rows[[1]], list(2019L, 60, "DHIS2"))
  expect_identical(chart$data$data$rows[[3]], list(2020L, 62.35, "DHIS2"))
  table <- r$blocks[[5]]
  expect_identical(table$data$data$columns, c("region", "coverage"))
  expect_length(table$data$data$rows, 5)
  expect_identical(table$data$data$totalRows, 40L)
  expect_null(r$blocks[[6]]$data)
  expect_identical(r$blocks[[6]]$format$caption, "A picture")
})

test_that("report_read says why a chart can't be drawn, caps long text, and can skip the data", {
  p <- ai_project()
  p$blocks[[3]]$kind <- "broken"
  p$blocks[[2]]$text <- strrep("word ", 100)
  p$lang <- NULL
  r <- report_read(ai_context(), p, max_chars = 20, lang = "pt")
  expect_identical(r$lang, "pt")
  expect_match(r$blocks[[3]]$data$error, "No data for this chart")
  expect_identical(nchar(r$blocks[[2]]$text), 21L)
  expect_null(report_read(ai_context(), ai_project(), data = FALSE)$blocks[[3]]$data)
  expect_error(report_read(ai_context(), NULL), "no such report")
})

test_that("report_read reads a slide deck's items, each with its slide", {
  deck <- list(id = "d1", name = "Slides", kind = "deck",
               slides = list(list(items = list(list(id = "i1", block = list(type = "heading", text = "Title")))),
                             list(items = list(list(id = "i2", block = list(type = "paragraph", text = "Body"))))))
  r <- report_read(ai_context(), deck, data = FALSE)
  expect_identical(r$kind, "deck")
  expect_identical(vapply(r$blocks, `[[`, "", "id"), c("i1", "i2"))
  expect_identical(vapply(r$blocks, `[[`, 1L, "slide"), 1:2)
})

# ---- changing: text --------------------------------------------------------------------------------------------------

test_that("updating text changes only that block's text; the other blocks, design and cover stay as they are", {
  p <- ai_project()
  r <- report_update_blocks(p, list(list(blockId = "p2", text = "Coverage rose to **70%** in 2023 & 2024.\nMore.")), kinds = ai_kinds)
  expect_identical(r$project$blocks[[4]]$text, "Coverage rose to <b>70%</b> in 2023 &amp; 2024.<br>More.")
  expect_identical(r$project$blocks[-4], p$blocks[-4])
  expect_identical(r$project[c("id", "name", "lang", "design", "cover", "updated")], p[c("id", "name", "lang", "design", "cover", "updated")])
  expect_identical(r$changes, "Paragraph (empty): text written")
})

test_that("text blocks can change type and level", {
  r <- report_update_blocks(ai_project(), list(list(blockId = "p1", type = "heading", level = 3),
                                               list(blockId = "h1", type = "note")), kinds = ai_kinds)
  expect_identical(r$project$blocks[[2]][c("type", "level")], list(type = "heading", level = 3L))
  expect_identical(r$project$blocks[[1]]$type, "note")
  expect_null(r$project$blocks[[1]]$level)
  expect_error(report_update_blocks(ai_project(), list(list(blockId = "p1", level = 2)), kinds = ai_kinds), "only a heading has a level")
  expect_error(report_update_blocks(ai_project(), list(list(blockId = "p1", type = "chart")), kinds = ai_kinds), "heading, paragraph or note")
})

test_that("inserted blocks go after the block named, in the order given", {
  r <- report_update_blocks(ai_project(), list(
    list(afterBlockId = "c1", insert = list(type = "paragraph", text = "First")),
    list(afterBlockId = "c1", insert = list(type = "paragraph", text = "Second")),
    list(afterBlockId = "@start", insert = list(type = "heading", text = "Summary"))
  ), kinds = ai_kinds)
  types <- vapply(r$project$blocks, `[[`, "", "type")
  texts <- vapply(r$project$blocks, function(b) b$text %||% "", "")
  expect_identical(texts[1:6], c("Summary", "Introduction", "Old <b>text</b> &amp; more", "", "First", "Second"))
  expect_identical(types[[1]], "heading")
  expect_identical(r$project$blocks[[1]]$level, 2L)
  ids <- vapply(r$project$blocks, `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0)
  expect_match(r$changes[[1]], "^Add a paragraph \"First\" after the chart \"Coverage trend\" \\(anc4\\)$")
})

test_that("unknown blocks and wrong changes are refused, and nothing is changed", {
  p <- ai_project()
  expect_error(report_update_blocks(p, list(list(blockId = "nope", text = "x")), kinds = ai_kinds), "there is no block nope")
  expect_error(report_update_blocks(p, list(list(afterBlockId = "nope", insert = list(type = "paragraph", text = "x"))), kinds = ai_kinds),
               "there is no block nope")
  expect_error(report_update_blocks(p, list(list(blockId = "c1", text = "x")), kinds = ai_kinds), "has no text to change")
  expect_error(report_update_blocks(p, list(list(afterBlockId = "c1", insert = list(type = "image"))), kinds = ai_kinds), "type must be one of")
  expect_error(report_update_blocks(p, list(), kinds = ai_kinds), "Give the changes")
  expect_error(report_update_blocks(p, lapply(p$blocks, function(b) list(blockId = b$id, delete = TRUE)), kinds = ai_kinds), "at least one block")
})

# ---- changing: charts and tables -------------------------------------------------------------------------------------

test_that("a chart's kind and settings change as the new kind allows; the others are left out", {
  r <- report_update_blocks(ai_project(), list(list(blockId = "c1", kind = "map", year = 2023, variant = "Greens")), kinds = ai_kinds)
  b <- r$project$blocks[[3]]
  expect_identical(b[c("id", "type", "kind", "indicator", "year", "variant")],
                   list(id = "c1", type = "chart", kind = "map", indicator = "anc4", year = 2023L, variant = "Greens"))
  expect_null(b$admin_level)
  expect_null(b$sig)
  expect_identical(b$options, list(show_labels = FALSE))
  expect_match(r$changes, "^Chart \"Coverage trend\" \\(anc4\\): Coverage trend -> Coverage map; admin_level national -> none; year none -> 2023; variant none -> Greens$")
  # a chart becomes a table of the same data: it has no chart options
  r <- report_update_blocks(ai_project(), list(list(blockId = "c1", kind = "coverage_table")), kinds = ai_kinds)
  expect_identical(r$project$blocks[[3]][c("type", "kind")], list(type = "table", kind = "coverage_table"))
  expect_null(r$project$blocks[[3]]$options)
  expect_null(r$project$blocks[[3]]$indicator)
})

test_that("settings a kind does not allow are refused with the allowed values", {
  p <- ai_project()
  expect_error(report_update_blocks(p, list(list(blockId = "c1", indicator = "bcg")), kinds = ai_kinds), "Allowed indicator: anc4, penta3")
  expect_error(report_update_blocks(p, list(list(blockId = "c1", year = 2020)), kinds = ai_kinds), "has no year. Its settings: indicator, admin_level, region")
  expect_error(report_update_blocks(p, list(list(blockId = "c1", kind = "pie")), kinds = ai_kinds), "no kind \"pie\". Kinds: coverage, map, coverage_table")
  expect_error(report_update_blocks(p, list(list(blockId = "c1", sort = "desc")), kinds = ai_kinds), "has no sort to change. What can be changed: kind")
})

test_that("chart options merge into the chart's own, checked, with the options named when one is unknown", {
  r <- report_update_blocks(ai_project(), list(list(blockId = "c1", options = list(show_labels = TRUE, y_limits = list(0, 100),
                                                                                  colors = list(DHIS2 = "#1f77b4")))), kinds = ai_kinds)
  expect_identical(r$project$blocks[[3]]$options, list(show_labels = TRUE, y_limits = c(0, 100), colors = list(DHIS2 = "#1f77b4")))
  r <- report_update_blocks(ai_project(), list(list(blockId = "c1", options = list(show_labels = NULL))), kinds = ai_kinds)
  expect_null(r$project$blocks[[3]]$options)
  expect_error(report_update_blocks(ai_project(), list(list(blockId = "c1", options = list(bogus = 1))), kinds = ai_kinds),
               "no chart option bogus. The chart options are: title, subtitle")
  expect_error(report_update_blocks(ai_project(), list(list(blockId = "c1", options = list(legend_position = "middle"))), kinds = ai_kinds),
               "legend_position.+must be one of")
  expect_error(report_update_blocks(ai_project(), list(list(blockId = "t1", options = list(show_labels = TRUE))), kinds = ai_kinds),
               "a table has no chart options")
})

test_that("layout: size, title, caption and a new page before a block", {
  r <- report_update_blocks(ai_project(), list(list(blockId = "c1", size = "half", title = "ANC4", caption = FALSE),
                                               list(blockId = "t1", pageBreakBefore = TRUE)), kinds = ai_kinds)
  expect_identical(r$project$blocks[[3]][c("size", "title", "caption")], list(size = "half", title = "ANC4", caption = FALSE))
  expect_identical(vapply(r$project$blocks, `[[`, "", "type")[5:6], c("pagebreak", "table"))
  expect_error(report_update_blocks(ai_project(), list(list(blockId = "c1", size = "huge")), kinds = ai_kinds), "size must be one of full, half, third")
  # and back
  r2 <- report_update_blocks(r$project, list(list(blockId = "t1", pageBreakBefore = FALSE)), kinds = ai_kinds)
  expect_false("pagebreak" %in% vapply(r2$project$blocks, `[[`, "", "type"))
})

test_that("blocks are moved and removed; a deck's items are only changed", {
  r <- report_update_blocks(ai_project(), list(list(blockId = "t1", moveAfter = "@start"), list(blockId = "img", delete = TRUE),
                                               list(blockId = "p2", moveAfter = "h1")), kinds = ai_kinds)
  expect_identical(vapply(r$project$blocks, `[[`, "", "id"), c("t1", "h1", "p2", "p1", "c1"))
  expect_identical(r$changes[[2]], "Remove the picture \"A picture\"")
  deck <- list(id = "d1", name = "Slides", kind = "deck", slides = list(list(items = list(list(id = "i1", x = 1, block = list(type = "paragraph", text = "x"))))))
  r <- report_update_blocks(deck, list(list(blockId = "i1", text = "New")), kinds = ai_kinds)
  expect_identical(r$project$slides[[1]]$items[[1]]$block$text, "New")
  expect_identical(r$project$slides[[1]]$items[[1]]$x, 1)
  expect_error(report_update_blocks(deck, list(list(blockId = "i1", delete = TRUE)), kinds = ai_kinds), "can be changed, but not added, removed or moved")
})

# ---- the builder's AI buttons ----------------------------------------------------------------------------------------

test_that("the AI buttons' prompts name the report and the block", {
  p <- ai_project()
  expect_identical(.rb_ai_ask_prompt(list(scope = "narrative"), p, NULL, "en"),
                   "Fill in the narrative of the report \"Benin National Coverage\" (id r1): write a short introduction, a paragraph after each chart and table, and a conclusion, from the data in the report.")
  expect_identical(.rb_ai_ask_prompt(list(scope = "write", block = "p2", after = "c1"), p, NULL, "en"),
                   "In the report \"Benin National Coverage\" (id r1), write the paragraph p2 after the chart \"coverage\" (anc4), from its data.")
  expect_identical(.rb_ai_ask_prompt(list(scope = "write", block = "p1"), p, NULL, "en"),
                   "In the report \"Benin National Coverage\" (id r1), write the paragraph p1: ")
  expect_identical(.rb_ai_ask_prompt(list(scope = "change", block = "p1"), p, NULL, "en"),
                   "Change the block \"Old text & more\" (p1) in the report \"Benin National Coverage\" (id r1): ")
})

# ---- the Reports page follows a change the AI made ------------------------------------------------------------------

test_that("the Reports page opens the open report again when it changes elsewhere, not when the builder saves it", {
  i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("translation", "ui.json", package = "datasuite.ui"))
  projects <- shiny::reactiveVal(list(r1 = ai_project()))
  ds <- new.env()
  makeActiveBinding("report_projects", function() projects(), ds)
  ds$set_report_project <- function(id, p) {
    all <- shiny::isolate(projects())
    all[[id]] <- p
    projects(all)
  }
  ds$data_years <- 2019:2023
  ds$language <- "en"
  ds$country <- "Benin"
  class(ds) <- "ai_fake_dataset"
  registerS3method("as_report_context", "ai_fake_dataset", function(x, ...) ai_context(), envir = asNamespace("datasuite.ui"))
  cache <- shiny::reactiveVal(ds)
  local_mocked_bindings(report_converter = function() NULL)
  shiny::testServer(reports_server, args = list(cache = cache, i18n = i18n, active = shiny::reactive(TRUE)), {
    session$setInputs(studio__action = list(type = "open", project = "r1", nonce = 1))
    expect_identical(state$open, "r1")
    # the builder saves an edit: nothing is sent back
    edited <- state$project
    edited$blocks[[4]]$text <- "Typed by the user"
    session$setInputs(studio = edited)
    expect_identical(state$project$blocks[[4]]$text, "Typed by the user")
    saved <- state$project
    # the AI changes the report: the page takes the saved report
    changed <- report_update_blocks(projects()$r1, list(list(blockId = "p1", text = "Written by the AI")), kinds = ai_kinds)$project
    ds$set_report_project("r1", changed)
    session$flushReact()
    expect_identical(state$project$blocks[[2]]$text, "Written by the AI")
    expect_identical(state$project$blocks[[4]]$text, "Typed by the user")
    expect_false(identical(state$project, saved))
  })
})
