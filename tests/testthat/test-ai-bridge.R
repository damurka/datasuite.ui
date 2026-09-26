test_that("ai_action checks its arguments", {
  a <- ai_action("x", function(args, session) 1, kind = "change", description = "d", args = list(v = "a value"))
  expect_s3_class(a, "ai_action")
  expect_identical(a$kind, "change")
  expect_error(ai_action("x", "not a function"))
  expect_error(ai_action("x", function(args, session) 1, kind = "delete"))
})

test_that("dispatch runs the action, and never throws", {
  actions <- list(
    echo = ai_action("echo", function(args, session) args$value),
    boom = ai_action("boom", function(args, session) stop("It broke.")),
    later = ai_action("later", function(args, session) shiny::req(FALSE))
  )
  expect_identical(.ai_dispatch(list(action = "echo", args = list(value = 3)), actions), list(ok = TRUE, result = 3))
  expect_identical(.ai_dispatch(list(action = "echo"), actions), list(ok = TRUE, result = NULL))
  expect_identical(.ai_dispatch(list(action = "boom"), actions), list(ok = FALSE, error = "It broke."))
  missing <- .ai_dispatch(list(action = "nope"), actions)
  expect_false(missing$ok)
  expect_match(missing$error, "no action \"nope\".*echo, boom, later")
  expect_match(.ai_dispatch(list(action = "later"), actions)$error, "not available yet")
  expect_false(.ai_dispatch(list(), actions)$ok)
})

test_that("a chart's table is rows of objects, cut at maxRows", {
  df <- data.frame(year = 2020:2024, value = c(1.5, NA, 3, 4, 5), region = factor(c("a", "b", "a", "b", "a")))
  t <- .ai_table(df, max_rows = 2)
  expect_identical(t$columns, c("year", "value", "region"))
  expect_identical(t$totalRows, 5L)
  expect_true(t$truncated)
  expect_length(t$rows, 2)
  expect_identical(t$rows[[1]], list(year = 2020L, value = 1.5, region = "a"))
  expect_true(is.na(t$rows[[2]]$value))
  all <- .ai_table(df)
  expect_false(all$truncated)
  expect_length(all$rows, 5)
  empty <- .ai_table(df[0, ])
  expect_identical(empty$rows, list())
  expect_identical(empty$totalRows, 0L)
  # shiny's toJSON keeps each row an object
  json <- shiny:::toJSON(t$rows)
  expect_match(as.character(json), '\\{"year":2020,"value":1.5,"region":"a"\\}', fixed = FALSE)
})

test_that("pages come from the nav tree, in the language asked for", {
  label <- function(en, fr) list(en = en, fr = fr)
  sections <- list(
    list(label = label("Data", "Donnees"), items = list(
      list(tabName = "upload_data", label = label("Load data", "Charger")),
      list(tabName = NULL, label = label("Quality", "Qualite"), children = list(
        list(tabName = "reporting_rate", label = label("Reporting rate", "Taux"))
      ))
    )),
    list(label = label("Analysis", "Analyse"), items = list(
      list(tabName = "coverage", label = label("Coverage", "Couverture"), requiresAdjustment = TRUE)
    ))
  )
  pages <- .ai_nav_pages(sections, "fr")
  expect_identical(vapply(pages, `[[`, "", "id"), c("upload_data", "reporting_rate", "coverage"))
  expect_identical(pages[[2]]$title, "Taux")
  expect_identical(pages[[2]]$section, "Donnees")
  expect_true(pages[[3]]$requiresAdjustment)
  expect_identical(.ai_page("coverage", pages), list(id = "coverage", title = "Couverture", section = "Analyse"))
  expect_identical(.ai_page("welcome", pages)$title, "welcome")
  expect_null(.ai_page(NULL, pages))
})

test_that("the state has the kit's fields, the app's additions and the actions (protocol 2)", {
  actions <- list(ai_action("navigate", function(args, session) NULL, "change", "Opens a page", list(page = "a page id")),
                  ai_action("getState", function(args, session) NULL, "read", "The state"))
  state <- .ai_build_state(
    app = list(name = "RMNCAH", version = "2.0.1"),
    page = list(id = "coverage", title = "Coverage", section = "Analysis"),
    components = list(list(id = "coverage-plot", type = "chart", outputId = "coverage-plot-plot",
                           about = list(kind = "coverage", options = list(indicator = "anc4")))),
    extra = list(filters = list(years = c(2020, 2023)), page = "ignored", cards = "ignored"),
    actions = actions,
    updated = as.POSIXct("2026-09-26 10:00:00", tz = "UTC")
  )
  expect_identical(state$protocol, 2L)
  expect_identical(state$page$id, "coverage")
  expect_null(state$cards)
  expect_identical(state$components[[1]]$about$kind, "coverage")
  expect_identical(state$filters$years, c(2020, 2023))
  expect_identical(state$updatedAt, "2026-09-26T10:00:00Z")
  expect_identical(vapply(state$actions, `[[`, "", "kind"), c("change", "read"))
  json <- as.character(shiny:::toJSON(state))
  expect_match(json, '"args":\\{"page":"a page id"\\}')
  expect_match(json, '"args":\\{\\}')
})

test_that("components register without computing anything, and give their about", {
  session <- shiny::MockShinySession$new()
  computed <- FALSE
  data <- function() { computed <<- TRUE; data.frame(x = 1) }
  ai_register_component(session, "coverage-a-plot", "chart", data, about = function() list(kind = "coverage"), id = "coverage-a")
  ai_register_component(session, "coverage-b-table", "table", data, id = "coverage-b")
  ai_register_component(session, "other-c-plot", "chart", data, id = "other-c")
  comps <- .ai_page_components(session, "coverage")
  expect_identical(vapply(comps, `[[`, "", "id"), c("coverage-a", "coverage-b"))
  expect_identical(comps[[1]]$about, list(kind = "coverage"))
  expect_null(comps[[2]]$about)
  expect_identical(comps[[2]]$type, "table")
  expect_false(computed)
  expect_error(ai_register_component(session, "x", "picture", data))
})
