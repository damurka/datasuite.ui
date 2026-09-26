# The AI bridge's R side (docs/AI-BRIDGE.md, protocol 2). The page (js/src/aibridge.ts) sends the AI's requests as
# input$datasuite_ai_request = list(id, action, args); the bridge server started by app_frame() runs the action and
# answers with the custom message "datasuite_ai_reply" = list(id, ok, result | error). It also publishes what R knows
# of the state -- the page, the components on it (charts and tables), what the app adds -- as "datasuite_ai_state"
# whenever it changes. What only the browser knows (cards, tabs, what is in view, what is drawn, titles) the page adds
# itself when getState() is called.
#
# Built in: getState, listPages, listComponents, getComponentData, focusComponent (read), selectTab and navigate
# (change). getState, listComponents, focusComponent and selectTab are answered by the page itself (they are about the
# page), the rest here. An app adds its own actions with app_frame(ai_actions = list(ai_action(...))) and adds to the
# state with app_frame(ai_state = function(session)). Components: every cd_plot_server() registers itself, and any
# other output can with ai_register_component(), so every chart card is reachable without the app doing anything.

#' An action the AI can ask an app to do
#'
#' Apps built with [app_frame()] answer requests from DataSuite's chat (see `docs/AI-BRIDGE.md`). The frame has the
#' built-in actions (the page, its charts and tables, navigation); an app adds its own with `ai_action()`, passed in
#' `app_frame(ai_actions = )`.
#'
#' @param name The action's name, e.g. `"setFilters"`.
#' @param fn `function(args, session)`: `args` is the named list the AI sent. Returns the result (anything JSON can
#'   hold: lists, vectors, data frames), or [ai_reply_when()] to answer once the browser has caught up. To refuse,
#'   stop with a message for the user.
#' @param kind `"read"` (looks, changes nothing the user sees) or `"change"` (changes what the user sees or adds to
#'   the dataset; DataSuite asks the user first, or doesn't allow it, depending on their settings).
#' @param description One sentence telling the AI what the action does.
#' @param args A named list describing each argument, `name = "what it is"`.
#' @return An `ai_action` object.
#' @examples
#' ai_action("highlight", function(args, session) list(done = TRUE), kind = "change",
#'           description = "Highlight a region on the map", args = list(region = "a region name"))
#' @export
ai_action <- function(name, fn, kind = c("read", "change"), description = "", args = list()) {
  stopifnot(is.character(name), length(name) == 1, nzchar(name), is.function(fn))
  kind <- match.arg(kind)
  structure(list(name = name, fn = fn, kind = kind, description = description, args = args), class = "ai_action")
}

#' Answer an AI request once the browser has caught up
#'
#' A change the app asks the browser to make (open a page, set a filter) only shows in `input` after a round trip. An
#' [ai_action()] returns `ai_reply_when()` to answer when `ready()` is `TRUE`, or after `timeout` seconds, with
#' `result()`.
#'
#' @param ready A function returning `TRUE` once the change has arrived (it may read inputs).
#' @param result A function returning the result to send.
#' @param timeout Seconds to wait at most.
#' @return An object [ai_action()] functions return.
#' @export
ai_reply_when <- function(ready, result, timeout = 3) {
  stopifnot(is.function(ready), is.function(result))
  structure(list(ready = ready, result = result, timeout = timeout), class = "ai_reply_when")
}

#' Make an output reachable by the AI
#'
#' Registers a chart or table so DataSuite's chat can list it, read the data it shows and screenshot it (see
#' `docs/AI-BRIDGE.md`). [cd_plot_server()] does this for every chart; call it for other outputs, e.g. a table, from
#' their module server.
#'
#' @param session The module's session.
#' @param output_id The output's full id (`session$ns("table")`), so the page can find it.
#' @param type `"chart"` or `"table"`.
#' @param data A function (or reactive) returning the data frame the output shows. It is only called when the AI asks
#'   for the data, never to fill in the state.
#' @param about What the output is, for the app's AI: `NULL`, a list, or a function (or reactive) returning one --
#'   e.g. `list(kind = "coverage", options = list(indicator = "anc4"))`. Passed to DataSuite as it is.
#' @param id The component's id; by default the module path (`session$ns("")` without its trailing `-`).
#' @return `NULL`, invisibly.
#' @export
ai_register_component <- function(session, output_id, type = c("chart", "table"), data, about = NULL,
                                  id = sub("-$", "", session$ns(""))) {
  type <- match.arg(type)
  stopifnot(is.function(data))
  registry <- .ai_components(session)
  page <- sub("-.*$", "", id)
  assign(id, list(id = id, page = page, output_id = output_id, type = type, data = data, about = about), envir = registry)
  invisible(NULL)
}

# An empty JSON object ({}), for lists that must not become [].
.ai_object <- function(x = NULL) if (length(x)) x else structure(list(), names = character(0))

# Runs one request against the actions (a named list of ai_action), as the reply's body: list(ok, result) or
# list(ok = FALSE, error). Never throws.
.ai_dispatch <- function(request, actions, session = NULL) {
  name <- request$action
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) return(list(ok = FALSE, error = "No action was given."))
  action <- actions[[name]]
  if (is.null(action)) {
    return(list(ok = FALSE, error = sprintf("This app has no action \"%s\". Its actions: %s.", name,
                                            paste(names(actions), collapse = ", "))))
  }
  args <- request$args
  if (!is.list(args)) args <- list()
  tryCatch(
    list(ok = TRUE, result = action$fn(args, session)),
    error = function(e) {
      msg <- conditionMessage(e)
      if (inherits(e, "shiny.silent.error") || !nzchar(msg)) msg <- "That is not available yet: the page or its data is not ready."
      list(ok = FALSE, error = msg)
    }
  )
}

# A data frame as the AI reads it: its columns, up to max_rows rows as objects, and how many there are.
.ai_table <- function(data, max_rows = 500) {
  if (!is.data.frame(data)) {
    data <- tryCatch(as.data.frame(data), error = function(e) stop("This component's data is not a table.", call. = FALSE))
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  total <- nrow(data)
  max_rows <- suppressWarnings(as.integer(max_rows %||% 500))
  if (is.na(max_rows) || max_rows < 1) max_rows <- 500L
  shown <- data[seq_len(min(total, max_rows)), , drop = FALSE]
  shown[] <- lapply(shown, function(col) {
    if (is.factor(col) || inherits(col, c("Date", "POSIXt", "difftime"))) as.character(col) else if (is.list(col)) vapply(col, function(v) paste(format(v), collapse = ", "), "") else col
  })
  rows <- if (nrow(shown)) lapply(seq_len(nrow(shown)), function(i) lapply(as.list(shown[i, , drop = FALSE]), function(v) v[[1]])) else list()
  list(columns = names(data), rows = rows, totalRows = total, truncated = total > nrow(shown))
}

# Picks one language's text from a label the nav holds in every language (list(en =, fr =, pt =)).
.ai_text <- function(label, lang) {
  if (is.null(label)) return("")
  if (is.list(label)) {
    value <- label[[lang]] %||% label[["en"]] %||% label[[1]]
    return(as.character(value %||% ""))
  }
  as.character(label)[1]
}

# Every page the sidebar can open: list(id, title, section, requiresAdjustment), in sidebar order.
.ai_nav_pages <- function(sections, lang) {
  pages <- list()
  walk <- function(items, section, needs_adjustment) {
    for (item in items) {
      adjust <- isTRUE(needs_adjustment) || isTRUE(item$requiresAdjustment)
      if (!is.null(item$tabName)) {
        pages[[length(pages) + 1]] <<- list(id = item$tabName, title = .ai_text(item$label, lang), section = section, requiresAdjustment = adjust)
      }
      if (length(item$children)) walk(item$children, section, adjust)
    }
  }
  for (s in sections) walk(s$items, .ai_text(s$label, lang), FALSE)
  pages
}

# The page `id` as the state shows it.
.ai_page <- function(id, pages) {
  if (is.null(id) || !nzchar(id)) return(NULL)
  hit <- Filter(function(p) identical(p$id, id), pages)
  if (length(hit)) list(id = id, title = hit[[1]]$title, section = hit[[1]]$section) else list(id = id, title = id, section = "")
}

# What R knows of the state (protocol 2): the kit's own fields, then whatever the app adds (it can't replace them).
# The page adds viewport, cards and the browser-side fields of each component when getState() runs.
.ai_build_state <- function(app, page, components, extra, actions, updated = Sys.time()) {
  own <- c("protocol", "app", "page", "viewport", "cards", "components", "actions", "updatedAt")
  state <- list(protocol = 2L, app = app, page = page, components = components)
  for (name in setdiff(names(extra), own)) state[[name]] <- extra[[name]]
  state$actions <- lapply(unname(actions), function(a) {
    list(name = a$name, kind = a$kind, description = a$description, args = .ai_object(a$args))
  })
  state$updatedAt <- format(updated, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  state
}

# Component registry, one per session (session$userData): id -> list(id, page, output_id, type, data(), about).
.ai_components <- function(session) {
  root <- session$userData
  if (is.null(root$datasuite_ai_components)) root$datasuite_ai_components <- new.env(parent = emptyenv())
  root$datasuite_ai_components
}

# A component's `about`: a list, or a function / reactive returning one (read without taking a dependency).
.ai_about <- function(about) {
  if (is.null(about)) return(NULL)
  value <- if (is.function(about)) tryCatch(shiny::isolate(about()), error = function(e) NULL) else about
  if (is.list(value) && length(value)) value else NULL
}

# The components on `page`, as R knows them (nothing here computes their data).
.ai_page_components <- function(session, page) {
  registry <- .ai_components(session)
  entries <- Filter(function(x) identical(x$page, page), mget(sort(ls(registry)), envir = registry))
  lapply(unname(entries), function(x) {
    out <- list(id = x$id, type = x$type, outputId = x$output_id)
    about <- .ai_about(x$about)
    if (!is.null(about)) out$about <- about
    out
  })
}

# Started by app_frame() once every page server (and so every chart) is up.
.ai_bridge_server <- function(input, session, app, nav_sections, language, data_ready, analysis_ready, open_tabs,
                              ai_state = NULL, ai_actions = list()) {
  pages <- function() .ai_nav_pages(nav_sections, shiny::isolate(language()))
  current_tab <- function() shiny::isolate(input$tabs) %||% ""

  locked <- function(page) {
    if (page$id %in% open_tabs) return(FALSE)
    if (!isTRUE(shiny::isolate(data_ready()))) return(TRUE)
    isTRUE(page$requiresAdjustment) && !isTRUE(shiny::isolate(analysis_ready()))
  }

  extra_state <- function() {
    if (is.null(ai_state)) return(list())
    tryCatch(ai_state(session), error = function(e) list())
  }

  actions <- list()
  state_now <- function(tab = current_tab()) {
    .ai_build_state(app = app, page = .ai_page(tab, pages()), components = .ai_page_components(session, tab),
                    extra = shiny::isolate(extra_state()), actions = actions)
  }

  find_component <- function(args) {
    id <- args$componentId
    if (!is.character(id) || length(id) != 1) stop("Say which component: componentId, from listComponents.", call. = FALSE)
    registry <- .ai_components(session)
    if (!exists(id, envir = registry, inherits = FALSE)) stop(sprintf("There is no component \"%s\"; listComponents gives the charts and tables on this page.", id), call. = FALSE)
    get(id, envir = registry, inherits = FALSE)
  }

  # The page answers these itself (they are about what the browser shows); here they describe themselves and are the
  # fallback when a request somehow reaches R.
  page_side <- function(what) function(args, session) stop(sprintf("%s is answered by the page; update datasuite.ui's page bundle.", what), call. = FALSE)

  builtin <- list(
    ai_action("getState", function(args, session) state_now(), "read",
              "Where the user is: the page, its cards, tabs and components, what is in view, the filters and dataset."),
    ai_action("listPages", function(args, session) {
      lapply(pages(), function(p) list(id = p$id, title = p$title, section = p$section, locked = locked(p)))
    }, "read", "Every page of the app, and whether it is locked (no dataset loaded yet, or not adjusted)."),
    ai_action("listComponents", function(args, session) .ai_page_components(session, current_tab()), "read",
              "The charts and tables on the current page, with their cards, tabs and whether they are in view."),
    ai_action("getComponentData", function(args, session) {
      x <- find_component(args)
      data <- tryCatch(shiny::isolate(x$data()), error = function(e) {
        stop(if (!identical(x$page, current_tab())) sprintf("The component's page (%s) is not open; navigate there first.", x$page)
             else "The component has no data yet.", call. = FALSE)
      })
      c(list(componentId = x$id), .ai_table(data, args$maxRows %||% 500))
    }, "read", "The table a chart or table shows (what its data download writes). Works for a tab that isn't showing too.",
    list(componentId = "a component id from listComponents", maxRows = "at most this many rows (default 500)")),
    ai_action("focusComponent", page_side("focusComponent"), "read",
              "Scrolls a component into view and gives a CSS selector for a screenshot of just its card. Its tab must be showing (selectTab).",
              list(componentId = "a component id from listComponents")),
    ai_action("selectTab", page_side("selectTab"), "change", "Shows one tab of a tabbed card.",
              list(cardId = "a card id from the state", key = "the tab's key")),
    ai_action("navigate", function(args, session) {
      target <- args$page
      if (!is.character(target) || length(target) != 1) stop("Say which page: page, an id from listPages.", call. = FALSE)
      page <- Filter(function(p) identical(p$id, target), pages())
      if (!length(page)) stop(sprintf("There is no page \"%s\"; listPages gives the pages.", target), call. = FALSE)
      if (locked(page[[1]])) stop(sprintf("The page \"%s\" is locked until a dataset is loaded (and, for analysis pages, adjusted).", page[[1]]$title), call. = FALSE)
      cd_navigate_to(session, target)
      ai_reply_when(ready = function() identical(input$tabs, target), result = function() state_now(target))
    }, "change", "Opens a page.", list(page = "a page id from listPages"))
  )
  actions <- c(builtin, ai_actions)
  names(actions) <- vapply(actions, function(a) a$name, character(1))

  reply <- function(id, body) session$sendCustomMessage("datasuite_ai_reply", c(list(id = id), body))

  observeEvent(input$datasuite_ai_request, {
    request <- input$datasuite_ai_request
    id <- request$id
    if (is.null(id)) return()
    body <- .ai_dispatch(request, actions, session)
    if (isTRUE(body$ok) && inherits(body$result, "ai_reply_when")) {
      wait <- body$result
      started <- Sys.time()
      waiter <- observe({
        shiny::invalidateLater(100)
        done <- tryCatch(isTRUE(wait$ready()), error = function(e) FALSE)
        if (done || difftime(Sys.time(), started, units = "secs") > wait$timeout) {
          waiter$destroy()
          reply(id, tryCatch(list(ok = TRUE, result = shiny::isolate(wait$result())),
                             error = function(e) list(ok = FALSE, error = conditionMessage(e))))
        }
      })
    } else {
      reply(id, body)
    }
  })

  # Published whenever the page, the language, the data or what the app adds changes (settled first: several of these
  # can change in one go).
  state <- shiny::debounce(reactive({
    input$tabs
    language()
    data_ready()
    extra_state()
    shiny::isolate(state_now())
  }), millis = 250)
  observe(session$sendCustomMessage("datasuite_ai_state", state()))

  invisible(NULL)
}
