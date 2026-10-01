# Requests to DataSuite (R/kit-host.R): Jovian's host channel when DataSuite answers it, else a line on stderr

# Environment variables set until the calling test ends
local_env <- function(..., .env = parent.frame()) {
  values <- c(...)
  old <- Sys.getenv(names(values), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(values))
  restore <- function() for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, as.list(old[n]))
  do.call(on.exit, list(as.call(list(restore)), add = TRUE), envir = .env)
}

line_of <- function(expr) {
  msg <- tryCatch(expr, message = function(m) conditionMessage(m))
  expect_match(msg, "^DATASUITE_HOST_REQUEST \\{")
  jsonlite::fromJSON(sub("^DATASUITE_HOST_REQUEST ", "", trimws(msg)), simplifyVector = FALSE)
}

test_that("without the host channel a request is one line on stderr, its action beside its arguments", {
  local_env(CDSUITE_HOST_UI = "")
  body <- line_of(ds_host_request("installPackages", list(packages = list("bayescoveragemodel"))))
  expect_equal(body$action, "installPackages")
  # one package is still an array
  expect_equal(body$packages, list("bayescoveragemodel"))
  expect_error(ds_host_request("openChat", list("x")), "named list")
})

test_that("waiting on the line protocol reads the answer from the done file it names, NULL without one", {
  local_env(CDSUITE_HOST_UI = "")
  local_mocked_bindings(.ds_host_line = function(action, args) {
    jsonlite::write_json(list(ok = TRUE, action = action, pdf = args$pdf), args$done, auto_unbox = TRUE)
  })
  answer <- ds_host_request("print", list(pdf = "C:/x.pdf"), wait = TRUE, timeout = 5)
  expect_equal(answer, list(ok = TRUE, action = "print", pdf = "C:/x.pdf"))

  local_mocked_bindings(.ds_host_line = function(action, args) NULL)
  expect_null(ds_host_request("print", list(pdf = "C:/x.pdf"), wait = TRUE, timeout = 0.3))
})

test_that("with the host channel a request is hera's datasuite.<action>: notified, or asked and answered", {
  sent <- list()
  local_mocked_bindings(
    .ds_host_channel = function() TRUE,
    .ds_hera = function(name) switch(name,
      host_notify = function(method, params) { sent[[length(sent) + 1]] <<- list(method, params); TRUE },
      host_ask = function(method, params, default = NULL) list(ok = TRUE, method = method, params = params))
  )
  expect_silent(expect_true(ds_host_request("openChat", list(query = "Explain: "))))
  expect_equal(sent, list(list("datasuite.openChat", list(query = "Explain: "))))
  answer <- ds_host_request("print", list(pdf = "a.pdf"), wait = TRUE)
  # no done file on the host channel: the answer comes back to host_ask()
  expect_equal(answer, list(ok = TRUE, method = "datasuite.print", params = list(pdf = "a.pdf")))
})

test_that("a notification hera could not send goes on the line instead", {
  local_mocked_bindings(.ds_host_channel = function() TRUE, .ds_hera = function(name) function(...) FALSE)
  expect_equal(line_of(ds_host_request("openChat", list(query = "q")))$query, "q")
})

test_that("the host channel needs CDSUITE_HOST_UI=1 and a Jovian kernel", {
  local_env(CDSUITE_HOST_UI = "")
  expect_false(.ds_host_channel())
  local_env(CDSUITE_HOST_UI = "1")
  # these tests do not run in a Jovian kernel
  expect_false(.ds_host_channel())
})

test_that("DataSuite's printing is a print request; its failure or silence is an error", {
  asked <- NULL
  local_mocked_bindings(ds_host_request = function(action, args, wait, timeout) {
    asked <<- list(action = action, args = args, wait = wait)
    list(ok = TRUE)
  })
  .ds_print(file.path(tempdir(), "r.pdf"), html = "<p>Hello</p>", pages = tempdir(), dpi = 60)
  expect_equal(asked$action, "print")
  expect_true(asked$wait)
  expect_setequal(names(asked$args), c("pdf", "dpi", "html", "pages"))
  expect_equal(asked$args$dpi, 60)

  local_mocked_bindings(ds_host_request = function(...) list(ok = FALSE, error = "No printer"))
  expect_error(.ds_print("r.pdf", html = "<p>x</p>"), "No printer")
  local_mocked_bindings(ds_host_request = function(...) NULL)
  expect_error(.ds_print("r.pdf", html = "<p>x</p>"), "in time")
})

test_that("inside DataSuite a PDF made without Word or LibreOffice is printed by DataSuite", {
  local_env(CDSUITE_PRINT = "1")
  printed <- NULL
  local_mocked_bindings(
    .rb_quire_write = function(context, project, file, format, i18n) writeLines("<html></html>", file),
    .ds_print = function(pdf, html = NULL, ...) { printed <<- html; writeLines("%PDF", pdf) }
  )
  f <- tempfile(fileext = ".pdf")
  out <- export_report(report_context(), list(name = "R", blocks = list()), f, "pdf", converter = "browser")
  expect_equal(attr(out, "converter"), "browser")
  expect_match(printed, "report[.]html$")
  expect_true(file.exists(f))
})
