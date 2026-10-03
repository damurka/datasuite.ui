# Requests to DataSuite: what an app asks of the application running it (open the chat, print a page, install
# packages). Two ways, the same requests (docs/HOST-REQUESTS.md):
#   * Jovian's host channel, when DataSuite says it answers it (it starts the app with CDSUITE_HOST_UI=1) and the app
#     runs in a Jovian R kernel (Elara): its host_notify() / host_ask(), method "datasuite.<action>". host_ask() waits
#     for DataSuite's answer while the app runs, with no file to watch.
#   * Otherwise, as before: one line on the R session's output, `DATASUITE_HOST_REQUEST <json>`, which DataSuite reads
#     (message() goes to stderr, which is not buffered); an answer comes back in a `done` file named in the request.
# The kernel's R code is not a dependency: it is there in Jovian's kernel, so it is looked up when it is used --
# `.elara.host_ask()` in "tools:jovian" (Jovian 0.2.6 and later, every name dot-named), else hera's namespace
# (`hera::host_ask()`, Jovian 0.2.3 to 0.2.5).

# Whether DataSuite answers on Jovian's host channel: it said so, and the app runs in a Jovian kernel that has it
.ds_host_channel <- function() {
  if (!identical(Sys.getenv("CDSUITE_HOST_UI"), "1")) return(FALSE)
  is_elara <- .ds_hera("is_elara")
  if (is.null(is_elara) || is.null(.ds_hera("host_notify")) || is.null(.ds_hera("host_ask"))) return(FALSE)
  isTRUE(tryCatch(is_elara(), error = function(e) FALSE))
}

# hera's namespace, named through a variable: it is not a dependency (R CMD check looks for packages named in
# requireNamespace())
.ds_hera_package <- "hera"

# One of the kernel's functions (`host_ask`, `host_notify`, `is_elara`), or NULL when the session has none
.ds_hera <- function(name) {
  if ("tools:jovian" %in% search()) {
    return(get0(paste0(".elara.", name), envir = as.environment("tools:jovian"), mode = "function", inherits = FALSE))
  }
  if (!requireNamespace(.ds_hera_package, quietly = TRUE) || !name %in% getNamespaceExports(.ds_hera_package)) return(NULL)
  getExportedValue(.ds_hera_package, name)
}

#' Ask DataSuite for something
#'
#' What an app asks of DataSuite, the application running it: open the chat, print a page, install packages. Inside a
#' Jovian R kernel whose host answers it (DataSuite starts the app with `CDSUITE_HOST_UI=1`), the request goes on
#' Jovian's host channel as the method `"datasuite.<action>"` (`hera::host_notify()`, or `hera::host_ask()` to wait for
#' the answer while the app keeps running); otherwise it is one line on the R session's output,
#' `DATASUITE_HOST_REQUEST <json>`, with the action in the JSON object beside `args`, and an answer is read from a
#' `done` file named in the request. The requests, their arguments and answers are described in the package's
#' `docs/HOST-REQUESTS.md`.
#'
#' It asks whenever it is called: a caller checks first that the app runs inside DataSuite (`CDSUITE_SHINY_ID` is
#' set, or `CDSUITE_PRINT` for printing).
#'
#' @param action The request: `"openChat"`, `"print"`, `"installPackages"`.
#' @param args Its arguments: a named list (a JSON object; give an array as a list, so that one element stays an
#'   array).
#' @param wait `TRUE` to wait for DataSuite's answer.
#' @param timeout For `wait = TRUE` on the line protocol: how long to wait for the answer, in seconds.
#' @return With `wait = FALSE`, `TRUE` invisibly once the request is sent. With `wait = TRUE`, DataSuite's answer (a
#'   list, as in the `done` file: `ok`, and `error` when it failed), or `NULL` when it gave none.
#' @examples
#' \dontrun{
#' ds_host_request("openChat", list(query = "Explain this chart: "))
#' }
#' @export
ds_host_request <- function(action, args = list(), wait = FALSE, timeout = 120) {
  stopifnot(is_scalar_character(action), is.list(args))
  if (length(args) && (is.null(names(args)) || !all(nzchar(names(args))))) .ds_abort("{.arg args} must be a named list.")
  if (.ds_host_channel()) {
    method <- paste0("datasuite.", action)
    if (wait) return(.ds_hera("host_ask")(method, args, default = NULL))
    # FALSE when it could not be sent: then the line, below
    if (isTRUE(.ds_hera("host_notify")(method, args))) return(invisible(TRUE))
  }

  if (!wait) {
    .ds_host_line(action, args)
    return(invisible(TRUE))
  }
  # the answer is written to `done`; read once it is complete (it may be seen while still being written)
  dir <- tempfile("ds_request_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  done <- file.path(dir, "done.json")
  .ds_host_line(action, c(args, list(done = normalizePath(done, winslash = "/", mustWork = FALSE))))
  deadline <- Sys.time() + timeout
  answer <- NULL
  while (is.null(answer) && Sys.time() < deadline) {
    Sys.sleep(0.1)
    if (file.exists(done)) answer <- tryCatch(jsonlite::fromJSON(done), error = function(e) NULL)
  }
  answer
}

# One request on the R session's output, on one line
.ds_host_line <- function(action, args) {
  line <- jsonlite::toJSON(c(list(action = action), args), auto_unbox = TRUE)
  message("DATASUITE_HOST_REQUEST ", gsub("[\r\n]+", " ", line))
}
