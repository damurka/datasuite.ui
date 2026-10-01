# Printing inside DataSuite. DataSuite starts an app with CDSUITE_PRINT=1 and then, when asked, prints the printable
# page itself (Chromium's printing, as chromote does) and draws a PDF's pages as pictures (pdf.js, as pdftools does):
# a "print" request on the R session's output, answered by a `done` file (DataSuite's shinyAppPrint.contribution.ts).
# So inside DataSuite the app needs neither chromote nor a Chrome, nor pdftools -- DESCRIPTION's
# Config/datasuite/onDemand keeps DataSuite from installing them. Anywhere else, or when DataSuite cannot, Quire's own
# way: chromote and pdftools when they are installed, else the reader's browser prints.

# Whether DataSuite prints for this app.
.ds_can_print <- function() identical(Sys.getenv("CDSUITE_PRINT"), "1")

# Asks DataSuite to print and waits for it. `html` (text, or a file): the printable page to make into `pdf`; without
# it, `pdf` is an existing PDF. `pages`: a folder to draw the PDF's pages into (page_001.png, ...), or NULL. An error
# when DataSuite says it failed or does not answer within `timeout` seconds.
.ds_print <- function(pdf, html = NULL, pages = NULL, dpi = 80, timeout = 120) {
  dir <- tempfile("ds_print_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  done <- file.path(dir, "done.json")
  request <- list(action = "print", pdf = path(pdf), dpi = dpi, done = path(done))
  if (!is.null(html)) {
    is_file <- length(html) == 1 && nchar(html) < 2000 && !grepl("<", html, fixed = TRUE) && file.exists(html)
    page <- html
    if (!is_file) {
      page <- file.path(dir, "page.html")
      writeBin(charToRaw(enc2utf8(paste(html, collapse = "\n"))), page)
    }
    request$html <- path(page)
  }
  if (!is.null(pages)) request$pages <- path(pages)
  message("DATASUITE_HOST_REQUEST ", gsub("[\r\n]+", " ", jsonlite::toJSON(request, auto_unbox = TRUE)))

  deadline <- Sys.time() + timeout
  status <- NULL
  while (is.null(status) && Sys.time() < deadline) {
    Sys.sleep(0.1)
    # read once it is complete (it may be seen while still being written)
    if (file.exists(done)) status <- tryCatch(jsonlite::fromJSON(done), error = function(e) NULL)
  }
  if (is.null(status)) stop("DataSuite did not print the page in time.", call. = FALSE)
  if (!isTRUE(status$ok)) stop(status$error %||% "DataSuite could not print the page.", call. = FALSE)
  invisible(status)
}

.ds_png_data_urls <- function(dir) {
  files <- sort(list.files(dir, pattern = "[.]png$", full.names = TRUE))
  lapply(files, function(f) paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))))
}

# Quire host methods (quire::quire_host()'s `pdf` and `pages`): DataSuite's printing inside DataSuite, else NULL, so
# Quire uses its own.
.ds_host_pdf <- function() {
  if (!.ds_can_print()) return(NULL)
  function(html, name) {
    pdf <- tempfile(fileext = ".pdf")
    on.exit(unlink(pdf), add = TRUE)
    .ds_print(pdf, html = html)
    list(type = "application/pdf", data = jsonlite::base64_enc(readBin(pdf, "raw", file.info(pdf)$size)))
  }
}

.ds_host_pages <- function() {
  if (!.ds_can_print()) return(NULL)
  function(html, name) {
    dir <- tempfile("ds_pages_")
    dir.create(file.path(dir, "pages"), recursive = TRUE)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    .ds_print(file.path(dir, "page.pdf"), html = html, pages = file.path(dir, "pages"))
    list(pages = .ds_png_data_urls(file.path(dir, "pages")), converter = "browser")
  }
}
