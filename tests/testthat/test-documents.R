# a fresh, empty temporary folder
new_dir <- function() {
  d <- tempfile("docs")
  dir.create(d)
  d
}

test_that("the documents folder is the analysis folder's documents/, and none outside DataSuite", {
  expect_null(ds_documents_dir(""))
  expect_equal(ds_documents_dir("C:/x/Benin_rmncah.shiny-workspace"), file.path("C:/x/Benin_rmncah.shiny-workspace", "documents"))
  old <- Sys.getenv("CDSUITE_SHINY_WORKSPACE_DIR", unset = NA)
  Sys.unsetenv("CDSUITE_SHINY_WORKSPACE_DIR")
  on.exit(if (!is.na(old)) Sys.setenv(CDSUITE_SHINY_WORKSPACE_DIR = old), add = TRUE)
  expect_null(ds_documents_dir())
})

test_that("documents are added, listed newest first, renamed when taken, and removed", {
  dir <- file.path(new_dir(), "documents")
  expect_equal(nrow(ds_documents_list(dir)), 0)

  src <- new_dir()
  a <- file.path(src, "report.pdf"); writeLines("a", a)
  b <- file.path(src, "notes.txt"); writeLines("b", b)
  added <- ds_documents_add(c(a, b), dir = dir)
  expect_true(all(file.exists(added)))

  # an upload's temp file has another name: the given name is kept
  tmp <- file.path(src, "0.pdf"); writeLines("c", tmp)
  again <- ds_documents_add(tmp, "report.pdf", dir = dir)
  expect_equal(basename(again), "report (2).pdf")

  listed <- ds_documents_list(dir)
  expect_setequal(listed$name, c("report.pdf", "notes.txt", "report (2).pdf"))
  expect_true(all(listed$size > 0))

  ds_documents_remove("report.pdf", dir = dir)
  expect_false("report.pdf" %in% ds_documents_list(dir)$name)
  # only a file in the folder: a path elsewhere is taken as a name there
  expect_error(ds_documents_remove(file.path(src, "notes.txt"), dir = file.path(new_dir(), "none")), "no document")
  expect_error(ds_documents_remove("missing.pdf", dir = dir), "no document")
})

test_that("unsupported types and a missing analysis folder are refused", {
  dir <- file.path(new_dir(), "documents")
  src <- new_dir()
  exe <- file.path(src, "tool.exe"); writeLines("x", exe)
  expect_error(ds_documents_add(exe, dir = dir), "Not a document")
  expect_error(ds_documents_add(exe, dir = NULL), "no analysis folder")
})

test_that("the card lists the folder's documents", {
  dir <- file.path(new_dir(), "documents")
  dir.create(dir)
  writeLines("x", file.path(dir, "plan.docx"))
  i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("translation", "ui.json", package = "datasuite.ui"))
  i18n$set_translation_language("en")
  shiny::testServer(documents_card_server, args = list(i18n = i18n, dir = function() dir), {
    html <- as.character(output$list$html)
    expect_match(html, "plan.docx", fixed = TRUE)
  })
})
