# Reference documents for a dataset: files the user gives the AI as context -- national reports, strategies, survey
# reports, notes -- kept in the dataset's analysis folder `documents/` (DataSuite sets CDSUITE_SHINY_WORKSPACE_DIR to
# that folder). The AI reads them with its own tool (the Countdown AI's countdown_documents); the app only adds, lists
# and removes them. Documents give context: the dataset stays the source of its numbers.

# The file types the AI can read.
.ds_document_types <- c("pdf", "docx", "pptx", "xlsx", "xls", "xlsm", "csv", "tsv", "txt", "md")

# The dataset's documents folder, or NULL outside DataSuite (no analysis folder).
ds_documents_dir <- function(workspace = Sys.getenv("CDSUITE_SHINY_WORKSPACE_DIR", unset = "")) {
  if (!nzchar(workspace)) return(NULL)
  file.path(workspace, "documents")
}

# The documents in the folder, newest first: name, size (bytes), modified.
ds_documents_list <- function(dir = ds_documents_dir()) {
  empty <- data.frame(name = character(), size = numeric(), modified = as.POSIXct(character()), stringsAsFactors = FALSE)
  if (is.null(dir) || !dir.exists(dir)) return(empty)
  files <- list.files(dir, full.names = TRUE)
  files <- files[!dir.exists(files) & !startsWith(basename(files), "~$")]
  if (!length(files)) return(empty)
  info <- file.info(files)
  out <- data.frame(name = basename(files), size = info$size, modified = info$mtime, stringsAsFactors = FALSE)
  out <- out[order(out$modified, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Copies `paths` into the folder under `names` (a name already there becomes "name (2).ext"). Returns the new paths.
ds_documents_add <- function(paths, names = basename(paths), dir = ds_documents_dir()) {
  if (is.null(dir)) {
    .ds_abort(c("x" = "This dataset has no analysis folder: open it in DataSuite to add documents."))
  }
  if (!length(paths)) return(invisible(character()))
  ext <- tolower(tools::file_ext(names))
  bad <- names[!ext %in% .ds_document_types]
  if (length(bad)) {
    .ds_abort(c("x" = "Not a document the AI can read: {bad}.",
                "i" = "Use PDF, Word (.docx), PowerPoint (.pptx), Excel (.xlsx), CSV or text files."))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  targets <- character(length(paths))
  for (i in seq_along(paths)) {
    target <- .ds_unique_path(file.path(dir, basename(names[[i]])))
    if (!file.copy(paths[[i]], target, overwrite = FALSE)) {
      .ds_abort(c("x" = "Could not copy {names[[i]]} into {dir}."))
    }
    targets[[i]] <- target
  }
  invisible(targets)
}

# Removes one document from the folder (only a file directly in it: `name` is taken as a file name).
ds_documents_remove <- function(name, dir = ds_documents_dir()) {
  if (is.null(dir)) .ds_abort(c("x" = "This dataset has no analysis folder."))
  target <- file.path(dir, basename(name))
  if (!file.exists(target) || dir.exists(target)) .ds_abort(c("x" = "There is no document {basename(name)}."))
  unlink(target)
  invisible(TRUE)
}

# `path`, or "stem (2).ext", "stem (3).ext"... when it is taken.
.ds_unique_path <- function(path) {
  if (!file.exists(path)) return(path)
  ext <- tools::file_ext(path)
  stem <- if (nzchar(ext)) substr(path, 1, nchar(path) - nchar(ext) - 1) else path
  i <- 2
  repeat {
    candidate <- if (nzchar(ext)) sprintf("%s (%d).%s", stem, i, ext) else sprintf("%s (%d)", stem, i)
    if (!file.exists(candidate)) return(candidate)
    i <- i + 1
  }
}

# A file size for people.
.ds_file_size <- function(bytes) {
  ifelse(bytes >= 1024^2, sprintf("%.1f MB", bytes / 1024^2), sprintf("%.0f KB", pmax(1, bytes / 1024)))
}

# ---- the card: add, list and remove documents ----------------------------------------------------------------------

documents_card_ui <- function(id, i18n = cd_i18n()) {
  ns <- NS(id)
  cd_card(
    title = i18n$t("lbl_docs_title"),
    subtitle = i18n$t("lbl_docs_subtitle"),
    fileInput(ns("add"), label = NULL, multiple = TRUE, buttonLabel = i18n$t("btn_docs_add"),
              accept = paste0(".", .ds_document_types)),
    uiOutput(ns("list"))
  )
}

documents_card_server <- function(id, i18n = cd_i18n(), dir = ds_documents_dir) {
  moduleServer(id, function(input, output, session) {
    changed <- reactiveVal(0)
    # files added outside the app (Explorer, the chat) show up too
    listing <- reactivePoll(4000, session,
      checkFunc = function() {
        d <- dir()
        if (is.null(d) || !dir.exists(d)) return("")
        paste(list.files(d), collapse = "|")
      },
      valueFunc = function() ds_documents_list(dir()))

    observeEvent(input$add, {
      files <- input$add
      tryCatch({
        ds_documents_add(files$datapath, files$name, dir = dir())
        changed(changed() + 1)
        showNotification(sprintf(cd_plain_text(i18n, "msg_docs_added"), nrow(files)), type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
    })

    observeEvent(input$remove, {
      tryCatch({
        ds_documents_remove(input$remove, dir = dir())
        changed(changed() + 1)
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
    })

    output$list <- renderUI({
      changed()
      if (is.null(dir())) return(tags$p(class = "cd-muted", i18n$t("lbl_docs_unavailable")))
      docs <- listing()
      if (changed() > 0) docs <- ds_documents_list(dir())
      if (!nrow(docs)) return(tags$p(class = "cd-muted", i18n$t("lbl_docs_empty")))
      remove_id <- session$ns("remove")
      tags$table(
        class = "cd-table cd-docs-table",
        tags$tbody(lapply(seq_len(nrow(docs)), function(i) {
          tags$tr(
            tags$td(docs$name[[i]]),
            tags$td(.ds_file_size(docs$size[[i]])),
            tags$td(format(docs$modified[[i]], "%Y-%m-%d %H:%M")),
            tags$td(tags$a(
              href = "#", class = "cd-link",
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
                                remove_id, jsonlite::toJSON(docs$name[[i]], auto_unbox = TRUE)),
              i18n$t("btn_docs_remove")
            ))
          )
        }))
      )
    })
  })
}
