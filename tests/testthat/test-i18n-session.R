# Text drawn on the server follows the session's language, and is drawn again when it changes

test_that("cd_plain_text() follows the session's language, and a reactive reading it runs again when it changes", {
  i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("translation", "ui.json", package = "datasuite.ui"))
  i18n$set_translation_language("en")
  key <- "msg_docs_added"
  skip_if(identical(cd_plain_text(i18n, key, "en"), cd_plain_text(i18n, key, "fr")), "the key is not translated")

  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    expect_equal(cd_plain_text(i18n, key), cd_plain_text(i18n, key, "en"))   # no session language yet: the translator's
    shiny.i18n::update_lang("en", session)
    text <- shiny::reactive(cd_plain_text(i18n, key))
    expect_equal(shiny::isolate(text()), cd_plain_text(i18n, key, "en"))
    shiny.i18n::update_lang("fr", session)
    expect_equal(shiny::isolate(text()), cd_plain_text(i18n, key, "fr"))
  })
})
