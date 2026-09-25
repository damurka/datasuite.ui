# Small Office files with a theme of their own, for test-report-template.R (and the manual checks): officer's blank
# files with their theme's colours and fonts changed, rezipped.

.rb_test_rezip <- function(dir, file) {
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  unlink(file)
  zip::zip(file, files = files, root = dir, mode = "mirror")
  file
}

.rb_test_read <- function(p) {
  x <- readChar(p, file.info(p)$size, useBytes = TRUE)
  Encoding(x) <- "UTF-8"
  x
}

.rb_test_write <- function(txt, p) writeChar(enc2utf8(txt), p, eos = NULL, useBytes = TRUE)

# The theme's colours and fonts changed
.rb_test_theme_xml <- function(theme, colours, major, minor) {
  for (k in names(colours)) {
    theme <- sub(sprintf("<a:%1$s>.*?</a:%1$s>", k), sprintf('<a:%1$s><a:srgbClr val="%2$s"/></a:%1$s>', k, colours[[k]]), theme, perl = TRUE)
  }
  theme <- sub('(<a:majorFont>\\s*<a:latin typeface=")[^"]*"', paste0("\\1", major, '"'), theme, perl = TRUE)
  sub('(<a:minorFont>\\s*<a:latin typeface=")[^"]*"', paste0("\\1", minor, '"'), theme, perl = TRUE)
}

# A PowerPoint file (a template, .potx, when `file` ends so) of `size` ("16:9" or "4:3") with a theme of its own and its
# master's background `background` (hex without #, or NULL); a small band in the accent colour on the master (as a
# template's logo or decoration would be)
.rb_test_pptx_template <- function(file, size = "16:9", background = "F3EEE4",
                                   colours = c(dk1 = "1B1B1B", dk2 = "243B55", accent1 = "B5472B", accent2 = "2E7D6B", accent3 = "E0A526",
                                               accent4 = "5B4C9A", accent5 = "3F88C5", accent6 = "7A8691"),
                                   major = "Georgia", minor = "Verdana") {
  tmp <- tempfile(fileext = ".pptx")
  print(officer::read_pptx(), target = tmp)
  dir <- tempfile("pptx_")
  utils::unzip(tmp, exdir = dir)
  theme_path <- file.path(dir, "ppt", "theme", "theme1.xml")
  .rb_test_write(.rb_test_theme_xml(.rb_test_read(theme_path), colours, major, minor), theme_path)
  pres_path <- file.path(dir, "ppt", "presentation.xml")
  cx <- if (identical(size, "4:3")) "9144000" else "12192000"
  .rb_test_write(sub("<p:sldSz [^>]*/>", sprintf('<p:sldSz cx="%s" cy="6858000"/>', cx), .rb_test_read(pres_path)), pres_path)
  master_path <- file.path(dir, "ppt", "slideMasters", "slideMaster1.xml")
  master <- .rb_test_read(master_path)
  if (!is.null(background)) {
    master <- sub("<p:bg>.*?</p:bg>", sprintf('<p:bg><p:bgPr><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:effectLst/></p:bgPr></p:bg>', background), master, perl = TRUE)
  }
  band <- sprintf(paste0('<p:sp><p:nvSpPr><p:cNvPr id="90" name="Template band"/><p:cNvSpPr/><p:nvPr userDrawn="1"/></p:nvSpPr><p:spPr>',
                         '<a:xfrm><a:off x="0" y="6583680"/><a:ext cx="%s" cy="274320"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom>',
                         '<a:solidFill><a:schemeClr val="accent1"/></a:solidFill><a:ln><a:noFill/></a:ln></p:spPr></p:sp>'), cx)
  master <- sub("(<p:grpSpPr>.*?</p:grpSpPr>)", paste0("\\1", band), master, perl = TRUE)
  .rb_test_write(master, master_path)
  if (grepl("\\.potx$", file)) {
    types_path <- file.path(dir, "[Content_Types].xml")
    .rb_test_write(sub("presentationml.presentation.main+xml", "presentationml.template.main+xml", .rb_test_read(types_path), fixed = TRUE), types_path)
  }
  .rb_test_rezip(dir, file)
}

# A Word file (a template, .dotx, when `file` ends so) with a theme of its own, Heading 1 in `heading` colour at 20 pt,
# Normal at 11 pt, and a header saying "TEMPLATE HEADER"
.rb_test_docx_template <- function(file, heading = "1F5C4A",
                                   colours = c(dk1 = "202020", dk2 = "30343B", accent1 = "2A7F62", accent2 = "C2562B", accent3 = "E0A526",
                                               accent4 = "5B4C9A", accent5 = "3F88C5", accent6 = "7A8691"),
                                   major = "Cambria", minor = "Corbel") {
  tmp <- tempfile(fileext = ".docx")
  print(officer::read_docx(), target = tmp)
  dir <- tempfile("docx_")
  utils::unzip(tmp, exdir = dir)
  theme_path <- file.path(dir, "word", "theme", "theme1.xml")
  .rb_test_write(.rb_test_theme_xml(.rb_test_read(theme_path), colours, major, minor), theme_path)
  styles_path <- file.path(dir, "word", "styles.xml")
  styles <- xml2::read_xml(styles_path)
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  set_run <- function(name, xml) {
    node <- xml2::xml_find_first(styles, sprintf("//w:style[w:name/@w:val='%s']", name), ns)
    rpr <- xml2::xml_find_first(node, "w:rPr", ns)
    if (inherits(rpr, "xml_missing")) {
      xml2::xml_add_child(node, "w:rPr")
      rpr <- xml2::xml_find_first(node, "w:rPr", ns)
    }
    for (old in xml2::xml_find_all(rpr, "w:rFonts|w:color|w:sz|w:szCs", ns)) xml2::xml_remove(old)
    frag <- xml2::read_xml(paste0('<x xmlns:w="', ns[["w"]], '">', xml, "</x>"))
    for (k in xml2::xml_children(frag)) xml2::xml_add_child(rpr, k, .where = 0)
  }
  set_run("heading 1", sprintf('<w:rFonts w:asciiTheme="majorHAnsi" w:hAnsiTheme="majorHAnsi"/><w:color w:val="%s"/><w:sz w:val="40"/>', heading))
  set_run("Normal", '<w:rFonts w:asciiTheme="minorHAnsi" w:hAnsiTheme="minorHAnsi"/><w:sz w:val="22"/>')
  xml2::write_xml(styles, styles_path)
  # a header
  .rb_test_write(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:hdr xmlns:w="', ns[["w"]], '"><w:p><w:pPr><w:jc w:val="right"/></w:pPr>',
                        '<w:r><w:rPr><w:color w:val="2A7F62"/></w:rPr><w:t>TEMPLATE HEADER</w:t></w:r></w:p></w:hdr>'), file.path(dir, "word", "header1.xml"))
  rels_path <- file.path(dir, "word", "_rels", "document.xml.rels")
  .rb_test_write(sub("</Relationships>", '<Relationship Id="rId90" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" Target="header1.xml"/></Relationships>',
                     .rb_test_read(rels_path), fixed = TRUE), rels_path)
  types_path <- file.path(dir, "[Content_Types].xml")
  types <- sub("</Types>", '<Override PartName="/word/header1.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/></Types>', .rb_test_read(types_path), fixed = TRUE)
  if (grepl("\\.dotx$", file)) types <- sub("wordprocessingml.document.main+xml", "wordprocessingml.template.main+xml", types, fixed = TRUE)
  .rb_test_write(types, types_path)
  doc_path <- file.path(dir, "word", "document.xml")
  doc <- .rb_test_read(doc_path)
  doc <- sub("(<w:sectPr[^>]*>)", '\\1<w:headerReference w:type="default" r:id="rId90"/>', doc)
  if (!grepl('xmlns:r="', doc, fixed = TRUE)) doc <- sub("<w:document ", '<w:document xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ', doc, fixed = TRUE)
  .rb_test_write(doc, doc_path)
  .rb_test_rezip(dir, file)
}

# A test picture: a landscape with a sky, a sun and hills, `w` x `h` pixels
.rb_test_picture <- function(file, w = 900, h = 600) {
  img <- magick::image_blank(w, h, "#8ec5e8")
  img <- magick::image_draw(img)
  graphics::rect(0, h * 0.62, w, h, col = "#4f8a3c", border = NA)
  graphics::polygon(c(0, w * 0.3, w * 0.65, w, w, 0), c(h * 0.65, h * 0.45, h * 0.6, h * 0.5, h, h), col = "#2f6b3a", border = NA)
  graphics::symbols(w * 0.78, h * 0.22, circles = h * 0.09, inches = FALSE, add = TRUE, bg = "#f5c542", fg = NA)
  grDevices::dev.off()
  magick::image_write(img, file, format = "png")
  file
}
