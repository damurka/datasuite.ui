# Slide designs: the look a PowerPoint file gives its slides, read from the slides themselves (and their layout and master).
#
# Many decks carry their look on the slides, not in the theme: a logo pasted on the title slide, the title of every slide
# in a coloured band. report_theme_from_file() returns what it finds as `slide_designs = list(title = <design>,
# content = <design>)` (either may be NULL), and export_deck() draws a design's background and decor behind a slide's
# items (see .rb_deck_design()).
#
# A design:
#   background  "#rrggbb" or NULL
#   decor       list of, back first:
#                 list(type = "image", file = <picture file, png / jpg>, x, y, w, h, from)
#                 list(type = "rect", fill = "#rrggbb", fill_opacity = 0..1, outline = "#rrggbb" or NULL, x, y, w, h, from)
#               `from` ("master", "layout" or "slide", optional) says where it was found: a deck written on top of
#               the same file as its template already has the master's, and does not draw them again
#   title, subtitle, body   the style of that text, or NULL:
#                 list(x, y, w, h, fill = "#rrggbb" or NULL, fill_opacity, color = "#rrggbb", font_size (pt), bold,
#                      align = "left" | "center" | "right", font = family or NULL)
# Boxes are in inches from the slide's top-left corner.
#
# Where it comes from: as PowerPoint makes a theme, the master and the layouts first; what the slides draw themselves
# only where it repeats (.rb_design_consensus()): the "title" design from the slides on a title layout (or slide 1), the
# "content" design from the others; a file without slides (a template) from its title layout and its title and
# content layout. Decor is what the master, the layout and the slide draw that is not a placeholder: pictures (on the
# slide only small ones, logo-like, or thin ones along an edge; a large picture is the slide's content) and shapes
# with a solid fill and no text (bars, bands). The text styles are those of the title, subtitle and body placeholders
# (or, when a slide has none with text, of its text boxes), with what they do not set inherited from the layout, the
# master and the presentation, scheme colours resolved through the theme.

.rb_emu <- 914400

# Every target of the relationships of `type` of a part, in order
.rb_rel_targets <- function(dir, part, type) {
  rels <- file.path(dir, dirname(part), "_rels", paste0(basename(part), ".rels"))
  if (!file.exists(rels)) return(character())
  xml <- xml2::read_xml(rels)
  nodes <- xml2::xml_find_all(xml, "//rel:Relationship", .rb_ooxml_ns)
  nodes <- nodes[endsWith(xml2::xml_attr(nodes, "Type"), paste0("/", type))]
  from <- dirname(part)
  vapply(nodes, function(n) .rb_part_path(if (from == ".") "" else from, xml2::xml_attr(n, "Target")), character(1))
}

.rb_xfind <- function(node, path) xml2::xml_find_first(node, path, .rb_ooxml_ns)
.rb_found <- function(node) !is.null(node) && !inherits(node, "xml_missing")

# The shapes of a shape tree, groups opened, in drawing order (back first): list(node, kind = "pic" | "sp", box =
# c(x, y, w, h) in EMU or NULL, ph = list(type, idx) or NULL)
.rb_tree_shapes <- function(tree, transform = function(b) b) {
  out <- list()
  if (!.rb_found(tree)) return(out)
  box_of <- function(xfrm) {
    if (!.rb_found(xfrm)) return(NULL)
    off <- .rb_xfind(xfrm, "a:off")
    ext <- .rb_xfind(xfrm, "a:ext")
    if (!.rb_found(off) || !.rb_found(ext)) return(NULL)
    as.numeric(c(xml2::xml_attr(off, "x"), xml2::xml_attr(off, "y"), xml2::xml_attr(ext, "cx"), xml2::xml_attr(ext, "cy")))
  }
  for (node in xml2::xml_children(tree)) {
    name <- xml2::xml_name(node)
    if (name == "grpSp") {
      xfrm <- .rb_xfind(node, "p:grpSpPr/a:xfrm")
      g <- box_of(xfrm)
      ch_off <- .rb_xfind(xfrm, "a:chOff")
      ch_ext <- .rb_xfind(xfrm, "a:chExt")
      inner <- transform
      if (!is.null(g) && .rb_found(ch_off) && .rb_found(ch_ext)) {
        co <- as.numeric(c(xml2::xml_attr(ch_off, "x"), xml2::xml_attr(ch_off, "y")))
        ce <- as.numeric(c(xml2::xml_attr(ch_ext, "cx"), xml2::xml_attr(ch_ext, "cy")))
        sx <- if (isTRUE(ce[1] > 0)) g[3] / ce[1] else 1
        sy <- if (isTRUE(ce[2] > 0)) g[4] / ce[2] else 1
        inner <- function(b) transform(c(g[1] + (b[1] - co[1]) * sx, g[2] + (b[2] - co[2]) * sy, b[3] * sx, b[4] * sy))
      }
      out <- c(out, .rb_tree_shapes(node, inner))
      next
    }
    if (!name %in% c("sp", "pic")) next
    nv <- if (name == "sp") "p:nvSpPr" else "p:nvPicPr"
    ph <- .rb_xfind(node, paste0(nv, "/p:nvPr/p:ph"))
    box <- box_of(.rb_xfind(node, "p:spPr/a:xfrm"))
    out[[length(out) + 1]] <- list(
      node = node, kind = name, box = if (!is.null(box)) transform(box),
      ph = if (.rb_found(ph)) list(type = xml2::xml_attr(ph, "type") %|NA|% "obj", idx = xml2::xml_attr(ph, "idx") %|NA|% NA_character_)
    )
  }
  out
}

`%|NA|%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# The placeholder of `level`'s shapes matching `ph` (by index on a layout, by type on a master)
.rb_match_ph <- function(shapes, ph, by_index = TRUE) {
  if (is.null(ph)) return(NULL)
  norm <- function(t) switch(t, ctrTitle = "title", subTitle = "body", obj = "body", t)
  with_ph <- Filter(function(s) !is.null(s$ph), shapes)
  if (by_index && !is.na(ph$idx)) {
    hit <- Filter(function(s) identical(s$ph$idx, ph$idx), with_ph)
    if (length(hit)) return(hit[[1]])
  }
  hit <- Filter(function(s) identical(s$ph$type, ph$type), with_ph)
  if (!length(hit)) hit <- Filter(function(s) identical(norm(s$ph$type), norm(ph$type)), with_ph)
  if (length(hit)) hit[[1]] else NULL
}

# A shape's text, joined
.rb_shape_text <- function(node) paste(xml2::xml_text(xml2::xml_find_all(node, ".//a:t", .rb_ooxml_ns)), collapse = "")
.rb_has_text <- function(node) nzchar(trimws(gsub(intToUtf8(160L), " ", .rb_shape_text(node), fixed = TRUE)))

# The largest font size (pt) a shape's text is written in, from its runs (NA when none says)
.rb_max_size <- function(node) {
  sz <- suppressWarnings(as.numeric(xml2::xml_attr(xml2::xml_find_all(node, ".//a:r/a:rPr", .rb_ooxml_ns), "sz")))
  sz <- sz[is.finite(sz)]
  if (length(sz)) max(sz) / 100 else NA_real_
}

# A colour node's opacity (a:alpha), 0 to 1
.rb_color_alpha <- function(node) {
  if (!.rb_found(node)) return(1)
  v <- suppressWarnings(as.numeric(xml2::xml_attr(.rb_xfind(node, "a:alpha"), "val")))
  if (is.finite(v)) min(1, max(0, v / 100000)) else 1
}

# A shape's fill: list(state = "none" | "fill" | "unset", color, opacity)
.rb_shape_fill <- function(node, scheme, map) {
  unset <- list(state = "unset", color = NULL, opacity = 1)
  if (!.rb_found(node)) return(unset)
  sppr <- .rb_xfind(node, "p:spPr")
  if (.rb_found(.rb_xfind(sppr, "a:noFill"))) return(list(state = "none", color = NULL, opacity = 1))
  colour_node <- .rb_xfind(sppr, "a:solidFill/*")
  if (!.rb_found(colour_node)) {
    # a gradient: the average of its stops (a dark veil over a photo stays dark, not its lightest stop)
    stops <- xml2::xml_find_all(sppr, "a:gradFill/a:gsLst/a:gs/*", .rb_ooxml_ns)
    if (length(stops)) {
      hexes <- vapply(stops, function(n) .rb_dml_color(n, scheme, map), character(1))
      alphas <- vapply(stops, .rb_color_alpha, numeric(1))
      keep <- !is.na(hexes)
      if (any(keep)) {
        rgb <- grDevices::col2rgb(hexes[keep])
        w <- pmax(alphas[keep], 1e-6)
        mix <- rowSums(rgb * rep(w, each = 3)) / sum(w)
        return(list(state = "fill", color = tolower(grDevices::rgb(mix[1], mix[2], mix[3], maxColorValue = 255)),
                    opacity = round(mean(alphas[keep]), 3)))
      }
    }
  }
  if (!.rb_found(colour_node) && !.rb_found(.rb_xfind(sppr, "a:blipFill|a:pattFill|a:grpFill"))) {
    # a shape styled by the theme (as PowerPoint inserts them): its fill reference
    ref <- .rb_xfind(node, "p:style/a:fillRef")
    idx <- suppressWarnings(as.integer(xml2::xml_attr(ref, "idx")))
    if (.rb_found(ref) && isTRUE(idx > 0)) colour_node <- xml2::xml_child(ref, 1)
  }
  if (!.rb_found(colour_node)) return(unset)
  hex <- .rb_dml_color(colour_node, scheme, map)
  if (is.na(hex)) return(unset)
  list(state = "fill", color = tolower(hex), opacity = .rb_color_alpha(colour_node))
}

# A shape's outline colour, or NULL
.rb_shape_outline <- function(node, scheme, map) {
  ln <- .rb_xfind(node, "p:spPr/a:ln")
  if (.rb_found(ln)) {
    if (.rb_found(.rb_xfind(ln, "a:noFill"))) return(NULL)
    c <- .rb_xfind(ln, "a:solidFill/*")
    if (.rb_found(c)) { hex <- .rb_dml_color(c, scheme, map); return(if (is.na(hex)) NULL else tolower(hex)) }
  }
  ref <- .rb_xfind(node, "p:style/a:lnRef")
  idx <- suppressWarnings(as.integer(xml2::xml_attr(ref, "idx")))
  if (.rb_found(ref) && isTRUE(idx > 0)) {
    hex <- .rb_dml_color(xml2::xml_child(ref, 1), scheme, map)
    if (!is.na(hex)) return(tolower(hex))
  }
  NULL
}

# A background colour (p:cSld/p:bg) of a slide, layout or master, or NA when it sets none
.rb_part_background <- function(xml, theme, scheme, map) {
  bg <- .rb_xfind(xml, "//p:cSld/p:bg")
  if (!.rb_found(bg)) return(NA_character_)
  c <- .rb_xfind(bg, "p:bgPr/a:solidFill/*")
  if (!.rb_found(c)) c <- .rb_xfind(bg, "p:bgPr/a:gradFill/a:gsLst/a:gs/*")
  if (.rb_found(c)) return(.rb_dml_color(c, scheme, map))
  ref <- .rb_xfind(bg, "p:bgRef")
  if (.rb_found(ref)) return(.rb_dml_color(xml2::xml_child(ref, 1), scheme, map))
  NA_character_
}

# One source of a design: a slide (with its layout and master) or a layout alone (a template without slides)
.rb_design_source <- function(dir, part, is_layout = FALSE) {
  xml <- .rb_read_part(dir, part)
  if (is.null(xml)) return(NULL)
  layout_part <- if (is_layout) part else .rb_rel_target(dir, part, type = "slideLayout")
  layout <- if (is_layout) xml else .rb_read_part(dir, layout_part)
  master_part <- if (!is.null(layout_part)) .rb_rel_target(dir, layout_part, type = "slideMaster")
  master <- .rb_read_part(dir, master_part)
  level <- function(p, x) if (!is.null(x)) list(part = p, xml = x, shapes = .rb_tree_shapes(.rb_xfind(x, "//p:cSld/p:spTree")))
  list(slide = if (!is_layout) level(part, xml), layout = level(layout_part, layout), master = level(master_part, master))
}

.rb_layout_type <- function(dir, layout_part) {
  x <- .rb_read_part(dir, layout_part)
  if (is.null(x)) return(NA_character_)
  xml2::xml_attr(xml2::xml_root(x), "type") %|NA|% "cust"
}

# The file's designs: list(title, content)
.rb_slide_designs <- function(dir, pres, theme, th, map) {
  scheme <- th$scheme
  size <- c(12192000, 6858000)
  sz <- .rb_xfind(pres, "//p:sldSz")
  if (.rb_found(sz)) size <- as.numeric(c(xml2::xml_attr(sz, "cx"), xml2::xml_attr(sz, "cy")))
  ctx <- list(dir = dir, pres = pres, theme = theme, th = th, scheme = scheme, map = map, size = size,
              files = tempfile("slide_design_"))
  dir.create(ctx$files, showWarnings = FALSE)

  ids <-xml2::xml_attr(xml2::xml_find_all(pres, "//p:sldIdLst/p:sldId", .rb_ooxml_ns), "r:id", ns = .rb_ooxml_ns)
  slides <- unlist(lapply(ids, function(id) .rb_rel_target(dir, "ppt/presentation.xml", id = id)))
  slide_types <- vapply(slides, function(s) .rb_layout_type(dir, .rb_rel_target(dir, s, type = "slideLayout") %||% ""), character(1))

  # the layouts of the first master, for a file without slides (or without a content slide)
  master_id <- xml2::xml_attr(.rb_xfind(pres, "//p:sldMasterIdLst/p:sldMasterId"), "r:id", ns = .rb_ooxml_ns)
  master_part <- if (!is.na(master_id)) .rb_rel_target(dir, "ppt/presentation.xml", id = master_id)
  layouts <- if (!is.null(master_part)) .rb_rel_targets(dir, master_part, "slideLayout") else character()
  layout_types <- vapply(layouts, function(l) .rb_layout_type(dir, l), character(1))
  pick_layout <- function(types) {
    for (t in types) { hit <- which(layout_types == t); if (length(hit)) return(layouts[[hit[1]]]) }
    NULL
  }

  from_layout <- function(l, name) {
    if (is.null(l)) return(NULL)
    tryCatch(.rb_design_from(.rb_design_source(dir, l, is_layout = TRUE), ctx, name), error = function(e) NULL)
  }
  title <- NULL
  content <- NULL
  if (length(slides)) {
    hit <- which(slide_types == "title")
    title_slides <- if (length(hit)) hit else 1
    other <- setdiff(seq_along(slides), title_slides)
    title <- .rb_design_consensus(slides[title_slides], ctx, "title")
    if (length(other)) content <- .rb_design_consensus(slides[other], ctx, "content")
  } else {
    title <- from_layout(pick_layout("title"), "title")
  }
  if (is.null(content)) {
    l <- pick_layout(c("obj", "tx", "titleOnly", "twoObj"))
    if (is.null(l)) l <- pick_layout(setdiff(unique(layout_types), c("title", "blank")))
    content <- from_layout(l, "content")
  }
  designs <- lapply(list(title = title, content = content), .rb_design_contrast, size = ctx$size / .rb_emu)
  .rb_designs_to_slide(designs, ctx$size / .rb_emu)
}

# A design on a dark slide (a dark background, or a dark veil over most of the slide, as over a photo): the text it
# gives no style of its own is white, so it can be read there
.rb_design_contrast <- function(d, size) {
  if (!is.list(d)) return(d)
  lum <- function(hex) { v <- grDevices::col2rgb(hex)[, 1] / 255; sum(v * c(0.299, 0.587, 0.114)) }
  dark <- is.character(d$background) && lum(d$background) < 0.4
  for (item in d$decor %||% list()) {
    if (!identical(item$type, "rect") || !is.character(item$fill)) next
    covers <- item$w * item$h >= 0.8 * size[1] * size[2]
    if (covers && (item$fill_opacity %||% 1) >= 0.5) dark <- lum(item$fill) < 0.4
  }
  if (!dark) return(d)
  for (k in c("title", "subtitle", "body")) {
    if (is.null(d[[k]])) d[[k]] <- list(color = "#ffffff")
  }
  d
}

# Designs read on a slide of `size` (inches) moved to the builder's slide of the same shape (16:9: 13.33 x 7.5 in, 4:3:
# 10 x 7.5 in): boxes scaled to it, text sizes by the same factor (a 26.67 in wide slide's 72 pt title is 36 pt here)
.rb_designs_to_slide <- function(designs, size) {
  if (!all(is.finite(size)) || any(size <= 0)) return(designs)
  target <- if (abs(size[1] / size[2] - 16 / 9) <= abs(size[1] / size[2] - 4 / 3)) c(13.333, 7.5) else c(10, 7.5)
  fx <- target[1] / size[1]
  fy <- target[2] / size[2]
  if (abs(fx - 1) < 0.01 && abs(fy - 1) < 0.01) return(designs)
  box <- function(b) {
    if (!is.list(b)) return(b)
    for (k in c("x", "w")) if (is.numeric(b[[k]])) b[[k]] <- round(b[[k]] * fx, 3)
    for (k in c("y", "h")) if (is.numeric(b[[k]])) b[[k]] <- round(b[[k]] * fy, 3)
    if (is.numeric(b$font_size)) b$font_size <- round(b$font_size * fy, 1)
    b
  }
  lapply(designs, function(d) {
    if (!is.list(d)) return(d)
    d$decor <- lapply(d$decor %||% list(), box)
    for (k in c("title", "subtitle", "body")) if (!is.null(d[[k]])) d[[k]] <- box(d[[k]])
    d
  })
}

# One design from the slides of one kind (title slides, or the others), as PowerPoint makes a theme: the master and the
# layouts are the design; what the slides draw themselves counts only where it repeats. A logo or band is kept when at
# least half of the slides have it at the same place, and the title, subtitle and body styles and the background are
# those at least half of the slides share (else the layout's). A single slide (a deck's one title slide) is taken as
# it is. At most 40 slides are read.
.rb_design_consensus <- function(parts, ctx, name) {
  parts <- utils::head(parts, 40)
  srcs <- lapply(parts, function(p) tryCatch(.rb_design_source(ctx$dir, p), error = function(e) NULL))
  designs <- lapply(srcs, function(s) if (!is.null(s)) tryCatch(.rb_design_from(s, ctx, name), error = function(e) NULL))
  ok <- !vapply(designs, is.null, logical(1))
  designs <- designs[ok]
  srcs <- srcs[ok]
  if (!length(designs)) return(NULL)
  if (length(designs) == 1) return(designs[[1]])
  need <- ceiling(length(designs) / 2)
  # the design of the most used layout alone (the master's and the layout's decor and text styles)
  lay <- vapply(srcs, function(s) s$layout$part %||% "", character(1))
  first <- match(names(sort(table(lay), decreasing = TRUE))[1], lay)
  base_src <- srcs[[first]]
  base <- tryCatch(.rb_design_from(list(slide = NULL, layout = base_src$layout, master = base_src$master), ctx, name),
                   error = function(e) NULL) %||% list(decor = list())

  # decor the slides draw themselves, kept where at least half of them draw it at the same place
  at <- function(d) paste(d$type, round(d$x * 10), round(d$y * 10), round(d$w * 10), round(d$h * 10),
                          if (identical(d$type, "rect")) d$fill else "")
  own <- lapply(designs, function(d) Filter(function(x) identical(x$from, "slide"), d$decor))
  keys <- unlist(lapply(own, function(items) unique(vapply(items, at, character(1)))))
  common <- names(which(table(keys) >= need))
  added <- list()
  for (items in own) for (item in items) {
    k <- at(item)
    if (k %in% common && !k %in% names(added)) added[[k]] <- item
  }
  decor <- c(base$decor %||% list(), unname(added))

  # the text styles and background most slides share
  sig <- function(st) {
    if (is.null(st)) return("")
    paste(st$fill %||% "", st$fill_opacity, st$color, st$font_size, st$bold, st$align, st$font %||% "",
          round(st$x * 4), round(st$y * 4), round(st$w * 4), round(st$h * 4))
  }
  shared <- function(field) {
    sigs <- vapply(designs, function(d) if (field == "background") d$background %||% "" else sig(d[[field]]), character(1))
    counts <- table(sigs)
    top <- names(counts)[which.max(counts)]
    if (max(counts) >= need && nzchar(top)) return(designs[[match(top, sigs)]][[field]])
    if (field == "background") return(base[[field]])
    # no one style on most slides, but most have one: each property as most of them have it (its size, colour, font...)
    styles <- Filter(Negate(is.null), lapply(designs, function(d) d[[field]]))
    if (length(styles) < need) return(base[[field]])
    mode_of <- function(values) {
      keys <- vapply(values, function(v) if (is.null(v)) "" else paste(v, collapse = ","), character(1))
      values[[match(names(which.max(table(keys))), keys)]]
    }
    boxes <- vapply(styles, function(st) paste(round(st$x * 4), round(st$y * 4), round(st$w * 4), round(st$h * 4)), character(1))
    out <- styles[[match(names(which.max(table(boxes))), boxes)]]
    for (f in c("fill", "fill_opacity", "color", "font_size", "bold", "align", "font")) out[f] <- list(mode_of(lapply(styles, function(st) st[[f]])))
    out
  }
  list(background = shared("background"), decor = decor, title = shared("title"), subtitle = shared("subtitle"),
       body = shared("body"))
}

# One design from its source
.rb_design_from <- function(src, ctx, name) {
  inch <- function(v) round(v / .rb_emu, 3)
  box_list <- function(b) list(x = inch(b[1]), y = inch(b[2]), w = inch(b[3]), h = inch(b[4]))
  slide_area <- ctx$size[1] * ctx$size[2]

  # background: the slide's, else the layout's, else the master's
  background <- NA_character_
  for (lv in list(src$slide, src$layout, src$master)) {
    if (is.null(lv) || !is.na(background)) next
    background <- .rb_part_background(lv$xml, ctx$theme, ctx$scheme, ctx$map)
  }
  background <- if (!is.na(background) && toupper(background) != "#FFFFFF") tolower(background)

  # decor, back first: the master's (unless the layout or slide hides it), the layout's, the slide's
  shows_master <- function(lv) !is.null(lv) && !identical(xml2::xml_attr(xml2::xml_root(lv$xml), "showMasterSp"), "0")
  levels <- list()
  slide_hides <- !is.null(src$slide) && !shows_master(src$slide)
  if (!slide_hides && (is.null(src$layout) || shows_master(src$layout))) levels$master <- src$master
  if (!slide_hides) levels$layout <- src$layout
  levels$slide <- src$slide
  decor <- list()
  n_img <- 0
  for (lv_name in names(levels)) {
    lv <- levels[[lv_name]]
    if (is.null(lv)) next
    for (s in lv$shapes) {
      if (!is.null(s$ph) || is.null(s$box) || any(!is.finite(s$box)) || s$box[3] <= 0 || s$box[4] <= 0) next
      if (s$kind == "pic") {
        if (lv_name == "slide") {
          share <- s$box[3] * s$box[4] / slide_area
          tol <- 0.02 * ctx$size
          at_edge <- s$box[1] <= tol[1] || s$box[2] <= tol[2] || s$box[1] + s$box[3] >= ctx$size[1] - tol[1] ||
            s$box[2] + s$box[4] >= ctx$size[2] - tol[2]
          thin <- s$box[3] < 0.2 * ctx$size[1] || s$box[4] < 0.2 * ctx$size[2]
          # a logo (small), a strip along an edge, or the slide's background photo (nearly all of it); a large picture
          # in between is the slide's content
          if (!(share < 0.2 || (at_edge && thin && share < 0.35) || share >= 0.8)) next
        }
        embed <- xml2::xml_attr(.rb_xfind(s$node, "p:blipFill/a:blip"), "embed")
        if (is.na(embed)) embed <- xml2::xml_attr(.rb_xfind(s$node, "p:blipFill/a:blip"), "r:embed", ns = .rb_ooxml_ns)
        target <- if (!is.na(embed)) .rb_rel_target(ctx$dir, lv$part, id = embed)
        if (is.null(target) || !file.exists(file.path(ctx$dir, target))) next
        ext <- tolower(tools::file_ext(target))
        if (!ext %in% c("png", "jpg", "jpeg", "gif")) next
        n_img <- n_img + 1
        file <- file.path(ctx$files, sprintf("%s_%d_%s", name, n_img, basename(target)))
        file.copy(file.path(ctx$dir, target), file, overwrite = TRUE)
        if (isTRUE(file.info(file)$size > 8e5) && requireNamespace("magick", quietly = TRUE)) {
          small <- sub("\\.[A-Za-z]+$", ".jpg", file)
          ok <- tryCatch({
            img <- magick::image_read(file)
            if (magick::image_info(img)$width > 1920) img <- magick::image_resize(img, "1920x")
            magick::image_write(magick::image_background(img, "white"), small, format = "jpeg", quality = 82)
            TRUE
          }, error = function(e) FALSE)
          if (ok) { if (small != file) unlink(file); file <- small }
        }
        decor[[length(decor) + 1]] <- c(list(type = "image", file = normalizePath(file, winslash = "/")), box_list(s$box),
                                        list(from = lv_name))
      } else {
        if (.rb_has_text(s$node)) next
        fill <- .rb_shape_fill(s$node, ctx$scheme, ctx$map)
        if (!identical(fill$state, "fill")) next
        decor[[length(decor) + 1]] <- c(list(type = "rect", fill = fill$color, fill_opacity = fill$opacity,
                                             outline = .rb_shape_outline(s$node, ctx$scheme, ctx$map)), box_list(s$box),
                                        list(from = lv_name))
      }
    }
  }

  # the texts: the placeholders of the slide (or of the layout, for a layout)
  top <- src$slide %||% src$layout
  is_layout <- is.null(src$slide)
  text_shapes <- Filter(function(s) s$kind == "sp" && .rb_found(.rb_xfind(s$node, "p:txBody")), top$shapes)
  ph_of <- function(types) Filter(function(s) !is.null(s$ph) && s$ph$type %in% types, text_shapes)
  usable <- function(s) is_layout || .rb_has_text(s$node)

  title_shape <- NULL
  titles <- ph_of(c("title", "ctrTitle"))
  if (length(titles) && usable(titles[[1]])) {
    title_shape <- titles[[1]]
  } else {
    # the text box with the largest text in the top two thirds
    cands <- Filter(function(s) is.null(s$ph) && !is.null(s$box) && .rb_has_text(s$node) && s$box[2] < ctx$size[2] * 2 / 3, text_shapes)
    if (length(cands)) {
      sizes <- vapply(cands, function(s) .rb_max_size(s$node) %|NA|% 0, numeric(1))
      tops <- vapply(cands, function(s) s$box[2], numeric(1))
      title_shape <- cands[[order(-sizes, tops)[1]]]
    } else if (length(titles)) {
      title_shape <- titles[[1]]
    }
  }
  same <- function(a, b) !is.null(a) && !is.null(b) && identical(a$node, b$node)

  subtitle_shape <- NULL
  if (name == "title") {
    subs <- ph_of("subTitle")
    if (length(subs)) {
      subtitle_shape <- subs[[1]]
    } else {
      others <- Filter(function(s) !same(s, title_shape) && (is.null(s$ph) || !s$ph$type %in% c("title", "ctrTitle", "dt", "ftr", "sldNum")),
                       text_shapes)
      with_text <- Filter(function(s) .rb_has_text(s$node), others)
      if (length(with_text)) others <- with_text
      if (length(others)) subtitle_shape <- others[[1]]
    }
  }
  body <- NULL
  if (name == "content") {
    bodies <- ph_of(c("body", "obj"))
    if (length(bodies)) {
      body <- .rb_text_style(bodies[[1]], src, ctx)
    } else if (!is.null(src$slide) && !is.null(src$layout)) {
      # the slide has none: the layout's
      lay <- Filter(function(s) s$kind == "sp" && !is.null(s$ph) && s$ph$type %in% c("body", "obj"), src$layout$shapes)
      if (length(lay)) body <- .rb_text_style(lay[[1]], list(slide = NULL, layout = src$layout, master = src$master), ctx)
    }
  }

  list(
    background = background,
    decor = decor,
    title = if (!is.null(title_shape)) .rb_text_style(title_shape, src, ctx),
    subtitle = if (!is.null(subtitle_shape)) .rb_text_style(subtitle_shape, src, ctx),
    body = body
  )
}

# The style of a text shape: its box, fill and first text's colour, size, weight, alignment and font, what it does not set
# taken from the matching placeholders of the layout and master, the master's text styles and the presentation's
.rb_text_style <- function(shape, src, ctx) {
  own_level <- if (!is.null(src$slide)) "slide" else "layout"
  ph <- shape$ph
  # the chain of shapes this one inherits from
  chain <- list(shape)
  if (!is.null(ph)) {
    if (own_level == "slide" && !is.null(src$layout)) {
      l <- .rb_match_ph(src$layout$shapes, ph, by_index = TRUE)
      if (!is.null(l)) chain[[length(chain) + 1]] <- l
    }
    if (!is.null(src$master)) {
      m <- .rb_match_ph(src$master$shapes, ph, by_index = FALSE)
      if (!is.null(m)) chain[[length(chain) + 1]] <- m
    }
  }

  box <- NULL
  for (s in chain) if (is.null(box) && !is.null(s$box)) box <- s$box
  if (is.null(box)) box <- c(0, 0, ctx$size)
  fill <- list(state = "unset")
  for (s in chain) if (identical(fill$state, "unset")) fill <- .rb_shape_fill(s$node, ctx$scheme, ctx$map)

  # the first run with text (else the first paragraph's end), its paragraph and level
  node <- shape$node
  run <- NULL
  for (r in xml2::xml_find_all(node, ".//a:p/a:r", .rb_ooxml_ns)) {
    if (nzchar(trimws(xml2::xml_text(.rb_xfind(r, "a:t"))))) { run <- r; break }
  }
  para <- if (!is.null(run)) xml2::xml_parent(run) else .rb_xfind(node, "p:txBody/a:p")
  rpr <- if (!is.null(run)) .rb_xfind(run, "a:rPr") else .rb_xfind(para, "a:endParaRPr")
  ppr <- .rb_xfind(para, "a:pPr")
  lvl <- suppressWarnings(as.integer(xml2::xml_attr(ppr, "lvl")))
  lvl_tag <- sprintf("a:lvl%dpPr", (if (is.finite(lvl)) lvl else 0L) + 1L)

  # paragraph-level nodes, nearest first: the paragraph, the shapes' list styles, the master's and presentation's styles
  pnodes <- list(ppr)
  for (s in chain) pnodes[[length(pnodes) + 1]] <- .rb_xfind(s$node, paste0("p:txBody/a:lstStyle/", lvl_tag))
  style_name <- if (is.null(ph)) "otherStyle" else if (ph$type %in% c("title", "ctrTitle")) "titleStyle" else "bodyStyle"
  if (!is.null(src$master)) pnodes[[length(pnodes) + 1]] <- .rb_xfind(src$master$xml, sprintf("//p:txStyles/p:%s/%s", style_name, lvl_tag))
  pnodes[[length(pnodes) + 1]] <- .rb_xfind(ctx$pres, paste0("//p:defaultTextStyle/", lvl_tag))
  if (is.null(ph) && !is.null(src$master)) pnodes[[length(pnodes) + 1]] <- .rb_xfind(src$master$xml, paste0("//p:txStyles/p:otherStyle/", lvl_tag))
  rnodes <- c(list(rpr), lapply(pnodes[-1], function(n) if (.rb_found(n)) .rb_xfind(n, "a:defRPr")))

  first_attr <- function(nodes, attr) {
    for (n in nodes) if (.rb_found(n)) { v <- xml2::xml_attr(n, attr); if (!is.na(v)) return(v) }
    NA_character_
  }
  first_node <- function(nodes, path) {
    for (n in nodes) if (.rb_found(n)) { v <- .rb_xfind(n, path); if (.rb_found(v)) return(v) }
    NULL
  }
  colour <- .rb_dml_color(first_node(rnodes, "a:solidFill/*"), ctx$scheme, ctx$map)
  if (is.na(colour)) {
    # the text colour of the colour map (tx1)
    key <- if (!is.null(ctx$map) && "tx1" %in% names(ctx$map)) ctx$map[["tx1"]] else "dk1"
    colour <- ctx$scheme[[key]] %||% "#000000"
  }
  size <- suppressWarnings(as.numeric(first_attr(rnodes, "sz"))) / 100
  bold <- first_attr(rnodes, "b")
  align <- first_attr(pnodes, "algn")
  font <- xml2::xml_attr(first_node(rnodes, "a:latin"), "typeface")
  if (!is.null(font) && !is.na(font)) {
    if (startsWith(font, "+mj")) font <- ctx$th$major else if (startsWith(font, "+mn")) font <- ctx$th$minor
  }
  if (is.null(font) || is.na(font) || !nzchar(font)) font <- if (style_name == "titleStyle") ctx$th$major else ctx$th$minor
  inch <- function(v) round(v / .rb_emu, 3)
  list(
    x = inch(box[1]), y = inch(box[2]), w = inch(box[3]), h = inch(box[4]),
    fill = if (identical(fill$state, "fill")) fill$color else NULL,
    fill_opacity = if (identical(fill$state, "fill")) fill$opacity else 1,
    color = tolower(colour %|NA|% "#000000"),
    font_size = if (is.finite(size)) size else 18,
    bold = !is.na(bold) && bold %in% c("1", "true"),
    align = switch(align %|NA|% "l", ctr = "center", r = "right", "left"),
    font = if (!is.null(font) && !is.na(font) && nzchar(font)) font else NULL
  )
}
