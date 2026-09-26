spec <- function(...) {
  s <- list(
    title = "Coverage by region",
    data = list(member = "calculate_coverage", args = list(admin_level = "adminlevel_1")),
    transform = list(list(filter = list(year = list(2023))), list(select = list("region", "anc4", "penta3")),
                     list(pivot_longer = list(cols = list("anc4", "penta3"), names_to = "indicator", values_to = "value"))),
    plot = list(geom = "col", x = "region", y = "value", fill = "indicator", position = "dodge", percent = TRUE)
  )
  extra <- list(...)
  for (n in names(extra)) s[[n]] <- extra[[n]]
  s
}

test_that("a spec is checked and tidied", {
  s <- report_validate_spec(spec())
  expect_identical(s$transform[[2]]$select, c("region", "anc4", "penta3"))
  expect_identical(s$transform[[3]]$pivot_longer$cols, c("anc4", "penta3"))
  expect_true(s$plot$percent)
  expect_false(s$plot$flip)
  expect_error(report_validate_spec(spec(data = list(member = "x"))), NA)
  expect_error(report_validate_spec(spec(), members = c("a", "b")), "can't be charted")
  expect_error(report_validate_spec(spec(plot = list(geom = "pie"))), "geom")
  expect_error(report_validate_spec(spec(plot = list(geom = "tile", x = "a", y = "b"))), "fill")
  bad <- spec()
  bad$transform <- list(list(eval = "system('x')"))
  expect_error(report_validate_spec(bad), "must be one of")
  expect_error(report_validate_spec("x"), "must be a list")
})

test_that("transforms reshape data without running code", {
  d <- data.frame(region = c("A", "B", "A", "B"), year = c(2022, 2022, 2023, 2023),
                  anc4 = c(50, 60, 55, 65), penta3 = c(70, 80, 75, 85), births = c(10, 20, 10, 20))
  s <- report_validate_spec(spec())
  out <- report_apply_transforms(d, s$transform)
  expect_identical(nrow(out), 4L)
  expect_setequal(out$indicator, c("anc4", "penta3"))
  expect_identical(sort(out$value), c(55, 65, 75, 85))

  agg <- report_apply_transforms(d, report_validate_spec(spec(transform = list(list(aggregate = list(by = "year", fun = "sum", cols = "births")))))$transform)
  expect_identical(agg$births, c(30, 30))
  ratio <- report_apply_transforms(d, report_validate_spec(spec(transform = list(list(mutate_ratio = list(name = "r", numerator = "anc4", denominator = "penta3", percent = TRUE)))))$transform)
  expect_equal(ratio$r[1], 50 / 70 * 100)
  top <- report_apply_transforms(d, report_validate_spec(spec(transform = list(list(top_n = list(n = 2, by = "anc4")))))$transform)
  expect_identical(top$anc4, c(65, 60))
  ren <- report_apply_transforms(d, report_validate_spec(spec(transform = list(list(rename = list(area = "region")))))$transform)
  expect_true("area" %in% names(ren))
  expect_error(report_apply_transforms(d, list(list(select = "nope"))), "no column nope")
})

test_that("a plot description draws a ggplot", {
  d <- data.frame(region = c("A", "B", "A", "B"), indicator = c("anc4", "anc4", "penta3", "penta3"), value = c(55, 65, 75, 85))
  p <- report_plot_spec(d, list(geom = "col", x = "region", y = "value", fill = "indicator", position = "dodge", percent = TRUE, flip = TRUE), title = "T")
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "T")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  for (g in c("line", "point", "area")) expect_s3_class(ggplot2::ggplot_build(report_plot_spec(d, list(geom = g, x = "region", y = "value", colour = "indicator"))), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(report_plot_spec(d, list(geom = "tile", x = "region", y = "indicator", fill = "value"))), "ggplot_built")
  expect_error(report_plot_spec(d, list(geom = "col", x = "nope", y = "value")), "nope")
})

test_that("a report project is checked, and a custom chart's spec kept in spec", {
  p <- report_validate_project(list(name = "R", blocks = list(
    list(type = "heading", text = "Hi"),
    list(type = "chart", kind = "coverage", indicator = "anc4"),
    list(type = "chart", kind = "custom_chart", options = spec())
  )), kinds = c("coverage", "custom_chart"))
  expect_length(p$blocks, 3)
  expect_true(all(nzchar(vapply(p$blocks, `[[`, "", "id"))))
  expect_identical(anyDuplicated(vapply(p$blocks, `[[`, "", "id")), 0L)
  expect_null(p$blocks[[3]]$options)
  expect_identical(p$blocks[[3]]$spec$plot$geom, "col")
  expect_error(report_validate_project(list(blocks = list(list(type = "chart", kind = "nope"))), kinds = "coverage"), "no kind")
  expect_error(report_validate_project(list(blocks = list(list(type = "video")))), "type must be")
  expect_error(report_validate_project(list(name = "empty", blocks = list())), "needs blocks")
})
