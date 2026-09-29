test_that("cd_chart_options() takes every chart option of Quire's contract, and only those", {
  args <- setdiff(names(formals(cd_chart_options)), "...")
  expect_setequal(args, chart_option_fields())
})

test_that("the chart panel's settings are chart options", {
  expect_true(all(.chart_panel_fields() %in% chart_option_fields()))
})
