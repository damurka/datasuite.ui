# The CLIENT's own reported container height (getCurrentOutputInfo(), the same source shiny::renderPlot()'s
# own built-in "auto" detection would use if a caller didn't specify height= at all), used whenever it's
# actually taller than a plot's own natural/default height -- explicit user request ("the full screen is
# hiding with a lot of white space"): ExpandButton.tsx (js/src/components) stretches a card's own
# .shiny-plot-output container to fill the screen on expand, but every height=function(){...} in this app
# ignored that entirely and kept drawing at its own fixed/computed size, leaving the rest of the now-
# fullscreen card empty below it. `fallback`: the plot's own normal height (a flat 400, render-plot.R's own
# previous unconditional default -- or cd_plot_server()'s own content-aware layout()$height, e.g.
# taller for a chart with more facet rows) -- returned unchanged whenever the client hasn't reported
# anything taller, so every page's normal (non-expanded) look is exactly what it was before this existed.
cd_plot_client_height <- function(fallback) {
  info <- shiny::getCurrentOutputInfo()
  client_height <- if (is.function(info$height)) info$height() else info$height
  if (is.numeric(client_height) && length(client_height) == 1 && client_height > fallback) client_height else fallback
}

# `height` is a function returning the plot's height in pixels; the plot output must be created with height = "auto"
# (see cd_plot_output()) so the chart can grow with the number of categories it draws. `expr` is evaluated once: a
# ggplot it gives is drawn (from its kept build, kit-chart-build.R), a recorded plot (.ds_capture_plot()) replayed, and
# a plot drawn with base graphics while it ran is already on the device.
cd_render_plot <- function(expr, height = function() cd_plot_client_height(400)) {
  # Helper function to generate an error plot
  generate_error_plot <- function(message, color = 'red') {
    graphics::plot.new()
    graphics::text(
      x = 0.5, y = 0.5,
      labels = message,
      cex = 1.2, col = color
    )
  }

  renderPlot({
    # Evaluate the plot (or its data) once
    check_data <- tryCatch(
      eval_tidy(enquo(expr)),
      error = function(e) e
    )

    # req()/validate() with nothing to say -- the inputs for this chart are not ready, or the page it is on has
    # been left (pages gate their outputs on `active`). That is "not yet", not "no data": passing it on as the
    # silent error it is lets the client keep showing the loader, where the branch below used to draw a gray
    # "No data available" that then flashed up every time a page was left and came back to.
    if (inherits(check_data, "shiny.silent.error")) req(FALSE)

    # Check if data is empty or invalid
    if (inherits(check_data, "error") ||
        (is.data.frame(check_data) && nrow(check_data) == 0) ||
        (is.vector(check_data) && length(check_data) == 0) ||
        (is.matrix(check_data) && nrow(check_data) == 0)) { #Added matrix check

      text <- .ds_clean_error(check_data)

      if (nchar(text) == 0 || text == '') {
        generate_error_plot('No data available', 'gray')
      } else {
        # validate()'s message is for the screen; anything else is a failure, logged too
        if (!inherits(check_data, "validation")) message('Error drawing a chart: ', text)
        generate_error_plot(text, 'red')
      }
      return() # Return early, no progress or further processing
    }
    tryCatch({
      if (inherits(check_data, "ggplot")) {
        .ds_draw_plot(check_data)
      } else if (inherits(check_data, "recordedplot")) {
        grDevices::replayPlot(check_data)
      } else if (inherits(check_data, "htmlwidget")) {
        # a plotly chart (any htmlwidget) is a web page, not a picture: it needs its own output (plotly::plotlyOutput())
        generate_error_plot('This chart is interactive and cannot be drawn here.', 'gray')
      }
      # anything else: a plot drawn with base graphics, already drawn while `expr` ran
    },
    error = function(e) {
      message('Error drawing a chart: ', .ds_clean_error(e))
      generate_error_plot(paste('Error:', .ds_clean_error(e)))
    })
  }, height = height)
}

# What a plot function gives, ready to draw again: a ggplot (or anything else it returns) as it is, but a plot it
# drew with base graphics recorded (grDevices::recordPlot()), so that the screen, a resize and the image download all
# draw it. It runs on an off-screen device of its own, so nothing is drawn where it is not wanted.
.ds_capture_plot <- function(fun) {
  old <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(dev)
    if (old > 1) grDevices::dev.set(old)
  }, add = TRUE)
  grDevices::dev.control("enable")
  value <- fun()
  if (inherits(value, "ggplot")) return(value)
  drawn <- grDevices::recordPlot()
  if (length(drawn[[1]])) drawn else value
}

cd_plot_output <- function(id) {
  cd_spinner(plotOutput(id, height = "auto"), min_height = 400)
}
