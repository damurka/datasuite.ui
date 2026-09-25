# datasuite.ui

The shared interface of DataSuite's Shiny apps. It holds nothing specific to one app's data; an app gives it its pages,
its charts and its data.

What is in it so far:

- **Chart options** (`cd_chart_options()`, `apply_chart_options()`): restyle any ggplot2 chart - its texts, fonts,
  colours, axes, legend, grid, facets - and show or hide each element (`show_title`, `show_legend`, ...).

Coming from cd2030.core and countdown-analytics: the report builder (Word, PowerPoint and PDF export, themes, the
editor) and the Shiny and React components, page frame and navigation.
