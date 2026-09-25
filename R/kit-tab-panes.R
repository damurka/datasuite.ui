# Tab panes: one pane per key, one shown at a time; cd_update_tab_panes() switches them from the server.

cd_tab_panes <- function(id, panels, active = names(panels)[[1]]) {
  keys <- names(panels)
  div(
    id = id, class = "cd-tabpanels",
    lapply(seq_along(panels), function(i) {
      div(
        class = paste("cd-tabpane", if (identical(keys[[i]], active)) "cd-tabpane--active" else NULL),
        `data-tab-key` = keys[[i]],
        panels[[i]]
      )
    })
  )
}

cd_update_tab_panes <- function(session, id, selected) {
  session$sendCustomMessage("cd-tab-switch", list(containerId = session$ns(id), activeKey = selected))
}
