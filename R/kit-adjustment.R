# The Data adjustment page's editor (js/src/components/AdjustmentEditor.tsx): the data kept for analysis (years
# removed everywhere, an area's data removed for some or every year) and the adjustment settings (completeness by
# k-factor, outliers, missing values) everywhere or with a region's or district's own settings, with a summary and
# Adjust data / Edit. The settings are cd2030.core's (adjust_service_data(settings =)):
#
#   list(removed_years = c(2019),
#        removals = list(list(area, level = "adminlevel_1" | "district", region, years = c(2021))),  # years: none = all
#        everywhere = list(group_k = c(anc = 0.25), indicator_k = c(anc1 = 0), outliers = c(csection = FALSE),
#                          missing = c(maternal_deaths = FALSE)),
#        areas = list(list(area, level, region, group_k, indicator_k, outliers, missing,
#                          all_outliers = NULL | TRUE | FALSE, all_missing = NULL | TRUE | FALSE, reported = FALSE)))
#
# The editor reports input$<inputId> = list(event = "adjust" | "edit" | "cancel", settings (with "adjust"), nonce);
# the server answers with cd_update_adjustment_editor() (the saved settings, adjusted, busy, a message).

.adj_text_keys <- c(
  "btn_adj_adjust", "btn_adj_adjust_again", "btn_adj_area_settings", "btn_adj_cancel", "btn_adj_cancel_dlg",
  "btn_adj_close", "btn_adj_close_dlg", "btn_adj_confirm_remove", "btn_adj_confirm_scope", "btn_adj_default",
  "btn_adj_edit", "btn_adj_keep_it", "btn_adj_keep_reported", "btn_adj_open", "btn_adj_remove_area", "btn_adj_reset",
  "btn_adj_use_everywhere", "lbl_adj_all_group", "lbl_adj_all_inherited", "lbl_adj_also_removed", "lbl_adj_as_reported",
  "lbl_adj_changed_for", "lbl_adj_changed_list", "lbl_adj_col_indicator", "lbl_adj_col_k", "lbl_adj_col_missing",
  "lbl_adj_col_outliers", "lbl_adj_district", "lbl_adj_district_in", "lbl_adj_every_year", "lbl_adj_everywhere",
  "lbl_adj_find", "lbl_adj_fn_area", "lbl_adj_fn_default", "lbl_adj_fn_removal_all", "lbl_adj_fn_removal_years",
  "lbl_adj_fn_years", "lbl_adj_footnote", "lbl_adj_group_here", "lbl_adj_group_k_opt", "lbl_adj_inherited", "lbl_adj_k",
  "lbl_adj_k_group", "lbl_adj_k_one", "lbl_adj_kept_hint", "lbl_adj_missing_all", "lbl_adj_missing_one", "lbl_adj_months",
  "lbl_adj_n_changed", "lbl_adj_no_match", "lbl_adj_none", "lbl_adj_outliers_all", "lbl_adj_outliers_one",
  "lbl_adj_outliers_only", "lbl_adj_own", "lbl_adj_own_list", "lbl_adj_region", "lbl_adj_region_n", "lbl_adj_removed_line",
  "lbl_adj_reporting", "lbl_adj_settings_hint", "lbl_adj_status_adjusted", "lbl_adj_status_editing", "lbl_adj_status_not",
  "lbl_adj_sum_as_everywhere", "lbl_adj_sum_group_k", "lbl_adj_sum_no_missing", "lbl_adj_sum_no_outliers",
  "lbl_adj_sum_own_k", "lbl_adj_sum_removals", "lbl_adj_sum_reported", "lbl_adj_sum_rest", "lbl_adj_sum_scopes",
  "lbl_adj_sum_years", "lbl_adj_sum_years_value", "lbl_adj_type_district", "lbl_adj_when", "lbl_adj_where",
  "lbl_adj_years_everywhere", "msg_adj_busy", "msg_adj_editing", "msg_adj_editing_more", "msg_adj_locked",
  "msg_adj_locked_more", "msg_adj_remove_note", "title_adj_dlg_remove", "title_adj_dlg_scope", "title_adj_kept",
  "title_adj_settings", "title_adj_summary"
)

# JSON shapes that survive R -> JS: a map is an object even when empty; a list of values is an array even with one
.adj_map <- function(x) {
  if (is.null(x) || !length(x)) return(stats::setNames(list(), character()))
  x <- as.list(x)
  x[!vapply(x, function(v) is.null(v) || (length(v) == 1 && is.na(v)), logical(1))]
}
.adj_array <- function(x) if (is.null(x)) list() else as.list(unname(x))
.adj_drop_null <- function(x) x[!vapply(x, is.null, logical(1))]
.adj_scope <- function(s) {
  list(group_k = .adj_map(s$group_k), indicator_k = .adj_map(s$indicator_k), outliers = .adj_map(s$outliers),
       missing = .adj_map(s$missing))
}

# The settings as the editor takes them
.adj_settings_props <- function(s) {
  s <- s %||% list()
  list(
    removed_years = .adj_array(as.numeric(s$removed_years)),
    removals = lapply(s$removals %||% list(), function(r) {
      .adj_drop_null(list(area = r$area, level = r$level, region = r$region, years = .adj_array(as.numeric(r$years))))
    }),
    everywhere = .adj_scope(s$everywhere),
    areas = lapply(s$areas %||% list(), function(a) {
      .adj_drop_null(c(
        list(area = a$area, level = a$level, region = a$region),
        .adj_scope(a),
        list(all_outliers = a$all_outliers, all_missing = a$all_missing, reported = isTRUE(a$reported))
      ))
    })
  )
}

# The indicator groups: list(list(id, label (a translation key or LocalText), k, missing, rr, below,
# indicators = list(list(id, label, outliers, missing))))
.adj_groups_props <- function(groups, i18n) {
  label <- function(x) if (is.character(x) && length(x) == 1) cd_text(i18n, x) else x
  num <- function(x) if (is.null(x) || !length(x) || is.na(x[[1]])) NULL else as.numeric(x[[1]])
  lapply(groups, function(g) {
    .adj_drop_null(list(
      id = g$id, label = label(g$label), k = isTRUE(g$k %||% TRUE), missing = isTRUE(g$missing %||% TRUE),
      rr = num(g$rr), below = num(g$below),
      indicators = lapply(g$indicators, function(ind) {
        .adj_drop_null(list(id = ind$id, label = label(ind$label), outliers = num(ind$outliers), missing = num(ind$missing)))
      })
    ))
  })
}

# The regions and their districts: list(list(region, rr, districts = list(list(name, rr))))
.adj_areas_props <- function(areas) {
  num <- function(x) if (is.null(x) || !length(x) || is.na(x[[1]])) NULL else as.numeric(x[[1]])
  lapply(areas, function(r) {
    .adj_drop_null(list(
      region = r$region, rr = num(r$rr),
      districts = lapply(r$districts, function(d) .adj_drop_null(list(name = d$name, rr = num(d$rr))))
    ))
  })
}

.adj_message_props <- function(message, i18n) {
  if (is.null(message) || !length(message)) return(stats::setNames(list(), character()))
  text <- message$text
  if (is.character(text) && length(text) == 1) text <- cd_text(i18n, text)
  list(type = message$type %||% "info", text = text)
}

#' @rdname interface-kit
cd_adjustment_editor <- function(inputId, i18n = cd_i18n(), years, groups, areas, settings, defaults, adjusted = FALSE,
                                 rr_year = NULL, threshold = 90, rr_cutoff = 75, k_choices = c(0, 0.25, 0.5, 0.75, 1),
                                 busy = FALSE, message = NULL) {
  cd_react_element("AdjustmentEditor", do.call(shiny.react::asProps, .adj_drop_null(list(
    inputId = inputId,
    texts = stats::setNames(lapply(.adj_text_keys, function(k) cd_text(i18n, k)), .adj_text_keys),
    years = .adj_array(as.numeric(years)),
    groups = .adj_groups_props(groups, i18n),
    areas = .adj_areas_props(areas),
    rrYear = if (!is.null(rr_year)) as.numeric(rr_year),
    threshold = threshold,
    rrCutoff = rr_cutoff,
    kChoices = .adj_array(as.numeric(k_choices)),
    defaults = .adj_settings_props(defaults),
    settings = .adj_settings_props(settings),
    adjusted = isTRUE(adjusted),
    busy = isTRUE(busy),
    message = .adj_message_props(message, i18n)
  ))))
}

#' @rdname interface-kit
cd_update_adjustment_editor <- function(session, inputId, i18n = cd_i18n(), settings = NULL, adjusted = NULL, busy = NULL,
                                        message = NULL, years = NULL, groups = NULL, areas = NULL, rr_year = NULL,
                                        defaults = NULL) {
  props <- .adj_drop_null(list(
    settings = if (!is.null(settings)) .adj_settings_props(settings),
    defaults = if (!is.null(defaults)) .adj_settings_props(defaults),
    adjusted = if (!is.null(adjusted)) isTRUE(adjusted),
    busy = if (!is.null(busy)) isTRUE(busy),
    # message = list() clears it
    message = if (!is.null(message)) .adj_message_props(message, i18n),
    years = if (!is.null(years)) .adj_array(as.numeric(years)),
    groups = if (!is.null(groups)) .adj_groups_props(groups, i18n),
    areas = if (!is.null(areas)) .adj_areas_props(areas),
    rrYear = if (!is.null(rr_year)) as.numeric(rr_year)
  ))
  if (length(props)) do.call(cd_update_input, c(list(inputId = inputId, session = session), props))
  invisible(NULL)
}
