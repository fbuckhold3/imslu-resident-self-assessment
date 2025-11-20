#' Display Previous Wellness Data with Enhanced Styling
#'
#' Displays wellness data from previous period with subtle card styling
#' and history icon.
#'
#' @param rdm_data Data frame containing REDCap data OR data structure with $all_forms$s_eval
#' @param record_id Character or numeric record ID
#' @param current_period Current period number (1-7) or list with period_number, or character name
#'
#' @return Shiny UI element (card with wellness display) or NULL
#'
#' @importFrom shiny div icon h6 p
#' @importFrom dplyr filter
#' @export
display_wellness <- function(rdm_data, record_id, current_period) {

  # Handle different data structure formats
  if (!is.null(rdm_data$all_forms) && !is.null(rdm_data$all_forms$s_eval)) {
    s_eval_data <- rdm_data$all_forms$s_eval
  } else {
    s_eval_data <- rdm_data
  }

  # Convert period to number - handle all types
  current_period_num <- if (is.numeric(current_period)) {
    current_period
  } else if (is.list(current_period) && "period_number" %in% names(current_period)) {
    current_period$period_number
  } else if (is.character(current_period)) {
    period_map <- c(
      "Entering Residency" = 7, "Mid Intern" = 1, "End Intern" = 2,
      "Mid PGY2" = 3, "End PGY2" = 4, "Mid PGY3" = 5, "Graduating" = 6
    )
    period_map[current_period]
  } else {
    as.numeric(current_period)
  }

  # Get previous period data
  prev_period <- current_period_num - 1
  if (prev_period < 1) return(NULL)

  # Filter to previous period's data
  prev_data <- s_eval_data %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == prev_period
    )

  if (nrow(prev_data) == 0 || is.na(prev_data$s_e_well[1]) || prev_data$s_e_well[1] == "") {
    return(NULL)  # Don't show anything if no previous data
  }

  return(
    div(
      class = "card mb-3",
      style = "background-color: #f5f5f5; border-left: 4px solid #9575cd;",
      div(
        class = "card-body",
        h6(class = "card-subtitle mb-2 text-muted",
           icon("history"), " Previous Wellness (Period ", prev_period, ")"),
        p(class = "card-text", style = "font-style: italic;", prev_data$s_e_well[1])
      )
    )
  )
}
