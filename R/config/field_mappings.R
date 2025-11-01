# ============================================================================
# REDCAP FIELD MAPPINGS
# Central configuration for REDCap field names
# ============================================================================

#' Get Scholarship Field Mapping
#' @export
get_scholarship_fields <- function() {
  list(
    title = "scholar_title",
    type = "scholar_type",
    date = "scholar_date",
    description = "scholar_desc",
    authors = "scholar_authors",
    venue = "scholar_venue"
  )
}

#' Get Career Goals Field Mapping
#' @export
get_career_fields <- function() {
  list(
    career_path = "s_e_career_path",
    career_other = "s_e_career_oth",
    fellowship = "s_e_fellow",
    fellowship_other = "s_e_fellow_oth",
    track = "s_e_track",
    track_type = "s_e_track_type"
  )
}

#' Get Wellness Field Mapping
#' @export
get_wellness_fields <- function() {
  list(
    wellbeing_score = "wellness_score",
    stress_level = "stress_level",
    work_life_balance = "work_life",
    comments = "wellness_comments"
  )
}

#' Get Period-Specific Forms
#' @param period_number Integer 1-7
#' @export
get_period_forms <- function(period_number) {
  
  # Base forms for all periods
  base_forms <- c("self_evaluation")
  
  # Add period-specific forms
  period_forms <- switch(as.character(period_number),
    "7" = c("entering_residency"),
    "6" = c("graduation_data"),
    "1" = c("mid_intern"),
    "2" = c("end_intern"),
    "3" = c("mid_pgy2"),
    "4" = c("end_pgy2"),
    "5" = c("mid_pgy3"),
    character(0)
  )
  
  c(base_forms, period_forms)
}