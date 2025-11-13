# ============================================================================
# PERIOD-BASED ASSESSMENT CONFIGURATION
# R/period_config.R
# ============================================================================

#' Determine Resident Period with Smart Defaults
#'
#' Automatically detects period with fallback to Period 7 for safety.
#' Period 7 is safest default as it's for new residents with minimal data.
#'
#' @param resident_data Data frame row with resident information
#' @param current_date Date. Defaults to Sys.Date()
#' @param force_period Numeric. If provided, overrides automatic detection
#' @return List with period information
#' @export
get_resident_period_smart <- function(resident_data, 
                                      current_date = Sys.Date(),
                                      force_period = NULL) {
  
  # Manual override takes precedence
  if (!is.null(force_period) && force_period %in% 1:7) {
    config <- get_period_structure(force_period)
    return(list(
      period_number = force_period,
      period_name = config$period_name,
      detection_method = "manual_override",
      is_default = FALSE
    ))
  }
  
  # Try automatic detection if we have required data
  if (!is.null(resident_data$graduation_year) && 
      !is.na(resident_data$graduation_year)) {
    
    # Try automatic detection if we have required data
if (!is.null(resident_data$graduation_year) && 
    !is.na(resident_data$graduation_year)) {
  
  tryCatch({  # ADD THIS LINE - YOU'RE MISSING IT!
    
    type_code <- if (is.character(resident_data$residency_type)) {
      if (tolower(resident_data$residency_type) == "categorical") 2 else 1
    } else {
      as.numeric(resident_data$residency_type)
    }
    
    period_calc <- calculate_pgy_and_period(
      grad_yr = resident_data$graduation_year,
      type = type_code,
      current_date = current_date
    )
    
    # Check if calculation is valid
    if (!is.null(period_calc$period_number) && 
        !is.na(period_calc$period_number) &&
        period_calc$period_number %in% 1:7 &&
        period_calc$is_valid) {
      
      config <- get_period_structure(period_calc$period_number)
      
      return(list(
        period_number = period_calc$period_number,
        period_name = config$period_name,
        pgy_year = period_calc$pgy_year,
        detection_method = "automatic",
        is_default = FALSE,
        calculation_details = period_calc
      ))
    }
    
  }, error = function(e) {
    message("Period calculation failed: ", e$message)
  })
}
  }
  
  # Default to Period 7 (safest for new/unknown residents)
  message("Using default Period 7 for resident ", 
          resident_data$record_id %||% "unknown")
  
  config <- get_period_structure(7)
  
  return(list(
    period_number = 7,
    period_name = config$period_name,
    detection_method = "default_fallback",
    is_default = TRUE,
    reason = "Missing graduation year or calculation failed"
  ))
}


#' Get Assessment Structure for Period
#'
#' Returns the appropriate module structure based on evaluation period
#'
#' @param period_number Numeric. Period number (1-7)
#' @return List with module configuration
#' @export
get_period_structure <- function(period_number) {
  
  # Validate period
  if (!period_number %in% 1:7) {
    stop("Period must be between 1-7")
  }
  
  # Period 7: Entering Residency (Interns only, July-Sept)
  if (period_number == 7) {
    return(list(
      period_name = "Entering Residency",
      period_number = 7,
      modules = c("skills_review", "learning_styles", "goals", 
                  "concerns", "milestone_self_eval"),
      module_titles = list(
        skills_review = "Skills Review",
        learning_styles = "Learning Styles and Topics",
        goals = "Goals",
        concerns = "Concerns Coming Into Residency",
        milestone_self_eval = "Milestone Self-Evaluation"
      ),
      skip_sections = c("scholarship", "program_feedback", "assessment_review", 
                       "ilp_generation", "career_planning", "board_prep", "graduation_data")
    ))
  }
  
  # Period 6: Graduation (PGY3, Feb-June)
  if (period_number == 6) {
    return(list(
      period_name = "Graduating",
      period_number = 6,
      modules = c("scholarship", "graduation_data", "program_feedback",
                  "assessment_review", "board_prep", "milestone_self_eval"),
      module_titles = list(
        scholarship = "Scholarship",
        graduation_data = "Graduation Data",
        program_feedback = "Program Feedback",
        assessment_review = "Assessment Review",
        board_prep = "Board Preparation",
        milestone_self_eval = "Milestone Self-Evaluation"
      ),
      skip_sections = c("career_planning", "ilp_generation", "learning", 
                       "skills_review", "goals", "concerns", "learning_styles")
    ))
  }
  
  # Periods 1-5: Standard Structure
return(list(
  period_name = switch(as.character(period_number),
                      "1" = "Mid Intern",
                      "2" = "End Intern", 
                      "3" = "Mid PGY2",
                      "4" = "End PGY2",
                      "5" = "Mid PGY3"),
  period_number = period_number,
  modules = c("scholarship", "career_planning", "program_feedback",    # CHANGE HERE
              "assessment_review", "learning", "milestone_self_eval", 
              "ilp_generation"),
  module_titles = list(
    scholarship = "Scholarship",
    career_planning = "Wellness and Career Planning",                  # CHANGE HERE
    program_feedback = "Program Feedback",
    assessment_review = "Assessment Review",
    learning = "Learning",
    milestone_self_eval = "Milestone Self-Evaluation",
    ilp_generation = "ILP Generation"
  ),
  skip_sections = c("skills_review", "graduation_data", "board_prep",
                   "goals", "concerns", "learning_styles")
))
}

#' Determine Current Period from Resident Data
#'
#' Calculates which period a resident should be in based on their
#' PGY year, residency type, and current date
#'
#' Determine Current Period from Resident Data
#'
#' @export
determine_resident_period <- function(grad_year, residency_type = "Categorical", 
                                     current_date = Sys.Date()) {
  
  # Use existing period calculation function
  period_calc <- gmed::calculate_pgy_and_period(
    grad_yr = grad_year,  # Note: function expects grad_yr
    type = residency_type,
    current_date = current_date
  )
  
  return(list(
    period_number = period_calc$period_number,
    period_name = period_calc$period_name,
    pgy_year = period_calc$pgy_year,
    is_valid = period_calc$is_valid
  ))
}

#' Get Module UI Function Name
#'
#' Returns the correct UI function name for a module
#'
#' @param module_id Character. Module identifier
#' @return Character. Function name
#' @export
get_module_ui_function <- function(module_id) {
  module_map <- c(
    scholarship = "mod_scholarship_ui",
    career_planning = "mod_career_planning_wrapper_ui",
    program_feedback = "mod_program_feedback_ui",
    assessment_review = "mod_assessment_review_ui",
    learning = "mod_learning_ui",  # ADD THIS LINE
    milestone_self_eval = "mod_milestone_entry_ui",
    ilp_generation = "mod_ilp_generation_ui",
    graduation_data = "mod_graduation_data_ui",
    board_prep = "mod_board_prep_ui",
    skills_review = "mod_skills_review_ui",
    learning_styles = "mod_learning_styles_ui",
    goals = "mod_goals_ui",
    concerns = "mod_concerns_ui"
  )
  
  module_map[module_id]
}

#' Get Module Server Function Name
#'
#' Returns the correct server function name for a module
#'
#' @param module_id Character. Module identifier
#' @return Character. Function name
#' @export
get_module_server_function <- function(module_id) {
  module_map <- c(
    scholarship = "mod_scholarship_server",
    career_planning = "mod_career_planning_wrapper_server",
    program_feedback = "mod_program_feedback_server",
    assessment_review = "mod_assessment_review_server",
    learning = "mod_learning_server",  # ADD THIS LINE
    milestone_self_eval = "mod_milestone_entry_server",
    ilp_generation = "mod_ilp_generation_server",
    graduation_data = "mod_graduation_data_server",
    board_prep = "mod_board_prep_server",
    skills_review = "mod_skills_review_server",
    learning_styles = "mod_learning_styles_server",
    goals = "mod_goals_server",
    concerns = "mod_concerns_server"
  )
  
  module_map[module_id]
}

# ============================================================================
# GOAL/ILP HELPER FUNCTIONS
# ============================================================================

#' Load Previous Goals from ILP Data
#'
#' @param ilp_data ILP data from REDCap
#' @param record_id Resident record ID
#' @param current_period Period name to look back from
#' @return Data frame row with previous goals, or NULL
#' @export
load_previous_goals <- function(ilp_data, record_id, current_period) {
  
  # Period name to number mapping
  period_to_number <- c(
    "Entering Residency" = 0,
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "Graduating" = 6
  )
  
  # Period sequence (which period comes before)
  period_sequence <- c(
    "Entering Residency" = NA,
    "Mid Intern" = "Entering Residency",
    "End Intern" = "Mid Intern",
    "Mid PGY2" = "End Intern",
    "End PGY2" = "Mid PGY2",
    "Mid PGY3" = "End PGY2",
    "Graduating" = "Mid PGY3"
  )
  
  # Get previous period name
  previous_period <- period_sequence[current_period]
  
  if (is.na(previous_period)) {
    return(NULL)
  }
  
  # Convert to number
  prev_period_num <- period_to_number[previous_period]
  
  if (is.na(prev_period_num)) {
    return(NULL)
  }
  
  # Look up in ILP data
  prev_data <- ilp_data %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instance == prev_period_num
    ) %>%
    dplyr::slice(1)
  
  if (nrow(prev_data) > 0) {
    return(prev_data)
  } else {
    return(NULL)
  }
}


#' Get Previous Period Name
#'
#' @param current_period Current period name
#' @return Previous period name, or NA if none exists
#' @export
get_previous_period <- function(current_period) {
  period_sequence <- c(
    "Entering Residency" = NA,
    "Mid Intern" = "Entering Residency",
    "End Intern" = "Mid Intern",
    "Mid PGY2" = "End Intern",
    "End PGY2" = "Mid PGY2",
    "Mid PGY3" = "End PGY2",
    "Graduating" = "Mid PGY3"
  )
  
  previous_period <- period_sequence[current_period]
  
  if (is.null(previous_period)) {
    return(NA_character_)
  }
  
  return(previous_period)
}