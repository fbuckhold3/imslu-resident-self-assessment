# ============================================================================
# MODULE REGISTRY
# Maps module IDs to their UI/Server functions
# ============================================================================

#' Get Module Function Names
#'
#' @param module_id Character module identifier
#' @param data_dict Optional data dictionary for dynamic labels
#' @return List with ui_function and server_function names
#' @export
get_module_functions <- function(module_id, data_dict = NULL) {
  
  registry <- list(
    # Standard modules (periods 1-5)
    scholarship = list(
      ui = "mod_scholarship_wrapper_ui",
      server = "mod_scholarship_wrapper_server",
      title = "Scholarly Activities",
      icon = "graduation-cap"
    ),
    wellness = list(
      ui = "mod_wellness_entry_ui",
      server = "mod_wellness_entry_server",
      title = "Wellness and Career Planning",
      icon = "heart"
    ),
    program_feedback = list(
      ui = "mod_program_feedback_ui",
      server = "mod_program_feedback_server",
      title = "Program Feedback",
      icon = "comments"
    ),
    assessment_review = list(
      ui = "mod_assessment_wrapper_ui",
      server = "mod_assessment_wrapper_server",
      title = "Assessment Review",
      icon = "clipboard-check"
    ),
    learning = list(
      ui = "mod_learning_entry_ui",
      server = "mod_learning_entry_server",
      title = "Learning Activities",
      icon = "book"
    ),
    milestone_self_eval = list(
      ui = "mod_milestone_entry_ui",    # gmed module
      server = "mod_milestone_entry_server",
      title = "Milestone Self-Assessment",
      icon = "tasks"
    ),
    ilp_generation = list(
      ui = "mod_ilp_builder_ui",
      server = "mod_ilp_builder_server",
      title = "Individualized Learning Plan",
      icon = "bullseye"
    ),
    
    # Period 6 modules
    graduation_data = list(
      ui = "mod_graduation_data_ui",
      server = "mod_graduation_data_server",
      title = "Graduation Information",
      icon = "user-graduate"
    ),
    board_prep = list(
      ui = "mod_board_prep_ui",
      server = "mod_board_prep_server",
      title = "Board Preparation",
      icon = "stethoscope"
    ),
    
    # Period 7 modules
    skills_review = list(
      ui = "mod_skills_review_ui",
      server = "mod_skills_review_server",
      title = "Skills Review",
      icon = "hand-holding-medical"
    ),
    learning_styles = list(
      ui = "mod_learning_styles_ui",
      server = "mod_learning_styles_server",
      title = "Learning Styles",
      icon = "brain"
    ),
    goals = list(
      ui = "mod_goals_ui",
      server = "mod_goals_server",
      title = "Initial Goals",
      icon = "flag"
    ),
    concerns = list(
      ui = "mod_concerns_ui",
      server = "mod_concerns_server",
      title = "Concerns and Questions",
      icon = "question-circle"
    )
  )
  
  module_config <- registry[[module_id]]
  
  # Pull title from data dictionary if available
  if (!is.null(data_dict) && !is.null(module_config$redcap_field)) {
    dd_title <- data_dict %>%
      filter(field_name == module_config$redcap_field) %>%
      pull(field_label)
    
    if (length(dd_title) > 0) {
      module_config$title <- dd_title[1]
    }
  }
  
  return(module_config)
}

#' Standard Module Server Parameters
#' 
#' All modules should accept these parameters for consistency
#' @export
standard_module_params <- function() {
  list(
    id = "character - module namespace",
    rdm_data = "reactive - full app data structure",
    record_id = "reactive - current resident ID",
    period = "reactive or static - period information",
    data_dict = "reactive or static - data dictionary",
    context = "character - 'self', 'coach', or 'ccc'",
    read_only = "logical - whether in view-only mode",
    show_previous = "logical - show previous period data"
  )
}