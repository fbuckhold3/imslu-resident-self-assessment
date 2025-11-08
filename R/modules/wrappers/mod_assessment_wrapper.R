#' Assessment Review Wrapper Module UI
#' 
#' Wrapper for all assessment visualizations
#' @param id Module namespace
#' @export
mod_assessment_wrapper_ui <- function(id) {
  ns <- NS(id)
  
  # Just call the gmed wrapper
  gmed::mod_assessment_viz_wrapper_ui(ns("viz_wrapper"))
}

#' Assessment Review Wrapper Module Server
#' 
#' @param id Module namespace
#' @param rdm_data Reactive returning app data structure from load_rdm_complete
#' @param record_id Reactive returning resident record_id
#' @param period Reactive returning period number (not used but kept for consistency)
#' @param data_dict Data dictionary
#' @export
mod_assessment_wrapper_server <- function(id, rdm_data, record_id, period, data_dict) {
  moduleServer(id, function(input, output, session) {
    
    # Get resident name separately
    resident_name <- reactive({
      req(rdm_data(), record_id())
      
      app_data <- rdm_data()
      
      # Get name from residents table
      resident_info <- app_data$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
      
      if (nrow(resident_info) > 0 && !is.na(resident_info$name)) {
        return(resident_info$name)
      } else {
        return(paste("Resident", record_id()))
      }
    })
    
    # Prepare combined data for charts
    combined_data <- reactive({
      req(rdm_data())
      
      app_data <- rdm_data()
      
      if ("faculty_evaluation" %in% names(app_data$all_forms)) {
        combined <- bind_rows(
          app_data$all_forms$assessment %>% mutate(source_form = "assessment"),
          app_data$all_forms$questions %>% mutate(source_form = "questions"),
          app_data$all_forms$faculty_evaluation %>% mutate(source_form = "faculty_evaluation")
        )
      } else {
        combined <- bind_rows(
          app_data$all_forms$assessment %>% mutate(source_form = "assessment"),
          app_data$all_forms$questions %>% mutate(source_form = "questions")
        )
      }
      
      return(combined)
    })
    
    # Raw assessment data for plus_delta (expects redcap_repeat_instrument)
    raw_assessment_data <- reactive({
      req(rdm_data())
      rdm_data()$all_forms$assessment
    })
    
    # Call the gmed wrapper with BOTH data sources
    gmed::mod_assessment_viz_wrapper_server(
      "viz_wrapper",
      rdm_data = combined_data,
      rdm_data_raw = raw_assessment_data,  # ADD THIS
      record_id = record_id,
      data_dict = data_dict,
      include_questions = TRUE,
      resident_name = resident_name
    )
  })
}