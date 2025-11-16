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

    # Raw assessment data with redcap_repeat_instrument column
    # This is what the detail viz component needs to filter properly
    raw_assessment_data <- reactive({
      req(rdm_data())
      rdm_data()$all_forms$assessment
    })

    # Call the gmed wrapper
    # Pass raw assessment data as main rdm_data so detail viz can filter on
    # redcap_repeat_instrument == "assessment"
    # The include_questions = TRUE parameter handles questions display separately
    gmed::mod_assessment_viz_wrapper_server(
      "viz_wrapper",
      rdm_data = raw_assessment_data,
      rdm_data_raw = raw_assessment_data,
      record_id = record_id,
      data_dict = data_dict,
      include_questions = TRUE,
      resident_name = resident_name
    )
  })
}