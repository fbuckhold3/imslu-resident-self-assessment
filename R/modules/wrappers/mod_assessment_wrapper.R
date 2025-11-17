#' Assessment Review Wrapper Module UI
#'
#' Wrapper for all assessment visualizations
#' @param id Module namespace
#' @param use_custom_detail Use custom detail viz instead of gmed (default: TRUE)
#' @export
mod_assessment_wrapper_ui <- function(id, use_custom_detail = TRUE) {
  ns <- NS(id)

  tagList(
    # Call the gmed wrapper (without detail viz if using custom)
    gmed::mod_assessment_viz_wrapper_ui(ns("viz_wrapper")),

    # Add custom detail viz if requested
    if (use_custom_detail) {
      mod_assessment_detail_custom_ui(ns("custom_detail"))
    }
  )
}

#' Assessment Review Wrapper Module Server
#'
#' @param id Module namespace
#' @param rdm_data Reactive returning app data structure from load_rdm_complete
#' @param record_id Reactive returning resident record_id
#' @param period Reactive returning period number (not used but kept for consistency)
#' @param data_dict Data dictionary
#' @param use_custom_detail Use custom detail viz instead of gmed (default: TRUE)
#' @export
mod_assessment_wrapper_server <- function(id, rdm_data, record_id, period, data_dict, use_custom_detail = TRUE) {
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

    # Get resident data for CC completion
    resident_info_data <- reactive({
      req(rdm_data(), record_id())

      app_data <- rdm_data()

      app_data$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
    })

    # Prepare combined assessment + questions + faculty evaluation data
    # Need BOTH redcap_repeat_instrument AND source_form columns
    combined_assessment_questions <- reactive({
      req(rdm_data())

      app_data <- rdm_data()

      # Add source_form while preserving redcap_repeat_instrument
      # Include faculty_evaluation if it exists
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

    # Call the gmed wrapper
    # Pass combined data that has both assessment and questions with redcap_repeat_instrument
    gmed::mod_assessment_viz_wrapper_server(
      "viz_wrapper",
      rdm_data = combined_assessment_questions,
      rdm_data_raw = combined_assessment_questions,
      record_id = record_id,
      data_dict = data_dict,
      include_questions = TRUE,
      include_cc_completion = TRUE,
      resident_name = resident_name,
      resident_data = resident_info_data
    )

    # Initialize custom detail viz if requested
    if (use_custom_detail) {
      mod_assessment_detail_custom_server(
        "custom_detail",
        rdm_data = combined_assessment_questions,
        record_id = record_id,
        data_dict = data_dict
      )
    }
  })
}