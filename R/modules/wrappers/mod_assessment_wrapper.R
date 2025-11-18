#' Assessment Review Wrapper Module UI
#'
#' Wrapper for all assessment visualizations
#' @param id Module namespace
#' @param use_custom_detail Use custom detail viz instead of gmed (default: TRUE)
#' @export
mod_assessment_wrapper_ui <- function(id, use_custom_detail = TRUE) {
  ns <- NS(id)

  tagList(
    # Call individual gmed modules instead of full wrapper to avoid duplicate detail viz

    # Plus/Delta feedback table
    gmed::mod_plus_delta_table_ui(ns("plus_delta"), title = "Recent Feedback"),

    # Assessment progress charts
    gmed::assessment_viz_ui(ns("charts"), title = "Assessment Progress"),

    # CC Completion Status
    gmed::mod_cc_completion_ui(ns("cc_completion")),

    # Questions/conference attendance
    gmed::mod_questions_viz_ui(ns("questions"), title = "Conference Attendance by Rotation"),

    # Add custom detail viz (replaces gmed's detail viz to avoid duplication)
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

    # Get raw assessment data for table display
    raw_assessment_data <- reactive({
      req(rdm_data())

      app_data <- rdm_data()

      # Return the assessment data from the data structure
      if ("assessment" %in% names(app_data)) {
        return(app_data$assessment)
      } else if ("all_forms" %in% names(app_data) && "assessment" %in% names(app_data$all_forms)) {
        return(app_data$all_forms$assessment)
      } else {
        return(data.frame())  # Return empty df if no assessment data
      }
    })

    # Prepare combined assessment + questions + faculty evaluation data
    # Need BOTH redcap_repeat_instrument AND source_form columns
    combined_data <- reactive({
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

    # Call individual gmed modules (instead of wrapper to avoid duplicate detail viz)

    # Plus/Delta table
    gmed::mod_plus_delta_table_server(
      "plus_delta",
      rdm_data = raw_assessment_data,
      record_id = record_id
    )

    # Assessment charts
    gmed::assessment_viz_server(
      "charts",
      data = combined_data,
      record_id = record_id,
      resident_name = resident_name
    )

    # CC Completion Status
    gmed::mod_cc_completion_server(
      "cc_completion",
      rdm_data = combined_data,
      record_id = record_id,
      resident_data = resident_info_data
    )

    # Questions/conference attendance
    gmed::mod_questions_viz_server(
      "questions",
      rdm_data = combined_data,
      record_id = record_id,
      data_dict = data_dict
    )

    # Initialize custom detail viz (replaces gmed's detail viz to avoid duplication)
    if (use_custom_detail) {
      mod_assessment_detail_custom_server(
        "custom_detail",
        rdm_data = combined_data,
        record_id = record_id,
        data_dict = data_dict
      )
    }
  })
}