# ============================================================================
# UPDATED mod_assessment_wrapper.R
# This shows how to use the gmed assessment detail category module
# once it's installed in the gmed package
# ============================================================================

#' Assessment Review Wrapper Module UI
#'
#' Wrapper for all assessment visualizations
#' @param id Module namespace
#' @export
mod_assessment_wrapper_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # gmed assessment visualizations (charts, plus/delta, CC completion)
    gmed::mod_assessment_viz_wrapper_ui(ns("viz_wrapper")),

    # gmed category detail module (NEW - replaces custom module)
    gmed::mod_assessment_detail_category_ui(
      ns("detail_category"),
      title = "Detailed Assessment Analysis"
    )
  )
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

    # Call the gmed assessment viz wrapper
    # This includes: plus/delta table, charts, CC completion, questions
    gmed::mod_assessment_viz_wrapper_server(
      "viz_wrapper",
      rdm_data = combined_data,
      rdm_data_raw = raw_assessment_data,
      record_id = record_id,
      data_dict = data_dict,
      include_questions = TRUE,
      include_cc_completion = TRUE,
      resident_name = resident_name,
      resident_data = resident_info_data
    )

    # Call the gmed category detail module (NEW - replaces custom module)
    # This provides category-based visualization with filtered Plus/Delta table
    gmed::mod_assessment_detail_category_server(
      "detail_category",
      rdm_data = combined_data,
      record_id = record_id,
      data_dict = data_dict
    )
  })
}
