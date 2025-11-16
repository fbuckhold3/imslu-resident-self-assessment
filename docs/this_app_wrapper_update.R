# ============================================================================
# UPDATE FOR mod_assessment_wrapper.R in THIS APP
# (After adding CC completion module to gmed)
# ============================================================================

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

    # ADD THIS: Get resident data for CC completion
    resident_info_data <- reactive({
      req(rdm_data(), record_id())

      app_data <- rdm_data()

      app_data$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
    })

    # Prepare combined assessment + questions data
    # Need BOTH redcap_repeat_instrument AND source_form columns
    combined_assessment_questions <- reactive({
      req(rdm_data())

      app_data <- rdm_data()

      # Add source_form while preserving redcap_repeat_instrument
      combined <- bind_rows(
        app_data$all_forms$assessment %>% mutate(source_form = "assessment"),
        app_data$all_forms$questions %>% mutate(source_form = "questions")
      )

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
      include_cc_completion = TRUE,  # ADD THIS
      resident_name = resident_name,
      resident_data = resident_info_data  # ADD THIS
    )
  })
}
