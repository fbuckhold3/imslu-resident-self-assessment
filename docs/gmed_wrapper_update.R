# ============================================================================
# UPDATES FOR mod_assessment_viz_wrapper.R in gmed package
# ============================================================================

# Add to the UI function (mod_assessment_viz_wrapper_ui):
#
# After the assessment detail viz, before questions (if applicable):

mod_assessment_viz_wrapper_ui <- function(id,
                                          title = "Assessment Review",
                                          include_questions = TRUE,
                                          include_cc_completion = TRUE) {  # ADD THIS PARAMETER
  ns <- NS(id)

  tagList(
    h2(title),

    # Plus/Delta feedback table
    mod_plus_delta_table_ui(ns("plus_delta")),

    # Assessment progress charts
    assessment_viz_ui(ns("charts")),

    # CC COMPLETION STATUS - ADD THIS SECTION
    if (include_cc_completion) {
      mod_cc_completion_ui(ns("cc_completion"))
    },

    # Detailed assessment breakdown
    mod_assessment_detail_viz_ui(ns("details")),

    # Questions/conference attendance (optional)
    if (include_questions) {
      mod_questions_viz_ui(ns("questions"))
    }
  )
}

# ============================================================================

# Add to the SERVER function (mod_assessment_viz_wrapper_server):
#
# After the detail viz, before questions:

mod_assessment_viz_wrapper_server <- function(id,
                                              rdm_data,
                                              record_id,
                                              data_dict,
                                              include_questions = TRUE,
                                              include_cc_completion = TRUE,  # ADD THIS PARAMETER
                                              resident_name = NULL,
                                              resident_data = NULL,  # ADD THIS PARAMETER
                                              rdm_data_raw = NULL) {
  moduleServer(id, function(input, output, session) {

    # Resident name fallback
    if (is.null(resident_name)) {
      resident_name <- reactive({
        paste("Resident", record_id())
      })
    }

    # Choose data source for plus_delta
    data_for_plus_delta <- if (!is.null(rdm_data_raw)) {
      rdm_data_raw
    } else {
      rdm_data
    }

    # Plus/Delta table
    mod_plus_delta_table_server(
      "plus_delta",
      rdm_data = data_for_plus_delta,
      record_id = record_id,
      data_dict = data_dict,
      resident_name = resident_name
    )

    # Assessment charts
    assessment_viz_server(
      "charts",
      rdm_data = rdm_data,
      record_id = record_id,
      data_dict = data_dict
    )

    # CC COMPLETION STATUS - ADD THIS SECTION
    if (include_cc_completion) {
      mod_cc_completion_server(
        "cc_completion",
        rdm_data = rdm_data,
        record_id = record_id,
        resident_data = resident_data  # Pass resident info if available
      )
    }

    # Detailed assessment breakdown
    mod_assessment_detail_viz_server(
      "details",
      rdm_data = rdm_data,
      record_id = record_id,
      data_dict = data_dict
    )

    # Questions visualization (optional)
    if (include_questions) {
      mod_questions_viz_server(
        "questions",
        rdm_data = rdm_data,
        record_id = record_id,
        data_dict = data_dict
      )
    }
  })
}
