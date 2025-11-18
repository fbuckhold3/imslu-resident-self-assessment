#' Assessment Review Wrapper Module UI
#'
#' Wrapper for all assessment visualizations
#' @param id Module namespace
#' @param use_custom_detail Use custom detail viz instead of gmed (default: TRUE)
#' @export
mod_assessment_wrapper_ui <- function(id, use_custom_detail = TRUE) {
  ns <- NS(id)

  tagList(
    # gmed visualizations
    gmed::mod_assessment_viz_wrapper_ui(ns("viz_wrapper")),

    # Add detailed assessment table section
    div(
      class = "card mt-4",
      div(
        class = "card-header bg-info text-white",
        h4(class = "mb-0", icon("table", class = "me-2"), "Detailed Assessment Records")
      ),
      div(
        class = "card-body",
        p(class = "text-muted",
          "Complete record of all faculty assessments including Plus feedback (what you're doing well) and Delta feedback (areas for growth)."),
        uiOutput(ns("assessment_table"))
      )
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

    # Call the gmed wrapper
    # Pass combined data that has both assessment and questions with redcap_repeat_instrument
    gmed::mod_assessment_viz_wrapper_server(
      "viz_wrapper",
      rdm_data = combined_data,
      rdm_data_raw = raw_assessment_data,
      record_id = record_id,
      data_dict = data_dict,
      include_questions = TRUE,
      resident_name = resident_name
    )

    # Render detailed assessment table
    output$assessment_table <- renderUI({
      req(raw_assessment_data(), record_id())

      # Filter to current resident's assessments
      resident_assessments <- raw_assessment_data() %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::arrange(desc(ass_date))

      if (nrow(resident_assessments) == 0) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle", class = "me-2"),
            "No assessment records found."
          )
        )
      }

      # Create formatted table with nice column names
      table_data <- resident_assessments %>%
        dplyr::select(
          Date = ass_date,
          Level = ass_level,
          Faculty = ass_faculty,
          Specialty = ass_specialty,
          `Plus Feedback` = ass_plus,
          `Delta Feedback` = ass_delta
        ) %>%
        dplyr::mutate(
          # Format date nicely
          Date = format(as.Date(Date), "%b %d, %Y"),
          # Replace NA with empty string for better display
          across(everything(), ~ifelse(is.na(.), "", .))
        )

      # Render as HTML table with Bootstrap styling
      div(
        class = "table-responsive",
        tags$table(
          class = "table table-striped table-hover",
          tags$thead(
            class = "table-light",
            tags$tr(
              lapply(names(table_data), function(col_name) {
                tags$th(col_name)
              })
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(table_data)), function(row_idx) {
              tags$tr(
                lapply(names(table_data), function(col_name) {
                  cell_value <- table_data[[col_name]][row_idx]

                  # Special formatting for Plus/Delta feedback - show full text
                  if (col_name %in% c("Plus Feedback", "Delta Feedback")) {
                    if (nchar(cell_value) > 0) {
                      tags$td(
                        style = "white-space: pre-wrap; max-width: 400px;",
                        cell_value
                      )
                    } else {
                      tags$td(
                        class = "text-muted",
                        style = "font-style: italic;",
                        "(none)"
                      )
                    }
                  } else {
                    tags$td(cell_value)
                  }
                })
              )
            })
          )
        )
      )
    })
  })
}