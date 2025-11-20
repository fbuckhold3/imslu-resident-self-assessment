#' Assessment Review Wrapper Module UI

#'

#' Wrapper for all assessment visualizations

#' @param id Module namespace

#' @export

mod_assessment_wrapper_ui <- function(id) {

  ns <- NS(id)

  tagList(
  # Introductory text
  div(
    class = "alert alert-info mb-4",
    p("Please take some time to review your assessments. Specific assessments are below as well as a summary of all the Plus (what you do well) and Delta (Areas to improve) comments.")
  ),

  # Call individual gmed modules in correct order, using custom detail viz

  # Assessment progress charts
  gmed::assessment_viz_ui(ns("charts"), title = "Assessment Progress"),

  # Custom detail viz from gmed (replaces default detail viz)
  gmed::mod_assessment_detail_custom_ui(ns("custom_detail")),

  # Custom data display for selected evaluation (NEW!)
  gmed::mod_assessment_data_display_ui(ns("data_display")),

  # CC Completion Status
  gmed::mod_cc_completion_ui(ns("cc_completion")),

  # Questions/conference attendance
  gmed::mod_questions_viz_ui(ns("questions"), title = "Conference Attendance by Rotation"),

  # Plus/Delta feedback table
  gmed::mod_plus_delta_table_ui(ns("plus_delta"), title = "Plus / Delta Feedback"),

  # Reflection section
  div(
    class = "card mt-4",
    div(
      class = "card-header",
      h4("Assessment Reflection")
    ),
    div(
      class = "card-body",
      p("Based on this review, take a note of:"),

      # Plus feedback
      div(
        class = "mb-3",
        tags$label(
          tags$strong("What you do well (Plus)")
        ),
        textAreaInput(
          ns("s_e_plus"),
          label = NULL,
          placeholder = "Reflect on the positive feedback you've received...",
          rows = 4,
          width = "100%"
        )
      ),

      # Delta feedback
      div(
        class = "mb-3",
        tags$label(
          tags$strong("Areas to improve (Delta)")
        ),
        textAreaInput(
          ns("s_e_delta"),
          label = NULL,
          placeholder = "Reflect on areas where you can improve...",
          rows = 4,
          width = "100%"
        )
      ),

      # Submit button
      div(
        class = "text-center mt-3",
        actionButton(
          ns("submit_reflection"),
          "Submit Assessment Reflection",
          class = "btn btn-primary btn-lg",
          icon = icon("save")
        )
      ),

      # Status message
      uiOutput(ns("submit_status"))
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

 

    # Get raw assessment data for plus/delta table

    raw_assessment_data <- reactive({

      req(rdm_data())

      app_data <- rdm_data()

 

      if ("assessment" %in% names(app_data$all_forms)) {

        return(app_data$all_forms$assessment)

      } else {

        return(data.frame())

      }

    })

 

    # Prepare combined assessment + questions + faculty evaluation data

    combined_data <- reactive({

      req(rdm_data())

 

      app_data <- rdm_data()

 

      # Add source_form while preserving redcap_repeat_instrument

      if ("faculty_evaluation" %in% names(app_data$all_forms)) {

        combined <- dplyr::bind_rows(

          app_data$all_forms$assessment %>% dplyr::mutate(source_form = "assessment"),

          app_data$all_forms$questions %>% dplyr::mutate(source_form = "questions"),

          app_data$all_forms$faculty_evaluation %>% dplyr::mutate(source_form = "faculty_evaluation")

        )

      } else {

        combined <- dplyr::bind_rows(

          app_data$all_forms$assessment %>% dplyr::mutate(source_form = "assessment"),

          app_data$all_forms$questions %>% dplyr::mutate(source_form = "questions")

        )

      }

 

      return(combined)

    })

 

    # Call individual gmed modules in correct order

 

    # Assessment charts

    gmed::assessment_viz_server(

      "charts",

      data = combined_data,

      record_id = record_id,

      resident_name = resident_name

    )

 

# Custom detail viz from gmed - returns reactive values for data display
detail_viz_state <- gmed::mod_assessment_detail_custom_server(
  "custom_detail",
  rdm_data = combined_data,
  record_id = record_id,
  data_dict = data_dict
)

# Custom data display for selected evaluation (NEW!)
gmed::mod_assessment_data_display_server(
  "data_display",
  selected_category = detail_viz_state$selected_category,
  category_data = detail_viz_state$category_data,
  data_dict = data_dict
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

 

    # Plus/Delta table

    gmed::mod_plus_delta_table_server(

      "plus_delta",

      rdm_data = raw_assessment_data,

      record_id = record_id

    )

    # Load existing reflection data
    observe({
      req(rdm_data(), record_id(), period())

      # Get current period number
      period_num <- if (is.character(period())) {
        switch(period(),
               "Entering Residency" = 7,
               "Mid Intern" = 1,
               "End Intern" = 2,
               "Mid PGY2" = 3,
               "End PGY2" = 4,
               "Mid PGY3" = 5,
               "Graduating" = 6,
               1)
      } else {
        as.numeric(period())
      }

      # Get s_eval data for current period
      app_data <- rdm_data()
      if ("s_eval" %in% names(app_data$all_forms)) {
        s_eval_data <- app_data$all_forms$s_eval %>%
          dplyr::filter(
            record_id == !!record_id(),
            redcap_repeat_instrument == "s_eval",
            redcap_repeat_instance == !!period_num
          )

        if (nrow(s_eval_data) > 0) {
          # Update text areas with existing data
          if (!is.na(s_eval_data$s_e_plus[1]) && s_eval_data$s_e_plus[1] != "") {
            updateTextAreaInput(session, "s_e_plus", value = s_eval_data$s_e_plus[1])
          }
          if (!is.na(s_eval_data$s_e_delta[1]) && s_eval_data$s_e_delta[1] != "") {
            updateTextAreaInput(session, "s_e_delta", value = s_eval_data$s_e_delta[1])
          }
        }
      }
    })

    # Handle reflection submission
    observeEvent(input$submit_reflection, {
      req(record_id(), period())

      # Get period number
      period_num <- if (is.character(period())) {
        switch(period(),
               "Entering Residency" = 7,
               "Mid Intern" = 1,
               "End Intern" = 2,
               "Mid PGY2" = 3,
               "End PGY2" = 4,
               "Mid PGY3" = 5,
               "Graduating" = 6,
               1)
      } else {
        as.numeric(period())
      }

      # Prepare reflection data
      reflection_data <- list(
        s_e_plus = input$s_e_plus %||% "",
        s_e_delta = input$s_e_delta %||% "",
        s_e_period = as.character(period_num),
        s_e_date = format(Sys.Date(), "%Y-%m-%d")
      )

      # Submit to REDCap using gmed function
      result <- tryCatch({
        gmed::submit_self_eval_data(
          record_id = record_id(),
          period = period_num,
          data = reflection_data
        )
      }, error = function(e) {
        list(success = FALSE, message = paste("Error:", e$message))
      })

      # Show result
      if (result$success) {
        output$submit_status <- renderUI({
          div(
            class = "alert alert-success mt-3",
            icon("check-circle"),
            " Assessment reflection saved successfully!"
          )
        })

        # Trigger data refresh
        if (!is.null(session$userData$refresh_trigger)) {
          session$userData$refresh_trigger(session$userData$refresh_trigger() + 1)
        }
      } else {
        output$submit_status <- renderUI({
          div(
            class = "alert alert-danger mt-3",
            icon("exclamation-triangle"),
            " Error saving reflection: ", result$message
          )
        })
      }
    })

  })

}