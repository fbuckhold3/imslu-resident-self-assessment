# ============================================================================
# PROGRAM FEEDBACK MODULE
# Simple text entry for 4 program feedback questions
# ============================================================================

#' Program Feedback Module UI
#' 
#' @param id Module namespace ID
#' @export
mod_program_feedback_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "section-container",

      # Header with instructions
      div(
        class = "alert alert-info",
        h4(icon("comments"), " Program Feedback"),
        p("Please provide feedback to the program. This data is collated anonymously and reviewed by the Program Evaluation Committee (PEC).
          As part of that review, we use AI to find common themes for the PEC to review as well."),
        p(strong("While any feedback is helpful, specific scenarios and suggestions for improvement are more helpful."))
      ),

      # Display previous period feedback
      uiOutput(ns("previous_feedback_display")),
      
      # Question 1: Plus
      div(
        class = "question-group",
        h5(class = "question-header", "What is the Program doing well?"),
        textAreaInput(
          ns("s_e_prog_plus"),
          label = "Name at least one thing the Program is doing well:",
          placeholder = "Provide specific examples of what's working well...",
          width = "100%",
          rows = 4
        )
      ),
      
      # Question 2: Delta
      div(
        class = "question-group",
        h5(class = "question-header", "What can the Program improve?"),
        textAreaInput(
          ns("s_e_prog_delta"),
          label = "Name at least one thing the Program can improve on:",
          placeholder = "Provide specific examples and suggestions for improvement...",
          width = "100%",
          rows = 4
        )
      ),
      
      # Question 3: Conference feedback
      div(
        class = "question-group",
        h5(class = "question-header", "Conference Feedback"),
        textAreaInput(
          ns("s_e_progconf"),
          label = "What is going well with conferences, and what do you suggest to improve?",
          placeholder = "If you feel the time of conference is an issue, please mention it, but include something else as well...",
          width = "100%",
          rows = 4
        )
      ),
      
      # Question 4: Broader issues
      div(
        class = "question-group",
        h5(class = "question-header", "Broader Issues"),
        textAreaInput(
          ns("s_e_progfeed"),
          label = "What are some broader issues that affect the Program?",
          placeholder = "Consider issues within the Department, VA, SSM, hospital, or clinical units that affect the Program. This helps with strategic planning and advocacy...",
          width = "100%",
          rows = 5
        )
      ),
      
      # Save button
      div(
        class = "mt-4 text-center",
        actionButton(
          ns("save_feedback"),
          "Save Feedback",
          class = "btn-primary btn-lg",
          icon = icon("save")
        )
      ),
      
      # Status message
      uiOutput(ns("save_status"))
    )
  )
}

#' Program Feedback Module Server
#' 
#' @param id Module namespace ID
#' @param rdm_data Reactive containing app data structure
#' @param record_id Reactive containing current resident ID
#' @param period Reactive or static period information (optional)
#' @param data_dict Data dictionary (optional)
#' @export
mod_program_feedback_server <- function(id, rdm_data, record_id, period = NULL, data_dict = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for tracking
    rv <- reactiveValues(
      save_in_progress = FALSE,
      last_save_time = NULL
    )

    # Display previous period feedback
    output$previous_feedback_display <- renderUI({
      req(rdm_data(), period, record_id())

      gmed::display_program_feedback(
        rdm_data = rdm_data(),
        record_id = record_id(),
        current_period = if (is.reactive(period)) period() else period
      )
    })

    # Period number to name mapping
    period_num_to_name <- c(
      "7" = "Entering Residency",
      "1" = "Mid Intern",
      "2" = "End Intern",
      "3" = "Mid PGY2",
      "4" = "End PGY2",
      "5" = "Mid PGY3",
      "6" = "Graduating"
    )

    period_name_to_num <- c(
      "Entering Residency" = 7,
      "Mid Intern" = 1,
      "End Intern" = 2,
      "Mid PGY2" = 3,
      "End PGY2" = 4,
      "Mid PGY3" = 5,
      "Graduating" = 6,
      "Graduation" = 6
    )

    # Load existing data for CURRENT period when resident/period changes
    observe({
      req(rdm_data(), record_id(), period)

      # Convert period to number
      current_period_num <- if (is.reactive(period)) {
        p <- period()
        if (is.numeric(p)) p
        else if (is.list(p) && "period_number" %in% names(p)) p$period_number
        else if (is.character(p)) period_name_to_num[p] %||% as.numeric(p)
        else as.numeric(p)
      } else {
        if (is.numeric(period)) period
        else if (is.list(period) && "period_number" %in% names(period)) period$period_number
        else if (is.character(period)) period_name_to_num[period] %||% as.numeric(period)
        else as.numeric(period)
      }

      # Get expected period name for this period number
      current_period_name <- period_num_to_name[as.character(current_period_num)]

      message("DEBUG: Loading data for period ", current_period_num, " (", current_period_name, ")")

      # Get s_eval form data for this resident and period
      if (!is.null(rdm_data()$all_forms) && !is.null(rdm_data()$all_forms$s_eval)) {
        current_data <- rdm_data()$all_forms$s_eval %>%
          dplyr::filter(
            record_id == !!record_id(),
            redcap_repeat_instrument == "s_eval",
            !is.na(s_e_period),
            s_e_period == !!current_period_name
          )

        message("DEBUG: Found ", nrow(current_data), " rows for current period")

        if (nrow(current_data) > 0) {
          # Update inputs with existing data from CURRENT period
          if (!is.na(current_data$s_e_prog_plus[1]) && current_data$s_e_prog_plus[1] != "") {
            updateTextAreaInput(session, "s_e_prog_plus", value = current_data$s_e_prog_plus[1])
          }
          if (!is.na(current_data$s_e_prog_delta[1]) && current_data$s_e_prog_delta[1] != "") {
            updateTextAreaInput(session, "s_e_prog_delta", value = current_data$s_e_prog_delta[1])
          }
          if (!is.na(current_data$s_e_progconf[1]) && current_data$s_e_progconf[1] != "") {
            updateTextAreaInput(session, "s_e_progconf", value = current_data$s_e_progconf[1])
          }
          if (!is.na(current_data$s_e_progfeed[1]) && current_data$s_e_progfeed[1] != "") {
            updateTextAreaInput(session, "s_e_progfeed", value = current_data$s_e_progfeed[1])
          }
        }
      }
    })
    
    # Save handler
    observeEvent(input$save_feedback, {

      message("========================================")
      message("PROGRAM FEEDBACK SAVE BUTTON CLICKED")
      message("========================================")

      # Check if we have record_id
      if (is.null(record_id()) || length(record_id()) == 0) {
        message("ERROR: No record_id available")
        showNotification("Error: No record ID", type = "error", duration = 10)
        return()
      }

      message("Record ID: ", record_id())

      rv$save_in_progress <- TRUE

      # Get period info - handle all possible formats
      current_period <- if (is.reactive(period)) {
        message("Period is reactive, getting value...")
        period()
      } else {
        message("Period is static")
        period
      }

      message("Current period raw: '", current_period, "'")
      message("Current period class: ", class(current_period))

      # Convert period to number - robust handling
      period_number <- if (is.numeric(current_period)) {
        # Already a number
        message("Period is numeric: ", current_period)
        current_period
      } else if (is.list(current_period) && "period_number" %in% names(current_period)) {
        # It's a list from active_period() with period_number field
        message("Period is list, extracting period_number: ", current_period$period_number)
        current_period$period_number
      } else if (is.character(current_period)) {
        # It's a period name string
        pn <- switch(current_period,
               "Entering Residency" = 7,
               "Mid Intern" = 1,
               "End Intern" = 2,
               "Mid PGY2" = 3,
               "End PGY2" = 4,
               "Mid PGY3" = 5,
               "Graduating" = 6,
               1)  # Default fallback
        message("Converted period name to number: ", pn)
        pn
      } else if (is.null(current_period)) {
        message("Period is NULL, defaulting to 1")
        1
      } else {
        # Try to coerce to numeric
        message("Unknown period type, attempting coercion: ", current_period)
        as.numeric(current_period)
      }

      message("Final period_number: ", period_number)
      
      # Prepare field data
      feedback_data <- list(
        s_e_prog_plus = input$s_e_prog_plus %||% "",
        s_e_prog_delta = input$s_e_prog_delta %||% "",
        s_e_progconf = input$s_e_progconf %||% "",
        s_e_progfeed = input$s_e_progfeed %||% "",
        s_e_period = as.character(period_number),
        s_e_date = format(Sys.Date(), "%Y-%m-%d")
      )

      # Add feedback fields
      submit_data$s_e_prog_plus <- input$s_e_prog_plus %||% ""
      submit_data$s_e_prog_delta <- input$s_e_prog_delta %||% ""
      submit_data$s_e_progconf <- input$s_e_progconf %||% ""
      submit_data$s_e_progfeed <- input$s_e_progfeed %||% ""

      message("Feedback data:")
      message("  s_e_prog_plus: ", nchar(submit_data$s_e_prog_plus), " chars")
      message("  s_e_prog_delta: ", nchar(submit_data$s_e_prog_delta), " chars")
      message("  s_e_progconf: ", nchar(submit_data$s_e_progconf), " chars")
      message("  s_e_progfeed: ", nchar(submit_data$s_e_progfeed), " chars")
      message("Submitting to REDCap with instance = ", period_number)

      # Submit to REDCap using REDCapR directly (like Career Planning module)
      result <- tryCatch({
        result_obj <- REDCapR::redcap_write_oneshot(
          ds = submit_data,
          redcap_uri = app_config$redcap_url,
          token = app_config$rdm_token
        )

        if (result_obj$success) {
          list(success = TRUE, message = "Data saved successfully")
        } else {
          list(success = FALSE, message = result_obj$outcome_message)
        }
      }, error = function(e) {
        message("ERROR in submission: ", e$message)
        list(success = FALSE, message = paste("R error:", e$message))
      })

      rv$save_in_progress <- FALSE

      message("Submission result received:")
      message("  Success: ", result$success)
      message("  Message: ", result$message)

      if (result$success) {
        rv$last_save_time <- Sys.time()
        showNotification(
          "Program feedback saved successfully!",
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste("Error saving feedback:", result$message),
          type = "error",
          duration = 10
        )
      }

      message("========================================")
      message("SAVE PROCESS COMPLETE")
      message("========================================")
    })
    
    # Status output
    output$save_status <- renderUI({
      if (rv$save_in_progress) {
        div(
          class = "alert alert-warning mt-3",
          icon("spinner", class = "fa-spin"),
          " Saving..."
        )
      } else if (!is.null(rv$last_save_time)) {
        div(
          class = "alert alert-success mt-3",
          icon("check-circle"),
          " Last saved: ", format(rv$last_save_time, "%I:%M %p")
        )
      }
    })
    
    # Return reactive values for parent module
    return(list(
      has_changes = reactive({
        any(
          nzchar(input$s_e_prog_plus %||% ""),
          nzchar(input$s_e_prog_delta %||% ""),
          nzchar(input$s_e_progconf %||% ""),
          nzchar(input$s_e_progfeed %||% "")
        )
      }),
      is_valid = reactive({
        any(
          nzchar(input$s_e_prog_plus %||% ""),
          nzchar(input$s_e_prog_delta %||% ""),
          nzchar(input$s_e_progconf %||% ""),
          nzchar(input$s_e_progfeed %||% "")
        )
      })
    ))
  })
}