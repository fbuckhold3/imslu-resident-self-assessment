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
    
    # Load existing data when resident changes
    # Load existing data when resident changes
    observe({
      req(rdm_data(), record_id())
      
      # Get s_eval form data for this resident
      if (!is.null(rdm_data()$all_forms) && !is.null(rdm_data()$all_forms$s_eval)) {
        s_eval_data <- rdm_data()$all_forms$s_eval %>%
          dplyr::filter(record_id == !!record_id())
        
        if (nrow(s_eval_data) > 0) {
          # Load most recent data
          latest_data <- s_eval_data %>% 
            dplyr::filter(!is.na(s_e_period)) %>%
            dplyr::arrange(desc(redcap_repeat_instance)) %>%
            dplyr::slice(1)
          
          # Update inputs with existing data
          if (nrow(latest_data) > 0) {
            if (!is.na(latest_data$s_e_prog_plus) && latest_data$s_e_prog_plus != "") {
              updateTextAreaInput(session, "s_e_prog_plus", value = latest_data$s_e_prog_plus)
            }
            if (!is.na(latest_data$s_e_prog_delta) && latest_data$s_e_prog_delta != "") {
              updateTextAreaInput(session, "s_e_prog_delta", value = latest_data$s_e_prog_delta)
            }
            if (!is.na(latest_data$s_e_progconf) && latest_data$s_e_progconf != "") {
              updateTextAreaInput(session, "s_e_progconf", value = latest_data$s_e_progconf)
            }
            if (!is.na(latest_data$s_e_progfeed) && latest_data$s_e_progfeed != "") {
              updateTextAreaInput(session, "s_e_progfeed", value = latest_data$s_e_progfeed)
            }
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
      
      # Get period info
      current_period <- if (is.reactive(period)) {
        message("Period is reactive, getting value...")
        period()
      } else {
        message("Period is static")
        period
      }
      
      message("Current period raw: '", current_period, "'")
      message("Current period class: ", class(current_period))
      
      # Convert period name to number if necessary
      period_number <- if (is.character(current_period)) {
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
        message("Using period as-is: ", current_period)
        current_period
      }
      
      message("Final period_number: ", period_number)
      
      # Prepare field data
      feedback_data <- list(
        s_e_prog_plus = input$s_e_prog_plus %||% "",
        s_e_prog_delta = input$s_e_prog_delta %||% "",
        s_e_progconf = input$s_e_progconf %||% "",
        s_e_progfeed = input$s_e_progfeed %||% ""
      )
      
      message("Feedback data:")
      message("  s_e_prog_plus: ", nchar(feedback_data$s_e_prog_plus), " chars")
      message("  s_e_prog_delta: ", nchar(feedback_data$s_e_prog_delta), " chars")
      message("  s_e_progconf: ", nchar(feedback_data$s_e_progconf), " chars")
      message("  s_e_progfeed: ", nchar(feedback_data$s_e_progfeed), " chars")
      
      # Check if submit_self_eval_data function exists
      if (!exists("submit_self_eval_data")) {
        message("ERROR: submit_self_eval_data function does not exist!")
        rv$save_in_progress <- FALSE
        showNotification(
          "Error: Submission function not found. Check console for details.",
          type = "error",
          duration = 10
        )
        return()
      }
      
      message("Calling submit_self_eval_data...")
      
      # Call the submission function
        # Call the submission function - now uses gmed package
      result <- tryCatch({
        gmed::submit_self_eval_data(  # Explicitly call gmed version
          record_id = record_id(),
          period = period_number,
          data = feedback_data
        )
      }, error = function(e) {
        message("ERROR in submit_self_eval_data: ", e$message)
        list(success = FALSE, message = paste("R error:", e$message))
      })
      
      rv$save_in_progress <- FALSE
      
      message("Submission result received:")
      message("  Success: ", result$success)
      message("  Message: ", result$message)
      if (!is.null(result$instance)) {
        message("  Instance: ", result$instance)
      }
      
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