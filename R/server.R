# ============================================================================
# SERVER LOGIC
# RDM 2.0 Resident Self-Assessment
# ============================================================================

server <- function(input, output, session) {

  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================

  values <- reactiveValues(
    authenticated = FALSE,
    current_resident = NULL,
    resident_info = NULL,
    app_data = NULL,
    data_loaded = FALSE
  )

  # ============================================================================
  # DATA LOADING ON STARTUP - with progress messages
  # ============================================================================

  observe({
    # Run once on startup
    isolate({
      if (!values$data_loaded) {

        # Update loading message
        shinyjs::html("loading_message", "Connecting to REDCap database...")

        # Small delay to ensure UI renders
        Sys.sleep(0.5)

        tryCatch({
          # Update message
          shinyjs::html("loading_message", "Loading resident data...")

          # Load the data
          values$app_data <- load_app_data()

          # Update message
          shinyjs::html("loading_message", "Processing assessments...")
          Sys.sleep(0.3)

          # Update message
          shinyjs::html("loading_message", "Preparing milestones...")
          Sys.sleep(0.3)

          # Mark as loaded
          values$data_loaded <- TRUE

          # Update message
          shinyjs::html("loading_message", "Ready!")
          Sys.sleep(0.2)

          # Hide the loading overlay
          shinyjs::hide("loading_overlay", anim = TRUE, animType = "fade")

#           message("App data loaded successfully on startup")

        }, error = function(e) {
          # Show error message
          shinyjs::html("loading_message",
                       paste0("Error loading data: ", e$message,
                             "<br>Please refresh the page or contact support."))
          showNotification("Failed to load application data. Please refresh the page.",
                         type = "error", duration = NULL)
        })
      }
    })
  })

  # ============================================================================
  # AUTHENTICATION - Using gmed universal function
  # ============================================================================

  observeEvent(input$submit_code, {
    req(input$access_code_input, values$data_loaded)

    # Data is already loaded, just authenticate
    auth_result <- gmed::authenticate_resident(
      access_code = input$access_code_input,
      residents_df = values$app_data$residents,
      allow_record_id = TRUE,
      debug = app_config$debug_mode
    )

    if (auth_result$success) {
      # Success!
      values$authenticated <- TRUE
      values$resident_info <- auth_result$resident_info
      values$current_resident <- auth_result$resident_info$record_id

      # Navigate to intro page
      updateNavbarPage(session, "main_nav", selected = "intro")

      showNotification(auth_result$message, type = "message")

    } else {
      # Failed
      shinyjs::show("access_code_error")
      updateTextInput(session, "access_code_input", value = "")
      showNotification(auth_result$message, type = "warning")
    }
  })
  
# ============================================================================
# ACTIVE PERIOD WITH OVERRIDE
# ============================================================================

active_period <- reactive({
  req(values$current_resident, values$app_data$residents)
  
  # Check for manual override
  if (!is.null(input$period_override) && input$period_override != "auto") {
    override_num <- as.numeric(input$period_override)
    config <- get_period_structure(override_num)
    
    return(list(
      period_number = override_num,
      period_name = config$period_name,
      period_code = as.character(override_num),
      detection_method = "manual_override",
      is_override = TRUE
    ))
  }

    # ILP data reactive - extract from complete data structure
  ilp_data <- reactive({
    req(values$app_data)
    
    if ("ilp" %in% names(values$app_data$all_forms)) {
      values$app_data$all_forms$ilp
    } else {
      NULL
    }
  })
  
  # Use automatic detection
  resident_info <- values$app_data$residents %>%
    filter(record_id == values$current_resident) %>%
    slice(1)
  
  if (nrow(resident_info) == 0) return(NULL)
  
  # With raw data, type is already a code
  type_code <- as.numeric(resident_info$type)
  
  # Validate type
  if (is.na(type_code) || !type_code %in% c(1, 2)) {
#     message("Invalid type code: ", resident_info$type)
    return(NULL)
  }
  
  # grad_yr is a CODED value - need to translate it
  grad_yr_code <- as.character(resident_info$grad_yr)
  grad_yr_field <- values$app_data$data_dict %>%
    filter(field_name == "grad_yr") %>%
    slice(1)
  
  if (nrow(grad_yr_field) == 0) {
#     message("grad_yr field not found in data dictionary")
    return(NULL)
  }
  
  # Parse the choices to get the actual year
  grad_yr_choices <- gmed::parse_redcap_choices(
    grad_yr_field$select_choices_or_calculations[1]
  )
  
  # Get the actual year from the code
  grad_yr_actual <- as.numeric(grad_yr_choices[grad_yr_code])
  
  if (is.na(grad_yr_actual) || grad_yr_actual < 2020 || grad_yr_actual > 2040) {
# message("Invalid grad_yr after translation. Code: ", grad_yr_code, " -> Year: ", grad_yr_actual)
    return(NULL)
  }
  
# message("Translated grad_yr: code ", grad_yr_code, " -> year ", grad_yr_actual)
  
  # Calculate the period - WITH DEBUG
#   message("About to call calculate_pgy_and_period")
#   message("grad_yr_actual: ", grad_yr_actual)
#   message("type_code: ", type_code)
  
  detected_period <- tryCatch({
# message("Calling with: grad_yr=", grad_yr_actual, ", type=", type_code)
    gmed::calculate_pgy_and_period(
      grad_yr = grad_yr_actual,
      type = type_code,
      current_date = Sys.Date()
    )
  }, error = function(e) {
#     message("FULL ERROR: ", e$message)
#     message("ERROR CALL: ", deparse(e$call))
    NULL
  })
  
  # Return the period info
  if (!is.null(detected_period) && detected_period$period_number %in% 1:7) {
    return(list(
      period_number = detected_period$period_number,
      period_name = detected_period$period_name,
      period_code = as.character(detected_period$period_number),
      pgy_year = detected_period$pgy_year,
      detection_method = "automatic",
      is_override = FALSE
    ))
  }
  
  return(NULL)
})
  
  # ============================================================================
  # INTRO PAGE OUTPUTS
  # ============================================================================

  # Initialize intro page checklist
  mod_completion_checklist_server(
    "intro_checklist",
    rdm_data = reactive(values$app_data),
    record_id = reactive(values$current_resident),
    period = active_period,
    show_details = TRUE
  )

  output$resident_name_display_intro <- renderText({
    req(values$resident_info)
    values$resident_info$name %||% "Resident"
  })
  
  output$resident_level_display_intro <- renderText({
    req(values$resident_info, active_period())
    period_info <- active_period()
    
    level_text <- switch(as.character(period_info$pgy_year),
                        "1" = "PGY-1 (Intern)",
                        "2" = "PGY-2",
                        "3" = "PGY-3",
                        "Unknown")
    
    level_text
  })
  
  output$resident_period_display_intro <- renderText({
    req(values$resident_info, active_period())
    period_info <- active_period()
    
    paste0(period_info$period_name, " (Period ", period_info$period_number, ")")
  })
  
  output$resident_academic_year_display_intro <- renderText({
    year <- as.numeric(format(Sys.Date(), "%Y"))
    month <- as.numeric(format(Sys.Date(), "%m"))
    
    if (month >= 7) {
      paste0(year, "-", year + 1)
    } else {
      paste0(year - 1, "-", year)
    }
  })
  
  output$resident_grad_year_display_intro <- renderText({
  req(values$resident_info, values$app_data$data_dict)
  
  # With raw data, need to translate grad_yr code to actual year
  grad_yr_code <- as.character(values$resident_info$grad_yr)
  grad_yr_field <- values$app_data$data_dict %>%
    filter(field_name == "grad_yr") %>%
    slice(1)
  
  grad_yr_choices <- gmed::parse_redcap_choices(
    grad_yr_field$select_choices_or_calculations[1]
  )
  
  as.character(grad_yr_choices[grad_yr_code]) %||% "Unknown"
})
  
  output$resident_coach_display_intro <- renderText({
    req(values$resident_info, values$app_data$data_dict)

    coach_code <- values$resident_info$coach

    if (is.null(coach_code) || is.na(coach_code) || coach_code == "") {
      return("Not assigned")
    }

    # Translate coach code to name using data dictionary
    coach_field <- values$app_data$data_dict %>%
      dplyr::filter(field_name == "coach") %>%
      dplyr::slice(1)

    if (nrow(coach_field) > 0) {
      coach_choices <- gmed::parse_redcap_choices(
        coach_field$select_choices_or_calculations[1]
      )
      coach_name <- coach_choices[as.character(coach_code)]
      return(coach_name %||% paste("Coach #", coach_code))
    }

    return(paste("Coach #", coach_code))
  })

  output$resident_coach_email_display_intro <- renderUI({
    req(values$resident_info)

    email <- values$resident_info$coach_email

    if (!is.null(email) && nchar(trimws(email)) > 0) {
      tags$a(
        href = paste0("mailto:", email),
        icon("envelope", class = "me-1"),
        email,
        class = "text-decoration-none"
      )
    } else {
      tags$span(class = "text-muted", "No email on file")
    }
  })

  output$resident_track_display_intro <- renderText({
    req(values$resident_info)

    # With raw data, type is a code (1 or 2)
    type_code <- as.numeric(values$resident_info$type)

    switch(as.character(type_code),
      "1" = "Preliminary",
      "2" = "Categorical",
      "Unknown")
  })

  # ============================================================================
  # COMPACT VERSIONS FOR SIDE-BY-SIDE LAYOUT
  # ============================================================================

  output$resident_level_display_intro_compact <- renderText({
    req(values$resident_info, active_period())
    period_info <- active_period()

    level_text <- switch(as.character(period_info$pgy_year),
                        "1" = "PGY-1",
                        "2" = "PGY-2",
                        "3" = "PGY-3",
                        "Unknown")

    level_text
  })

  output$resident_track_display_intro_compact <- renderText({
    req(values$resident_info)

    type_code <- as.numeric(values$resident_info$type)

    switch(as.character(type_code),
      "1" = "Preliminary",
      "2" = "Categorical",
      "Unknown")
  })

  output$resident_period_display_intro_compact <- renderText({
    req(values$resident_info, active_period())
    period_info <- active_period()

    paste0(period_info$period_name, " (P", period_info$period_number, ")")
  })

  output$resident_academic_year_display_intro_compact <- renderText({
    year <- as.numeric(format(Sys.Date(), "%Y"))
    month <- as.numeric(format(Sys.Date(), "%m"))

    if (month >= 7) {
      paste0(year, "-", year + 1)
    } else {
      paste0(year - 1, "-", year)
    }
  })

  output$resident_grad_year_display_intro_compact <- renderText({
    req(values$resident_info, values$app_data$data_dict)

    grad_yr_code <- as.character(values$resident_info$grad_yr)
    grad_yr_field <- values$app_data$data_dict %>%
      dplyr::filter(field_name == "grad_yr") %>%
      dplyr::slice(1)

    grad_yr_choices <- gmed::parse_redcap_choices(
      grad_yr_field$select_choices_or_calculations[1]
    )

    as.character(grad_yr_choices[grad_yr_code]) %||% "Unknown"
  })

  output$resident_coach_display_intro_compact <- renderText({
    req(values$resident_info, values$app_data$data_dict)

    coach_code <- values$resident_info$coach

    if (is.null(coach_code) || is.na(coach_code) || coach_code == "") {
      return("Not assigned")
    }

    coach_field <- values$app_data$data_dict %>%
      dplyr::filter(field_name == "coach") %>%
      dplyr::slice(1)

    if (nrow(coach_field) > 0) {
      coach_choices <- gmed::parse_redcap_choices(
        coach_field$select_choices_or_calculations[1]
      )
      coach_name <- coach_choices[as.character(coach_code)]
      return(coach_name %||% paste("Coach #", coach_code))
    }

    return(paste("Coach #", coach_code))
  })

  output$resident_coach_email_display_intro_compact <- renderUI({
    req(values$resident_info)

    email <- values$resident_info$coach_email

    if (!is.null(email) && nchar(trimws(email)) > 0) {
      tags$a(
        href = paste0("mailto:", email),
        icon("envelope", class = "me-1"),
        email,
        class = "text-decoration-none small"
      )
    } else {
      tags$span(class = "text-muted small", "No email on file")
    }
  })
  
  # ============================================================================
  # PORTFOLIO CONTENT - DYNAMIC BASED ON CURRENT MODULE
  # ============================================================================
  
  current_module_index <- reactiveVal(1)
  
  output$portfolio_content <- renderUI({
  req(values$authenticated, active_period())
  
  period_info <- active_period()
  config <- get_period_structure(period_info$period_number)
  modules <- config$modules
  module_titles <- config$module_titles
  
  current_index <- current_module_index()
  module_key <- modules[current_index]
  
  div(
    # Module content
    uiOutput(paste0("module_", module_key)),
    
    # Navigation buttons
    div(
      class = "d-flex justify-content-between mt-5 pt-4 border-top",
      
      if (current_index > 1) {
        actionButton(
          "nav_prev_module",
          "Previous",
          class = "btn-outline-secondary",
          icon = icon("arrow-left")
        )
      } else {
        div()
      },
      
      if (current_index < length(modules)) {
        actionButton(
          "nav_next_module",
          "Next",
          class = "btn-primary",
          icon = icon("arrow-right")
        )
      } else {
        actionButton(
          "nav_complete",
          "Complete",
          class = "btn-success",
          icon = icon("check")
        )
      }
    )
  )
})
  
  # ============================================================================
# PROGRESS STEPPER
# ============================================================================

output$progress_stepper <- renderUI({
  req(active_period())
  
  period_info <- active_period()
  config <- get_period_structure(period_info$period_number)
  modules <- config$modules
  module_titles <- config$module_titles
  current_index <- current_module_index()
  
  div(
    class = "stepper-container",
    lapply(seq_along(modules), function(i) {
      module_key <- modules[i]
      is_active <- (i == current_index)
      is_completed <- (i < current_index)
      
      circle_class <- if (is_active) {
        "stepper-circle active"
      } else if (is_completed) {
        "stepper-circle completed"
      } else {
        "stepper-circle incomplete"
      }
      
      label_class <- if (is_active) {
        "stepper-label active"
      } else {
        "stepper-label"
      }
      
      line_class <- if (is_completed) {
        "stepper-line completed"
      } else {
        "stepper-line"
      }
      
      # Make each stepper item clickable using tags$a instead of actionLink
      tags$a(
        href = "#",
        class = "stepper-item text-decoration-none",
        onclick = sprintf("Shiny.setInputValue('nav_to_module_%d', Math.random());", i),
        div(
          div(class = circle_class, i),
          div(class = label_class, module_titles[[module_key]]),
          div(class = line_class)
        )
      )
    })
  )
})
  
  # ============================================================================
# STEPPER NAVIGATION - Allow clicking to jump to modules
# ============================================================================

observe({
  req(active_period())
  
  period_info <- active_period()
  config <- get_period_structure(period_info$period_number)
  modules <- config$modules
  
  # Create observers for each module click
  lapply(seq_along(modules), function(i) {
    observeEvent(input[[paste0("nav_to_module_", i)]], {
      # Allow navigation to any module (free navigation)
      current_module_index(i)
      
      # Alternative: Only allow backward or to current
      # if (i <= current_module_index()) {
      #   current_module_index(i)
      # } else {
      #   showNotification("Please complete modules in order", type = "warning", duration = 3)
      # }
    })
  })
})
  
  # ============================================================================
  # RENDER MODULE UIs DYNAMICALLY
  # ============================================================================
  
  observe({
    req(values$authenticated, active_period())
    
    period_info <- active_period()
    config <- get_period_structure(period_info$period_number)
    modules <- config$modules
    
    # Render UI for each module
    lapply(modules, function(module_key) {
      output[[paste0("module_", module_key)]] <- renderUI({
        
        # Get the module config
        module_config <- get_module_functions(module_key, values$app_data$data_dict)
        
        # Get the UI function name and call it
        ui_function_name <- module_config$ui
        ui_function <- get(ui_function_name, mode = "function")
        
        # Call the UI function with the module's namespace
        ui_function(paste0("wrapper_", module_key))
      })
    })
  })
  
# ============================================================================
# INITIALIZE MODULE SERVERS
# ============================================================================

# Use observeEvent with once=TRUE to ensure modules are initialized exactly once
observeEvent(
  {
    values$authenticated
    values$app_data
    values$current_resident
    active_period()
  },
  {
    req(values$authenticated, values$app_data, values$current_resident, active_period())

    period_info <- active_period()
    config <- get_period_structure(period_info$period_number)
    modules <- config$modules

    # Storage for module outputs
    if (is.null(values$module_outputs)) {
      values$module_outputs <- reactiveValues()
    }

    # Initialize server for each module
    lapply(modules, function(module_key) {

      switch(module_key,
        "scholarship" = {
          values$module_outputs[[module_key]] <- mod_scholarship_wrapper_server(
            paste0("wrapper_", module_key),
            rdm_data = reactive(values$app_data$all_forms$scholarship),
            record_id = reactive(values$current_resident),
            period = reactive(period_info$period_number),
            data_dict = values$app_data$data_dict
          )
        },
      "career_planning" = {
        values$module_outputs[[module_key]] <- mod_career_planning_wrapper_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data$all_forms$s_eval),
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict
        )
      },
      "program_feedback" = {
        values$module_outputs[[module_key]] <- mod_program_feedback_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data),
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict
        )
      },
      "assessment_review" = {
        values$module_outputs[[module_key]] <- mod_assessment_wrapper_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data),
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict
        )
      },
      "learning" = {
        values$module_outputs[[module_key]] <- mod_learning_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data),
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict
        )
      },
      "milestone_self_eval" = {
        values$module_outputs[[module_key]] <- mod_milestone_entry_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data),
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_name)
        )
      },
      "ilp_generation" = {
        values$module_outputs[[module_key]] <- mod_goals_wrapper_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data),
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict,
          milestone_output = reactive({
            if (!is.null(values$module_outputs$milestone_self_eval)) {
              values$module_outputs$milestone_self_eval
            } else {
              NULL
            }
          })
        )
      }
    )
  },
  once = TRUE,
  ignoreInit = TRUE
)
# ============================================================================
# NAVIGATION EVENT HANDLERS
# ============================================================================

  # Navigate from intro to first module
  observeEvent(input$nav_intro_next, {
    req(active_period())
    current_module_index(1)
    updateNavbarPage(session, "main_nav", selected = "portfolio")
  })
  
  # Previous button
  observeEvent(input$nav_prev_module, {
    current_index <- current_module_index()
    if (current_index > 1) {
      current_module_index(current_index - 1)
    }
  })
  
  # Back to intro button
  observeEvent(input$nav_back_to_intro, {
    updateNavbarPage(session, "main_nav", selected = "intro")
  })
  
  # Next button
  observeEvent(input$nav_next_module, {
    req(active_period())
    period_info <- active_period()
    config <- get_period_structure(period_info$period_number)
    
    current_index <- current_module_index()
    if (current_index < length(config$modules)) {
      current_module_index(current_index + 1)
    }
  })
  
  # Complete button
  observeEvent(input$nav_complete, {
    updateNavbarPage(session, "main_nav", selected = "complete")
  })
  
  # ============================================================================
  # PERIOD DETECTION TEST AND DEBUG
  # ============================================================================
  
  observe({
    req(values$current_resident, values$app_data$residents, active_period())
    
    period_info <- active_period()
    resident_info <- values$app_data$residents %>%
      filter(record_id == values$current_resident) %>%
      slice(1)
    
    if (nrow(resident_info) > 0) {
# message("=== PERIOD DETECTION TEST ===")
# message("Resident: ", values$current_resident)
# message("Type: ", resident_info$type)
# message("Grad Year: ", resident_info$grad_yr)
# message("Period Number: ", period_info$period_number)
# message("Period Name: ", period_info$period_name)
# message("PGY Year: ", period_info$pgy_year)
      
      config <- get_period_structure(period_info$period_number)
# message("Modules for this period: ", paste(config$modules, collapse = ", "))
    }
  })

  # Add this observe block in server.R
observe({
  if (!is.null(input$nav_intro_next) && input$nav_intro_next > 0) {
# message("Button clicked! Count: ", input$nav_intro_next)
# message("Authenticated: ", values$authenticated)
# message("Active period exists: ", !is.null(active_period()))
    if (!is.null(active_period())) {
# message("Period number: ", active_period()$period_number)
    }
  }
})

  # ============================================================================
  # COMPLETION PAGE - Initialize checklist and ILP summary
  # ============================================================================

  # Completion page checklist
  mod_completion_checklist_server(
    "completion_checklist",
    rdm_data = reactive(values$app_data),
    record_id = reactive(values$current_resident),
    period = active_period,
    show_details = TRUE
  )

  # ILP Summary
  mod_ilp_summary_server(
    "ilp_summary",
    rdm_data = reactive(values$app_data),
    record_id = reactive(values$current_resident),
    period = reactive(active_period()$period_number),
    data_dict = values$app_data$data_dict
  )

  # Return to intro from completion page
  observeEvent(input$return_to_start, {
    updateNavbarPage(session, "main_nav", selected = "intro")
  })
}