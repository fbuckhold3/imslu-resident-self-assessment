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
    app_data = NULL
  )

  # ============================================================================
  # NAVIGATION CONTROL
  # ============================================================================
  
  # Define workflow
  workflow <- c("access", "intro", "scholarly", "wellbeing", 
                "plusdelta", "learning", "milestones", "ilp", "complete")
  
  current_step <- reactiveVal(1)
  
  # Progress indicator
  output$progress_indicator <- renderUI({
    req(current_step())
    
    if (current_step() == 1) return(NULL)  # Don't show on access page
    
    total_steps <- length(workflow) - 1  # Exclude access page from count
    current <- current_step() - 1  # Adjust for access page
    
    div(
      style = "min-width: 200px; padding: 8px;",
      tags$small(
        class = "text-muted me-2",
        sprintf("Step %d of %d", current, total_steps)
      ),
      tags$div(
        class = "progress",
        style = "height: 8px;",
        tags$div(
          class = "progress-bar bg-primary",
          role = "progressbar",
          style = paste0("width: ", round((current / total_steps) * 100), "%;")
        )
      )
    )
  })
  
  # Navigation functions
  go_to_step <- function(step_number) {
    if (step_number >= 1 && step_number <= length(workflow)) {
      current_step(step_number)
      updateNavbarPage(session, "main_nav", selected = workflow[step_number])
    }
  }
  
  # Navigation button observers
  observeEvent(input$nav_intro_next, { go_to_step(3) })
  
  observeEvent(input$nav_scholarly_prev, { go_to_step(2) })
  observeEvent(input$nav_scholarly_next, { go_to_step(4) })
  
  observeEvent(input$nav_wellbeing_prev, { go_to_step(3) })
  observeEvent(input$nav_wellbeing_next, { go_to_step(5) })
  
  observeEvent(input$nav_plusdelta_prev, { go_to_step(4) })
  observeEvent(input$nav_plusdelta_next, { go_to_step(6) })
  
  observeEvent(input$nav_learning_prev, { go_to_step(5) })
  observeEvent(input$nav_learning_next, { go_to_step(7) })
  
  observeEvent(input$nav_milestones_prev, { go_to_step(6) })
  observeEvent(input$nav_milestones_next, { go_to_step(8) })
  
  observeEvent(input$nav_ilp_prev, { go_to_step(7) })
  observeEvent(input$nav_ilp_next, { go_to_step(9) })
  
  observeEvent(input$return_to_start, { 
    # Reset authentication and go back to start
    values$authenticated <- FALSE
    values$current_resident <- NULL
    values$resident_info <- NULL
    go_to_step(1)
  })

    # ============================================================================
  # PERIOD AND RESIDENT INFO CALCULATION
  # ============================================================================
  
# Calculate current period info for resident
  resident_period_info <- reactive({
    req(values$resident_info, values$app_data)
    
    # Debug what we're getting
    if (app_config$debug_mode) {
      message("=== CALCULATING PERIOD INFO ===")
      message("Raw grad_yr: '", values$resident_info$grad_yr, "'")
      message("  Class: ", class(values$resident_info$grad_yr))
      message("Raw type: '", values$resident_info$type, "'")
      message("  Class: ", class(values$resident_info$type))
    }
    
    # Convert grad_yr
    grad_yr_clean <- suppressWarnings(as.numeric(values$resident_info$grad_yr))
    
    # Convert type - handle both numeric codes and text labels
    type_value <- values$resident_info$type
    if (is.character(type_value)) {
      # Convert text to numeric code
      type_clean <- switch(tolower(trimws(type_value)),
                          "preliminary" = 1,
                          "categorical" = 2,
                          "prelim" = 1,
                          "cat" = 2,
                          suppressWarnings(as.numeric(type_value)))
    } else {
      type_clean <- suppressWarnings(as.numeric(type_value))
    }
    
    if (app_config$debug_mode) {
      message("After conversion:")
      message("  grad_yr_clean: ", grad_yr_clean)
      message("  type_clean: ", type_clean)
    }
    
    # Check if conversion worked
    if (is.na(grad_yr_clean) || is.na(type_clean)) {
      warning("Invalid grad_yr or type - returning default period info")
      return(list(
        pgy_year = "Unknown",
        period_number = NA,
        period_name = "Unknown Period",
        academic_year = "Unknown",
        is_valid = FALSE
      ))
    }
    
    # Pass data dictionary so labels come from REDCap
    gmed::calculate_pgy_and_period(
      grad_yr = grad_yr_clean,
      type = type_clean,
      data_dict = values$app_data$data_dict
    )
  })
  
# ============================================================================
  # AUTHENTICATION - Using gmed universal function
  # ============================================================================
  
  observeEvent(input$submit_code, {
    req(input$access_code_input)
    
    # Load app data first
    if (is.null(values$app_data)) {
      values$app_data <- load_app_data()
    }
    
    # Authenticate using gmed function
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
      go_to_step(2)
      
      showNotification(auth_result$message, type = "message")
      
    } else {
      # Failed
      shinyjs::show("access_code_error")
      updateTextInput(session, "access_code_input", value = "")
      showNotification(auth_result$message, type = "warning")
    }
  })
  
  # ============================================================================
  # RESIDENT INFO DISPLAY
  # ============================================================================
  
   # ============================================================================
  # INTRO PAGE OUTPUTS
  # ============================================================================
  
  output$resident_name_display_intro <- renderText({
    req(values$resident_info)
    values$resident_info$name %||% "Resident"
  })
  
  output$resident_level_display_intro <- renderText({
    req(values$resident_info)
    period_info <- resident_period_info()
    
    # Show both calculated level and period
    level_text <- switch(as.character(period_info$pgy_year),
                        "1" = "PGY-1 (Intern)",
                        "2" = "PGY-2",
                        "3" = "PGY-3",
                        "Unknown")
    
    level_text
  })
  
  output$resident_track_display_intro <- renderText({
    req(values$resident_info)
    
    # Show type (Categorical vs Preliminary)
    type_display <- if (!is.null(values$resident_info$type)) {
      if (values$resident_info$type == 2 || tolower(values$resident_info$type) == "categorical") {
        "Categorical"
      } else if (values$resident_info$type == 1 || tolower(values$resident_info$type) == "preliminary") {
        "Preliminary"
      } else {
        values$resident_info$type
      }
    } else {
      "Unknown"
    }
    
    type_display
  })
  
  output$resident_period_display_intro <- renderText({
    req(values$resident_info)
    period_info <- resident_period_info()
    
    if (period_info$is_valid) {
      paste0(period_info$period_name, " (Period ", period_info$period_number, ")")
    } else {
      "No active period"
    }
  })
  
  output$resident_academic_year_display_intro <- renderText({
    req(values$resident_info)
    period_info <- resident_period_info()
    period_info$academic_year %||% "Unknown"
  })
  
  output$resident_grad_year_display_intro <- renderText({
    req(values$resident_info)
    values$resident_info$grad_yr %||% "Unknown"
  })

  output$resident_coach_display_intro <- renderText({
    req(values$resident_info)
    values$resident_info$coach %||% "Not assigned"
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

  # In your app R/server.R - now much simpler!

# Call the module
career_module <- mod_career_goals_server(
  "career",
  resident_info = reactive(values$resident_info),
  app_data = reactive(values$app_data),
  current_period = NULL,  # Auto-calculate
  mode = "edit",
  redcap_token = app_config$rdm_token
)

# Navigation with save
observeEvent(input$nav_intro_next, {
  # Save career data
  if (career_module$has_changes()) {
    result <- career_module$save()
    if (result$success) {
      showNotification("Career goals saved", type = "message")
    } else {
      showNotification(paste("Error:", result$error), type = "error")
      return()  # Don't navigate if save failed
    }
  }
  
  go_to_step(3)
})
  
 
  # ============================================================================
  # MODULE PLACEHOLDERS - Ready for gmed modules
  # ============================================================================
  
# ============================================================================
  # SCHOLARSHIP
  # ============================================================================
# Reactive value to track scholarship data
scholarship_rv <- shiny::reactiveVal(data.frame())

# Function to refresh scholarship data
refresh_scholarship <- function() {
  req(values$current_resident)
  data <- gmed::fetch_scholarship_data(
    record_id = values$current_resident,
    redcap_url = app_config$redcap_url,
    redcap_token = app_config$rdm_token
  )
  scholarship_rv(data)
}

# Initial load
shiny::observe({
  shiny::req(values$current_resident)
  refresh_scholarship()
})

# Display scholarship
output$scholarship_display <- shiny::renderUI({
  # Don't require scholarship_rv - it might be empty initially
  
  # Get data dictionary from app_data
  data_dict <- if (!is.null(values$app_data) && !is.null(values$app_data$data_dict)) {
    values$app_data$data_dict
  } else {
    NULL
  }
  
  # Get scholarship data (might be empty data frame)
  schol_data <- scholarship_rv()
  
  # Display even if empty
  scholarship_summary <- gmed::display_scholarship(
    schol_data, 
    data_dict = data_dict
  )
  
  shiny::tagList(
    gmed::scholarship_badge_ui(scholarship_summary$badges),
    shiny::hr(),
    gmed::scholarship_tables_ui(scholarship_summary)
  )
})

# Entry module - pass data_dict as VALUE not reactive
scholarship_entry_server(
  "scholarship_entry",
  record_id = reactive(values$current_resident),
  redcap_url = app_config$redcap_url,
  redcap_token = app_config$rdm_token,
  data_dict = if (!is.null(values$app_data)) values$app_data$data_dict else NULL,
  refresh_callback = refresh_scholarship
)

 # ============================================================================
# PLUS/DELTA MODULE INTEGRATION
# ============================================================================

# Plus/Delta Module using gmed
mod_plus_delta_table_server(
  "resident_plus_delta",
  rdm_data = reactive({
    req(values$app_data, values$current_resident)
    
    # Get assessment data for current resident
    values$app_data$all_forms$assessment %>%
      filter(record_id == values$current_resident)
  }),
  record_id = reactive(values$current_resident)
)
  
  # Test scholarship wrapper module
# Test scholarship wrapper module
observe({
  req(values$authenticated, values$app_data, values$current_resident)
  
  # Test calling the wrapper
  mod_scholarship_wrapper_server(
    "test_scholarship_wrapper",
    rdm_data = reactive(values$app_data),
    record_id = reactive(values$current_resident),
    period = NULL,
    data_dict = reactive(values$app_data$data_dict)
  )
})
  
  # Progress Module (placeholder)
  # observe({
  #   req(values$authenticated, values$app_data)
  #   # Progress visualization module will go here
  # })
  
  # Goals Module (placeholder)
  # observe({
  #   req(values$authenticated, values$app_data)
  #   # ILP goals module will go here
  # })
  
  # Scholarship Module (placeholder)
  # observe({
  #   req(values$authenticated, values$app_data)
  #   # Scholarship tracking module will go here
  # })
  
  # Self-Assessment Module (placeholder)
  # observe({
  #   req(values$authenticated, values$app_data)
  #   # Milestone self-assessment entry will go here
  # })


  # ============================================================================
# PERIOD DETECTION TEST (SAFE - DOESN'T CHANGE EXISTING FUNCTIONALITY)
# ============================================================================

observe({
  req(values$current_resident, values$app_data$residents)
  
  resident_info <- values$app_data$residents %>%
    filter(record_id == values$current_resident) %>%
    slice(1)
  
  if (nrow(resident_info) == 0) return(NULL)
  
  # Test period detection
  detected_period <- tryCatch({
  gmed::calculate_pgy_and_period(
    grad_yr = resident_info$grad_yr,  # CHANGED from graduation_year
    type = resident_info$residency_type %||% "Categorical",
    current_date = Sys.Date()
  )
}, error = function(e) {
  message("Period detection error: ", e$message)
  NULL
})
  
  if (!is.null(detected_period)) {
    message("=== PERIOD DETECTION TEST ===")
    message("Resident: ", values$current_resident)
    message("Period Number: ", detected_period$period_number)
    message("Period Name: ", detected_period$period_name)
    message("PGY Year: ", detected_period$pgy_year)
    message("Is Valid: ", detected_period$is_valid)
    
    # Get period structure
    if (detected_period$period_number %in% 1:7) {
      config <- get_period_structure(detected_period$period_number)
      message("Modules for this period: ", paste(config$modules, collapse = ", "))
    }
  }
})
  
# Period debug output
# Period debug output
output$period_debug_info <- renderPrint({
  req(values$current_resident, values$app_data$residents, values$app_data$data_dict)
  
  resident_info <- values$app_data$residents %>%
    filter(record_id == values$current_resident) %>%
    slice(1)
  
  if (nrow(resident_info) == 0) return("No resident found")
  
  data_dict <- values$app_data$data_dict
  
  # Use gmed's parse_redcap_choices to get grad_yr mapping
  grad_yr_field <- data_dict %>%
    filter(field_name == "grad_yr") %>%
    slice(1)
  
  grad_yr_choices <- gmed::parse_redcap_choices(
    grad_yr_field$select_choices_or_calculations[1]
  )
  
  # Get actual year from coded value
  grad_yr_actual <- as.numeric(grad_yr_choices[as.character(resident_info$grad_yr)])
  
  # Use gmed's existing translate_resident_type
  res_type_label <- gmed::translate_resident_type(resident_info$type, data_dict)
  
  detected_period <- tryCatch({
    gmed::calculate_pgy_and_period(
      grad_yr = grad_yr_actual,
      type = resident_info$type,
      current_date = Sys.Date()
    )
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
  
  if (!is.null(detected_period) && !is.na(detected_period$period_number) && 
      detected_period$period_number %in% 1:7) {
    config <- get_period_structure(detected_period$period_number)
    
    list(
      resident_type_coded = resident_info$type,
      resident_type_label = res_type_label,
      grad_yr_coded = resident_info$grad_yr,
      grad_yr_actual = grad_yr_actual,
      period_info = detected_period,
      modules = config$modules
    )
  } else {
    list(
      error = "Period detection issue",
      resident_type = resident_info$type,
      grad_yr_coded = resident_info$grad_yr,
      grad_yr_actual = grad_yr_actual,
      period_result = detected_period
    )
  }
})
}