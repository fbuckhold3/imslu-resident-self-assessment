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
  req(values$current_resident, values$app_data$residents, values$app_data$data_dict)
  
  # Check for manual override
  if (!is.null(input$period_override) && input$period_override != "auto") {
    override_num <- as.numeric(input$period_override)
    config <- get_period_structure(override_num)
    
    return(list(
      period_number = override_num,
      period_name = config$period_name,
      detection_method = "manual_override",
      is_override = TRUE
    ))
  }

  
  
  # Use automatic detection
  resident_info <- values$app_data$residents %>%
    filter(record_id == values$current_resident) %>%
    slice(1)
  
  if (nrow(resident_info) == 0) return(NULL)
  
  data_dict <- values$app_data$data_dict

  # Handle type field - convert label to code if needed
type_value <- resident_info$type
type_code <- if (is.character(type_value) && tolower(type_value) == "categorical") {
  2
} else if (is.character(type_value) && tolower(type_value) == "preliminary") {
  1
} else {
  as.numeric(type_value)
}
  
# Get grad_yr - check if it's already a year or needs translation
grad_yr_value <- resident_info$grad_yr

# Try to use it directly first
grad_yr_actual <- suppressWarnings(as.numeric(grad_yr_value))

# If it's not a valid year (< 2020 or > 2040), try translating from data dict
if (is.na(grad_yr_actual) || grad_yr_actual < 2020 || grad_yr_actual > 2040) {
  # Get grad_yr mapping from data dictionary
  grad_yr_field <- data_dict %>%
    filter(field_name == "grad_yr") %>%
    slice(1)
  
  if (nrow(grad_yr_field) > 0) {
    grad_yr_choices <- gmed::parse_redcap_choices(
      grad_yr_field$select_choices_or_calculations[1]
    )
    
    # Translate coded value to actual year
    grad_yr_actual <- as.numeric(grad_yr_choices[as.character(grad_yr_value)])
  }
}

# NOW CALCULATE THE PERIOD
detected_period <- tryCatch({
  gmed::calculate_pgy_and_period(
    grad_yr = grad_yr_actual,
    type = type_code,  # Use converted code
    current_date = Sys.Date()
  )
}, error = function(e) {
  NULL
})


# RETURN THE PERIOD INFO (THIS WAS MISSING!)
if (!is.null(detected_period) && detected_period$period_number %in% 1:7) {
  return(list(
    period_number = detected_period$period_number,
    period_name = detected_period$period_name,
    pgy_year = detected_period$pgy_year,
    detection_method = "automatic",
    is_override = FALSE
  ))
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
  req(values$resident_info, active_period())
  period_info <- active_period()
  
  # Show both calculated level and period
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
  req(values$resident_info, active_period())
  period_info <- active_period()
  
  # Calculate academic year from period info
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
  
  # Get actual year from coded value
  grad_yr_field <- values$app_data$data_dict %>%
    filter(field_name == "grad_yr") %>%
    slice(1)
  
  grad_yr_choices <- gmed::parse_redcap_choices(
    grad_yr_field$select_choices_or_calculations[1]
  )
  
  grad_yr_choices[as.character(values$resident_info$grad_yr)] %||% "Unknown"
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

output$resident_track_display_intro <- renderText({
  req(values$resident_info)
  
  # Handle type - it's already a label, just display it
  type_value <- values$resident_info$type
  
  if (is.character(type_value)) {
    type_value  # Already "Categorical" or "Preliminary"
  } else {
    # If it's a code, translate it
    switch(as.character(type_value),
      "1" = "Preliminary",
      "2" = "Categorical",
      "Unknown")
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
    class = "container py-4",
    div(
      class = "row",
      div(
        class = "col-lg-10 offset-lg-1",
        
        # Page header
        h2(module_titles[[module_key]]),
        p(class = "lead text-muted mb-4", 
          paste("Period", period_info$period_number, "-", period_info$period_name)),
        hr(),
        
        # Module content
        uiOutput(paste0("module_", module_key)),
        
        # Navigation buttons
        div(
          class = "d-flex justify-content-between mt-4 pt-3 border-top",
          
          if (current_index > 1) {
            actionButton(
              "nav_prev_module",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            )
          } else {
            actionButton(
              "nav_back_to_intro",
              "Back to Introduction",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            )
          },
          
          if (current_index < length(modules)) {
            actionButton(
              "nav_next_module",
              paste("Continue to", module_titles[[modules[current_index + 1]]]),
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          } else {
            actionButton(
              "nav_complete",
              "Complete Portfolio Review",
              class = "btn-success btn-lg",
              icon = icon("check")
            )
          }
        )
      )
    )
  )
})

# Progress indicator
output$progress_indicator <- renderUI({
  req(values$authenticated, active_period())
  
  period_info <- active_period()
  config <- get_period_structure(period_info$period_number)
  modules <- config$modules
  
  current_index <- current_module_index()
  total <- length(modules)
  
  div(
    style = "min-width: 200px; padding: 8px;",
    tags$small(
      class = "text-muted me-2",
      sprintf("Step %d of %d", current_index, total)
    ),
    tags$div(
      class = "progress",
      style = "height: 8px;",
      tags$div(
        class = "progress-bar bg-primary",
        role = "progressbar",
        style = paste0("width: ", round((current_index / total) * 100), "%;")
      )
    )
  )
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

observe({
  req(values$authenticated, values$app_data, values$current_resident, active_period())
  
  period_info <- active_period()
  config <- get_period_structure(period_info$period_number)
  modules <- config$modules
  
  # Initialize server for each module
  lapply(modules, function(module_key) {
    
    switch(module_key,
      "scholarship" = {
        mod_scholarship_wrapper_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data$all_forms$self_evaluation),     # CHANGE THIS
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict
        )
      },
      "career_planning" = {
        mod_career_planning_wrapper_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive(values$app_data$all_forms$s_eval),     # CHANGE THIS
          record_id = reactive(values$current_resident),
          period = reactive(period_info$period_number),
          data_dict = values$app_data$data_dict
        )
      },
      "assessment_review" = {
        gmed::mod_plus_delta_table_server(
          paste0("wrapper_", module_key),
          rdm_data = reactive({
            values$app_data$all_forms$assessment %>%
              filter(record_id == values$current_resident)
          }),
          record_id = reactive(values$current_resident)
        )
      }
    )
  })
})
  
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
  req(values$current_resident, values$app_data$residents, values$app_data$data_dict)
  
  resident_info <- values$app_data$residents %>%
    filter(record_id == values$current_resident) %>%
    slice(1)
  
  if (nrow(resident_info) == 0) return(NULL)
  
  data_dict <- values$app_data$data_dict
  
  # Get grad_yr - try direct use first
  grad_yr_value <- resident_info$grad_yr
  grad_yr_actual <- suppressWarnings(as.numeric(grad_yr_value))
  
  # If not a valid year, try data dictionary translation
  if (is.na(grad_yr_actual) || grad_yr_actual < 2020 || grad_yr_actual > 2040) {
    grad_yr_field <- data_dict %>%
      filter(field_name == "grad_yr") %>%
      slice(1)
    
    grad_yr_choices <- gmed::parse_redcap_choices(
      grad_yr_field$select_choices_or_calculations[1]
    )
    
    grad_yr_actual <- as.numeric(grad_yr_choices[as.character(grad_yr_value)])
  }
  
  # Handle type field - convert label to code if needed
  type_value <- resident_info$type
  type_code <- if (is.character(type_value) && tolower(type_value) == "categorical") {
    2
  } else if (is.character(type_value) && tolower(type_value) == "preliminary") {
    1
  } else {
    as.numeric(type_value)
  }
  
  # Handle type field - convert label to code if needed
type_value <- resident_info$type
type_code <- if (is.character(type_value) && tolower(type_value) == "categorical") {
  2
} else if (is.character(type_value) && tolower(type_value) == "preliminary") {
  1
} else {
  as.numeric(type_value)
}

detected_period <- tryCatch({
  gmed::calculate_pgy_and_period(
    grad_yr = grad_yr_actual,
    type = type_code,  # ✅ CORRECT - Use converted code
    current_date = Sys.Date()
  )
}, error = function(e) {
  return(paste("Error:", e$message))
})
  
  if (!is.null(detected_period)) {
    message("=== PERIOD DETECTION TEST ===")
    message("Resident: ", values$current_resident)
    message("Type (original): ", type_value, " -> Code: ", type_code)
    message("Grad Year: ", grad_yr_value, " -> Actual: ", grad_yr_actual)
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



# Period debug output for UI
output$period_debug_info <- renderPrint({
  req(values$current_resident, values$app_data$residents, values$app_data$data_dict)
  
  resident_info <- values$app_data$residents %>%
    filter(record_id == values$current_resident) %>%
    slice(1)
  
  if (nrow(resident_info) == 0) return("No resident found")
  
  data_dict <- values$app_data$data_dict
  
  # Get grad_yr - try direct use first, then translate if needed
grad_yr_value <- resident_info$grad_yr
grad_yr_actual <- suppressWarnings(as.numeric(grad_yr_value))

# If not a valid year, try data dictionary translation
if (is.na(grad_yr_actual) || grad_yr_actual < 2020 || grad_yr_actual > 2040) {
  grad_yr_field <- data_dict %>%
    filter(field_name == "grad_yr") %>%
    slice(1)
  
  grad_yr_choices <- gmed::parse_redcap_choices(
    grad_yr_field$select_choices_or_calculations[1]
  )
  
  grad_yr_actual <- as.numeric(grad_yr_choices[as.character(grad_yr_value)])
}
  
  # Use gmed's existing translate_resident_type
  res_type_label <- gmed::translate_resident_type(resident_info$type, data_dict)
  
  detected_period <- tryCatch({
    gmed::calculate_pgy_and_period(
      grad_yr = grad_yr_actual,
      type = resident_info$type,  # ✅ CORRECT field name
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
      resident_type_coded = resident_info$type,
      resident_type_label = res_type_label,
      grad_yr_coded = resident_info$grad_yr,
      grad_yr_actual = grad_yr_actual,
      period_result = detected_period
    )
  }
})
}