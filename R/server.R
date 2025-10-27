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
  # AUTHENTICATION
  # ============================================================================
  
  # Handle access code submission
  observeEvent(input$submit_code, {
    req(input$access_code_input)
    
    code <- trimws(input$access_code_input)
    
    if (app_config$debug_mode) {
      message("Authentication attempt with code: ", code)
    }
    
    # Validate code
    if (validate_access_code(code)) {
      # Success!
      values$authenticated <- TRUE
      values$resident_info <- get_resident_by_code(code)
      values$current_resident <- values$resident_info$record_id
      
      # Load app data
      values$app_data <- load_app_data()
      
      # Hide access screen, show main content
      shinyjs::hide("access_code_screen")
      shinyjs::hide("access_code_error")
      shinyjs::show("resident_info_banner")
      shinyjs::show("quick_links_section")
      shinyjs::show("main_content_tabs")
      
      showNotification("Welcome! Loading your data...", type = "message")
      
      if (app_config$debug_mode) {
        message("Authentication successful for resident: ", values$current_resident)
      }
      
    } else {
      # Failed
      shinyjs::show("access_code_error")
      updateTextInput(session, "access_code_input", value = "")
      
      if (app_config$debug_mode) {
        message("Authentication failed")
      }
    }
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    values$authenticated <- FALSE
    values$current_resident <- NULL
    values$resident_info <- NULL
    
    # Reset UI
    shinyjs::show("access_code_screen")
    shinyjs::hide("resident_info_banner")
    shinyjs::hide("quick_links_section")
    shinyjs::hide("main_content_tabs")
    
    updateTextInput(session, "access_code_input", value = "")
    
    showNotification("Logged out successfully", type = "message")
  })
  
  # ============================================================================
  # RESIDENT INFO DISPLAY
  # ============================================================================
  
  output$resident_name_display <- renderText({
    req(values$resident_info)
    values$resident_info$name %||% paste("Resident", values$current_resident)
  })
  
  output$resident_level_display <- renderText({
    req(values$resident_info)
    values$resident_info$Level %||% "Unknown"
  })
  
  output$current_period_display <- renderText({
    # Calculate current academic period
    current_date <- Sys.Date()
    year <- as.numeric(format(current_date, "%Y"))
    month <- as.numeric(format(current_date, "%m"))
    
    academic_year <- if (month >= 7) {
      paste0(year, "-", year + 1)
    } else {
      paste0(year - 1, "-", year)
    }
    
    quarter <- ceiling((month %% 12) / 3)
    if (quarter == 0) quarter <- 4
    
    paste0(academic_year, " Q", quarter)
  })
  
  # ============================================================================
  # NAVIGATION - Quick Link Cards
  # ============================================================================
  
  observeEvent(input$nav_to_tab, {
    req(values$authenticated)
    updateNavbarPage(session, "main_tabs", selected = input$nav_to_tab)
  })
  
  # ============================================================================
  # MODULE PLACEHOLDERS - Ready for gmed modules
  # ============================================================================
  
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
}