# Scholarship Entry UI - Multi-Step Wizard
scholarship_entry_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Add New Scholarship Entry"),
    
    # Step indicator
    shiny::uiOutput(ns("step_indicator")),
    
    # Dynamic content area
    shiny::uiOutput(ns("dynamic_content")),
    
    # Messages
    shiny::uiOutput(ns("submit_message"))
  )
}

# Scholarship Entry Server - Multi-Step Logic
scholarship_entry_server <- function(id, record_id, redcap_url, redcap_token, data_dict = NULL, refresh_callback = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # State management
    state <- shiny::reactiveValues(
      step = "select_type",
      schol_type = NULL,
      base_data = list(),
      current_project_instance = NULL
    )
    
    standardize_dict_names <- function(dict) {
      if (is.null(dict)) return(NULL)
      
      # Map various possible column names to standard names
      name_mapping <- list(
        field_name = c("field_name", "Variable / Field Name", "Variable...Field.Name"),
        field_label = c("field_label", "Field Label", "Field.Label"),
        choices = c("select_choices_or_calculations", "Choices, Calculations, OR Slider Labels", 
                    "Choices..Calculations..OR.Slider.Labels")
      )
      
      for (standard_name in names(name_mapping)) {
        possible_names <- name_mapping[[standard_name]]
        for (poss_name in possible_names) {
          if (poss_name %in% names(dict)) {
            names(dict)[names(dict) == poss_name] <- standard_name
            break
          }
        }
      }
      
      return(dict)
    }
    
    # Parse choices from data dictionary
    get_choices <- function(field_name) {
      # Get the actual data_dict value (not reactive)
      dict <- data_dict
      
      # If data_dict is reactive, get its value
      if (is.function(dict)) {
        dict <- dict()
      }
      
      if (is.null(dict) || !is.data.frame(dict)) return(NULL)
      
      # Standardize column names
      dict <- standardize_dict_names(dict)
      
      # Check if standardized columns exist
      if (!"field_name" %in% names(dict) || !"choices" %in% names(dict)) {
        return(NULL)
      }
      
      dict_row <- dict[dict$field_name == field_name, ]
      if (nrow(dict_row) == 0) return(NULL)
      
      choices_raw <- dict_row$choices[1]
      if (is.na(choices_raw)) return(NULL)
      
      # Parse: "1, Label1 | 2, Label2 | 3, Label3"
      choice_list <- strsplit(choices_raw, "\\|")[[1]]
      choices <- c("Select..." = "")
      
      for (choice in choice_list) {
        parts <- trimws(strsplit(choice, ",", fixed = TRUE)[[1]])
        if (length(parts) >= 2) {
          value <- parts[1]
          label <- paste(parts[-1], collapse = ",")
          choices[label] <- value
        }
      }
      
      return(choices)
    }
    
    # Clear state for new entry
    reset_form <- function() {
      state$step <- "select_type"
      state$schol_type <- NULL
      state$base_data <- list()
      state$current_project_instance <- NULL
    }
    
    # Step indicator
    output$step_indicator <- shiny::renderUI({
      if (state$step == "select_type") return(NULL)
      
      steps <- list(
        type_2 = "Patient Safety Review",
        type_7 = "Committee Membership",
        step_1 = "Project Details",
        step_2_pres = "Add Presentations",
        step_3_pub = "Add Publications"
      )
      
      current_label <- steps[[state$step]]
      if (is.null(current_label)) return(NULL)
      
      shiny::div(
        class = "alert alert-info",
        shiny::strong("Current Step: "), current_label
      )
    })
    
    # Dynamic content based on step
    output$dynamic_content <- shiny::renderUI({
      switch(state$step,
        "select_type" = render_type_selection(ns, get_choices),
        "type_2" = render_safety_review(ns),
        "type_7" = render_committee(ns, get_choices),
        "step_1" = render_project_details(ns, state$schol_type, get_choices),
        "step_2_pres" = render_presentation_entry(ns, get_choices),
        "step_3_pub" = render_publication_entry(ns)
      )
    })
    
    # Message output
    output$submit_message <- shiny::renderUI({ NULL })
    
    # =========================================================================
    # OBSERVERS FOR BUTTONS
    # =========================================================================
    
    # Type selection
    shiny::observeEvent(input$select_type_btn, {
      shiny::req(input$schol_type_select)
      
      type <- input$schol_type_select
      state$schol_type <- type
      
      if (type == "2") {
        state$step <- "type_2"
      } else if (type == "7") {
        state$step <- "type_7"
      } else if (type %in% c("1", "3", "6")) {
        state$step <- "step_1"
      } else if (type == "4") {
        state$step <- "step_2_pres"
      } else if (type == "5") {
        state$step <- "step_3_pub"
      }
    })
    
    # Type 2 - Safety Review submission
    shiny::observeEvent(input$submit_safety, {
      safety_type <- input$safety_type
      
      scholarship_data <- list(
        schol_type = "2",
        schol_ps = if (safety_type == "safety") "1" else NA,
        schol_rca = if (safety_type == "rca") "1" else NA
      )
      
      result <- gmed::submit_scholarship_data(
        redcap_url = redcap_url,
        redcap_token = redcap_token,
        record_id = record_id(),
        scholarship_data = scholarship_data
      )
      
      handle_submission_result(result, "Safety review recorded successfully!", refresh_callback, reset_form, output, session)
    })
    
    # Type 7 - Committee submission
    shiny::observeEvent(input$submit_committee, {
      shiny::req(input$schol_comm, input$schol_comm_type)
      
      scholarship_data <- list(
        schol_type = "7",
        schol_comm = input$schol_comm,
        schol_comm_type = input$schol_comm_type,
        schol_comm_other = if (!is.null(input$schol_comm_other)) input$schol_comm_other else NA
      )
      
      result <- gmed::submit_scholarship_data(
        redcap_url = redcap_url,
        redcap_token = redcap_token,
        record_id = record_id(),
        scholarship_data = scholarship_data
      )
      
      handle_submission_result(result, "Committee membership recorded successfully!", refresh_callback, reset_form, output, session)
    })
    
    # Step 1 - Project details submission
    shiny::observeEvent(input$submit_step_1, {
      
      # Collect base project data
      base_data <- list(
        schol_type = state$schol_type,
        schol_qi = if (state$schol_type == "1" && !is.null(input$schol_qi)) input$schol_qi else NA,
        schol_res = if (state$schol_type %in% c("3", "6") && !is.null(input$schol_res)) input$schol_res else NA,
        schol_res_mentor = if (!is.null(input$schol_res_mentor)) input$schol_res_mentor else NA,
        schol_div = if (!is.null(input$schol_div)) input$schol_div else NA,
        schol_res_status = if (!is.null(input$schol_res_status)) input$schol_res_status else NA,
        schol_pres = if (!is.null(input$schol_pres_step1)) input$schol_pres_step1 else "0",
        schol_pub = if (!is.null(input$schol_pub_step1)) input$schol_pub_step1 else "0"
      )
      
      # Submit base project
      result <- gmed::submit_scholarship_data(
        redcap_url = redcap_url,
        redcap_token = redcap_token,
        record_id = record_id(),
        scholarship_data = base_data
      )
      
      if (result$success) {
        state$current_project_instance <- result$instance
        state$base_data <- base_data
        
        # Refresh display
        if (!is.null(refresh_callback)) refresh_callback()
        
        # Determine next step
        if (base_data$schol_pres == "1") {
          state$step <- "step_2_pres"
        } else if (base_data$schol_pub == "1") {
          state$step <- "step_3_pub"
        } else {
          output$submit_message <- shiny::renderUI({
            shiny::div(class = "alert alert-success", "Project recorded successfully!")
          })
          ask_another_entry(session)
        }
      } else {
        output$submit_message <- shiny::renderUI({
          shiny::div(class = "alert alert-danger", result$message)
        })
      }
    })
    
    # Step 2 - Presentation submission
    shiny::observeEvent(input$submit_presentation, {
      shiny::req(input$schol_pres_conf, input$schol_pres_type)
      
      pres_data <- list(
        schol_type = "4",
        schol_pres = "1",
        schol_pres_conf = input$schol_pres_conf,
        schol_pres_type = input$schol_pres_type
      )
      
      result <- gmed::submit_scholarship_data(
        redcap_url = redcap_url,
        redcap_token = redcap_token,
        record_id = record_id(),
        scholarship_data = pres_data
      )
      
      if (result$success) {
        if (!is.null(refresh_callback)) refresh_callback()
        output$submit_message <- shiny::renderUI({
          shiny::div(class = "alert alert-success", "Presentation added!")
        })
      }
    })
    
    # Add another presentation
    shiny::observeEvent(input$another_presentation, {
      shiny::updateTextInput(session, "schol_pres_conf", value = "")
      shiny::updateSelectInput(session, "schol_pres_type", selected = "")
      output$submit_message <- shiny::renderUI({ NULL })
    })
    
    # Done with presentations
    shiny::observeEvent(input$done_presentations, {
      if (!is.null(state$base_data$schol_pub) && state$base_data$schol_pub == "1") {
        state$step <- "step_3_pub"
      } else {
        ask_another_entry(session)
      }
    })
    
    # Step 3 - Publication submission
    shiny::observeEvent(input$submit_publication, {
      shiny::req(input$schol_cit)
      
      pub_data <- list(
        schol_type = "5",
        schol_pub = "1",
        schol_cit = input$schol_cit
      )
      
      result <- gmed::submit_scholarship_data(
        redcap_url = redcap_url,
        redcap_token = redcap_token,
        record_id = record_id(),
        scholarship_data = pub_data
      )
      
      if (result$success) {
        if (!is.null(refresh_callback)) refresh_callback()
        output$submit_message <- shiny::renderUI({
          shiny::div(class = "alert alert-success", "Publication added!")
        })
      }
    })
    
    # Add another publication
    shiny::observeEvent(input$another_publication, {
      shiny::updateTextAreaInput(session, "schol_cit", value = "")
      output$submit_message <- shiny::renderUI({ NULL })
    })
    
    # Done with publications
    shiny::observeEvent(input$done_publications, {
      ask_another_entry(session)
    })
    
       # Start new entry - USE NS() for button IDs
    shiny::observeEvent(input$modal_new_entry_yes, {
      shiny::removeModal()
      reset_form()
      output$submit_message <- shiny::renderUI({ NULL })
    })
    
    shiny::observeEvent(input$modal_new_entry_no, {
      shiny::removeModal()
      output$submit_message <- shiny::renderUI({
        shiny::div(class = "alert alert-success", 
                  shiny::icon("check-circle"), " All scholarship entries complete!")
      })
    })
  })
}

# =========================================================================
# UI RENDERING FUNCTIONS
# =========================================================================

render_type_selection <- function(ns, get_choices) {
  
  # Get choices from data dictionary or use defaults
  type_choices <- get_choices("schol_type")
  if (is.null(type_choices)) {
    type_choices <- c(
      "Select one..." = "",
      "Quality Improvement" = "1",
      "Patient Safety Review" = "2",
      "Research" = "3",
      "Presentation" = "4",
      "Publication" = "5",
      "Education" = "6",
      "Committee" = "7"
    )
  }
  
  shiny::tagList(
    shiny::selectInput(
      ns("schol_type_select"),
      "What type of scholarly activity would you like to add?",
      choices = type_choices
    ),
    shiny::actionButton(ns("select_type_btn"), "Continue", class = "btn-primary")
  )
}

render_safety_review <- function(ns) {
  shiny::tagList(
    shiny::radioButtons(
      ns("safety_type"),
      "Was this a Patient Safety Review or a Root Cause Analysis?",
      choices = c(
        "Patient Safety Review (review of an event you were involved in)" = "safety",
        "Root Cause Analysis (M&M or formal RCA)" = "rca"
      )
    ),
    shiny::actionButton(ns("submit_safety"), "Submit", class = "btn-primary")
  )
}

render_committee <- function(ns, get_choices) {
  
  # Get choices from data dictionary
  comm_type_choices <- get_choices("schol_comm_type")
  if (is.null(comm_type_choices)) {
    comm_type_choices <- c(
      "Select..." = "",
      "Division" = "1",
      "IM Program" = "2",
      "Hospital" = "3",
      "Other" = "4"
    )
  }
  
  shiny::tagList(
    shiny::textInput(ns("schol_comm"), "What is the name of this committee?"),
    shiny::selectInput(
      ns("schol_comm_type"),
      "Is this committee part of:",
      choices = comm_type_choices
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("schol_comm_type"), "'] == '4'"),
      shiny::textInput(ns("schol_comm_other"), "Please specify:")
    ),
    shiny::actionButton(ns("submit_committee"), "Submit", class = "btn-primary")
  )
}

render_project_details <- function(ns, schol_type, get_choices) {
  
  # Get choices from data dictionary
  div_choices <- get_choices("schol_div")
  if (is.null(div_choices)) {
    div_choices <- c(
      "Select..." = "",
      "Allergy" = "1",
      "Cardiology" = "2",
      "Critical Care" = "3",
      "Endocrinology" = "4",
      "Gastroenterology" = "5",
      "Geriatrics" = "6",
      "Hematology/Oncology" = "7",
      "Hospital Medicine" = "8",
      "ID" = "9",
      "Nephrology" = "10",
      "Pulmonary" = "11",
      "Rheumatology" = "12"
    )
  }
  
  status_choices <- get_choices("schol_res_status")
  if (is.null(status_choices)) {
    status_choices <- c(
      "Select..." = "",
      "Finished" = "1",
      "In Progress" = "2",
      "Submitted" = "3",
      "Accepted" = "4",
      "Published" = "5"
    )
  }
  
  shiny::tagList(
    # Description field (QI vs Research)
    if (schol_type == "1") {
      shiny::textAreaInput(ns("schol_qi"), "Please describe the QI project", rows = 4)
    } else {
      shiny::textAreaInput(ns("schol_res"), "Describe the project", rows = 4)
    },
    
    shiny::textInput(ns("schol_res_mentor"), "Who is your mentor in this project?"),
    
    shiny::selectInput(
      ns("schol_div"),
      "What Division is this occurring in?",
      choices = div_choices
    ),
    
    shiny::selectInput(
      ns("schol_res_status"),
      "Describe the status of this project",
      choices = status_choices
    ),
    
    shiny::radioButtons(
      ns("schol_pres_step1"),
      "Did you present this project anywhere?",
      choices = c("Yes" = "1", "No" = "0"),
      selected = "0"
    ),
    
    shiny::radioButtons(
      ns("schol_pub_step1"),
      "Has this been published?",
      choices = c("Yes" = "1", "No" = "0"),
      selected = "0"
    ),
    
    shiny::actionButton(ns("submit_step_1"), "Save Project", class = "btn-primary")
  )
}

render_presentation_entry <- function(ns, get_choices) {
  
  # Get choices from data dictionary
  pres_type_choices <- get_choices("schol_pres_type")
  if (is.null(pres_type_choices)) {
    pres_type_choices <- c(
      "Select..." = "",
      "Local" = "1",
      "Regional" = "2",
      "National" = "3",
      "International" = "4"
    )
  }
  
  shiny::tagList(
    shiny::p("Add details about where you presented this work:"),
    shiny::textInput(ns("schol_pres_conf"), "What conference or venue?"),
    shiny::selectInput(
      ns("schol_pres_type"),
      "Presentation level:",
      choices = pres_type_choices
    ),
    shiny::actionButton(ns("submit_presentation"), "Add Presentation", class = "btn-primary"),
    shiny::hr(),
    shiny::p("Did you present at another conference?"),
    shiny::actionButton(ns("another_presentation"), "Yes, add another", class = "btn-secondary"),
    shiny::actionButton(ns("done_presentations"), "No, I'm done", class = "btn-success")
  )
}

render_publication_entry <- function(ns) {
  shiny::tagList(
    shiny::p("Add publication details:"),
    shiny::textAreaInput(
      ns("schol_cit"), 
      "Please write the full citation (include all authors):", 
      rows = 3
    ),
    shiny::actionButton(ns("submit_publication"), "Add Publication", class = "btn-primary"),
    shiny::hr(),
    shiny::p("Do you have another publication from this project?"),
    shiny::actionButton(ns("another_publication"), "Yes, add another", class = "btn-secondary"),
    shiny::actionButton(ns("done_publications"), "No, I'm done", class = "btn-success")
  )
}

# =========================================================================
# HELPER FUNCTIONS
# =========================================================================

handle_submission_result <- function(result, success_message, refresh_callback, reset_form, output, session) {
  if (result$success) {
    if (!is.null(refresh_callback)) refresh_callback()
    output$submit_message <- shiny::renderUI({
      shiny::div(class = "alert alert-success", success_message)
    })
    ask_another_entry(session)
  } else {
    output$submit_message <- shiny::renderUI({
      shiny::div(class = "alert alert-danger", result$message)
    })
  }
}

ask_another_entry <- function(session) {
  ns <- session$ns
  
  shiny::showModal(
    shiny::modalDialog(
      title = "Add Another Entry?",
      "Would you like to add another scholarly activity?",
      footer = shiny::tagList(
        shiny::actionButton(ns("modal_new_entry_yes"), "Yes, add another", class = "btn-primary"),
        shiny::actionButton(ns("modal_new_entry_no"), "No, I'm done", class = "btn-secondary")
      ),
      easyClose = FALSE
    )
  )
}