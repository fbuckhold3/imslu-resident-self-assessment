#' Career Planning Wrapper Module UI
#'
#' @param id Module namespace ID
#'
#' @export
mod_career_planning_wrapper_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # WELLNESS SECTION
    h3("Wellness Check-In"),
    hr(),
    
    # Display previous wellness comment
    uiOutput(ns("previous_wellness_display")),
    
    # Current wellness entry
    h4("Current Wellness"),
    p("How are you doing from a wellness standpoint? Are there issues we can help with, or should be aware of to support you?"),
    textAreaInput(
      ns("wellness"),
      label = NULL,
      rows = 4,
      placeholder = "Enter your wellness reflection..."
    ),
    
    br(),
    hr(),
    
    # CAREER PLANNING SECTION
    h3("Career Planning"),
    hr(),
    
    # Display previous career planning
    uiOutput(ns("previous_career_display")),
    
    # Current career planning entry
    h4("Update Career Planning"),
    p("Enter your current career planning thoughts and goals."),
    uiOutput(ns("career_form")),
    
    br(),
    
    actionButton(
      ns("submit"),
      "Submit Wellness and Career Planning",
      class = "btn-primary"
    )
  )
}

#' Career Planning Wrapper Module Server
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive containing REDCap data (s_eval form)
#' @param record_id Reactive containing resident's record ID
#' @param period Reactive containing current period
#' @param data_dict Data dictionary
#'
#' @export
mod_career_planning_wrapper_server <- function(id, rdm_data, record_id, period, data_dict) {
  moduleServer(id, function(input, output, session) {
    
    # Helper to get field name column
    get_field_name_col <- function() {
      if ("field_name" %in% names(data_dict)) return("field_name")
      if ("Variable / Field Name" %in% names(data_dict)) return("Variable / Field Name")
      if ("Variable...Field.Name" %in% names(data_dict)) return("Variable...Field.Name")
      stop("Cannot find field name column in data dictionary")
    }
    
    # Helper to get field label column
    get_field_label_col <- function() {
      if ("field_label" %in% names(data_dict)) return("field_label")
      if ("Field Label" %in% names(data_dict)) return("Field Label")
      if ("Field.Label" %in% names(data_dict)) return("Field.Label")
      stop("Cannot find field label column in data dictionary")
    }
    
    # Helper to get choices column
    get_choices_col <- function() {
      if ("select_choices_or_calculations" %in% names(data_dict)) return("select_choices_or_calculations")
      if ("Choices, Calculations, OR Slider Labels" %in% names(data_dict)) return("Choices, Calculations, OR Slider Labels")
      if ("Choices..Calculations..OR.Slider.Labels" %in% names(data_dict)) return("Choices..Calculations..OR.Slider.Labels")
      stop("Cannot find choices column in data dictionary")
    }
    
    field_name_col <- get_field_name_col()
    field_label_col <- get_field_label_col()
    choices_col <- get_choices_col()
    
    # Helper to get field info from data dict
    get_field_info <- function(field_name_val) {
      data_dict %>%
        dplyr::filter(!!rlang::sym(field_name_col) == !!field_name_val) %>%
        dplyr::slice(1)
    }
    
    # Helper to get field label
    get_field_label <- function(field_row) {
      field_row[[field_label_col]]
    }
    
    # Helper to get choices string
    get_choices_string <- function(field_row) {
      field_row[[choices_col]]
    }
    
    # Helper to parse choices - returns named vector for selectInput/checkboxGroupInput
    parse_choices <- function(choices_string) {
      if (is.na(choices_string) || choices_string == "") return(NULL)
      
      # Split by pipe
      items <- strsplit(choices_string, "\\|")[[1]]
      items <- trimws(items)
      
      # Parse each item: "code, label"
      codes <- character()
      labels <- character()
      
      for (item in items) {
        parts <- strsplit(item, ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ","))
          codes <- c(codes, code)
          labels <- c(labels, label)
        }
      }
      
      # Return named vector: names = labels (what user sees), values = codes (what gets stored)
      setNames(codes, labels)
    }
    
#Display previous wellness
output$previous_wellness_display <- renderUI({
  req(rdm_data(), period())
  
  # Get previous period data
  prev_period <- as.numeric(period()) - 1
  if (prev_period < 1) return(NULL)
  
  prev_data <- rdm_data() %>%
    dplyr::filter(
      record_id == !!record_id(),
      redcap_repeat_instrument == "S Eval",
      s_e_period == as.character(prev_period)
    )
  
  if (nrow(prev_data) == 0 || is.na(prev_data$s_e_well[1])) {
    return(div(class = "alert alert-info", "No previous wellness comments."))
  }
  
  div(
    class = "card mb-3 bg-light",
    div(
      class = "card-body",
      h5(class = "card-title", "Previous Wellness Comments (Period ", prev_period, ")"),
      p(class = "card-text", prev_data$s_e_well[1])
    )
  )
})

# Display previous career planning
output$previous_career_display <- renderUI({
  req(rdm_data(), period())
  
  gmed::display_career_planning(
    rdm_data = rdm_data(),
    record_id = record_id(),
    current_period = period(),
    data_dict = data_dict
  )
})
    
    # Dynamic form rendering
    output$career_form <- renderUI({
      req(data_dict)
      
      ns <- session$ns
      
      # Get field definitions - FIXED: s_e_well not s_e_wellness
      wellness_field <- get_field_info("s_e_well")
      career_field <- get_field_info("s_e_career_path")
      career_oth_field <- get_field_info("s_e_career_oth")
      fellow_field <- get_field_info("s_e_fellow")
      fellow_oth_field <- get_field_info("s_e_fellow_oth")
      track_field <- get_field_info("s_e_track")
      track_type_field <- get_field_info("s_e_track_type")
      
      # Parse choices
      career_choices <- parse_choices(get_choices_string(career_field))
      fellow_choices <- parse_choices(get_choices_string(fellow_field))
      track_type_choices <- parse_choices(get_choices_string(track_type_field))
      
      tagList(

        
        # Career Path - MULTIPLE SELECTION
        checkboxGroupInput(
          ns("career_path"),
          label = get_field_label(career_field),
          choices = career_choices
        ),
        
        conditionalPanel(
          condition = "input.career_path && input.career_path.includes('4')",
          ns = ns,
          textInput(
            ns("career_oth"),
            label = get_field_label(career_oth_field)
          )
        ),
        
        # Fellowship Interest - ONLY show if code "2" (Fellowship) selected
        conditionalPanel(
          condition = "input.career_path && input.career_path.includes('2')",
          ns = ns,
          checkboxGroupInput(
            ns("fellow"),
            label = get_field_label(fellow_field),
            choices = fellow_choices
          ),
          
          conditionalPanel(
            condition = "input.fellow && input.fellow.includes('1')",
            ns = ns,
            textInput(
              ns("fellow_oth"),
              label = get_field_label(fellow_oth_field),
              placeholder = "Please specify fellowship interest"
            )
          )
        ),
        
        # Track question
        radioButtons(
          ns("track"),
          label = get_field_label(track_field),
          choices = c("Yes" = "1", "No" = "0"),
          selected = "0",
          inline = TRUE
        ),
        
        # Track type (conditional on track = Yes)
        conditionalPanel(
          condition = "input.track == '1'",
          ns = ns,
          checkboxGroupInput(
            ns("track_type"),
            label = get_field_label(track_type_field),
            choices = track_type_choices
          )
        )
      )
    })
    
    # Pre-populate fields with existing data from current period
    observe({
      req(rdm_data(), period(), record_id())
      
      # Map period to number if it's text
      period_map <- c(
        "Entering Residency" = "7",
        "Mid Intern" = "1",
        "End Intern" = "2",
        "Mid PGY2" = "3",
        "End PGY2" = "4",
        "Mid PGY3" = "5",
        "Graduating" = "6"
      )
      
      period_num <- if (is.numeric(period())) {
        as.character(period())
      } else if (period() %in% names(period_map)) {
        period_map[period()]
      } else {
        period()
      }
      
      current_data <- rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          redcap_repeat_instrument == "S Eval",
          s_e_period == !!period_num
        )
      
      if (nrow(current_data) > 0) {
        # Wellness - FIXED: s_e_well not s_e_wellness
        if (!is.na(current_data$s_e_well[1]) && "s_e_well" %in% names(current_data)) {
          updateTextAreaInput(session, "wellness", value = current_data$s_e_well[1])
        }
        
        # Career path - decode checkboxes
        career_cols <- grep("^s_e_career_path___", names(current_data), value = TRUE)
        if (length(career_cols) > 0) {
          checked_codes <- character()
          for (col in career_cols) {
            if (!is.na(current_data[[col]][1]) && current_data[[col]][1] == "1") {
              code <- sub("s_e_career_path___", "", col)
              checked_codes <- c(checked_codes, code)
            }
          }
          if (length(checked_codes) > 0) {
            updateCheckboxGroupInput(session, "career_path", selected = checked_codes)
          }
        }
        
        if (!is.na(current_data$s_e_career_oth[1]) && "s_e_career_oth" %in% names(current_data)) {
          updateTextInput(session, "career_oth", value = current_data$s_e_career_oth[1])
        }
        
        # Fellow - decode checkboxes
        fellow_cols <- grep("^s_e_fellow___", names(current_data), value = TRUE)
        if (length(fellow_cols) > 0) {
          checked_codes <- character()
          for (col in fellow_cols) {
            if (!is.na(current_data[[col]][1]) && current_data[[col]][1] == "1") {
              code <- sub("s_e_fellow___", "", col)
              checked_codes <- c(checked_codes, code)
            }
          }
          if (length(checked_codes) > 0) {
            updateCheckboxGroupInput(session, "fellow", selected = checked_codes)
          }
        }
        
        if (!is.na(current_data$s_e_fellow_oth[1]) && "s_e_fellow_oth" %in% names(current_data)) {
          updateTextInput(session, "fellow_oth", value = current_data$s_e_fellow_oth[1])
        }
        
        if (!is.na(current_data$s_e_track[1]) && "s_e_track" %in% names(current_data)) {
          updateRadioButtons(session, "track", selected = current_data$s_e_track[1])
        }
        
        # Track type - decode checkboxes
        track_cols <- grep("^s_e_track_type___", names(current_data), value = TRUE)
        if (length(track_cols) > 0) {
          checked_codes <- character()
          for (col in track_cols) {
            if (!is.na(current_data[[col]][1]) && current_data[[col]][1] == "1") {
              code <- sub("s_e_track_type___", "", col)
              checked_codes <- c(checked_codes, code)
            }
          }
          if (length(checked_codes) > 0) {
            updateCheckboxGroupInput(session, "track_type", selected = checked_codes)
          }
        }
      }
    })
    
    # Handle submission
    observeEvent(input$submit, {
      req(period(), record_id())

      # Validate required fields
      if (is.null(input$career_path) || length(input$career_path) == 0) {
        showNotification("Please select at least one career path", type = "error")
        return()
      }

      # Only validate fellowship if career path includes "2" (Fellowship)
      if ("2" %in% input$career_path && (is.null(input$fellow) || length(input$fellow) == 0)) {
        showNotification("Please indicate fellowship interest", type = "error")
        return()
      }

      if (is.null(input$track)) {
        showNotification("Please indicate if you're interested in program tracks", type = "error")
        return()
      }

      # Convert period to number
      period_num <- if (is.numeric(period())) {
        period()
      } else {
        # Convert period name to number if needed
        period_map <- c(
          "Entering Residency" = 7,
          "Mid Intern" = 1,
          "End Intern" = 2,
          "Mid PGY2" = 3,
          "End PGY2" = 4,
          "Mid PGY3" = 5,
          "Graduating" = 6
        )
        period_map[period()]
      }

      message("=== CAREER PLANNING SUBMISSION ===")
      message("Period: ", period())
      message("Period number: ", period_num)
      message("Record ID: ", record_id())

      # Prepare data for submission - Build data frame with proper checkbox expansion
      submit_data <- data.frame(
        record_id = record_id(),
        redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance = period_num,
        stringsAsFactors = FALSE
      )

      # Add wellness
      submit_data$s_e_well <- input$wellness %||% ""

      # Add career path checkboxes - EXPAND TO INDIVIDUAL FIELDS
      # Get all possible values from data dict
      career_field <- get_field_info("s_e_career_path")
      career_choices <- parse_choices(get_choices_string(career_field))

      for (code in career_choices) {
        field_name <- paste0("s_e_career_path___", code)
        submit_data[[field_name]] <- if (code %in% input$career_path) "1" else "0"
      }

      # Career path other
      submit_data$s_e_career_oth <- if ("4" %in% input$career_path) input$career_oth %||% "" else ""

      # Add fellowship checkboxes - EXPAND TO INDIVIDUAL FIELDS
      fellow_field <- get_field_info("s_e_fellow")
      fellow_choices <- parse_choices(get_choices_string(fellow_field))

      for (code in fellow_choices) {
        field_name <- paste0("s_e_fellow___", code)
        submit_data[[field_name]] <- if (code %in% input$fellow) "1" else "0"
      }

      # Fellowship other
      submit_data$s_e_fellow_oth <- if ("1" %in% input$fellow) input$fellow_oth %||% "" else ""

      # Track pursuit
      submit_data$s_e_track <- input$track %||% "0"

      # Track type checkboxes - EXPAND TO INDIVIDUAL FIELDS
      track_type_field <- get_field_info("s_e_track_type")
      track_type_choices <- parse_choices(get_choices_string(track_type_field))

      for (code in track_type_choices) {
        field_name <- paste0("s_e_track_type___", code)
        submit_data[[field_name]] <- if (code %in% input$track_type) "1" else "0"
      }

      message("Submitting to REDCap with ", ncol(submit_data), " fields")

      # Submit to REDCap using REDCapR directly (like Learning module)
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

      if (result$success) {
        showNotification("Wellness and career planning saved successfully!", type = "message")
      } else {
        showNotification(paste("Error:", result$message), type = "error")
      }
    })
  })
}