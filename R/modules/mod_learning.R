#' Learning Module UI
#'
#' Combines visualizations with data entry for learning goals
#'
#' @param id Module ID
#' @export
mod_learning_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Learning & Development"),
    
    # ITE Scores Section
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        h4("ITE Performance Summary")
      ),
      div(
        class = "card-body",
        uiOutput(ns("ite_visualization"))
      )
    ),
    
    # Risk Assessment Section
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        h4("Performance Risk Assessment")
      ),
      div(
        class = "card-body",
        uiOutput(ns("risk_visualization"))
      )
    ),
    
    # Step 3 Section
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        h4("Step 3 Status")
      ),
      div(
        class = "card-body",
        uiOutput(ns("step3_visualization"))
      )
    ),

    # Board Preparation Section
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        h4("Board Preparation Progress")
      ),
      div(
        class = "card-body",
        div(
          class = "mb-3",
          tags$label(
            class = "form-label",
            tags$strong("How is your progress with reviewing your MKSAP coming along?"),
            br(),
            "Given your In-Training results and your MKSAP prep, what concerns do you have in board preparation? Do you feel it is going well? Not well? What is your plan going forward?"
          ),
          textAreaInput(
            ns("board_discussion"),
            label = NULL,
            placeholder = "Describe your board preparation progress, concerns, and plans...",
            rows = 5,
            width = "100%"
          )
        )
      )
    ),

    # Previous Period Review
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        h4("Previous Period Learning Goals")
      ),
      div(
        class = "card-body",
        uiOutput(ns("previous_goals_visualization"))
      )
    ),
    
    # Current Period Entry
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        h4("Current Period Learning Goals")
        ),
      div(
        class = "card-body",
        div(
          class = "alert alert-info",
          role = "alert",
          icon("info-circle"),
          " ",
          tags$strong("Review and Update:"),
          " Please review your previous period's topics and learning styles shown above. ",
          "Update your selections below based on your current development needs."
        ),
        uiOutput(ns("current_entry")),
        br(),
        actionButton(ns("submit"), "Submit Learning Goals", 
                    class = "btn btn-primary")
      )
    ),
    
    # Success message
    uiOutput(ns("submit_message"))
  )
}

#' Learning Module Server
#'
#' @param id Module ID
#' @param rdm_data Reactive containing app data structure
#' @param record_id Reactive record ID
#' @param period Reactive or static period number
#' @param data_dict Data dictionary (reactive or static)
#' @export
mod_learning_server <- function(id, rdm_data, record_id, period, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper to parse REDCap choices safely - DEFINED FIRST
    parse_choices_safe <- function(choices_str) {
      if (is.na(choices_str) || choices_str == "" || is.null(choices_str)) {
        return(list(value = character(0), label = character(0)))
      }
      
      # Split by pipe
      choices <- strsplit(as.character(choices_str), " \\| ")[[1]]
      
      # Parse each choice
      values <- character(length(choices))
      labels <- character(length(choices))
      
      for (i in seq_along(choices)) {
        parts <- strsplit(choices[i], ", ", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          values[i] <- parts[1]
          labels[i] <- paste(parts[-1], collapse = ", ")
        }
      }
      
      list(value = values, label = labels)
    }
    
    # Get test_data (ITE scores) from rdm_data
    test_data <- reactive({
      req(rdm_data(), record_id())
      
      app_data <- rdm_data()
      
      if ("test_data" %in% names(app_data$all_forms)) {
        app_data$all_forms$test_data %>%
          dplyr::filter(record_id == !!record_id()) %>%
          dplyr::slice(1)
      } else {
        data.frame()
      }
    })
    
    # Get resident data
    resident_data <- reactive({
      req(rdm_data(), record_id())
      
      app_data <- rdm_data()
      app_data$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
    })
    
    # Get s_eval data
    s_eval_data <- reactive({
      req(rdm_data(), record_id())
      app_data <- rdm_data()
      
      if ("s_eval" %in% names(app_data$all_forms)) {
        app_data$all_forms$s_eval %>%
          dplyr::filter(record_id == !!record_id())
      } else {
        data.frame()
      }
    })
    
    # Get period info
    period_info <- reactive({
      if (is.reactive(period)) {
        period()
      } else {
        period
      }
    })
    
    # Get data dictionary
    dict <- reactive({
      if (is.reactive(data_dict)) {
        data_dict()
      } else {
        data_dict
      }
    })

    # Load existing data when resident/period changes
    observe({
      req(s_eval_data(), period_info(), dict())

      s_eval <- s_eval_data()
      current_period <- period_info()

      # Get current period data
      current_data <- s_eval %>%
        dplyr::filter(redcap_repeat_instrument == "s_eval",
                     redcap_repeat_instance == current_period)

      if (nrow(current_data) > 0) {
        # Load topic selections
        topic_fields <- grep("^s_e_topic_sel___", names(current_data), value = TRUE)
        if (length(topic_fields) > 0) {
          selected_topics <- character()
          for (field in topic_fields) {
            if (!is.na(current_data[[field]][1]) && current_data[[field]][1] == "1") {
              code <- sub("s_e_topic_sel___", "", field)
              selected_topics <- c(selected_topics, code)
            }
          }
          if (length(selected_topics) > 0) {
            updateCheckboxGroupInput(session, "topics", selected = selected_topics)
          }
        }

        # Load topic other
        if (!is.na(current_data$s_e_topic_oth[1]) && current_data$s_e_topic_oth[1] != "") {
          updateTextInput(session, "topic_other", value = current_data$s_e_topic_oth[1])
        }

        # Load learning style selections
        learn_fields <- grep("^s_e_learn_style___", names(current_data), value = TRUE)
        if (length(learn_fields) > 0) {
          selected_learns <- character()
          for (field in learn_fields) {
            if (!is.na(current_data[[field]][1]) && current_data[[field]][1] == "1") {
              code <- sub("s_e_learn_style___", "", field)
              selected_learns <- c(selected_learns, code)
            }
          }
          if (length(selected_learns) > 0) {
            updateCheckboxGroupInput(session, "learning_styles", selected = selected_learns)
          }
        }

        # Load learning other
        if (!is.na(current_data$s_e_learn_oth[1]) && current_data$s_e_learn_oth[1] != "") {
          updateTextInput(session, "learn_other", value = current_data$s_e_learn_oth[1])
        }

        # Load board discussion
        if (!is.na(current_data$s_e_board_discu[1]) && current_data$s_e_board_discu[1] != "") {
          updateTextAreaInput(session, "board_discussion", value = current_data$s_e_board_discu[1])
        }
      }
    })

    # ITE Scores Visualization
    output$ite_visualization <- renderUI({
      data <- test_data()

      if (is.null(data) || nrow(data) == 0) {
        return(p("No ITE scores available yet."))
      }

      # Try gmed visualization, fall back to custom table if it fails
      tryCatch({
        gmed::visualize_ite_scores(data, dict())
      }, error = function(e) {
        # Build table with rows by PGY year from wide column format
        # Column naming: pgy{1,2,3}_{category}_{metric}

        # Category abbreviation mappings - removed "ile" from percent
        cat_map <- c(
          "cards" = "Cardiology", "pulm" = "Pulmonary", "gi" = "Gastroenterology",
          "neph" = "Nephrology", "nephro" = "Nephrology", "heme" = "Hematology",
          "onc" = "Oncology", "hemonc" = "Hem/Onc",
          "id" = "Infectious Disease", "endo" = "Endocrinology", "rheum" = "Rheumatology",
          "neuro" = "Neurology", "allergy" = "Allergy/Immunology", "gim" = "General IM",
          "hvc" = "High Value Care", "geri" = "Geriatrics", "geriatrics" = "Geriatrics",
          "palliative" = "Palliative", "total" = "Total",
          "score" = "Score", "pct" = "Percent", "percent" = "Percent"
        )

        # Build table with topics as rows and years as columns
        pgy_years <- c("pgy1", "pgy2", "pgy3")

        # Collect all unique topics/metrics across all years
        all_topics <- list()
        for (pgy in pgy_years) {
          pgy_cols <- grep(paste0("^", pgy, "_"), names(data), value = TRUE)
          for (col in pgy_cols) {
            # Extract category from column name
            parts <- strsplit(gsub(paste0("^", pgy, "_"), "", col), "_")[[1]]
            cat_abbrev <- parts[1]
            metric <- if (length(parts) > 1) parts[length(parts)] else ""

            # Map to display name - remove "ile" from percentile
            display_name <- if (cat_abbrev %in% names(cat_map)) cat_map[cat_abbrev] else cat_abbrev
            if (metric != "" && metric != cat_abbrev) {
              metric_clean <- gsub("ile$", "", metric)  # Remove "ile" suffix
              metric_name <- if (metric_clean %in% names(cat_map)) cat_map[metric_clean] else metric_clean
              display_name <- paste(display_name, metric_name)
            }

            all_topics[[display_name]] <- TRUE
          }
        }

        topic_names <- names(all_topics)

        if (length(topic_names) > 0) {
          # Create data frame with topics as rows
          df <- data.frame(Topic = topic_names, stringsAsFactors = FALSE)

          # Add columns for each PGY year
          for (pgy in pgy_years) {
            pgy_label <- toupper(pgy)
            df[[pgy_label]] <- NA

            pgy_cols <- grep(paste0("^", pgy, "_"), names(data), value = TRUE)
            for (col in pgy_cols) {
              # Extract category from column name
              parts <- strsplit(gsub(paste0("^", pgy, "_"), "", col), "_")[[1]]
              cat_abbrev <- parts[1]
              metric <- if (length(parts) > 1) parts[length(parts)] else ""

              # Map to display name - remove "ile" from percentile
              display_name <- if (cat_abbrev %in% names(cat_map)) cat_map[cat_abbrev] else cat_abbrev
              if (metric != "" && metric != cat_abbrev) {
                metric_clean <- gsub("ile$", "", metric)  # Remove "ile" suffix
                metric_name <- if (metric_clean %in% names(cat_map)) cat_map[metric_clean] else metric_clean
                display_name <- paste(display_name, metric_name)
              }

              # Find the row for this topic
              row_idx <- which(df$Topic == display_name)
              if (length(row_idx) > 0) {
                df[row_idx, pgy_label] <- data[[col]][1]
              }
            }
          }

          DT::datatable(
            df,
            options = list(dom = 't', pageLength = 25, scrollX = TRUE),
            rownames = FALSE
          )
        } else {
          p("ITE data available but cannot be displayed.")
        }
      })
    })
    
    # Risk Assessment Visualization
    output$risk_visualization <- renderUI({
      data <- test_data()

      if (is.null(data) || nrow(data) == 0) {
        return(p("No ITE scores available for risk assessment."))
      }

      # Wrap in tryCatch to handle missing data gracefully
      tryCatch({
        risk <- gmed::assess_ite_risk(data)

        div(
          class = paste("alert", risk$risk_class),
          role = "alert",
          tags$strong("Risk Level: ", risk$risk_level),
          tags$br(),
          tags$small(risk$details)
        )
      }, error = function(e) {
        div(
          class = "alert alert-info",
          role = "alert",
          icon("info-circle"),
          " Risk assessment not available yet."
        )
      })
    })
    
    # Step 3 Visualization
    output$step3_visualization <- renderUI({
      res_data <- resident_data()
      req(res_data)
      
      step3_complete <- !is.na(res_data$step3[1]) && res_data$step3[1] == "1"
      
      if (step3_complete) {
        div(
          class = "alert alert-success",
          role = "alert",
          icon("check-circle"),
          " Step 3 Completed"
        )
      } else {
        tagList(
          radioButtons(
            ns("step3_status"),
            "Have you completed Step 3 (USMLE or COMLEX)?",
            choices = c("Yes" = "1", "No" = "0"),
            selected = character(0)
          ),
          uiOutput(ns("step3_followup"))
        )
      }
    })
    
    # Step 3 Follow-up
    output$step3_followup <- renderUI({
      req(input$step3_status)
      
      if (input$step3_status == "0") {
        tagList(
          radioButtons(
            ns("step3_date_set"),
            "Have you set a date for Step 3?",
            choices = c("Yes" = "1", "No" = "0"),
            selected = character(0)
          ),
          uiOutput(ns("step3_date_input"))
        )
      } else {
        NULL
      }
    })
    
    # Step 3 Date Input
    output$step3_date_input <- renderUI({
      req(input$step3_date_set)
      
      if (input$step3_date_set == "1") {
        dateInput(
          ns("step3_date"),
          "When are you scheduled to take Step 3?",
          value = NULL,
          format = "mm/dd/yyyy"
        )
      } else {
        NULL
      }
    })
    
    # Previous Goals Visualization
    output$previous_goals_visualization <- renderUI({
      s_eval <- s_eval_data()
      dictionary <- dict()
      req(dictionary, period_info())
      
      current_period <- period_info()
      
      if (is.null(s_eval) || nrow(s_eval) == 0 || current_period == 1) {
        return(p(em("No previous learning goals recorded.")))
      }
      
      # Get previous period
      prev_period <- current_period - 1
      prev_data <- s_eval %>%
        dplyr::filter(redcap_repeat_instrument == "s_eval",
                     redcap_repeat_instance == prev_period)
      
      if (nrow(prev_data) == 0) {
        return(p(em("No learning goals recorded for previous period.")))
      }
      
      # Get choices using safe parser
      topic_choices_str <- dictionary$select_choices_or_calculations[dictionary$field_name == "s_e_topic_sel"]
      if (length(topic_choices_str) > 0) {
        topic_choices_str <- topic_choices_str[1]
      } else {
        return(p(em("Error: field configuration not found")))
      }
      
      learn_choices_str <- dictionary$select_choices_or_calculations[dictionary$field_name == "s_e_learn_style"]
      if (length(learn_choices_str) > 0) {
        learn_choices_str <- learn_choices_str[1]
      } else {
        return(p(em("Error: field configuration not found")))
      }
      
      topic_choices <- parse_choices_safe(topic_choices_str)
      learn_choices <- parse_choices_safe(learn_choices_str)
      
      # Get topic selections
      topic_fields <- grep("^s_e_topic_sel___", names(prev_data), value = TRUE)
      selected_topics <- topic_fields[prev_data[1, topic_fields] == "1"]
      selected_topics <- gsub("s_e_topic_sel___", "", selected_topics)
      
      topic_labels <- topic_choices$label[match(selected_topics, topic_choices$value)]
      
      # Check for "Other"
      if ("23" %in% selected_topics && !is.na(prev_data$s_e_topic_oth[1]) && prev_data$s_e_topic_oth[1] != "") {
        topic_labels[topic_labels == "Other"] <- paste0("Other: ", prev_data$s_e_topic_oth[1])
      }
      
      # Get learning style selections
      learn_fields <- grep("^s_e_learn_style___", names(prev_data), value = TRUE)
      selected_learns <- learn_fields[prev_data[1, learn_fields] == "1"]
      selected_learns <- gsub("s_e_learn_style___", "", selected_learns)
      
      learn_labels <- learn_choices$label[match(selected_learns, learn_choices$value)]
      
      # Check for "Other"
      if ("12" %in% selected_learns && !is.na(prev_data$s_e_learn_oth[1]) && prev_data$s_e_learn_oth[1] != "") {
        learn_labels[learn_labels == "Other"] <- paste0("Other: ", prev_data$s_e_learn_oth[1])
      }
      
      div(
        class = "p-3",
        style = "background-color: #f8f9fa; border-left: 3px solid #007bff;",
        h5(paste("Period", prev_period)),
        div(
          tags$strong("Topics for Development:"),
          if (length(topic_labels) > 0 && !all(is.na(topic_labels))) {
            tags$ul(
              lapply(topic_labels[!is.na(topic_labels)], function(x) tags$li(x))
            )
          } else {
            p(em("None recorded"))
          }
        ),
        div(
          tags$strong("Desired Learning Experiences:"),
          if (length(learn_labels) > 0 && !all(is.na(learn_labels))) {
            tags$ul(
              lapply(learn_labels[!is.na(learn_labels)], function(x) tags$li(x))
            )
          } else {
            p(em("None recorded"))
          }
        )
      )
    })
    
    # Current Period Entry
    output$current_entry <- renderUI({
      dictionary <- dict()
      req(dictionary, period_info())
      
      # Get choices using safe parser
      topic_choices_str <- dictionary$select_choices_or_calculations[dictionary$field_name == "s_e_topic_sel"]
      if (length(topic_choices_str) > 0) {
        topic_choices_str <- topic_choices_str[1]
      } else {
        return(p(em("Error: field configuration not found")))
      }
      
      learn_choices_str <- dictionary$select_choices_or_calculations[dictionary$field_name == "s_e_learn_style"]
      if (length(learn_choices_str) > 0) {
        learn_choices_str <- learn_choices_str[1]
      } else {
        return(p(em("Error: field configuration not found")))
      }
      
      topic_choices <- parse_choices_safe(topic_choices_str)
      learn_choices <- parse_choices_safe(learn_choices_str)
      
      tagList(
        h5(paste("Period", period_info())),
        
        # Topic Selection
        div(
          class = "mb-3",
          tags$label(
            class = "form-label",
            tags$strong("Select the top 3 topics you feel least confident about:")
          ),
          checkboxGroupInput(
            ns("topics"),
            label = NULL,
            choices = setNames(topic_choices$value, topic_choices$label)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("topics"), "'].includes('23')"),
            textInput(ns("topic_other"), "Specify other topic:")
          )
        ),
        
        # Learning Style Selection
        div(
          class = "mb-3",
          tags$label(
            class = "form-label",
            tags$strong("Select desired learning experiences:")
          ),
          checkboxGroupInput(
            ns("learning_styles"),
            label = NULL,
            choices = setNames(learn_choices$value, learn_choices$label)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("learning_styles"), "'].includes('12')"),
            textInput(ns("learn_other"), "Specify other learning experience:")
          )
        )
      )
    })
    
    # Submit handler
    observeEvent(input$submit, {
      req(record_id(), period_info())
      
      dictionary <- dict()
      
      # Prepare data for submission
      submit_data <- data.frame(
        record_id = record_id(),
        redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance = period_info(),
        s_e_period = as.character(period_info()),
        s_e_date = format(Sys.Date(), "%Y-%m-%d"),
        stringsAsFactors = FALSE
      )
      
      # Add Step 3 data if not complete
      resident_complete <- !is.na(resident_data()$step3[1]) && resident_data()$step3[1] == "1"
      if (!resident_complete && !is.null(input$step3_status)) {
        submit_data$s_e_step3 <- input$step3_status
        
        if (input$step3_status == "1") {
          # Mark resident data step3 as complete
          step3_data <- data.frame(
            record_id = record_id(),
            step3 = "1",
            stringsAsFactors = FALSE
          )
          
          REDCapR::redcap_write_oneshot(
            ds = step3_data,
            redcap_uri = app_config$redcap_url,
            token = app_config$rdm_token
          )
        } else {
          if (!is.null(input$step3_date_set)) {
            submit_data$s_e_step3_date_set <- input$step3_date_set
            
            if (input$step3_date_set == "1" && !is.null(input$step3_date)) {
              submit_data$s_e_step3_date <- format(input$step3_date, "%Y-%m-%d")
            }
          }
        }
      }
      
      # Add topic selections (checkbox format)
      if (!is.null(input$topics)) {
        topic_choices_str <- dictionary$select_choices_or_calculations[dictionary$field_name == "s_e_topic_sel"][1]
        topic_choices <- parse_choices_safe(topic_choices_str)
        
        for (val in topic_choices$value) {
          field_name <- paste0("s_e_topic_sel___", val)
          submit_data[[field_name]] <- if (val %in% input$topics) "1" else "0"
        }
        
        if ("23" %in% input$topics && !is.null(input$topic_other) && input$topic_other != "") {
          submit_data$s_e_topic_oth <- input$topic_other
        }
      }
      
      # Add learning style selections (checkbox format)
      if (!is.null(input$learning_styles)) {
        learn_choices_str <- dictionary$select_choices_or_calculations[dictionary$field_name == "s_e_learn_style"][1]
        learn_choices <- parse_choices_safe(learn_choices_str)

        for (val in learn_choices$value) {
          field_name <- paste0("s_e_learn_style___", val)
          submit_data[[field_name]] <- if (val %in% input$learning_styles) "1" else "0"
        }

        if ("12" %in% input$learning_styles && !is.null(input$learn_other) && input$learn_other != "") {
          submit_data$s_e_learn_oth <- input$learn_other
        }
      }

      # Add board discussion
      if (!is.null(input$board_discussion) && input$board_discussion != "") {
        submit_data$s_e_board_discu <- input$board_discussion
      }

      # Submit to REDCap using REDCapR
      result <- tryCatch({
        result_obj <- REDCapR::redcap_write_oneshot(
          ds = submit_data,
          redcap_uri = app_config$redcap_url,
          token = app_config$rdm_token
        )
        
        if (result_obj$success) {
          TRUE
        } else {
          showNotification(
            paste("Error submitting data:", result_obj$outcome_message),
            type = "error",
            duration = 10
          )
          FALSE
        }
      }, error = function(e) {
        showNotification(
          paste("Error submitting data:", e$message),
          type = "error",
          duration = 10
        )
        FALSE
      })
      
      if (result) {
        output$submit_message <- renderUI({
          div(
            class = "alert alert-success mt-3",
            role = "alert",
            icon("check-circle"),
            " Learning goals submitted successfully!"
          )
        })
        
        shinyjs::delay(5000, {
          output$submit_message <- renderUI(NULL)
        })
      }
    })
  })
}