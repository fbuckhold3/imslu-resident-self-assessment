# ============================================================================
# GOALS MODULE - COMPLETE INTEGRATION
# ============================================================================

#' Goal Setting Module UI
#' @export
goalSettingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .previous-goal-panel {
        background-color: #f8f9fa;
        border-left: 4px solid #0072B2;
        padding: 15px;
        margin-bottom: 20px;
      }
      .goal-card {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 15px;
        background-color: white;
      }
      .milestone-table-container {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-top: 20px;
        background-color: white;
      }
      .milestone-table {
        width: 100%;
        font-size: 0.85em;
      }
      .milestone-table th {
        background-color: #f8f9fa;
        font-weight: 600;
        padding: 8px;
        border: 1px solid #dee2e6;
      }
      .milestone-table td {
        padding: 8px;
        border: 1px solid #dee2e6;
      }
    ")),
    
    fluidRow(
      # Left: Milestone Spider Plot
      column(5,
       div(class = "goal-card",
           h5("Your Milestone Self-Assessment",
              style = "color: #2c3e50; text-align: center; margin-bottom: 20px;"),
           plotly::plotlyOutput(ns("spider_plot"), height = "500px")
       )
      ),

      
      # Right: Goal Entry
      column(7,
             uiOutput(ns("goal_content"))
      )
    )
  )
}

#' Goal Setting Module Server
#' @export
goalSettingServer <- function(id, rdm_dict_data, subcompetency_maps, 
                              competency_list, milestone_levels, 
                              current_milestone_data, resident_info, 
                              selected_period, ilp_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: Extract period number from period name
    extract_period_number <- function(period_name) {
      period_map <- c(
        "Mid Intern" = 1, "End Intern" = 2,
        "Mid PGY2" = 3, "End PGY2" = 4,
        "Mid PGY3" = 5, "End PGY3" = 6, "Graduating" = 6,
        "Entering Residency" = 7
      )
      as.numeric(period_map[period_name])
    }

    # Track current page
    current_page <- reactiveVal("pcmk")
    
    # Store responses
    responses <- reactiveVal(list())
    
    # Previous goals data
    previous_goals <- reactiveVal(NULL)
    
    # Get period info
    period_name <- reactive({
      if (is.function(selected_period)) {
        p <- selected_period()
        if (is.list(p) && "period_name" %in% names(p)) {
          return(p$period_name)
        }
        return(p)
      }
      return(selected_period)
    })
    
    # Get milestone data from workflow
    milestone_data <- reactive({
      req(current_milestone_data)
      
      ms_data <- if (is.function(current_milestone_data)) {
        current_milestone_data()
      } else {
        current_milestone_data
      }
      
      message("=== Milestone data check ===")
      message("  Is list: ", is.list(ms_data))
      message("  Has data: ", !is.null(ms_data$data))
      if (!is.null(ms_data$data)) {
        message("  Data rows: ", nrow(ms_data$data))
        message("  Milestone cols: ", length(ms_data$milestone_cols))
      }
      
      return(ms_data)
    })
    
    # Load previous goals from ILP
    observe({
      req(ilp_data)
      
      ilp <- if (is.function(ilp_data)) ilp_data() else ilp_data
      
      if (is.null(ilp) || nrow(ilp) == 0) {
        message("No ILP data found")
        return()
      }
      
      # Get most recent ILP entry for this resident
      res_info <- if (is.function(resident_info)) resident_info() else resident_info
      rec_id <- res_info$record_id
      
      prev_ilp <- ilp %>%
        dplyr::filter(record_id == !!rec_id) %>%
        dplyr::arrange(dplyr::desc(redcap_repeat_instance)) %>%
        dplyr::slice(1)
      
      if (nrow(prev_ilp) > 0) {
        message("Found previous ILP data for resident ", rec_id)
        previous_goals(prev_ilp)
      }
    })
    
    # Render spider plot using gmed function
    output$spider_plot <- plotly::renderPlotly({
      ms_data <- milestone_data()

      if (is.null(ms_data) || is.null(ms_data$data) || nrow(ms_data$data) == 0) {
        return(NULL)
      }

      milestone_cols <- ms_data$milestone_cols

      if (is.null(milestone_cols) || length(milestone_cols) == 0) {
        milestone_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$",
                              names(ms_data$data), value = TRUE)
      }

      if (length(milestone_cols) == 0) {
        return(NULL)
      }

      # Get resident info
      res_info <- if (is.function(resident_info)) resident_info() else resident_info
      rec_id <- res_info$record_id

      # Get current period name
      current_period <- period_name()

      # Filter for this resident
      resident_data <- ms_data$data %>%
        dplyr::filter(record_id == !!rec_id)

      # If data is already pre-filtered (small number of rows), skip period filtering
      # This happens when wrapper provides ACGME data from previous period
      if (nrow(resident_data) > 10) {
        # Filter by period if period_name column exists
        if ("period_name" %in% names(resident_data)) {
          resident_data <- resident_data %>%
            dplyr::filter(period_name == !!current_period)
        }
      }

      # If no data, try filtering by period number field
      # Check for both prog_mile_period and acgme_mile_period
      if (nrow(resident_data) == 0) {
        period_field <- NULL
        if ("prog_mile_period" %in% names(ms_data$data)) {
          period_field <- "prog_mile_period"
        } else if ("acgme_mile_period" %in% names(ms_data$data)) {
          period_field <- "acgme_mile_period"
        }

        if (!is.null(period_field)) {
          # Extract period number from period name
          period_num <- extract_period_number(current_period)
          if (!is.na(period_num)) {
            resident_data <- ms_data$data %>%
              dplyr::filter(
                record_id == !!rec_id,
                !!rlang::sym(period_field) == !!period_num
              )
          }
        }
      }

      if (nrow(resident_data) == 0) {
        # Try without period filter (get most recent)
        resident_data <- ms_data$data %>%
          dplyr::filter(record_id == !!rec_id) %>%
          dplyr::arrange(dplyr::desc(redcap_repeat_instance)) %>%
          dplyr::slice(1)
      }

      if (nrow(resident_data) == 0) {
        return(NULL)
      }

      # Ensure we have exactly 1 row for the spider plot
      # If multiple rows exist, take the most recent one
      if (nrow(resident_data) > 1) {
        if ("redcap_repeat_instance" %in% names(resident_data)) {
          resident_data <- resident_data %>%
            dplyr::arrange(dplyr::desc(redcap_repeat_instance)) %>%
            dplyr::slice(1)
        } else {
          resident_data <- resident_data %>% dplyr::slice(1)
        }
      }

      message("=== SPIDER PLOT DATA ===")
      message("Resident data rows after filtering: ", nrow(resident_data))
      message("Milestone columns: ", length(milestone_cols))
      message("Sample milestone cols: ", paste(head(milestone_cols, 3), collapse = ", "))

      # Get median data if available
      median_data <- NULL
      if (!is.null(ms_data$medians) && nrow(ms_data$medians) > 0) {
        median_data <- ms_data$medians %>%
          dplyr::filter(period_name == !!current_period)

        if (nrow(median_data) == 0) {
          median_data <- NULL
        }
      }

      # Detect milestone type from column names
      milestone_type <- if (any(grepl("^acgme_", milestone_cols))) {
        "acgme"
      } else if (any(grepl("_self$", milestone_cols))) {
        "self"
      } else {
        "program"
      }

      message("Using milestone type: ", milestone_type)

      # Create resident lookup data frame using record_id
      # record_id is consistent across repeating instances
      resident_lookup <- data.frame(
        record_id = rec_id,
        name_first = as.character(rec_id),
        name_last = "",
        stringsAsFactors = FALSE
      )

      # Call gmed enhanced spider plot function
      tryCatch({
        gmed::create_enhanced_milestone_spider_plot(
          milestone_data = resident_data,
          median_data = median_data,
          resident_id = as.character(rec_id),
          period_text = current_period,
          milestone_type = milestone_type,
          resident_data = resident_lookup
        )
      }, error = function(e) {
        message("Spider plot error: ", e$message)
        return(NULL)
      })
    })
    
    # Render goal content based on page
    output$goal_content <- renderUI({
      page <- current_page()
      
      div(class = "goal-card",
          h4(paste("Goal:", page_title(page)), 
             style = "color: #2c3e50; margin-top: 0;"),
          
          # Previous goal review
          wellPanel(
            class = "previous-goal-panel",
            h5("Previous Goal Review", style = "margin-top: 0;"),
            previous_goal_ui(ns, page)
          ),
          
          # New goal setting
          wellPanel(
            style = "background-color: #f8f9fa; padding: 15px;",
            h5(paste("Set New", page_title(page), "Goal"), style = "margin-top: 0;"),
            new_goal_ui(ns, page)
          ),
          
          # Milestone table for selected subcompetency
          uiOutput(ns(paste0("milestone_table_", page))),
          
          # Navigation
          hr(),
          div(class = "d-flex justify-content-between",
              if (page != "pcmk") {
                actionButton(ns("btn_prev"), "← Previous", class = "btn-secondary")
              } else {
                div()
              },
              if (page == "profics") {
                actionButton(ns("btn_submit"), "Submit Goals", class = "btn-success")
              } else {
                actionButton(ns("btn_next"), "Next →", class = "btn-primary")
              }
          )
      )
    })
    
    # Helper: page title
    page_title <- function(page) {
      switch(page,
             "pcmk" = "Patient Care / Medical Knowledge",
             "sbppbl" = "Systems-Based Practice / PBLI",
             "profics" = "Professionalism / ICS",
             "Unknown")
    }
    
    # Helper: previous goal UI
    previous_goal_ui <- function(ns, domain) {
      prev <- previous_goals()
      
      if (is.null(prev) || nrow(prev) == 0) {
        return(p("No previous goal recorded for this domain", class = "text-muted"))
      }
      
      # Map domain to ILP field names
      field_map <- get_ilp_field_map(domain)
      
      goal_val <- prev[[field_map$goal]][1]
      level_val <- prev[[field_map$level]][1]
      how_val <- prev[[field_map$how]][1]
      
      if (is.na(goal_val) || goal_val == "") {
        return(p("No previous goal recorded for this domain", class = "text-muted"))
      }
      
      # Get label for the goal value
      goal_label <- get_goal_label(domain, goal_val)
      
      tagList(
        div(style = "margin-bottom: 10px;",
            strong("Subcompetency: "),
            span(goal_label)
        ),
        div(style = "margin-bottom: 10px;",
            strong("Target Level: "),
            span(paste("Level", level_val))
        ),
        if (!is.na(how_val) && nzchar(how_val)) {
          div(style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
              strong("How to achieve: "),
              div(style = "margin-top: 5px; font-style: italic;", how_val)
          )
        },
        hr(),
        radioButtons(ns(paste0("reached_", domain)),
                    "Did you reach this goal?",
                    choices = c("Yes" = "1", "No" = "0"),
                    inline = TRUE),
        uiOutput(ns(paste0("review_text_", domain)))
      )
    }
    
    # Dynamic review text based on yes/no
    observe({
      req(input$reached_pcmk)
      output$review_text_pcmk <- renderUI({
        if (input$reached_pcmk == "1") {
          textAreaInput(ns("review_pcmk_yes"),
                       "Why were you able to achieve it?",
                       rows = 3, width = "100%")
        } else {
          textAreaInput(ns("review_pcmk_no"),
                       "What prevented you from achieving it?",
                       rows = 3, width = "100%")
        }
      })
    })
    
    observe({
      req(input$reached_sbppbl)
      output$review_text_sbppbl <- renderUI({
        if (input$reached_sbppbl == "1") {
          textAreaInput(ns("review_sbppbl_yes"),
                       "Why were you able to achieve it?",
                       rows = 3, width = "100%")
        } else {
          textAreaInput(ns("review_sbppbl_no"),
                       "What prevented you from achieving it?",
                       rows = 3, width = "100%")
        }
      })
    })
    
    observe({
      req(input$reached_profics)
      output$review_text_profics <- renderUI({
        if (input$reached_profics == "1") {
          textAreaInput(ns("review_profics_yes"),
                       "Why were you able to achieve it?",
                       rows = 3, width = "100%")
        } else {
          textAreaInput(ns("review_profics_no"),
                       "What prevented you from achieving it?",
                       rows = 3, width = "100%")
        }
      })
    })
    
    # Helper: new goal UI
    new_goal_ui <- function(ns, domain) {
      choices <- get_subcomp_choices(domain)

      tagList(
        selectInput(ns(paste0("subcomp_", domain)),
                   "Select Subcompetency:",
                   choices = choices,
                   width = "100%")
      )
    }
    
    # Render milestone tables
    output$milestone_table_pcmk <- renderUI({
      req(input$subcomp_pcmk)
      render_milestone_table("pcmk", input$subcomp_pcmk)
    })
    
    output$milestone_table_sbppbl <- renderUI({
      req(input$subcomp_sbppbl)
      render_milestone_table("sbppbl", input$subcomp_sbppbl)
    })
    
    output$milestone_table_profics <- renderUI({
      req(input$subcomp_profics)
      render_milestone_table("profics", input$subcomp_profics)
    })

    # Helper: render milestone table
    render_milestone_table <- function(domain, selected_subcomp) {
      # Get the competency code
      comp_code <- get_comp_code_from_selection(domain, selected_subcomp)

      if (is.null(comp_code)) {
        return(div(class = "alert alert-warning", "No milestone data available"))
      }

      message("Rendering milestone table for: ", comp_code)

      # Get milestone table from data dictionary
      table_data <- get_milestone_table_from_dict(comp_code)

      if (is.null(table_data) || nrow(table_data) == 0) {
        return(div(class = "alert alert-warning",
                   paste("No milestone descriptions found for", comp_code)))
      }

      div(class = "milestone-table-container",
          h5(paste(comp_code, "Milestone Levels"),
             style = "color: #0072B2; margin-bottom: 15px;"),
          div(style = "overflow-x: auto;",
              tags$table(class = "milestone-table",
                        tags$thead(
                          tags$tr(
                            tags$th("Milestone", style = "width: 100px;"),
                            tags$th("Level 1: Novice"),
                            tags$th("Level 2: Advanced Beginner"),
                            tags$th("Level 3: Competent"),
                            tags$th("Level 4: Proficient"),
                            tags$th("Level 5: Expert")
                          )
                        ),
                        tags$tbody(
                          lapply(1:nrow(table_data), function(i) {
                            tags$tr(
                              tags$td(strong(table_data$Row[i])),
                              tags$td(table_data$Level_1[i]),
                              tags$td(table_data$Level_2[i]),
                              tags$td(table_data$Level_3[i]),
                              tags$td(table_data$Level_4[i]),
                              tags$td(table_data$Level_5[i])
                            )
                          })
                        )
              )
          ),
          div(style = "margin-top: 15px;",
              fluidRow(
                column(6,
                       selectInput(ns(paste0("milestone_row_", domain)),
                                  "Select Milestone Row:",
                                  choices = setNames(1:nrow(table_data), table_data$Row),
                                  width = "100%")
                ),
                column(6,
                       selectInput(ns(paste0("target_level_", domain)),
                                  "Target Level:",
                                  choices = setNames(1:5, c("1: Novice", "2: Advanced Beginner",
                                                            "3: Competent", "4: Proficient", "5: Expert")),
                                  width = "100%")
                )
              )
          ),
          div(class = "alert alert-info", style = "margin-top: 15px;",
              uiOutput(ns(paste0("selected_milestone_desc_", domain)))
          ),
          hr(),
          div(style = "margin-top: 20px;",
              h5("Goal Achievement Plan", style = "margin-bottom: 10px;"),
              textAreaInput(ns(paste0("how_", domain)),
                           "How will you achieve this goal?",
                           rows = 4,
                           width = "100%",
                           placeholder = "Describe your specific actions and strategies to reach this milestone level...")
          )
      )
    }

    # Helper: get milestone table from data dictionary
    get_milestone_table_from_dict <- function(comp_code) {
      dict <- tryCatch({
        if (is.function(rdm_dict_data)) {
          rdm_dict_data()
        } else {
          rdm_dict_data
        }
      }, error = function(e) {
        message("Error getting data dictionary: ", e$message)
        return(NULL)
      })

      if (is.null(dict)) {
        message("Data dictionary is NULL")
        return(NULL)
      }

      # Parse comp_code (e.g., "PC1" -> domain="PC", num="1")
      domain <- gsub("\\d+$", "", comp_code)
      comp_num <- gsub("^[A-Z]+", "", comp_code)

      # Convert to lowercase for field matching
      prefix_lower <- tolower(domain)
      if (domain == "PBLI") prefix_lower <- "pbl"

      # Find milestone row fields: pc1_r1, pc1_r2, etc.
      pattern <- paste0("^", prefix_lower, comp_num, "_r\\d+$")

      message("Looking for pattern: ", pattern, " (comp_code: ", comp_code, ")")

      # Filter for matching fields
      fields <- dict %>%
        dplyr::filter(grepl(pattern, field_name)) %>%
        dplyr::select(field_name, field_label, select_choices_or_calculations) %>%
        dplyr::arrange(field_name)

      message("Found ", nrow(fields), " matching milestone rows")

      if (nrow(fields) == 0) {
        return(NULL)
      }

      # Parse choices into table
      result <- data.frame(
        Row = character(nrow(fields)),
        Level_1 = character(nrow(fields)),
        Level_2 = character(nrow(fields)),
        Level_3 = character(nrow(fields)),
        Level_4 = character(nrow(fields)),
        Level_5 = character(nrow(fields)),
        stringsAsFactors = FALSE
      )

      for (i in 1:nrow(fields)) {
        # Extract row number from field name
        row_num <- gsub(paste0("^", prefix_lower, comp_num, "_r"), "", fields$field_name[i])
        result$Row[i] <- paste("Row", row_num)

        choices_text <- fields$select_choices_or_calculations[i]

        if (is.na(choices_text) || choices_text == "") {
          message("Row ", i, ": No choices text")
          next
        }

        # Parse: "1, Description | 2, Description | ..."
        choices <- strsplit(choices_text, "\\s*\\|\\s*")[[1]]

        for (choice in choices) {
          # Split on first comma to separate level from description
          parts <- strsplit(trimws(choice), ",\\s*", perl = TRUE)[[1]]

          if (length(parts) < 2) next

          level <- as.numeric(trimws(parts[1]))

          if (is.na(level) || level < 1 || level > 5) next

          # Join remaining parts as description
          description <- paste(parts[-1], collapse = ", ")
          description <- trimws(description)

          # Store in appropriate column
          result[i, paste0("Level_", level)] <- description
        }
      }

      message("Returning table with ", nrow(result), " rows")

      return(result)
    }
    
    # Helper: get ILP field map
    get_ilp_field_map <- function(domain) {
      switch(domain,
             "pcmk" = list(goal = "goal_pcmk", 
                          level = "goal_level_pcmk",
                          how = "how_pcmk"),
             "sbppbl" = list(goal = "goal_sbppbl",
                            level = "goal_level_sbppbl",
                            how = "how_sbppbl"),
             "profics" = list(goal = "goal_subcomp_profics",
                             level = "goal_level_profics",
                             how = "how_profics"),
             list(goal = NA, level = NA, how = NA))
    }
    
    # Helper: get goal label
    get_goal_label <- function(domain, value) {
      choices <- get_subcomp_choices(domain)
      label <- names(choices)[choices == value]
      if (length(label) > 0) return(label[1])
      return(paste("Option", value))
    }
    
    # Helper: get comp code from selection
    get_comp_code_from_selection <- function(domain, value) {
      switch(domain,
             "pcmk" = {
               if (as.numeric(value) <= 6) {
                 paste0("PC", value)
               } else {
                 paste0("MK", as.numeric(value) - 6)
               }
             },
             "sbppbl" = {
               if (as.numeric(value) <= 3) {
                 paste0("SBP", value)
               } else {
                 paste0("PBLI", as.numeric(value) - 3)
               }
             },
             "profics" = {
               if (as.numeric(value) <= 4) {
                 paste0("PROF", value)
               } else {
                 paste0("ICS", as.numeric(value) - 4)
               }
             },
             NULL)
    }
    
    # Helper: get subcompetency choices
    get_subcomp_choices <- function(domain) {
      switch(domain,
             "pcmk" = c("PC1: History" = "1",
                       "PC2: Physical Examination" = "2",
                       "PC3: Clinical Reasoning" = "3",
                       "PC4: Patient Management - Inpatient" = "4",
                       "PC5: Patient Management - Outpatient" = "5",
                       "PC6: Digital Health" = "6",
                       "MK1: Applied Foundational Sciences" = "7",
                       "MK2: Therapeutic Knowledge" = "8",
                       "MK3: Knowledge of Diagnostic Testing" = "9"),
             "sbppbl" = c("SBP1: Patient Safety and Quality Improvement" = "1",
                         "SBP2: System Navigation for Patient-Centered Care" = "2",
                         "SBP3: Physician Role in Health Care Systems" = "3",
                         "PBLI1: Evidence-Based and Informed Practice" = "4",
                         "PBLI2: Reflective Practice and Commitment to Personal Growth" = "5"),
             "profics" = c("PROF1: Professional Behavior" = "1",
                          "PROF2: Ethical Principles" = "2",
                          "PROF3: Accountability/Conscientiousness" = "3",
                          "PROF4: Knowledge of Systemic and Individual Factors of Well-Being" = "4",
                          "ICS1: Patient- and Family-Centered Communication" = "5",
                          "ICS2: Interprofessional and Team Communication" = "6",
                          "ICS3: Communication within Health Care Systems" = "7"),
             c("Unknown" = "0"))
    }
    
    # Navigation observers
    observeEvent(input$btn_next, {
      page <- current_page()
      save_page_responses(page)
      
      next_page <- switch(page,
                         "pcmk" = "sbppbl",
                         "sbppbl" = "profics",
                         "pcmk")
      current_page(next_page)
    })
    
    observeEvent(input$btn_prev, {
      page <- current_page()
      save_page_responses(page)
      
      prev_page <- switch(page,
                         "sbppbl" = "pcmk",
                         "profics" = "sbppbl",
                         "pcmk")
      current_page(prev_page)
    })
    
    # Save page responses
    save_page_responses <- function(page) {
      current_responses <- responses()
      
      current_responses[[page]] <- list(
        subcompetency = input[[paste0("subcomp_", page)]],
        target_level = input[[paste0("target_level_", page)]],
        milestone_row = input[[paste0("milestone_row_", page)]],
        how_to_achieve = input[[paste0("how_", page)]],
        reached_previous = input[[paste0("reached_", page)]],
        review_text = if (!is.null(input[[paste0("reached_", page)]])) {
          if (input[[paste0("reached_", page)]] == "1") {
            input[[paste0("review_", page, "_yes")]]
          } else {
            input[[paste0("review_", page, "_no")]]
          }
        } else {
          NULL
        }
      )
      
      responses(current_responses)
    }

    # Show selected milestone description
observe({
  req(input$subcomp_pcmk, input$milestone_row_pcmk, input$target_level_pcmk)
  
  output$selected_milestone_desc_pcmk <- renderUI({
    comp_code <- get_comp_code_from_selection("pcmk", input$subcomp_pcmk)
    table_data <- get_milestone_table_from_dict(comp_code)
    
    if (is.null(table_data)) return(NULL)
    
    row_idx <- as.numeric(input$milestone_row_pcmk)
    level_idx <- as.numeric(input$target_level_pcmk)
    
    desc <- table_data[row_idx, paste0("Level_", level_idx)]
    
    tagList(
      strong("Your selected milestone goal:"),
      p(style = "margin-top: 10px;", desc)
    )
  })
})

observe({
  req(input$subcomp_sbppbl, input$milestone_row_sbppbl, input$target_level_sbppbl)
  
  output$selected_milestone_desc_sbppbl <- renderUI({
    comp_code <- get_comp_code_from_selection("sbppbl", input$subcomp_sbppbl)
    table_data <- get_milestone_table_from_dict(comp_code)
    
    if (is.null(table_data)) return(NULL)
    
    row_idx <- as.numeric(input$milestone_row_sbppbl)
    level_idx <- as.numeric(input$target_level_sbppbl)
    
    desc <- table_data[row_idx, paste0("Level_", level_idx)]
    
    tagList(
      strong("Your selected milestone goal:"),
      p(style = "margin-top: 10px;", desc)
    )
  })
})

observe({
  req(input$subcomp_profics, input$milestone_row_profics, input$target_level_profics)
  
  output$selected_milestone_desc_profics <- renderUI({
    comp_code <- get_comp_code_from_selection("profics", input$subcomp_profics)
    table_data <- get_milestone_table_from_dict(comp_code)
    
    if (is.null(table_data)) return(NULL)
    
    row_idx <- as.numeric(input$milestone_row_profics)
    level_idx <- as.numeric(input$target_level_profics)
    
    desc <- table_data[row_idx, paste0("Level_", level_idx)]
    
    tagList(
      strong("Your selected milestone goal:"),
      p(style = "margin-top: 10px;", desc)
    )
  })
})
    
    # Submit handler
    observeEvent(input$btn_submit, {
      save_page_responses(current_page())
      
      showNotification("Goals submitted!", type = "message")
      message("All responses: ", paste(capture.output(str(responses())), collapse = "\n"))
      
      # TODO: Add REDCap submission logic here
    })
    
    # Return module outputs
    return(list(
      responses = responses,
      current_page = current_page
    ))
  })
}