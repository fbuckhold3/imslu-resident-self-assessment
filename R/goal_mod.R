#' Goal Setting Module UI
#' 
#' Three-page wizard for milestone-based goal setting (PC/MK, SBP/PBLI, PROF/ICS)
#' @param id Module namespace
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
    .milestone-table-container {
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 15px;
      margin-bottom: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }
    .milestone-description-box {
      margin-top: 15px;
      padding: 15px;
      background-color: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 8px;
    }
    .milestone-description-text {
      font-size: 1.1em;
      line-height: 1.5;
      color: #2c3e50;
      padding: 10px;
      background-color: white;
      border-radius: 4px;
      margin: 10px 0;
    }
    .selection-controls {
      background-color: white;
      padding: 15px;
      border-radius: 8px;
      margin: 15px 0;
      border: 1px solid #e9ecef;
    }
    ")),
  
    div(id = ns("page_container"),
        fluidRow(
          column(4,
                 div(class = "milestone-plot-panel",
                     style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; background-color: white;",
                     h5("Your Milestone Self-Assessment", style = "color: #2c3e50; text-align: center;"),
                     plotOutput(ns("milestone_plot"), height = "400px")
                 )
          ),
          column(8,
                 div(class = "goal-setting-panel",
                     uiOutput(ns("current_page"))
                 )
          )
        )
    )
  )
}


#' Goal Setting Module Server
#' 
#' @param id Module namespace
#' @param rdm_dict_data Data dictionary
#' @param subcompetency_maps List of subcompetency mappings
#' @param competency_list Vector of competency domain codes for ordering
#' @param milestone_levels List of milestone level descriptions
#' @param current_milestone_data Reactive with current milestone self-assessment
#' @param resident_info Reactive with resident name
#' @param selected_period Reactive with period name
#' @export
goalSettingServer <- function(id, rdm_dict_data, subcompetency_maps, 
                              competency_list,  # ADD THIS PARAMETER
                              milestone_levels, current_milestone_data, 
                              resident_info, selected_period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    current_page <- reactiveVal("pcmk")
    stored_responses <- reactiveVal(list())
    previous_goals <- reactiveVal(NULL)
    submission_ready <- reactiveVal(FALSE)

    # ADD THIS COMPLETE SECTION:
    # Render milestone spider plot
    output$milestone_plot <- renderPlot({
      milestone_data <- current_milestone_data()
      
      # If empty or NULL, show placeholder
      if(is.null(milestone_data) || (is.data.frame(milestone_data) && nrow(milestone_data) == 0)) {
        plot.new()
        text(0.5, 0.5, "Complete milestone self-assessment\nto see your spider plot", 
             cex = 1.2, col = "gray60")
        return()
      }
      
      # Your data should be a data frame with columns like PC1, PC2, MK1, etc.
      # and rows for the resident and median
      tryCatch({
        req(resident_info(), selected_period())
        gmed::miles_plot(
          data = milestone_data,
          name = resident_info(),
          period = selected_period()
        )
      }, error = function(e) {
        message("Miles plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, "Spider plot unavailable", 
             cex = 1.2, col = "gray60")
      })
    })

    # Build competency choice lists
    pcmk_choices <- c()
    for(domain in c("PC", "MK")) {
      for(i in names(subcompetency_maps[[domain]])) {
        pcmk_choices <- c(pcmk_choices,
                          setNames(paste0(domain, i),
                                   paste0(domain, i, ": ", subcompetency_maps[[domain]][[i]])))
      }
    }

    sbppbl_choices <- c()
    for(domain in c("SBP", "PBLI")) {
      for(i in names(subcompetency_maps[[domain]])) {
        sbppbl_choices <- c(sbppbl_choices,
                            setNames(paste0(domain, i),
                                     paste0(domain, i, ": ", subcompetency_maps[[domain]][[i]])))
      }
    }

    profics_choices <- c()
    for(domain in c("PROF", "ICS")) {
      for(i in names(subcompetency_maps[[domain]])) {
        profics_choices <- c(profics_choices,
                             setNames(paste0(domain, i),
                                      paste0(domain, i, ": ", subcompetency_maps[[domain]][[i]])))
      }
    }

    # Milestone Plot
    output$milestone_plot <- renderPlot({
      req(current_milestone_data(), resident_info(), selected_period())

      tryCatch({
        gmed::miles_plot(
          data = current_milestone_data(),
          name = resident_info(),
          period = selected_period()
        )
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                   label = "No milestone data available",
                   color = "darkgray", size = 5) +
          ggplot2::theme_void()
      })
    })

    
 # Helper to get field labels from data dictionary
    get_field_label <- function(field_name_val, default_label = NULL) {
      # Unwrap reactive if needed
      dict <- if(is.function(rdm_dict_data)) {
        tryCatch(rdm_dict_data(), error = function(e) NULL)
      } else {
        rdm_dict_data
      }
      
      if (is.null(dict)) return(default_label %||% field_name_val)
      
      label <- dict %>%
        dplyr::filter(field_name == !!field_name_val) %>%
        dplyr::pull(field_label)

      if(length(label) > 0) label[1] else default_label %||% field_name_val
    }

# Helper to get milestone data from data dictionary
    # Helper to get milestone data from data dictionary
    get_milestone_data_from_dict <- function(competency_code) {
      # Unwrap reactive if needed
      dict <- if(is.function(rdm_dict_data)) {
        tryCatch(rdm_dict_data(), error = function(e) NULL)
      } else {
        rdm_dict_data
      }
      
      if (is.null(dict) || is.null(competency_code)) return(NULL)

      # Extract the domain prefix (PC, MK, SBP, etc.)
      domain <- sub("\\d+$", "", competency_code)
      comp_num <- sub("^[A-Z]+", "", competency_code)
      
      # Convert to lowercase for field name matching
      prefix_lower <- tolower(domain)
      
      # Handle PBLI -> PBL conversion for REDCap field names
      if (domain == "PBLI") {
        prefix_lower <- "pbl"
      }

      # Find all milestone row fields for this competency
      # Pattern: pc1_r1, pc1_r2, etc.
      pattern <- paste0("^", prefix_lower, comp_num, "_r\\d+$")
      
      fields <- dict %>%
        dplyr::filter(grepl(pattern, field_name)) %>%
        dplyr::select(field_name, select_choices_or_calculations) %>%
        dplyr::arrange(field_name)

      if (nrow(fields) == 0) {
        return(NULL)
      }

      # Create data frame to hold milestone table
      data <- data.frame(
        Milestone = character(),
        Novice = character(),
        `Advanced Beginner` = character(),
        Competent = character(),
        Proficient = character(),
        Expert = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Parse each row
      for (i in 1:nrow(fields)) {
        field_name_val <- fields$field_name[i]
        choices_text <- fields$select_choices_or_calculations[i]

        if (is.na(choices_text) || choices_text == "") next

        # Extract row number from field name (e.g., pc1_r1 -> 1)
        row_match <- regmatches(field_name_val, regexpr("_r(\\d+)$", field_name_val))
        if (length(row_match) == 0) next
        row_num <- sub("_r", "", row_match)
        
        milestone_name <- paste0(domain, comp_num, " Row ", row_num)

        # Parse choices: format is "1, Description | 2, Description | ..."
        choices <- strsplit(choices_text, "\\s*\\|\\s*")[[1]]
        level_texts <- rep("", 5)

        for (choice in choices) {
          # Split on first comma only
          parts <- strsplit(choice, ",\\s*", perl = TRUE)[[1]]
          if (length(parts) < 2) next

          level <- as.numeric(trimws(parts[1]))
          if (is.na(level) || level < 1 || level > 5) next

          # Rejoin rest of text in case there were commas in description
          description <- paste(parts[-1], collapse = ", ")
          level_texts[level] <- trimws(description)
        }

        # Add row to data frame
        data <- rbind(data, data.frame(
          Milestone = milestone_name,
          Novice = level_texts[1],
          `Advanced Beginner` = level_texts[2],
          Competent = level_texts[3],
          Proficient = level_texts[4],
          Expert = level_texts[5],
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      return(data)
    }

    # Milestone data reactives for each domain
    pcmk_milestone_data <- reactive({
      req(input$pcmk_subcompetency)
      get_milestone_data_from_dict(input$pcmk_subcompetency)
    })

    sbppbl_milestone_data <- reactive({
      req(input$sbppbl_subcompetency)
      get_milestone_data_from_dict(input$sbppbl_subcompetency)
    })

    profics_milestone_data <- reactive({
      req(input$profics_subcompetency)
      get_milestone_data_from_dict(input$profics_subcompetency)
    })

    # Page rendering functions
    pcmk_page <- function() {
      ns <- session$ns
      div(
        h4("Goal: Patient Care / Medical Knowledge"),
        uiOutput(ns("previous_goal_pcmk")),
        wellPanel(
          class = "previous-goal-panel",
          h5("Previous Goal Review"),
          uiOutput(ns("prior_goal_pcmk_radio")),
          uiOutput(ns("pcmk_textarea"))
        ),
        wellPanel(
          h4("Set New PC/MK Goal"),
          div(class = "selection-controls",
              selectInput(ns("pcmk_subcompetency"),
                          "Select Subcompetency:",
                          choices = pcmk_choices,
                          width = "100%")
          ),
          uiOutput(ns("pcmk_milestone_table"))
        ),
        uiOutput(ns("pcmk_validation_message")),
        div(
          style = "text-align: right;",
          actionButton(ns("next_pcmk"), "Next →", class = "btn-primary")
        )
      )
    }

    sbppbl_page <- function() {
      ns <- session$ns
      div(
        h4("Goal: Systems-Based Practice / Practice-Based Learning"),
        uiOutput(ns("previous_goal_sbppbl")),
        wellPanel(
          class = "previous-goal-panel",
          h5("Previous Goal Review"),
          uiOutput(ns("prior_goal_sbppbl_radio")),
          uiOutput(ns("sbppbl_textarea"))
        ),
        wellPanel(
          h4("Set New SBP/PBLI Goal"),
          div(class = "selection-controls",
              selectInput(ns("sbppbl_subcompetency"),
                          "Select Subcompetency:",
                          choices = sbppbl_choices,
                          width = "100%")
          ),
          uiOutput(ns("sbppbl_milestone_table"))
        ),
        uiOutput(ns("sbppbl_validation_message")),
        div(
          style = "text-align: right;",
          actionButton(ns("prev_sbppbl"), "← Previous", class = "btn-secondary me-2"),
          actionButton(ns("next_sbppbl"), "Next →", class = "btn-primary")
        )
      )
    }

    profics_page <- function() {
      ns <- session$ns
      div(
        h4("Goal: Professionalism / Interpersonal Communication"),
        uiOutput(ns("previous_goal_profics")),
        wellPanel(
          class = "previous-goal-panel",
          h5("Previous Goal Review"),
          uiOutput(ns("prior_goal_profics_radio")),
          uiOutput(ns("profics_textarea"))
        ),
        wellPanel(
          h4("Set New PROF/ICS Goal"),
          div(class = "selection-controls",
              selectInput(ns("profics_subcompetency"),
                          "Select Subcompetency:",
                          choices = profics_choices,
                          width = "100%")
          ),
          uiOutput(ns("profics_milestone_table"))
        ),
        uiOutput(ns("profics_validation_message")),
        div(
          style = "text-align: right;",
          actionButton(ns("prev_profics"), "← Previous", class = "btn-secondary me-2"),
          actionButton(ns("submit_goals"), "Submit All Goals", class = "btn-success")
        )
      )
    }

    # Render current page
    output$current_page <- renderUI({
      page <- current_page()
      switch(page,
             "pcmk" = pcmk_page(),
             "sbppbl" = sbppbl_page(),
             "profics" = profics_page())
    })

    # Radio buttons for prior goal questions
    output$prior_goal_pcmk_radio <- renderUI({
      req(rdm_dict_data)
      label <- get_field_label("prior_goal_pcmk", 
                               "Did you reach your previous milestone goal for Patient Care/Medical Knowledge?")
      radioButtons(ns("prior_goal_pcmk"), label, 
                   choices = list("Yes" = "1", "No" = "0"), 
                   inline = TRUE, selected = character(0))
    })

    output$prior_goal_sbppbl_radio <- renderUI({
      req(rdm_dict_data)
      label <- get_field_label("prior_goal_sbppbl",
                               "Did you reach your previous milestone goal for SBP/PBLI?")
      radioButtons(ns("prior_goal_sbppbl"), label,
                   choices = list("Yes" = "1", "No" = "0"),
                   inline = TRUE, selected = character(0))
    })

    output$prior_goal_profics_radio <- renderUI({
      req(rdm_dict_data)
      label <- get_field_label("prior_goal_profics",
                               "Did you reach your previous milestone goal for PROF/ICS?")
      radioButtons(ns("prior_goal_profics"), label,
                   choices = list("Yes" = "1", "No" = "0"),
                   inline = TRUE, selected = character(0))
    })

    # Textareas for review responses
    output$pcmk_textarea <- renderUI({
      req(input$prior_goal_pcmk)
      field_name <- if(input$prior_goal_pcmk == "1") "review_q2_pcmk" else "review_q_pcmk"
      label <- get_field_label(field_name)
      textAreaInput(ns(field_name), h5(label), width = "100%", rows = 5)
    })

    output$sbppbl_textarea <- renderUI({
      req(input$prior_goal_sbppbl)
      field_name <- if(input$prior_goal_sbppbl == "1") "review_q2_sbppbl" else "review_q_sbppbl"
      label <- get_field_label(field_name)
      textAreaInput(ns(field_name), h5(label), width = "100%", rows = 5)
    })

    output$profics_textarea <- renderUI({
      req(input$prior_goal_profics)
      field_name <- if(input$prior_goal_profics == "1") "review_q2_profics" else "review_q_profics"
      label <- get_field_label(field_name)
      textAreaInput(ns(field_name), h5(label), width = "100%", rows = 5)
    })

    # Previous goal displays
    render_previous_goal <- function(domain_key, domain_name) {
      renderUI({
        req(previous_goals())
        prev_goal_data <- previous_goals()
        
        # Use gmed display_goals if available
        goal <- tryCatch({
          gmed::display_goals(prev_goal_data, domain = domain_key, return_format = "html")
        }, error = function(e) {
          NULL
        })

        if(is.null(goal)) return(NULL)

        wellPanel(
          class = "alert alert-info",
          h5(paste("Your Milestone goal for", domain_name, "from your last review:")),
          HTML(goal)
        )
      })
    }

    output$previous_goal_pcmk <- render_previous_goal("pcmk", "Patient Care / Medical Knowledge")
    output$previous_goal_sbppbl <- render_previous_goal("sbppbl", "Systems-Based Practice / PBLI")
    output$previous_goal_profics <- render_previous_goal("profics", "Professionalism / ICS")

    # Milestone table renderers (simplified version - just show table with row selector)
    output$pcmk_milestone_table <- renderUI({
      req(pcmk_milestone_data())
      data <- pcmk_milestone_data()
      
      if(is.null(data) || nrow(data) == 0) {
        return(div(class = "alert alert-warning", "No milestone data available"))
      }

      tagList(
        div(class = "milestone-table-container",
            h5(paste(input$pcmk_subcompetency, "Milestone Levels"),
               style = "color: #0072B2;"),
            div(style = "overflow-x: auto;",
                tableOutput(ns("pcmk_table_display"))
            ),
            div(class = "selection-controls",
                fluidRow(
                  column(6,
                         selectInput(ns("pcmk_row_select"),
                                     "Select Milestone Row:",
                                     choices = setNames(1:nrow(data), data$Milestone),
                                     width = "100%")
                  ),
                  column(6,
                         selectInput(ns("pcmk_level_select"),
                                     "Target Level:",
                                     choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))),
                                     width = "100%")
                  )
                )
            ),
            uiOutput(ns("pcmk_goal_display"))
        )
      )
    })

    output$pcmk_table_display <- renderTable({
      req(pcmk_milestone_data())
      pcmk_milestone_data()
    }, striped = TRUE, bordered = TRUE)

    output$sbppbl_milestone_table <- renderUI({
      req(sbppbl_milestone_data())
      data <- sbppbl_milestone_data()
      
      if(is.null(data) || nrow(data) == 0) {
        return(div(class = "alert alert-warning", "No milestone data available"))
      }

      tagList(
        div(class = "milestone-table-container",
            h5(paste(input$sbppbl_subcompetency, "Milestone Levels"),
               style = "color: #0072B2;"),
            div(style = "overflow-x: auto;",
                tableOutput(ns("sbppbl_table_display"))
            ),
            div(class = "selection-controls",
                fluidRow(
                  column(6,
                         selectInput(ns("sbppbl_row_select"),
                                     "Select Milestone Row:",
                                     choices = setNames(1:nrow(data), data$Milestone),
                                     width = "100%")
                  ),
                  column(6,
                         selectInput(ns("sbppbl_level_select"),
                                     "Target Level:",
                                     choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))),
                                     width = "100%")
                  )
                )
            ),
            uiOutput(ns("sbppbl_goal_display"))
        )
      )
    })

    output$sbppbl_table_display <- renderTable({
      req(sbppbl_milestone_data())
      sbppbl_milestone_data()
    }, striped = TRUE, bordered = TRUE)

    output$profics_milestone_table <- renderUI({
      req(profics_milestone_data())
      data <- profics_milestone_data()
      
      if(is.null(data) || nrow(data) == 0) {
        return(div(class = "alert alert-warning", "No milestone data available"))
      }

      tagList(
        div(class = "milestone-table-container",
            h5(paste(input$profics_subcompetency, "Milestone Levels"),
               style = "color: #0072B2;"),
            div(style = "overflow-x: auto;",
                tableOutput(ns("profics_table_display"))
            ),
            div(class = "selection-controls",
                fluidRow(
                  column(6,
                         selectInput(ns("profics_row_select"),
                                     "Select Milestone Row:",
                                     choices = setNames(1:nrow(data), data$Milestone),
                                     width = "100%")
                  ),
                  column(6,
                         selectInput(ns("profics_level_select"),
                                     "Target Level:",
                                     choices = setNames(1:5, paste0(1:5, ": ", unlist(milestone_levels))),
                                     width = "100%")
                  )
                )
            ),
            uiOutput(ns("profics_goal_display"))
        )
      )
    })

    output$profics_table_display <- renderTable({
      req(profics_milestone_data())
      profics_milestone_data()
    }, striped = TRUE, bordered = TRUE)

    # Goal displays with "how to achieve" text boxes
    output$pcmk_goal_display <- renderUI({
      req(input$pcmk_row_select, input$pcmk_level_select, pcmk_milestone_data())
      
      data <- pcmk_milestone_data()
      row_idx <- as.numeric(input$pcmk_row_select)
      col_name <- paste0("Level.", input$pcmk_level_select)
      
      if(!(col_name %in% names(data))) return(NULL)
      goal_text <- data[row_idx, col_name]

      div(class = "milestone-description-box",
          strong("Milestone goal to achieve in the next 6 months:"),
          div(class = "milestone-description-text",
              p(goal_text, style = "background-color: #f8f9fa; padding: 10px;")
          ),
          div(class = "how-to-achieve",
              strong("How will you achieve this milestone?"),
              textAreaInput(ns("how_pcmk"), label = NULL, width = "100%", rows = 3,
                            placeholder = "Describe your plan...")
          )
      )
    })

    output$sbppbl_goal_display <- renderUI({
      req(input$sbppbl_row_select, input$sbppbl_level_select, sbppbl_milestone_data())
      
      data <- sbppbl_milestone_data()
      row_idx <- as.numeric(input$sbppbl_row_select)
      col_name <- paste0("Level.", input$sbppbl_level_select)
      
      if(!(col_name %in% names(data))) return(NULL)
      goal_text <- data[row_idx, col_name]

      div(class = "milestone-description-box",
          strong("Milestone goal to achieve in the next 6 months:"),
          div(class = "milestone-description-text",
              p(goal_text, style = "background-color: #f8f9fa; padding: 10px;")
          ),
          div(class = "how-to-achieve",
              strong("How will you achieve this milestone?"),
              textAreaInput(ns("how_sbppbl"), label = NULL, width = "100%", rows = 3,
                            placeholder = "Describe your plan...")
          )
      )
    })

    output$profics_goal_display <- renderUI({
      req(input$profics_row_select, input$profics_level_select, profics_milestone_data())
      
      data <- profics_milestone_data()
      row_idx <- as.numeric(input$profics_row_select)
      col_name <- paste0("Level.", input$profics_level_select)
      
      if(!(col_name %in% names(data))) return(NULL)
      goal_text <- data[row_idx, col_name]

      div(class = "milestone-description-box",
          strong("Milestone goal to achieve in the next 6 months:"),
          div(class = "milestone-description-text",
              p(goal_text, style = "background-color: #f8f9fa; padding: 10px;")
          ),
          div(class = "how-to-achieve",
              strong("How will you achieve this milestone?"),
              textAreaInput(ns("how_profics"), label = NULL, width = "100%", rows = 3,
                            placeholder = "Describe your plan...")
          )
      )
    })

    # Validation functions
    validate_pcmk_page <- function(input) {
      has_prior_goal_response <- !is.null(input$prior_goal_pcmk)
      
      has_review_response <- if (has_prior_goal_response) {
        if(input$prior_goal_pcmk == "1") {
          !is.null(input$review_q2_pcmk) && nchar(trimws(input$review_q2_pcmk)) > 0
        } else {
          !is.null(input$review_q_pcmk) && nchar(trimws(input$review_q_pcmk)) > 0
        }
      } else { TRUE }

      has_subcompetency <- !is.null(input$pcmk_subcompetency) && input$pcmk_subcompetency != ""
      has_milestone_selection <- has_subcompetency && 
        !is.null(input$pcmk_row_select) && !is.null(input$pcmk_level_select)
      has_achievement_plan <- !is.null(input$how_pcmk) && nchar(trimws(input$how_pcmk)) > 0

      return(has_prior_goal_response && has_review_response && 
             has_milestone_selection && has_achievement_plan)
    }

    validate_sbppbl_page <- function(input) {
      has_prior_goal_response <- !is.null(input$prior_goal_sbppbl)
      
      has_review_response <- if (has_prior_goal_response) {
        if(input$prior_goal_sbppbl == "1") {
          !is.null(input$review_q2_sbppbl) && nchar(trimws(input$review_q2_sbppbl)) > 0
        } else {
          !is.null(input$review_q_sbppbl) && nchar(trimws(input$review_q_sbppbl)) > 0
        }
      } else { TRUE }

      has_subcompetency <- !is.null(input$sbppbl_subcompetency) && input$sbppbl_subcompetency != ""
      has_milestone_selection <- has_subcompetency && 
        !is.null(input$sbppbl_row_select) && !is.null(input$sbppbl_level_select)
      has_achievement_plan <- !is.null(input$how_sbppbl) && nchar(trimws(input$how_sbppbl)) > 0

      return(has_prior_goal_response && has_review_response && 
             has_milestone_selection && has_achievement_plan)
    }

    validate_profics_page <- function(input) {
      has_prior_goal_response <- !is.null(input$prior_goal_profics)
      
      has_review_response <- if (has_prior_goal_response) {
        if(input$prior_goal_profics == "1") {
          !is.null(input$review_q2_profics) && nchar(trimws(input$review_q2_profics)) > 0
        } else {
          !is.null(input$review_q_profics) && nchar(trimws(input$review_q_profics)) > 0
        }
      } else { TRUE }

      has_subcompetency <- !is.null(input$profics_subcompetency) && input$profics_subcompetency != ""
      has_milestone_selection <- has_subcompetency && 
        !is.null(input$profics_row_select) && !is.null(input$profics_level_select)
      has_achievement_plan <- !is.null(input$how_profics) && nchar(trimws(input$how_profics)) > 0

      return(has_prior_goal_response && has_review_response && 
             has_milestone_selection && has_achievement_plan)
    }

    # Validation message outputs
    output$pcmk_validation_message <- renderUI({
      if (!validate_pcmk_page(input)) {
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Please complete all required fields before proceeding.")
      }
    })

    output$sbppbl_validation_message <- renderUI({
      if (!validate_sbppbl_page(input)) {
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Please complete all required fields before proceeding.")
      }
    })

    output$profics_validation_message <- renderUI({
      if (!validate_profics_page(input)) {
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Please complete all required fields before proceeding.")
      }
    })

    # Navigation handlers
    observeEvent(input$next_pcmk, {
      if (!validate_pcmk_page(input)) {
        showNotification("Please complete all required fields", type = "warning")
        return()
      }

      responses <- stored_responses()
      responses$pcmk <- list(
        had_prior_goal = input$prior_goal_pcmk,
        review = if(input$prior_goal_pcmk == "1") input$review_q2_pcmk else input$review_q_pcmk,
        subcompetency = input$pcmk_subcompetency,
        selected_row = input$pcmk_row_select,
        selected_level = input$pcmk_level_select,
        how_to_achieve = input$how_pcmk
      )
      stored_responses(responses)
      current_page("sbppbl")
    })

    observeEvent(input$next_sbppbl, {
      if (!validate_sbppbl_page(input)) {
        showNotification("Please complete all required fields", type = "warning")
        return()
      }

      responses <- stored_responses()
      responses$sbppbl <- list(
        had_prior_goal = input$prior_goal_sbppbl,
        review = if(input$prior_goal_sbppbl == "1") input$review_q2_sbppbl else input$review_q_sbppbl,
        subcompetency = input$sbppbl_subcompetency,
        selected_row = input$sbppbl_row_select,
        selected_level = input$sbppbl_level_select,
        how_to_achieve = input$how_sbppbl
      )
      stored_responses(responses)
      current_page("profics")
    })

    observeEvent(input$prev_sbppbl, {
      current_page("pcmk")
    })

    observeEvent(input$prev_profics, {
      current_page("sbppbl")
    })

    observeEvent(input$submit_goals, {
      if (!validate_profics_page(input)) {
        showNotification("Please complete all required fields", type = "warning")
        return()
      }

      responses <- stored_responses()
      responses$profics <- list(
        had_prior_goal = input$prior_goal_profics,
        review = if(input$prior_goal_profics == "1") input$review_q2_profics else input$review_q_profics,
        subcompetency = input$profics_subcompetency,
        selected_row = input$profics_row_select,
        selected_level = input$profics_level_select,
        how_to_achieve = input$how_profics
      )
      stored_responses(responses)
      submission_ready(TRUE)
    })

    # Return module interface
    return(list(
      get_responses = reactive({ stored_responses() }),
      set_previous_goals = function(data) { previous_goals(data) },
      submission_ready = reactive({ submission_ready() }),
      reset_submission = function() { submission_ready(FALSE) }
    ))
  })
}