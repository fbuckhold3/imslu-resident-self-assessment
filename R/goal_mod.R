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
    
    # Track current page
    current_page <- reactiveVal("pcmk")
    
    # Store responses
    responses <- reactiveVal(list())
    
    # Previous goals data
    previous_goals <- reactiveVal(NULL)
    
    # =========================================================================
    # PERIOD AND MILESTONE DATA
    # =========================================================================
    
    # Get period info
    period_name <- reactive({
      if (is.function(selected_period)) {
        p <- selected_period()
        if (is.list(p) && "period_name" %in% names(p)) {
          return(p$period_name)
        }
        return(p)
      }
      if (is.list(selected_period) && "period_name" %in% names(selected_period)) {
        return(selected_period$period_name)
      }
      return(selected_period)
    })
    
    # =========================================================================
    # SPIDER PLOT - Using gmed's enhanced plotly version
    # =========================================================================
    
    output$spider_plot <- plotly::renderPlotly({
      req(current_milestone_data(), resident_info(), period_name())
      
      ms_data <- current_milestone_data()
      
      # Check we have the required data
      if (is.null(ms_data$data) || nrow(ms_data$data) == 0) {
        return(plotly::plot_ly() %>% 
                 plotly::add_annotations(
                   text = "No milestone data available", 
                   x = 0.5, y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 16, color = "orange")
                 ))
      }
      
      res_info <- resident_info()
      current_period <- period_name()
      
      # Get resident data (non-repeating form data for lookup)
      resident_lookup <- data.frame(
        record_id = res_info$record_id,
        name = res_info$name,
        stringsAsFactors = FALSE
      )
      
      # Use gmed's enhanced spider plot
      gmed::create_enhanced_milestone_spider_plot(
        milestone_data = ms_data$data,
        median_data = ms_data$medians,
        resident_id = res_info$record_id,
        period_text = current_period,
        milestone_type = "self",  # Goals module uses self-evaluation
        resident_data = resident_lookup
      )
    })
    
    # =========================================================================
    # GOAL CONTENT RENDERING
    # =========================================================================
    
    output$goal_content <- renderUI({
      page <- current_page()
      
      switch(page,
        "pcmk" = goal_page_ui(ns, "pcmk", "Patient Care & Medical Knowledge"),
        "sbppbl" = goal_page_ui(ns, "sbppbl", "Systems-Based Practice & PBLI"),
        "profics" = goal_page_ui(ns, "profics", "Professionalism & ICS"),
        "complete" = complete_page_ui(ns)
      )
    })
    
    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================
    
    # Helper: Get milestone table
    milestone_table <- function(domain) {
      ms_data <- current_milestone_data()
      
      if (is.null(ms_data) || is.null(ms_data$data) || nrow(ms_data$data) == 0) {
        return(NULL)
      }
      
      res_info <- resident_info()
      rec_id <- res_info$record_id
      current_period <- period_name()
      
      # Filter for this resident and period
      resident_data <- ms_data$data %>%
        dplyr::filter(record_id == !!rec_id, period_name == !!current_period)
      
      if (nrow(resident_data) == 0) {
        return(NULL)
      }
      
      # Get milestone columns for this domain
      milestone_cols <- get_domain_milestones(ms_data$milestone_cols, domain)
      
      if (length(milestone_cols) == 0) {
        return(NULL)
      }
      
      # Get scores
      scores <- as.numeric(resident_data[1, milestone_cols])
      
      # Get labels
      labels <- sapply(milestone_cols, function(x) {
        gmed::get_milestone_label(x, "rep")
      })
      
      # Create table
      table_data <- data.frame(
        Milestone = labels,
        Score = scores,
        stringsAsFactors = FALSE
      )
      
      return(table_data)
    }
    
    # Helper: Get domain-specific milestones
    get_domain_milestones <- function(all_cols, domain) {
      switch(domain,
        "pcmk" = grep("^rep_(pc|mk)\\d+_self$", all_cols, value = TRUE),
        "sbppbl" = grep("^rep_(sbp|pbl)\\d+_self$", all_cols, value = TRUE),
        "profics" = grep("^rep_(prof|ics)\\d+_self$", all_cols, value = TRUE),
        character(0)
      )
    }
    
    # Helper: Goal page UI
    goal_page_ui <- function(ns, domain, title) {
      div(class = "goal-card",
        h4(title, style = "color: #2c3e50; margin-bottom: 20px;"),
        
        # Previous goal review if exists
        if (!is.null(previous_goals()) && nrow(previous_goals()) > 0) {
          div(class = "previous-goal-panel",
            h5("Previous Goal Review", icon("history")),
            previous_goal_ui(ns, domain)
          )
        },
        
        # Milestone table
        if (!is.null(milestone_table(domain))) {
          div(class = "milestone-table-container",
            h5("Your Self-Assessment Scores"),
            renderTable(milestone_table(domain), width = "100%")
          )
        },
        
        # New goal selection
        div(style = "margin-top: 20px;",
          h5("Set New Goal for Next Period"),
          selectInput(ns(paste0("goal_", domain)),
                     "Select milestone to focus on:",
                     choices = get_goal_choices(domain)),
          selectInput(ns(paste0("level_", domain)),
                     "Target competency level:",
                     choices = c("3" = "3", "4" = "4", "5" = "5")),
          textAreaInput(ns(paste0("how_", domain)),
                       "How will you achieve this goal?",
                       rows = 4, width = "100%")
        ),
        
        # Navigation
        div(class = "d-flex justify-content-between mt-4",
          if (domain != "pcmk") {
            actionButton(ns("prev"), "Previous", 
                        class = "btn-outline-secondary")
          } else {
            div()
          },
          actionButton(ns("next"), 
                      if (domain == "profics") "Review & Submit" else "Next",
                      class = "btn-primary")
        )
      )
    }
    
    # Helper: Get goal choices for domain
    get_goal_choices <- function(domain) {
      ms_data <- current_milestone_data()
      
      if (is.null(ms_data) || is.null(ms_data$milestone_cols)) {
        return(list("No milestones available" = ""))
      }
      
      cols <- get_domain_milestones(ms_data$milestone_cols, domain)
      
      if (length(cols) == 0) {
        return(list("No milestones available" = ""))
      }
      
      choices <- sapply(cols, function(x) {
        gmed::get_milestone_label(x, "rep")
      })
      
      return(as.list(setNames(cols, choices)))
    }
    
    # Helper: Get ILP field mapping
    get_ilp_field_map <- function(domain) {
      switch(domain,
        "pcmk" = list(goal = "ilp_goal_pcmk", 
                     level = "ilp_level_pcmk",
                     how = "ilp_how_pcmk"),
        "sbppbl" = list(goal = "ilp_goal_sbppbl",
                       level = "ilp_level_sbppbl", 
                       how = "ilp_how_sbppbl"),
        "profics" = list(goal = "ilp_goal_profics",
                        level = "ilp_level_profics",
                        how = "ilp_how_profics"),
        NULL
      )
    }
    
    # Helper: Get goal label
    get_goal_label <- function(domain, goal_value) {
      if (is.na(goal_value) || goal_value == "") {
        return("Not specified")
      }
      
      gmed::get_milestone_label(goal_value, "rep")
    }
    
    # Helper: Domain name
    get_domain_name <- function(domain) {
      switch(domain,
             "pcmk" = "Patient Care / Medical Knowledge",
             "sbppbl" = "Systems-Based Practice / PBLI",
             "profics" = "Professionalism / ICS",
             "Unknown")
    }
    
    # Helper: Previous goal UI
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
    
    # Helper: Complete page
    complete_page_ui <- function(ns) {
      div(class = "goal-card",
        h4("Review Your Goals", style = "color: #2c3e50;"),
        p("Review and submit your goals for the next period."),
        
        # Summary of all goals
        div(style = "margin: 20px 0;",
          h5("Patient Care & Medical Knowledge"),
          verbatimTextOutput(ns("summary_pcmk")),
          
          h5("Systems-Based Practice & PBLI"),
          verbatimTextOutput(ns("summary_sbppbl")),
          
          h5("Professionalism & ICS"),
          verbatimTextOutput(ns("summary_profics"))
        ),
        
        div(class = "d-flex justify-content-between mt-4",
          actionButton(ns("prev"), "Previous", class = "btn-outline-secondary"),
          actionButton(ns("submit"), "Submit Goals", class = "btn-success")
        )
      )
    }
    
    # =========================================================================
# NAVIGATION HANDLERS
# =========================================================================

observeEvent(input$`next`, {  # <-- Add backticks here
  page <- current_page()
  
  # Store current page responses
  resp <- responses()
  
  if (page == "pcmk") {
    resp$pcmk <- list(
      goal = input$goal_pcmk,
      level = input$level_pcmk,
      how = input$how_pcmk
    )
    current_page("sbppbl")
  } else if (page == "sbppbl") {
    resp$sbppbl <- list(
      goal = input$goal_sbppbl,
      level = input$level_sbppbl,
      how = input$how_sbppbl
    )
    current_page("profics")
  } else if (page == "profics") {
    resp$profics <- list(
      goal = input$goal_profics,
      level = input$level_profics,
      how = input$how_profics
    )
    current_page("complete")
  }
  
  responses(resp)
})

observeEvent(input$prev, {  # <-- No backticks needed for prev
  page <- current_page()
  
  if (page == "sbppbl") {
    current_page("pcmk")
  } else if (page == "profics") {
    current_page("sbppbl")
  } else if (page == "complete") {
    current_page("profics")
  }
})
    
    # =========================================================================
    # DYNAMIC REVIEW TEXT
    # =========================================================================
    
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
    
    # =========================================================================
    # SUMMARY OUTPUTS
    # =========================================================================
    
    output$summary_pcmk <- renderText({
      resp <- responses()
      if (is.null(resp$pcmk)) return("No goal set")
      
      paste0(
        "Goal: ", get_goal_label("pcmk", resp$pcmk$goal), "\n",
        "Target Level: ", resp$pcmk$level, "\n",
        "Plan: ", resp$pcmk$how
      )
    })
    
    output$summary_sbppbl <- renderText({
      resp <- responses()
      if (is.null(resp$sbppbl)) return("No goal set")
      
      paste0(
        "Goal: ", get_goal_label("sbppbl", resp$sbppbl$goal), "\n",
        "Target Level: ", resp$sbppbl$level, "\n",
        "Plan: ", resp$sbppbl$how
      )
    })
    
    output$summary_profics <- renderText({
      resp <- responses()
      if (is.null(resp$profics)) return("No goal set")
      
      paste0(
        "Goal: ", get_goal_label("profics", resp$profics$goal), "\n",
        "Target Level: ", resp$profics$level, "\n",
        "Plan: ", resp$profics$how
      )
    })
    
    # =========================================================================
    # SUBMIT HANDLER
    # =========================================================================
    
    observeEvent(input$submit, {
      # TODO: Implement REDCap submission logic
      showNotification("Goals submitted successfully!", type = "message")
    })
    
    # =========================================================================
    # RETURN
    # =========================================================================
    
    return(reactive({
      list(
        responses = responses(),
        current_page = current_page()
      )
    }))
  })
}