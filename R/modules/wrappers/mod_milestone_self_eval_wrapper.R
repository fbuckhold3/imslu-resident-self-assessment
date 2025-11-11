#' Milestone Self-Evaluation Wrapper Module UI
#'
#' @param id Module namespace ID
#' @export
mod_milestone_entry_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Milestone Self-Evaluation"),
    
    # Collapsible section for previous milestones
    bslib::accordion(
      id = ns("milestone_accordion"),
      open = FALSE,  # Start collapsed
      bslib::accordion_panel(
        "View Previous Milestone Assessments",
        shiny::p("Review your previous self-assessment and ACGME ratings for comparison."),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::h5("Previous Self-Assessment"),
            plotly::plotlyOutput(ns("prev_self_plot"), height = "400px")
          ),
          shiny::column(
            6,
            shiny::h5("Previous ACGME Assessment"),
            plotly::plotlyOutput(ns("prev_acgme_plot"), height = "400px")
          )
        )
      )
    ),
    
    shiny::hr(),
    
    # Entry section - the image-based milestone rating interface
    shiny::h3("Enter Current Milestone Self-Assessment"),
    shiny::p(
      "Use the interface below to rate your current milestone levels. ",
      "Click through each milestone image and select your level (1-9)."
    ),
    gmed::mod_miles_rating_ui(ns("rating"))
  )
}

#' Helper: Convert period name to number
#' @param period_name Period name (e.g., "Mid Intern", "End PGY2")
#' @return Period number (1-7)
get_period_number_from_name <- function(period_name) {
  period_mapping <- c(
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "End PGY3" = 6,
    "Graduating" = 6,
    "Entering Residency" = 7
  )
  
  as.numeric(period_mapping[period_name])
}

#' Milestone Self-Evaluation Wrapper Module Server
#'
#' @param id Module namespace ID  
#' @param record_id Reactive value for resident record ID
#' @param period Reactive value for current evaluation period (name)
#' @param rdm_data Reactive containing all RDM data
#' @export
mod_milestone_entry_server <- function(id, record_id, period, rdm_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Process milestone workflow when data loads
    milestone_results <- shiny::reactive({
      req(rdm_data())
      
      # Use the milestone_workflow that's already in rdm_data if it exists
      if (!is.null(rdm_data()$milestone_workflow)) {
        return(rdm_data()$milestone_workflow)
      }
      
      # Otherwise create it
      workflow <- tryCatch({
        gmed::create_milestone_workflow_from_dict(
          all_forms = rdm_data()$all_forms,
          data_dict = rdm_data()$data_dict,
          resident_data = rdm_data()$residents,
          verbose = FALSE
        )
      }, error = function(e) {
        message("Error creating milestone workflow: ", e$message)
        NULL
      })
      
      workflow
    })
    
    # Get previous period text
    previous_period <- shiny::reactive({
      req(period())
      
      # Get period number from period name
      period_num <- get_period_number_from_name(period())
      
      if (is.na(period_num) || period_num <= 1) return(NA)
      
      # Get previous period text
      period_mapping <- data.frame(
        num = 1:7,
        text = c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2", 
                 "Mid PGY3", "Graduating", "Entering Residency"),
        stringsAsFactors = FALSE
      )
      
      prev_num <- period_num - 1
      prev_text <- period_mapping$text[period_mapping$num == prev_num]
      
      if (length(prev_text) == 0) return(NA)
      prev_text
    })
    
    # Get self milestone data
    self_milestone_data <- shiny::reactive({
      req(milestone_results())
      
      gmed::get_milestone_data(
        workflow_results = milestone_results(),
        milestone_type = "self",
        milestone_system = "rep"
      )
    })
    
    # Get ACGME milestone data (instead of program)
    acgme_milestone_data <- shiny::reactive({
      req(milestone_results())
      
      gmed::get_milestone_data(
        workflow_results = milestone_results(),
        milestone_type = "program",
        milestone_system = "acgme"
      )
    })
    
    # ========================================================================
    # DEBUG OBSERVERS
    # ========================================================================
    
    # Debug 1: Check if milestone_results loads
    observe({
      message("=== MILESTONE RESULTS CHECK ===")
      message("milestone_results exists: ", !is.null(milestone_results()))
      
      if (!is.null(milestone_results())) {
        message("Configs available: ", paste(names(milestone_results()), collapse = ", "))
      }
    })
    
    # Debug 2: Check self milestone data
    observe({
      message("=== SELF MILESTONE DATA CHECK ===")
      
      self_data <- self_milestone_data()
      message("self_milestone_data exists: ", !is.null(self_data))
      
      if (!is.null(self_data)) {
        message("Has data component: ", !is.null(self_data$data))
        message("Has medians component: ", !is.null(self_data$medians))
        
        if (!is.null(self_data$data)) {
          message("Data rows: ", nrow(self_data$data))
          message("Data columns: ", paste(head(names(self_data$data), 10), collapse = ", "))
        }
      }
    })
    
    # Debug 3: Check ACGME milestone data
    observe({
      message("=== ACGME MILESTONE DATA CHECK ===")
      
      acgme_data <- acgme_milestone_data()
      message("acgme_milestone_data exists: ", !is.null(acgme_data))
      
      if (!is.null(acgme_data)) {
        message("Has data component: ", !is.null(acgme_data$data))
        message("Has medians component: ", !is.null(acgme_data$medians))
        
        if (!is.null(acgme_data$data)) {
          message("Data rows: ", nrow(acgme_data$data))
        }
      }
    })
    
    # Debug 4: Check record_id
    observe({
      message("=== RECORD ID CHECK ===")
      message("record_id: ", record_id())
    })
    
    # Debug 5: Check for resident's data
    observe({
      req(self_milestone_data(), record_id())
      
      data <- self_milestone_data()$data
      
      message("=== RESIDENT MILESTONE DATA ===")
      message("Total milestone records: ", nrow(data))
      
      if (!is.null(data) && nrow(data) > 0) {
        message("Unique residents in data: ", paste(head(unique(data$record_id), 5), collapse = ", "))
        
        resident_data <- data %>%
          dplyr::filter(record_id == !!record_id())
        
        message("Resident ", record_id(), " has ", nrow(resident_data), " self-milestone records")
        
        if (nrow(resident_data) > 0) {
          message("Periods in data (prog_mile_period): ", 
                  paste(unique(resident_data$prog_mile_period), collapse = ", "))
          if ("period_name" %in% names(resident_data)) {
            message("Period names in data (period_name): ", 
                    paste(unique(resident_data$period_name), collapse = ", "))
          }
          message("Previous period looking for: ", previous_period())
        } else {
          message("No data found for resident ", record_id())
        }
      }
    })
    
    # ========================================================================
    # RENDER PLOTS
    # ========================================================================
    # ========================================================================
# RENDER PLOTS
# ========================================================================

# Render previous self plot (PLOTLY)
# Render previous self plot (PLOTLY)
output$prev_self_plot <- plotly::renderPlotly({
  req(self_milestone_data(), record_id())
  
  prev_period <- previous_period()
  
  if (is.na(prev_period)) {
    return(plotly::plotly_empty() %>%
             plotly::add_annotations(
               text = "No previous assessment period",
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 16, color = "gray")
             ))
  }
  
  tryCatch({
    dashboard <- gmed::create_milestone_overview_dashboard(
      milestone_results = milestone_results(),
      resident_id = record_id(),
      period_text = prev_period,
      milestone_type = "self",
      milestone_system = "rep",
      resident_data = rdm_data()$residents
    )
    
    return(dashboard$spider_plot)
    
  }, error = function(e) {
    message("Error rendering self plot: ", e$message)
    return(plotly::plotly_empty() %>%
             plotly::add_annotations(
               text = "No data available",
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 14, color = "orange")
             ))
  })
})

# Render previous ACGME plot (PLOTLY)
output$prev_acgme_plot <- plotly::renderPlotly({
  req(acgme_milestone_data(), record_id())
  
  prev_period <- previous_period()
  
  if (is.na(prev_period)) {
    return(plotly::plotly_empty() %>%
             plotly::add_annotations(
               text = "No previous assessment period",
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 16, color = "gray")
             ))
  }
  
  tryCatch({
    dashboard <- gmed::create_milestone_overview_dashboard(
      milestone_results = milestone_results(),
      resident_id = record_id(),
      period_text = prev_period,
      milestone_type = "program",      # Program = CCC assessment
      milestone_system = "acgme",       # ACGME system
      resident_data = rdm_data()$residents
    )
    
    return(dashboard$spider_plot)
    
  }, error = function(e) {
    message("Error rendering ACGME plot: ", e$message)
    return(plotly::plotly_empty() %>%
             plotly::add_annotations(
               text = "No data available",
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 14, color = "orange")
             ))
  })
})
    
    # ========================================================================
    # MILESTONE ENTRY & SUBMISSION
    # ========================================================================
    
    # Initialize milestone rating module
    milestone_scores <- gmed::mod_miles_rating_server("rating", period = period)
    
    # Handle submission
    shiny::observeEvent(milestone_scores$done(), {
      req(milestone_scores$done() > 0)
      
      scores <- milestone_scores$scores()
      descriptions <- milestone_scores$desc()
      
      if (length(scores) == 0) {
        shiny::showNotification(
          "Please complete at least one milestone rating.",
          type = "warning"
        )
        return()
      }
      
      # Get period number
      period_num <- get_period_number_from_name(period())
      
      # Get field mapping
      field_mapping <- gmed::get_milestone_field_mapping_rdm2("milestone_selfevaluation_c33c")
      
      # Build submission
      submission_data <- list(
        record_id = record_id(),
        redcap_repeat_instrument = "milestone_selfevaluation_c33c",
        redcap_repeat_instance = period_num,
        prog_mile_date_self = as.character(Sys.Date()),
        prog_mile_period_self = period_num
      )
      
      # Add scores
      for (module_name in names(scores)) {
        redcap_field <- field_mapping[module_name]
        if (!is.na(redcap_field)) {
          submission_data[[redcap_field]] <- as.numeric(scores[[module_name]])
        }
      }
      
      # Add descriptions
      for (module_name in names(descriptions)) {
        base_field <- field_mapping[module_name]
        if (!is.na(base_field)) {
          desc_field <- paste0(base_field, "_desc")
          desc_text <- descriptions[[module_name]]
          if (!is.null(desc_text) && nchar(trimws(desc_text)) > 0) {
            submission_data[[desc_field]] <- desc_text
          }
        }
      }
      
      # Mark complete
      submission_data$milestone_selfevaluation_c33c_complete <- 2
      
      # Submit to REDCap
      result <- tryCatch({
        REDCapR::redcap_write_oneshot(
          ds_to_write = data.frame(submission_data, stringsAsFactors = FALSE),
          redcap_uri = Sys.getenv("REDCAP_URI"),
          token = Sys.getenv("RDM_TOKEN")
        )
      }, error = function(e) {
        list(success = FALSE, message = e$message)
      })
      
      if (isTRUE(result$success)) {
        shiny::showNotification(
          "Milestone self-evaluation submitted successfully!",
          type = "message",
          duration = 5
        )
      } else {
        shiny::showNotification(
          paste("Error:", result$message),
          type = "error",
          duration = NULL
        )
      }
    })
  })
}