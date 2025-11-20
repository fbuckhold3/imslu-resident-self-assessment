# ============================================================================
# SCHOLARSHIP WRAPPER MODULE
# Combines gmed display + app entry
# ============================================================================

#' Scholarship Wrapper UI
#' @export
mod_scholarship_wrapper_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Current Activities"),
    
    # gmed visualization
    uiOutput(ns("scholarship_display")),
    
    hr(),
    
    h4("Add New Activity"),
    
    # Entry section (app-specific)
    scholarship_entry_ui(ns("entry"))
  )
}

#' Scholarship Wrapper Server
#' @export
mod_scholarship_wrapper_server <- function(id, rdm_data, record_id, period = NULL, data_dict = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to track scholarship data
    scholarship_rv <- reactiveVal(data.frame())
    
    # Function to refresh scholarship data
    refresh_scholarship <- function() {
      req(record_id())
      
      data <- gmed::fetch_scholarship_data(
        record_id = record_id(),
        redcap_url = "https://redcapsurvey.slu.edu/api/",
        redcap_token = Sys.getenv("RDM_TOKEN")
      )
      
      scholarship_rv(data)
    }
    
    # Initial load
    observe({
      req(record_id())
      refresh_scholarship()
    })
    
    # Display scholarship using gmed functions
    output$scholarship_display <- renderUI({

      # Get data dictionary
      dict <- if (is.function(data_dict)) {
        data_dict()
      } else {
        data_dict
      }

      # Get scholarship data (might be empty)
      schol_data <- scholarship_rv()

      # Validate schol_data is a data frame
      if (!is.data.frame(schol_data)) {
        schol_data <- data.frame()
      }

      # Use gmed's display functions
      scholarship_summary <- gmed::display_scholarship(
        schol_data,
        data_dict = dict
      )

      tagList(
        gmed::scholarship_badge_ui(scholarship_summary$badges),
        hr(),
        gmed::scholarship_tables_ui(scholarship_summary)
      )
    })
    
    # Entry module - pass data_dict as VALUE not reactive
    scholarship_entry_server(
      "entry",
      record_id = record_id,
      redcap_url = "https://redcapsurvey.slu.edu/api/",
      redcap_token = Sys.getenv("RDM_TOKEN"),
      data_dict = if (is.function(data_dict)) data_dict() else data_dict,
      refresh_callback = refresh_scholarship
    )
  })
}