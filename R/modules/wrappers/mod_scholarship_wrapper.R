# ============================================================================
# SCHOLARSHIP WRAPPER MODULE
# Combines display + your existing entry wizard
# ============================================================================

#' Scholarship Wrapper UI
#' @export
mod_scholarship_wrapper_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Current Activities"),
    
    # Display section
    uiOutput(ns("scholarship_display")),
    
    hr(),
    
    # Entry section (your existing wizard)
    scholarship_entry_ui(ns("entry"))
  )
}

#' Scholarship Wrapper Server
#' @export
mod_scholarship_wrapper_server <- function(id, rdm_data, record_id, period = NULL, data_dict = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Refresh counter to trigger display updates
    refresh_trigger <- reactiveVal(0)
    
    # Display existing scholarship
    output$scholarship_display <- renderUI({
      # Trigger refresh
      refresh_trigger()
      
      req(rdm_data(), record_id())
      
      # Get scholarship data for this resident
      scholarship_data <- rdm_data()$all_forms$scholarship %>%
        filter(record_id == record_id())
      
      if (nrow(scholarship_data) == 0) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"),
            " No scholarship activities recorded yet. Use the form below to add your activities."
          )
        )
      }
      
      # Display scholarship items as cards
      lapply(1:nrow(scholarship_data), function(i) {
        item <- scholarship_data[i, ]
        
        # Determine type label
        type_label <- switch(as.character(item$schol_type),
          "1" = "Quality Improvement",
          "2" = "Patient Safety Review",
          "3" = "Research",
          "4" = "Presentation",
          "5" = "Publication",
          "6" = "Education",
          "7" = "Committee",
          "Activity"
        )
        
        div(
          class = "card mb-3",
          div(
            class = "card-body",
            h5(class = "card-title", 
               span(class = "badge bg-primary me-2", type_label),
               item$schol_qi %||% item$schol_res %||% item$schol_comm %||% "Scholarship Activity"
            ),
            if (!is.null(item$schol_res_mentor) && !is.na(item$schol_res_mentor)) {
              p(class = "mb-1", strong("Mentor: "), item$schol_res_mentor)
            },
            if (!is.null(item$schol_div) && !is.na(item$schol_div)) {
              p(class = "mb-1", strong("Division: "), item$schol_div)
            },
            if (!is.null(item$schol_res_status) && !is.na(item$schol_res_status)) {
              p(class = "mb-1", strong("Status: "), item$schol_res_status)
            },
            if (!is.null(item$schol_cit) && !is.na(item$schol_cit)) {
              p(class = "mb-1 text-muted small", item$schol_cit)
            }
          )
        )
      })
    })
    
    # Initialize entry module (your existing wizard)
    # Pass refresh callback so entry form can trigger display refresh
    scholarship_entry_server(
      "entry",
      record_id = record_id,
      redcap_url = "https://redcapsurvey.slu.edu/api/",  # Your REDCap URL
      redcap_token = Sys.getenv("RDM_TOKEN"),
      data_dict = data_dict,
      refresh_callback = function() {
        # Increment refresh trigger to update display
        refresh_trigger(refresh_trigger() + 1)
      }
    )
  })
}