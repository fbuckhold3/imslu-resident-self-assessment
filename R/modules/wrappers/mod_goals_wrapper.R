#' Goals Wrapper Server
#' @export
mod_goals_wrapper_server <- function(id, rdm_data, record_id, period, data_dict, 
                                     milestone_output = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Extract ILP data
    ilp_data <- reactive({
      req(rdm_data())
      app_data <- rdm_data()
      
      if ("ilp" %in% names(app_data$all_forms)) {
        app_data$all_forms$ilp
      } else {
        NULL
      }
    })
    
    # Get period info
    period_info <- reactive({
      if (is.function(period)) {
        p <- period()
        if (is.list(p) && "period_name" %in% names(p)) {
          return(p$period_name)
        }
        return(p)
      }
      if (is.list(period) && "period_name" %in% names(period)) {
        return(period$period_name)
      }
      return(period)
    })
    
    # Get resident info
    resident_info <- reactive({
      req(rdm_data(), record_id())
      app_data <- rdm_data()
      
      resident <- app_data$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
      
      if (nrow(resident) > 0) {
        list(
          name = paste(resident$name_first, resident$name_last),
          record_id = resident$record_id
        )
      } else {
        list(name = "Unknown", record_id = record_id())
      }
    })
    
    # Get milestone data from workflow
    current_milestone_data <- reactive({
      req(rdm_data())
      app_data <- rdm_data()
      
      message("=== GOAL MODULE: Getting Milestone Data ===")
      
      # Check for milestone_workflow
      if (!is.null(app_data$milestone_workflow)) {
        # Look for self configuration
        for (config_name in names(app_data$milestone_workflow)) {
          if (grepl("self", config_name, ignore.case = TRUE)) {
            config <- app_data$milestone_workflow[[config_name]]
            
            if (!is.null(config$data) && nrow(config$data) > 0) {
              message("Using milestone config: ", config_name)
              message("  Data rows: ", nrow(config$data))
              message("  Milestone cols: ", length(config$score_columns))
              
              return(list(
                data = config$data,
                milestone_cols = config$score_columns,
                medians = config$medians
              ))
            }
          }
        }
      }
      
      # Fallback: direct form access
      if ("milestone_selfevaluation_c33c" %in% names(app_data$all_forms)) {
        self_data <- app_data$all_forms$milestone_selfevaluation_c33c
        milestone_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", 
                              names(self_data), value = TRUE)
        
        message("Using direct form access")
        message("  Data rows: ", nrow(self_data))
        message("  Milestone cols: ", length(milestone_cols))
        
        return(list(
          data = self_data,
          milestone_cols = milestone_cols,
          medians = NULL
        ))
      }
      
      message("No milestone data found")
      return(list(data = data.frame(), milestone_cols = character(0), medians = NULL))
    })
    
    # Dummy objects for compatibility
    subcompetency_maps <- list()
    competency_list <- c("PC", "MK", "SBP", "PBLI", "PROF", "ICS")
    milestone_levels <- list("1" = "Critical Deficiency", "2" = "Early Learner",
                            "3" = "Developing", "4" = "Competent", "5" = "Advanced")
    
    # Initialize goal setting module
goals_mod <- goalSettingServer(
  "entry",
  rdm_dict_data = reactive({
    req(data_dict())
    data_dict()
  }),
  subcompetency_maps = subcompetency_maps,
  competency_list = competency_list,
  milestone_levels = milestone_levels,
  current_milestone_data = current_milestone_data,
  resident_info = resident_info,
  selected_period = reactive(period_info()),
  ilp_data = ilp_data
)
return(goals_mod)  # ADD THIS LINE
  })
}