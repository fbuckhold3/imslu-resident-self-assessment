# ============================================================================
# GOALS WRAPPER MODULE
# Combines gmed display + app goal entry
# ============================================================================

#' Goals Wrapper UI
#' @export
mod_goals_wrapper_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Milestone-based Goal Setting"),
    p("As part of your Individualized Learning Plan (ILP), set three goals based on the milestones you just self-reflected on. The spider plot on the left compares your self-assessment to the median of other residents at your level."),
    
    hr(),
    
    # Goal entry section (app-specific)
    goalSettingUI(ns("entry"))
  )
}

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
        period()
      } else {
        period
      }
    })
    
    # Get resident info
    resident_info <- reactive({
      req(rdm_data(), record_id())
      app_data <- rdm_data()
      
      resident <- app_data$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
      
      if (nrow(resident) > 0) {
        return(resident$name)
      } else {
        return(paste("Resident", record_id()))
      }
    })
    
# Get current milestone data - HYBRID approach with DEBUG mode
    current_milestone_data <- reactive({
      # DEBUG MODE: Use fake data for testing
      if (exists("app_config") && !is.null(app_config$use_fake_milestone_data) && app_config$use_fake_milestone_data) {
        message("⚠ DEBUG MODE: Using fake milestone data for goals")
        
        # Get period name directly
        period_name <- if (is.function(period_info)) period_info() else period_info
        
        fake_data <- data.frame(
          record_id = record_id(),
          period_name = period_name,
          # PC subcompetencies
          rep_pc1_self = 3, rep_pc2_self = 3, rep_pc3_self = 2, 
          rep_pc4_self = 3, rep_pc5_self = 2, rep_pc6_self = 3,
          # MK subcompetencies  
          rep_mk1_self = 3, rep_mk2_self = 3, rep_mk3_self = 2,
          # SBP subcompetencies
          rep_sbp1_self = 3, rep_sbp2_self = 2, rep_sbp3_self = 3,
          # PBLI subcompetencies (note: uses 'pbl' in field names)
          rep_pbl1_self = 3, rep_pbl2_self = 2,
          # PROF subcompetencies
          rep_prof1_self = 3, rep_prof2_self = 3, rep_prof3_self = 2, rep_prof4_self = 3,
          # ICS subcompetencies
          rep_ics1_self = 3, rep_ics2_self = 2, rep_ics3_self = 3,
          stringsAsFactors = FALSE
        )
        
        # Create fake median data
        fake_medians <- data.frame(
          period_name = period_name,
          rep_pc1_self = 2.5, rep_pc2_self = 2.5, rep_pc3_self = 1.5, 
          rep_pc4_self = 2.5, rep_pc5_self = 1.5, rep_pc6_self = 2.5,
          rep_mk1_self = 2.5, rep_mk2_self = 2.5, rep_mk3_self = 1.5,
          rep_sbp1_self = 2.5, rep_sbp2_self = 1.5, rep_sbp3_self = 2.5,
          rep_pbl1_self = 2.5, rep_pbl2_self = 1.5,
          rep_prof1_self = 2.5, rep_prof2_self = 2.5, rep_prof3_self = 1.5, rep_prof4_self = 2.5,
          rep_ics1_self = 2.5, rep_ics2_self = 1.5, rep_ics3_self = 2.5,
          n_residents = 50,
          stringsAsFactors = FALSE
        )
        
        return(list(
          data = fake_data,
          medians = fake_medians,
          record_id = record_id(),
          period_text = period_name,
          milestone_cols = grep("^rep_.*_self$", names(fake_data), value = TRUE)
        ))
      }
      
      # PRIORITY 1: Use just-entered milestone data from previous module
      if (!is.null(milestone_output) && is.function(milestone_output)) {
        milestone_mod <- milestone_output()
        if (!is.null(milestone_mod) && "current_data" %in% names(milestone_mod)) {
          fresh_data <- milestone_mod$current_data()
          if (!is.null(fresh_data) && is.data.frame(fresh_data) && nrow(fresh_data) > 0) {
            message("✓ Using fresh milestone data from current session")
            
            period_name <- if (is.function(period_info)) period_info() else period_info
            milestone_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", 
                                  names(fresh_data), value = TRUE)
            
            # Get median data from milestone_workflow structure
            app_data <- rdm_data()
            median_data <- NULL
            
            if (!is.null(app_data$milestone_workflow)) {
              if ("milestone_selfevaluation_c33c_rep_self" %in% names(app_data$milestone_workflow)) {
                workflow_obj <- app_data$milestone_workflow$milestone_selfevaluation_c33c_rep_self
                if (!is.null(workflow_obj$medians)) {
                  median_data <- workflow_obj$medians %>%
                    dplyr::filter(period_name == !!period_name)
                }
              }
            }
            
            return(list(
              data = fresh_data,
              medians = median_data,
              record_id = record_id(),
              period_text = period_name,
              milestone_cols = milestone_cols
            ))
          }
        }
      }
      
      # PRIORITY 2: Use milestone workflow from rdm_data (saved data)
      req(rdm_data(), record_id(), period_info)
      
      app_data <- rdm_data()
      period_name <- if (is.function(period_info)) period_info() else period_info
      
      # Check milestone_workflow structure
      if (!is.null(app_data$milestone_workflow) && 
          "milestone_selfevaluation_c33c_rep_self" %in% names(app_data$milestone_workflow)) {
        
        workflow_obj <- app_data$milestone_workflow$milestone_selfevaluation_c33c_rep_self
        
        if (!is.null(workflow_obj$data)) {
          milestone_data <- workflow_obj$data
          
          # Filter for current resident and period
          resident_data <- milestone_data %>%
            dplyr::filter(
              record_id == !!record_id(),
              period_name == !!period_name
            )
          
          if (nrow(resident_data) > 0) {
            message("✓ Using milestone data from milestone_workflow")
            
            milestone_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", 
                                  names(resident_data), value = TRUE)
            
            # Get median data for this period
            median_data <- NULL
            if (!is.null(workflow_obj$medians)) {
              median_data <- workflow_obj$medians %>%
                dplyr::filter(period_name == !!period_name)
            }
            
            return(list(
              data = resident_data,
              medians = median_data,
              record_id = record_id(),
              period_text = period_name,
              milestone_cols = milestone_cols
            ))
          }
        }
      }
      
      # FALLBACK: Return empty data frame (not NULL)
      message("⚠ No milestone data available - returning empty data frame")
      data.frame()
    })
    
    # Subcompetency maps
    subcompetency_maps <- list(
      PC = list(
        "1" = "History",
        "2" = "Physical Examination",
        "3" = "Clinical Reasoning",
        "4" = "Patient Management - Inpatient",
        "5" = "Patient Management - Outpatient",
        "6" = "Digital Health"
      ),
      MK = list(
        "1" = "Applied Foundational Sciences",
        "2" = "Therapeutic Knowledge",
        "3" = "Knowledge of Diagnostic Testing"
      ),
      SBP = list(
        "1" = "Patient Safety and Quality Improvement",
        "2" = "System Navigation for Patient-Centered Care",
        "3" = "Physician Role in Health Care Systems"
      ),
      PBLI = list(
        "1" = "Evidence-Based and Informed Practice",
        "2" = "Reflective Practice and Commitment to Personal Growth"
      ),
      PROF = list(
        "1" = "Professional Behavior",
        "2" = "Ethical Principles",
        "3" = "Accountability/Conscientiousness",
        "4" = "Knowledge of Systemic and Individual Factors of Well-Being"
      ),
      ICS = list(
        "1" = "Patient- and Family-Centered Communication",
        "2" = "Interprofessional and Team Communication",
        "3" = "Communication within Health Care Systems"
      )
    )
    
    # Competency list (for ordering)
    competency_list <- c("PC", "MK", "SBP", "PBLI", "PROF", "ICS")
    
    # Milestone levels
    milestone_levels <- list(
      "1" = "Critical Deficiency",
      "2" = "Early Learner",
      "3" = "Developing",
      "4" = "Competent",
      "5" = "Advanced"
    )
    
    # Initialize goal setting module
    goals_mod <- goalSettingServer(
      "entry",
      rdm_dict_data = reactive({
        req(data_dict())
        dict <- data_dict()
        if (is.data.frame(dict)) {
          if (!"Variable / Field Name" %in% names(dict) && "field_name" %in% names(dict)) {
            dict <- dict %>%
              dplyr::mutate(
                `Variable / Field Name` = field_name,
                `Choices, Calculations, OR Slider Labels` = select_choices_or_calculations,
                `Field Label` = field_label
              )
          }
        }
        dict
      }),
      subcompetency_maps = subcompetency_maps,
      competency_list = competency_list,
      milestone_levels = milestone_levels,
      current_milestone_data = current_milestone_data,
      resident_info = resident_info,
      selected_period = reactive({
        if (is.function(period_info)) period_info() else period_info
      })
    )
    
    # Load previous goals
    observe({
      req(ilp_data(), record_id())
      
      # Get current period name
      current_period_name <- if (is.function(period_info)) period_info() else period_info
      
      # Use the helper from period_config.R
      prev_period <- get_previous_period(current_period_name)
      
      if (!is.na(prev_period)) {
        prev_goals <- load_previous_goals(
          ilp_data = ilp_data(),
          record_id = record_id(),
          current_period = prev_period
        )
        
        if (!is.null(prev_goals)) {
          goals_mod$set_previous_goals(prev_goals)
        }
      }
    })
    
    # ADD THIS: Handle submission
    observeEvent(goals_mod$submission_ready(), {
      req(goals_mod$submission_ready())
      
      showNotification("Saving ILP goals...", type = "message", duration = NULL, id = "saving_goals")
      
      result <- tryCatch({
        submit_ilp_goals(
          goals_mod = goals_mod,
          record_id = record_id(),
          period = period_info(),
          redcap_url = app_config$redcap_url,
          token = app_config$rdm_token
        )
      }, error = function(e) {
        list(success = FALSE, message = e$message)
      })
      
      removeNotification(id = "saving_goals")
      goals_mod$reset_submission()
      
      if (result$success) {
        showNotification("✅ ILP goals submitted successfully!", type = "message", duration = 5)
      } else {
        showNotification(paste("Error:", result$message), type = "error", duration = 10)
      }
    })
    
    return(goals_mod)
  })
}



#' Submit ILP Goals to REDCap
#'
#' @param goals_mod Goal module object with responses
#' @param record_id Resident record ID
#' @param period Period number
#' @param redcap_url REDCap API URL
#' @param token REDCap API token
#' @return List with success status and message
submit_ilp_goals <- function(goals_mod, record_id, period, redcap_url, token) {
  
  # Get responses
  resp <- goals_mod$get_responses()
  
  # Get period code
  period_code <- as.character(period)
  
  # Build field list
  fields <- list(
    record_id = as.character(record_id),
    redcap_repeat_instrument = "ilp",
    redcap_repeat_instance = period_code,
    ilp_date = format(Sys.Date(), "%Y-%m-%d"),
    year_resident = period_code
  )
  
  # Map PC/MK goals
  if (!is.null(resp$pcmk)) {
    g <- resp$pcmk
    
    # Extract competency number
    if (startsWith(g$subcompetency, "PC")) {
      comp_num <- as.numeric(sub("PC", "", g$subcompetency))
    } else if (startsWith(g$subcompetency, "MK")) {
      comp_num <- as.numeric(sub("MK", "", g$subcompetency))
    }
    
    fields$prior_goal_pcmk <- g$had_prior_goal
    fields$review_q_pcmk <- if(g$had_prior_goal == "1") g$review else ""
    fields$review_q2_pcmk <- if(g$had_prior_goal == "0") g$review else ""
    fields$goal_pcmk <- as.character(comp_num)
    fields$goal_level_pcmk <- g$selected_level
    fields$goal_level_r_pcmk <- g$selected_row
    fields$how_pcmk <- g$how_to_achieve
  }
  
  # Map SBP/PBLI goals
  if (!is.null(resp$sbppbl)) {
    g <- resp$sbppbl
    
    if (startsWith(g$subcompetency, "SBP")) {
      comp_num <- as.numeric(sub("SBP", "", g$subcompetency))
    } else if (startsWith(g$subcompetency, "PBLI")) {
      comp_num <- as.numeric(sub("PBLI", "", g$subcompetency))
    }
    
    fields$prior_goal_sbppbl <- g$had_prior_goal
    fields$review_q_sbppbl <- if(g$had_prior_goal == "1") g$review else ""
    fields$review_q2_sbppbl <- if(g$had_prior_goal == "0") g$review else ""
    fields$goal_sbppbl <- as.character(comp_num)
    fields$goal_level_sbppbl <- g$selected_level
    fields$goal_r_sbppbl <- g$selected_row
    fields$how_sbppbl <- g$how_to_achieve
  }
  
  # Map PROF/ICS goals
  if (!is.null(resp$profics)) {
    g <- resp$profics
    
    if (startsWith(g$subcompetency, "PROF")) {
      comp_num <- as.numeric(sub("PROF", "", g$subcompetency))
    } else if (startsWith(g$subcompetency, "ICS")) {
      comp_num <- as.numeric(sub("ICS", "", g$subcompetency))
    }
    
    fields$prior_goal_profics <- g$had_prior_goal
    fields$review_q_profics <- if(g$had_prior_goal == "1") g$review else ""
    fields$review_q2_profics <- if(g$had_prior_goal == "0") g$review else ""
    fields$goal_subcomp_profics <- as.character(comp_num)
    fields$goal_level_profics <- g$selected_level
    fields$goal_r_profics <- g$selected_row
    fields$how_profics <- g$how_to_achieve
  }
  
  # Convert to JSON
  data_json <- jsonlite::toJSON(list(fields), auto_unbox = TRUE)
  
  # Submit to REDCap
  response <- httr::POST(
    url = redcap_url,
    body = list(
      token = token,
      content = "record",
      action = "import",
      format = "json",
      type = "flat",
      data = data_json,
      returnContent = "ids",
      returnFormat = "json"
    ),
    encode = "form"
  )
  
  if (httr::status_code(response) == 200) {
    return(list(success = TRUE, message = "Goals submitted successfully"))
  } else {
    return(list(success = FALSE, message = httr::content(response, "text")))
  }
}