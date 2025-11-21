#' Goals Wrapper Module UI
#' @export
mod_goals_wrapper_ui <- function(id) {
  ns <- NS(id)

  # Simple pass-through to the main goals UI
  goalSettingUI(ns("entry"))
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

      # PRIORITY 1: Check if milestone_output from this session exists
      if (!is.null(milestone_output) && is.function(milestone_output)) {
        session_milestone <- tryCatch(milestone_output(), error = function(e) NULL)

        if (!is.null(session_milestone) && !is.null(session_milestone$scores)) {
          # Convert scores to data format expected by spider plot
          # scores() returns a named list like: list(PC1 = 5, PC2 = 3, ...)
          # Force reactive dependency on scores
          scores_data <- tryCatch(session_milestone$scores(), error = function(e) NULL)

          if (!is.null(scores_data) && length(scores_data) > 0) {
            # Build a single-row data frame with current resident's scores
            resident_row <- data.frame(
              record_id = record_id(),
              period_name = period(),
              stringsAsFactors = FALSE
            )

            # Add milestone scores with proper field naming (rep_pc1_self, etc.)
            for (milestone_name in names(scores_data)) {
              field_name <- paste0("rep_", tolower(milestone_name), "_self")
              resident_row[[field_name]] <- as.numeric(scores_data[[milestone_name]])
            }

            milestone_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$",
                                  names(resident_row), value = TRUE)

# message("  Created data from session scores")
# message("  Milestone cols: ", length(milestone_cols))

            return(list(
              data = resident_row,
              milestone_cols = milestone_cols,
              medians = NULL
            ))
          }
        }
      }

      # PRIORITY 2: Use PREVIOUS period's ACGME milestone data
# message("No session milestone data - looking for PREVIOUS period ACGME data")

      # Calculate previous period
      current_period <- if (is.function(period)) period() else period
      prev_period <- current_period - 1

      if (prev_period >= 1 && !is.null(app_data$milestone_workflow)) {
# message("Looking for ACGME milestones from period ", prev_period)

        # Look for ACGME configuration
        for (config_name in names(app_data$milestone_workflow)) {
          if (grepl("acgme", config_name, ignore.case = TRUE)) {
            config <- app_data$milestone_workflow[[config_name]]

            if (!is.null(config$data) && nrow(config$data) > 0) {
              # Filter for this resident and previous period
              # ACGME uses acgme_mile_period, not prog_mile_period
              period_field <- if ("acgme_mile_period" %in% names(config$data)) {
                "acgme_mile_period"
              } else {
                "prog_mile_period"
              }

              prev_data <- config$data %>%
                dplyr::filter(
                  record_id == !!record_id(),
                  !!rlang::sym(period_field) == !!prev_period
                )

              if (nrow(prev_data) > 0) {
# message("Using ACGME milestones from period ", prev_period)
# message("  Data rows: ", nrow(prev_data))

                # Get milestone columns - try different field names
                milestone_cols <- config$score_columns
                if (is.null(milestone_cols) || length(milestone_cols) == 0) {
                  # Try alternative field names
                  milestone_cols <- config$milestone_cols
                }
                if (is.null(milestone_cols) || length(milestone_cols) == 0) {
                  # Extract from data directly
                  milestone_cols <- grep("^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$",
                                        names(prev_data), value = TRUE)
                }

# message("  Milestone cols: ", length(milestone_cols))
                if (length(milestone_cols) > 0) {
# message("  Sample cols: ", paste(head(milestone_cols, 3), collapse = ", "))
                }

                return(list(
                  data = prev_data,
                  milestone_cols = milestone_cols,
                  medians = config$medians
                ))
              }
            }
          }
        }
      }

      # FALLBACK: Try self-assessment workflow data
      if (!is.null(app_data$milestone_workflow)) {
        for (config_name in names(app_data$milestone_workflow)) {
          if (grepl("self", config_name, ignore.case = TRUE)) {
            config <- app_data$milestone_workflow[[config_name]]

            if (!is.null(config$data) && nrow(config$data) > 0) {
# message("Using milestone config (fallback): ", config_name)
# message("  Data rows: ", nrow(config$data))
# message("  Milestone cols: ", length(config$score_columns))

              return(list(
                data = config$data,
                milestone_cols = config$score_columns,
                medians = config$medians
              ))
            }
          }
        }
      }

# message("No milestone data found")
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
      rdm_dict_data = data_dict,  # Pass through directly, it's already available
      subcompetency_maps = subcompetency_maps,
      competency_list = competency_list,
      milestone_levels = milestone_levels,
      current_milestone_data = current_milestone_data,
      resident_info = resident_info,
      selected_period = reactive(period_info()),
      ilp_data = ilp_data
    )
    return(goals_mod)
  })
}