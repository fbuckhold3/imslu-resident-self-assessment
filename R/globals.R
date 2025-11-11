# ============================================================================
# SHINY BROWSER CONFIGURATION
# ============================================================================
options(shiny.launch.browser = TRUE)

# ============================================================================
# Load required packages
# ============================================================================
library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(plotly)
library(ggplot2)
library(gmed)

# ============================================================================
# SSL CONFIGURATION FOR REDCAP
# ============================================================================

# Disable SSL verification for REDCap (institutional certificate issues)
httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

# ============================================================================
# CONFIGURATION - Define BEFORE sourcing modules
# ============================================================================
app_config <- list(
  rdm_token = Sys.getenv("RDM_TOKEN"),
  fac_token = Sys.getenv("FAC_TOKEN", unset = NA),
  redcap_url = "https://redcapsurvey.slu.edu/api/",
  debug_mode = Sys.getenv("DEBUG_MODE", "true") == "true"
)

# Validate token
if (app_config$rdm_token == "") {
  stop("RDM_TOKEN not found in .Renviron file. Please create .Renviron with your token.")
}

# ============================================================================
# MODULE FILES - Source AFTER config is defined
# ============================================================================
source("R/scholarship_entry.R")
# Source new config files
source("R/config/period_config.R")
source("R/config/module_registry.R")
source("R/config/field_mappings.R")

# Source wrapper modules (once created)
source("R/modules/wrappers/mod_scholarship_wrapper.R")
source("R/modules/wrappers/mod_career_planning_wrapper.R")
source("R/modules/wrappers/mod_assessment_wrapper.R")
source("R/modules/wrappers/mod_milestone_self_eval_wrapper.R")

# Source individual modules
source("R/modules/mod_program_feedback.R")
source("R/modules/mod_learning.R")


# ============================================================================
# DATA LOADING
# ============================================================================
app_data_store <- NULL

load_app_data <- function() {
  if (is.null(app_data_store)) {
    message("=== Loading RDM 2.0 Data ===")
    tryCatch({
      complete_data <- gmed::load_rdm_complete(
        rdm_token = app_config$rdm_token,
        redcap_url = app_config$redcap_url,
        raw_or_label = "raw",  # ADD THIS - use raw to get checkboxes
        verbose = app_config$debug_mode
      )
      
      if (is.null(complete_data$residents)) {
        stop("Failed to load residents data")
      }
      
      app_data_store <<- complete_data
      
      message("Data loaded successfully!")
      message("  - Residents: ", nrow(complete_data$residents))
      message("  - Assessment records: ", nrow(complete_data$assessment))
      
      return(app_data_store)
      
    }, error = function(e) {
      stop("Failed to load RDM data: ", e$message)
    })
    
  } else {
    return(app_data_store)
  }
}

# ============================================================================
# STARTUP MESSAGE - Print LAST
# ============================================================================
message("=== RDM 2.0 Self-Assessment App ===")
message("Debug mode: ", app_config$debug_mode)
message("Ready to load data on first access")

