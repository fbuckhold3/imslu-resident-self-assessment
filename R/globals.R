# ============================================================================
# SHINY BROWSER CONFIGURATION
# ============================================================================

# Force Shiny to open in external browser
options(shiny.launch.browser = TRUE)

# ============================================================================
# GLOBAL CONFIGURATION AND DATA LOADING
# Using gmed package functions
# ============================================================================

# Load required packages
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
# CONFIGURATION
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
# DATA LOADING - Using gmed::load_rdm_complete
# ============================================================================

app_data_store <- NULL

#' Load Application Data
#' 
#' Loads RDM 2.0 data using gmed package, caches for session
#' 
#' @return List with all RDM data structures
load_app_data <- function() {
  if (is.null(app_data_store)) {
    message("=== Loading RDM 2.0 Data ===")
    
    tryCatch({
      # Use gmed's streamlined loader
      complete_data <- gmed::load_rdm_complete(
        rdm_token = app_config$rdm_token,
        redcap_url = app_config$redcap_url,
        verbose = app_config$debug_mode
      )
      
      # Verify critical components
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
# HELPER FUNCTIONS
# ============================================================================

#' Validate Access Code
#' 
#' Checks if provided code matches a resident's access code
#' 
#' @param code Character string access code
#' @return Logical
validate_access_code <- function(code) {
  if (is.null(code) || code == "") return(FALSE)
  
  data <- load_app_data()
  
  if (!"access_code" %in% names(data$residents)) {
    warning("No access_code column in resident data")
    return(FALSE)
  }
  
  return(code %in% data$residents$access_code)
}

#' Get Resident by Access Code
#' 
#' Retrieves resident record using access code
#' 
#' @param code Character string access code
#' @return List with resident data, or NULL if not found
get_resident_by_code <- function(code) {
  data <- load_app_data()
  
  resident <- data$residents %>%
    filter(access_code == !!code) %>%
    slice(1)
  
  if (nrow(resident) == 0) return(NULL)
  
  as.list(resident)
}

# ============================================================================
# STARTUP MESSAGE
# ============================================================================

message("=== RDM 2.0 Self-Assessment App ===")
message("Debug mode: ", app_config$debug_mode)
message("Ready to load data on first access")