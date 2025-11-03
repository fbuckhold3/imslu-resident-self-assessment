#' Submit Self-Evaluation Data to REDCap
#'
#' @param record_id Resident record ID
#' @param period Period number
#' @param data List of field data to submit
#'
#' @return List with success status and message
#' @export
submit_self_eval_data <- function(record_id, period, data) {
  
  tryCatch({
    # Map period text to number if needed
    period_map <- c(
      "Entering Residency" = "7",
      "Mid Intern" = "1",
      "End Intern" = "2",
      "Mid PGY2" = "3",
      "End PGY2" = "4",
      "Mid PGY3" = "5",
      "Graduating" = "6"
    )
    
    period_num <- if (is.numeric(period)) {
      period
    } else if (period %in% names(period_map)) {
      as.numeric(period_map[period])
    } else {
      as.numeric(period)
    }
    
    # Get instance number for this period
    instance_number <- gmed::get_redcap_instance(
      level = switch(as.character(period_num),
                     "7" = "Intern", "1" = "Intern", "2" = "Intern",
                     "3" = "PGY2", "4" = "PGY2",
                     "5" = "PGY3", "6" = "PGY3",
                     "Intern"),
      period = period_num,
      review_type = "scheduled",
      redcap_url = "https://redcapsurvey.slu.edu/api/",
      redcap_token = Sys.getenv("RDM_TOKEN"),
      record_id = record_id
    )
    
    # Prepare base data for REDCap
    redcap_data <- list(
      record_id = as.character(record_id),
      redcap_repeat_instrument = "s_eval",
      redcap_repeat_instance = as.character(instance_number),
      s_e_period = as.character(period_num)
    )
    
    # Handle checkbox fields - convert comma-separated to individual checkbox fields
    for (field_name in names(data)) {
      value <- data[[field_name]]
      
      # Check if this is a checkbox field
      if (field_name %in% c("s_e_career_path", "s_e_fellow", "s_e_track_type")) {
        if (!is.null(value) && value != "") {
          # Split comma-separated values
          selected_codes <- strsplit(value, ",")[[1]]
          selected_codes <- trimws(selected_codes)
          
          # Get all possible codes for this field - EXACT CODES FROM DATA DICT
          all_codes <- if (field_name == "s_e_career_path") {
            c("1", "2", "3", "4", "5", "6", "7", "8")
          } else if (field_name == "s_e_fellow") {
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
          } else if (field_name == "s_e_track_type") {
            c("1", "2", "3")
          } else {
            character(0)
          }
          
          # Create checkbox fields: field___code = "1" (checked) or "0" (unchecked)
          for (code in all_codes) {
            checkbox_field <- paste0(field_name, "___", code)
            redcap_data[[checkbox_field]] <- if (code %in% selected_codes) "1" else "0"
          }
        }
      } else {
        # Regular field - add as-is
        redcap_data[[field_name]] <- value
      }
    }
    
    # Mark as complete
    redcap_data[["s_eval_complete"]] <- "2"
    
    # Convert to data frame for REDCap
    redcap_df <- as.data.frame(redcap_data, stringsAsFactors = FALSE)
    
    # Submit to REDCap
    response <- httr::POST(
      url = "https://redcapsurvey.slu.edu/api/",
      body = list(
        token = Sys.getenv("RDM_TOKEN"),
        content = "record",
        format = "json",
        type = "flat",
        overwriteBehavior = "overwrite",
        data = jsonlite::toJSON(redcap_df, auto_unbox = TRUE),
        returnContent = "ids",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      message("Self-evaluation data submitted successfully to instance ", instance_number)
      return(list(
        success = TRUE, 
        instance = instance_number,
        message = "Self-evaluation submitted successfully"
      ))
    } else {
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      return(list(
        success = FALSE, 
        message = paste("REDCap error:", error_text)
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE, 
      message = paste("Submission error:", e$message)
    ))
  })
}