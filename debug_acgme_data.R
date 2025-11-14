# ============================================================================
# DIAGNOSTIC: Check ACGME Data Values
# ============================================================================

library(dplyr)

source("R/globals.R")
app_data <- load_app_data()

cat("\n=== ACGME DATA FOR RESIDENT 94, PERIOD 4 ===\n\n")

# Get ACGME workflow
acgme_config <- app_data$milestone_workflow$acgme_miles_acgme_program

if (!is.null(acgme_config)) {
  cat("ACGME config found\n")
  cat("Total rows:", nrow(acgme_config$data), "\n\n")

  # Filter for resident 94, period 4
  resident_94_p4 <- acgme_config$data %>%
    filter(record_id == 94, acgme_mile_period == 4)

  cat("Resident 94, Period 4 rows:", nrow(resident_94_p4), "\n\n")

  if (nrow(resident_94_p4) > 0) {
    # Get milestone columns
    milestone_cols <- grep("^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$",
                          names(resident_94_p4), value = TRUE)

    cat("Milestone columns found:", length(milestone_cols), "\n")
    cat("Sample columns:", paste(head(milestone_cols, 5), collapse = ", "), "\n\n")

    # Show first row values
    cat("MILESTONE VALUES (first row):\n")
    for (col in milestone_cols) {
      value <- resident_94_p4[[col]][1]
      cat(sprintf("  %-15s = %s\n", col, value))
    }

    cat("\n\nFull first row (all columns):\n")
    print(as.data.frame(t(resident_94_p4[1,])))
  } else {
    cat("NO DATA FOUND for resident 94, period 4\n\n")

    # Check what periods exist for resident 94
    resident_94_all <- acgme_config$data %>%
      filter(record_id == 94)

    if (nrow(resident_94_all) > 0) {
      cat("Resident 94 ACGME data exists for periods:",
          paste(unique(resident_94_all$acgme_mile_period), collapse = ", "), "\n")
    } else {
      cat("Resident 94 has NO ACGME data at all\n")
    }
  }
} else {
  cat("ERROR: ACGME config not found!\n")
}

cat("\n=== END DIAGNOSTIC ===\n")
