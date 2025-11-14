# ============================================================================
# DIAGNOSTIC SCRIPT FOR GOALS MODULE
# Run this WHILE the app is running to diagnose data issues
# ============================================================================

library(dplyr)

# Load the data
source("R/globals.R")
app_data <- load_app_data()

cat("\n=== DIAGNOSTIC REPORT ===\n\n")

# 1. Check milestone workflow configs
cat("1. MILESTONE WORKFLOW CONFIGURATIONS:\n")
if (!is.null(app_data$milestone_workflow)) {
  cat("   Available configs:", paste(names(app_data$milestone_workflow), collapse = ", "), "\n\n")

  for (config_name in names(app_data$milestone_workflow)) {
    config <- app_data$milestone_workflow[[config_name]]
    cat("   Config:", config_name, "\n")
    cat("     - Data rows:", nrow(config$data), "\n")
    cat("     - Score columns:", length(config$score_columns), "\n")
    if (length(config$score_columns) > 0) {
      cat("     - Sample cols:", paste(head(config$score_columns, 3), collapse = ", "), "\n")
    }

    # Check if has period 4 ACGME data for record_id 99
    if (grepl("acgme", config_name, ignore.case = TRUE)) {
      test_data <- config$data %>%
        filter(record_id == 99, prog_mile_period == 4)
      cat("     - Record 99, Period 4 data:", nrow(test_data), "rows\n")
      if (nrow(test_data) > 0) {
        # Show first few milestone scores
        milestone_cols <- config$score_columns[1:3]
        cat("     - Sample scores:", paste(names(test_data)[names(test_data) %in% milestone_cols],
                                           "=", as.character(test_data[1, milestone_cols]),
                                           collapse = ", "), "\n")
      }
    }
    cat("\n")
  }
} else {
  cat("   WARNING: No milestone_workflow found!\n\n")
}

# 2. Check data dictionary for milestone row fields
cat("2. DATA DICTIONARY MILESTONE FIELDS:\n")
dict <- app_data$data_dict

# Check for PC1 rows
pc1_fields <- dict %>%
  filter(grepl("^pc1_r\\d+$", field_name)) %>%
  select(field_name, field_label, select_choices_or_calculations)

cat("   PC1 milestone row fields found:", nrow(pc1_fields), "\n")
if (nrow(pc1_fields) > 0) {
  cat("   Sample field:\n")
  cat("     - field_name:", pc1_fields$field_name[1], "\n")
  cat("     - field_label:", pc1_fields$field_label[1], "\n")
  choices_preview <- substr(pc1_fields$select_choices_or_calculations[1], 1, 100)
  cat("     - choices (first 100 chars):", choices_preview, "...\n")
} else {
  cat("   WARNING: No PC1 milestone row fields found!\n")
  cat("   Checking alternative patterns:\n")

  # Try different patterns
  alt_patterns <- c("pc1", "PC1", "patient_care_1", "milestone.*pc1")
  for (pattern in alt_patterns) {
    matches <- dict %>% filter(grepl(pattern, field_name, ignore.case = TRUE))
    if (nrow(matches) > 0) {
      cat("     - Pattern '", pattern, "' found ", nrow(matches), " fields\n", sep = "")
      cat("       Sample:", paste(head(matches$field_name, 3), collapse = ", "), "\n")
    }
  }
}
cat("\n")

# 3. Check ILP data
cat("3. ILP (PREVIOUS GOALS) DATA:\n")
if ("ilp" %in% names(app_data$all_forms)) {
  ilp_data <- app_data$all_forms$ilp
  cat("   ILP records found:", nrow(ilp_data), "\n")

  # Check for record 99
  ilp_99 <- ilp_data %>% filter(record_id == 99)
  cat("   Record 99 ILP entries:", nrow(ilp_99), "\n")

  if (nrow(ilp_99) > 0) {
    cat("   Sample ILP fields:", paste(head(names(ilp_99), 10), collapse = ", "), "\n")
  }
} else {
  cat("   WARNING: No ILP form found in all_forms!\n")
}
cat("\n")

# 4. Check all_forms structure
cat("4. AVAILABLE FORMS:\n")
cat("   Forms in all_forms:", paste(names(app_data$all_forms), collapse = ", "), "\n")
cat("\n")

# 5. Check for record 99 specifically
cat("5. TEST RESIDENT (record_id = 99):\n")
resident_99 <- app_data$residents %>% filter(record_id == 99)
if (nrow(resident_99) > 0) {
  cat("   Name:", paste(resident_99$name_first, resident_99$name_last), "\n")
  cat("   Type:", resident_99$type, "\n")
  cat("   Grad Year code:", resident_99$grad_yr, "\n")
} else {
  cat("   WARNING: Record 99 not found!\n")
}

cat("\n=== END DIAGNOSTIC REPORT ===\n")
