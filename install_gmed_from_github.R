#!/usr/bin/env Rscript

# ============================================================================
# Install gmed Package from GitHub
# ============================================================================
# This script installs the latest version of gmed from GitHub, which includes
# the fixed display_career_planning() and display_wellness() functions.
#
# Run this script from R or RStudio:
#   source("install_gmed_from_github.R")
#
# Or from command line:
#   Rscript install_gmed_from_github.R
# ============================================================================

cat("========================================\n")
cat("Installing gmed from GitHub\n")
cat("========================================\n\n")

# Check if devtools is installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  cat("Installing devtools...\n")
  install.packages("devtools")
}

# Install gmed from GitHub
cat("Installing gmed from fbuckhold3/gmed...\n\n")
devtools::install_github("fbuckhold3/gmed", force = TRUE, upgrade = "never")

cat("\n========================================\n")
cat("✓ gmed installed successfully!\n")
cat("========================================\n\n")

cat("The fixed functions are now available:\n")
cat("  - gmed::display_career_planning()\n")
cat("  - gmed::display_wellness()\n\n")

cat("These functions now correctly use:\n")
cat("  - lowercase 's_eval' for repeat instrument\n")
cat("  - redcap_repeat_instance for period filtering\n\n")

cat("You can now restart your Shiny app to use the updated functions.\n")
