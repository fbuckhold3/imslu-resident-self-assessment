#!/usr/bin/env Rscript

# ============================================================================
# Reinstall gmed Package from Local Source
# ============================================================================
# This script uninstalls the current gmed package and reinstalls it from
# the local source directory at /home/user/gmed
#
# Run this from R console or RStudio:
#   source("reinstall_local_gmed.R")
# ============================================================================

cat("========================================\n")
cat("Reinstalling gmed from Local Source\n")
cat("========================================\n\n")

# Step 1: Remove existing package
cat("Step 1: Removing existing gmed package...\n")
tryCatch({
  remove.packages("gmed")
  cat("✓ Existing package removed\n\n")
}, error = function(e) {
  cat("Note: Package not currently installed or already removed\n\n")
})

# Step 2: Install from local source
cat("Step 2: Installing gmed from /home/user/gmed...\n")

# Check if devtools is available
if (!requireNamespace("devtools", quietly = TRUE)) {
  cat("Installing devtools first...\n")
  install.packages("devtools")
}

# Install from local path
devtools::install("/home/user/gmed", upgrade = "never", force = TRUE)

cat("\n========================================\n")
cat("✓ gmed reinstalled successfully!\n")
cat("========================================\n\n")

cat("The package has been installed from your local source.\n")
cat("Any changes you made to the gmed package are now active.\n\n")

cat("Next steps:\n")
cat("  1. Restart your R session (Ctrl+Shift+F10 in RStudio)\n")
cat("  2. Restart your Shiny app\n\n")

# Load the package to verify
cat("Verifying installation...\n")
tryCatch({
  library(gmed)
  cat("✓ Package loads successfully\n")

  # Check if the career planning function exists
  if (exists("display_career_planning", where = "package:gmed")) {
    cat("✓ display_career_planning() function found\n")
  }
  if (exists("display_wellness", where = "package:gmed")) {
    cat("✓ display_wellness() function found\n")
  }
}, error = function(e) {
  cat("Error loading package: ", e$message, "\n")
})
