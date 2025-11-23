#!/usr/bin/env Rscript

# ============================================================================
# GENERATE ACCURATE MANIFEST.JSON FOR POSIT CONNECT CLOUD
# ============================================================================
# This script generates a manifest.json file with accurate package versions
# based on your currently installed packages.
#
# Usage: Rscript generate_manifest.R
# ============================================================================

library(jsonlite)

message("=== Generating manifest.json for Posit Connect Cloud ===\n")

# Define required packages from CRAN
cran_packages <- c(
  "shiny", "shinyjs", "bslib", "DT",
  "dplyr", "tidyr", "purrr", "lubridate",
  "plotly", "ggplot2", "httr", "jsonlite", "data.table"
)

# Check which packages are installed
installed <- installed.packages()

# Build package list for manifest
packages_list <- list()

for (pkg in cran_packages) {
  if (pkg %in% rownames(installed)) {
    version <- installed[pkg, "Version"]

    packages_list[[pkg]] <- list(
      Source = "CRAN",
      Repository = "https://cran.rstudio.com/",
      description = list(
        Package = pkg,
        Version = version
      )
    )

    message(sprintf("✓ %s: %s", pkg, version))
  } else {
    warning(sprintf("✗ %s: NOT INSTALLED", pkg))
  }
}

# Add gmed from GitHub
message("\n--- GitHub Packages ---")
if ("gmed" %in% rownames(installed)) {
  version <- installed["gmed", "Version"]
  message(sprintf("✓ gmed: %s (GitHub: fbuckhold3/gmed)", version))
} else {
  message("⚠ gmed: NOT INSTALLED - will use default GitHub reference")
}

packages_list[["gmed"]] <- list(
  Source = "github",
  RemoteType = "github",
  RemoteHost = "api.github.com",
  RemoteUsername = "fbuckhold3",
  RemoteRepo = "gmed",
  RemoteRef = "HEAD",
  RemoteSha = "main",
  GithubRepo = "gmed",
  GithubUsername = "fbuckhold3",
  GithubRef = "main",
  description = list(
    Package = "gmed",
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteUsername = "fbuckhold3",
    RemoteRepo = "gmed"
  )
)

# Get R version
r_version <- paste(R.version$major, R.version$minor, sep = ".")
message(sprintf("\n--- Platform ---"))
message(sprintf("✓ R version: %s", r_version))

# Build complete manifest structure
manifest <- list(
  version = 1,
  locale = "en_US.UTF-8",
  metadata = list(
    appmode = "shiny",
    entrypoint = "app.R",
    description = "IMSLU Resident Self-Assessment Application - RDM 2.0"
  ),
  platform = list(
    version = r_version,
    type = "R"
  ),
  packages = packages_list,
  files = list(
    "app.R" = list(checksum = "")
  ),
  users = NULL
)

# Write manifest.json
manifest_file <- "manifest.json"
write_json(manifest, manifest_file, auto_unbox = TRUE, pretty = TRUE)

message(sprintf("\n✅ manifest.json generated successfully!"))
message(sprintf("📄 Location: %s", normalizePath(manifest_file)))
message("\n--- Next Steps ---")
message("1. Review manifest.json")
message("2. Configure environment variables in Posit Connect Cloud:")
message("   - RDM_TOKEN (required)")
message("   - FAC_TOKEN (optional)")
message("3. Deploy using: rsconnect::deployApp()")
message("\nSee DEPLOYMENT.md for detailed instructions.")
