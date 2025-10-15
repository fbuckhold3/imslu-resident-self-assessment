# RDM 2.0 Resident Self-Assessment

SSM-SLUH Internal Medicine resident self-assessment application using RDM 2.0 database.

## Setup

### 1. Install Dependencies
```r
install.packages(c(
  "shiny", "shinyjs", "bslib", "DT",
  "dplyr", "tidyr", "purrr", "lubridate",
  "plotly", "ggplot2"
))

# Install gmed package
remotes::install_github("fbuckhold3/gmed")