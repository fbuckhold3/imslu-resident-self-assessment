# ============================================================================
# RDM 2.0 RESIDENT SELF-ASSESSMENT APPLICATION
# Entry Point
# ============================================================================

library(shiny)
library(gmed)

# Source all components
source("R/globals.R")
source("R/ui.R")
source("R/server.R")

# Run the application
shinyApp(ui = ui, server = server)

