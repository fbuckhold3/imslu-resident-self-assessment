# ============================================================================
# RDM 2.0 RESIDENT SELF-ASSESSMENT APPLICATION
# Entry Point
# ============================================================================


## IN DEV MODE, need to install updated gmed: 
devtools::load_all("~/Documents/GitHub/gmed")

library(shiny)

# Source all components
source("R/globals.R")
source("R/ui.R")
source("R/server.R")


# Run the application
shinyApp(ui = ui, server = server)

#VzvHJY
#PkWIkr

#.rs.api.restartSession()