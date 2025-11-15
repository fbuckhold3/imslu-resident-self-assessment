# ============================================================================
# RDM 2.0 RESIDENT SELF-ASSESSMENT APPLICATION
# Entry Point
# ============================================================================


## IN DEV MODE, need to install updated gmed: 
devtools::install("~/Documents/GitHub/gmed")

library(shiny)
library(gmed)

# Source all components
source("R/globals.R")
source("R/ui.R")
source("R/server.R")


# Run the application

shinyApp(ui = ui, server = server)

#VzvHJY - record_id 99
#PkWIkr - record_id 94

