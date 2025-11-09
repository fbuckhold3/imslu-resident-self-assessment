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

#VzvHJY - record_id 99
#PkWIkr - record_id 94

app_data <- complete_data

td <- app_data$all_forms$test_data %>%
  filter(record_id == "94")

print(td)

