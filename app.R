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

miles <- app_data$all_forms$milestone_selfevaluation_c33c %>%
  filter(record_id == "94")

print(miles)

d_mile <-app_data$milestone_workflow

td <- app_data$all_forms$test_data %>%
  filter(record_id == "94")

print(td)

# Check what columns are in acgme_miles
names(app_data$milestone_workflow$acgme_miles_acgme_program$data)

# Or check the raw form
names(app_data$all_forms$acgme_miles)

# Look at a sample row
head(app_data$all_forms$acgme_miles[, 1:10])
