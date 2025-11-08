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

app_data <- complete_data

# Check what plus_delta is receiving
app_data$all_forms$assessment %>%
  filter(record_id == "94") %>%
  filter(!is.na(redcap_repeat_instrument) & 
         redcap_repeat_instrument == "Assessment") %>%
  nrow()

app_data$all_forms$assessment %>%
  filter(record_id == "94") %>%
  filter(!is.na(redcap_repeat_instrument) & 
         redcap_repeat_instrument == "assessment") %>%
  nrow()


# Check the instrument name
app_data$all_forms$assessment %>%
  filter(record_id == "94") %>%
  count(redcap_repeat_instrument)

app_data$all_forms$questions %>%
  filter(record_id == "94") %>%
  select(record_id, redcap_repeat_instrument, q_date, q_rotation) %>%
  head(10)
