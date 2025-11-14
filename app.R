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

dict <- app_data$data_dict
pc2_fields <- dict %>% filter(grepl("^pc2_r", field_name))
pc2_fields %>% select(field_name, field_label, select_choices_or_calculations) %>% head(3)
