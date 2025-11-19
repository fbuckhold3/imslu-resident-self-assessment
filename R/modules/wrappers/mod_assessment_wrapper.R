#' Assessment Review Wrapper Module UI

#'

#' Wrapper for all assessment visualizations

#' @param id Module namespace

#' @export

mod_assessment_wrapper_ui <- function(id) {

  ns <- NS(id)

  tagList(
  # Call individual gmed modules in correct order, using custom detail viz

  # Assessment progress charts
  gmed::assessment_viz_ui(ns("charts"), title = "Assessment Progress"),
  
  # Custom detail viz from gmed (replaces default detail viz)
  gmed::mod_assessment_detail_custom_ui(ns("custom_detail")),
  
  # Custom data display for selected evaluation (NEW!)
  gmed::mod_assessment_data_display_ui(ns("data_display")),
  
  # CC Completion Status
  gmed::mod_cc_completion_ui(ns("cc_completion")),
  
  # Questions/conference attendance
  gmed::mod_questions_viz_ui(ns("questions"), title = "Conference Attendance by Rotation"),
  
  # Plus/Delta feedback table
  gmed::mod_plus_delta_table_ui(ns("plus_delta"), title = "Plus / Delta Feedback")
)

}

 

#' Assessment Review Wrapper Module Server

#'

#' @param id Module namespace

#' @param rdm_data Reactive returning app data structure from load_rdm_complete

#' @param record_id Reactive returning resident record_id

#' @param period Reactive returning period number (not used but kept for consistency)

#' @param data_dict Data dictionary

#' @export

mod_assessment_wrapper_server <- function(id, rdm_data, record_id, period, data_dict) {

  moduleServer(id, function(input, output, session) {

 

    # Get resident name separately

    resident_name <- reactive({

      req(rdm_data(), record_id())

 

      app_data <- rdm_data()

 

      # Get name from residents table

      resident_info <- app_data$residents %>%

        dplyr::filter(record_id == !!record_id()) %>%

        dplyr::slice(1)

 

      if (nrow(resident_info) > 0 && !is.na(resident_info$name)) {

        return(resident_info$name)

      } else {

        return(paste("Resident", record_id()))

      }

    })

 

    # Get resident data for CC completion

    resident_info_data <- reactive({

      req(rdm_data(), record_id())

 

      app_data <- rdm_data()

 

      app_data$residents %>%

        dplyr::filter(record_id == !!record_id()) %>%

        dplyr::slice(1)

    })

 

    # Get raw assessment data for plus/delta table

    raw_assessment_data <- reactive({

      req(rdm_data())

      app_data <- rdm_data()

 

      if ("assessment" %in% names(app_data$all_forms)) {

        return(app_data$all_forms$assessment)

      } else {

        return(data.frame())

      }

    })

 

    # Prepare combined assessment + questions + faculty evaluation data

    combined_data <- reactive({

      req(rdm_data())

 

      app_data <- rdm_data()

 

      # Add source_form while preserving redcap_repeat_instrument

      if ("faculty_evaluation" %in% names(app_data$all_forms)) {

        combined <- dplyr::bind_rows(

          app_data$all_forms$assessment %>% dplyr::mutate(source_form = "assessment"),

          app_data$all_forms$questions %>% dplyr::mutate(source_form = "questions"),

          app_data$all_forms$faculty_evaluation %>% dplyr::mutate(source_form = "faculty_evaluation")

        )

      } else {

        combined <- dplyr::bind_rows(

          app_data$all_forms$assessment %>% dplyr::mutate(source_form = "assessment"),

          app_data$all_forms$questions %>% dplyr::mutate(source_form = "questions")

        )

      }

 

      return(combined)

    })

 

    # Call individual gmed modules in correct order

 

    # Assessment charts

    gmed::assessment_viz_server(

      "charts",

      data = combined_data,

      record_id = record_id,

      resident_name = resident_name

    )

 

# Custom detail viz from gmed - returns reactive values for data display
detail_viz_state <- gmed::mod_assessment_detail_custom_server(
  "custom_detail",
  rdm_data = combined_data,
  record_id = record_id,
  data_dict = data_dict
)

# Custom data display for selected evaluation (NEW!)
gmed::mod_assessment_data_display_server(
  "data_display",
  selected_category = detail_viz_state$selected_category,
  category_data = detail_viz_state$category_data,
  data_dict = data_dict
)

 

    # CC Completion Status

    gmed::mod_cc_completion_server(

      "cc_completion",

      rdm_data = combined_data,

      record_id = record_id,

      resident_data = resident_info_data

    )

 

    # Questions/conference attendance

    gmed::mod_questions_viz_server(

      "questions",

      rdm_data = combined_data,

      record_id = record_id,

      data_dict = data_dict

    )

 

    # Plus/Delta table

    gmed::mod_plus_delta_table_server(

      "plus_delta",

      rdm_data = raw_assessment_data,

      record_id = record_id

    )

  })

}