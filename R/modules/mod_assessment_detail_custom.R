#' Custom Assessment Detail Visualization Module UI
#'
#' Dynamic assessment visualization with proper categorization and label decoding
#' @param id Module namespace
#' @export
mod_assessment_detail_custom_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
        .assessment-category-btn {
          margin: 5px;
          padding: 10px 15px;
          border-radius: 5px;
          transition: all 0.3s ease;
        }
        .assessment-category-btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        .assessment-category-btn.active {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-color: #667eea;
        }
        .assessment-viz-container {
          background: white;
          padding: 20px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-top: 20px;
        }
      "))
    ),

    div(
      class = "card mb-4",
      div(
        class = "card-header bg-primary text-white",
        h4(class = "mb-0",
           icon("chart-bar", class = "me-2"),
           "Detailed Assessment Analysis")
      ),
      div(
        class = "card-body",
        p(class = "text-muted mb-3",
          "View your assessment data organized by rotation type and evaluation category."),

        # Category buttons
        uiOutput(ns("category_buttons")),

        # Visualization output
        uiOutput(ns("viz_output"))
      )
    )
  )
}

#' Custom Assessment Detail Visualization Module Server
#'
#' @param id Module namespace
#' @param rdm_data Reactive containing assessment data
#' @param record_id Reactive containing resident record_id
#' @param data_dict Data dictionary
#' @export
mod_assessment_detail_custom_server <- function(id, rdm_data, record_id, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Extract assessment categories from data dictionary
    assessment_categories <- reactive({
      req(data_dict)

      # Get all assessment fields
      ass_fields <- data_dict %>%
        dplyr::filter(grepl("^ass_", field_name)) %>%
        dplyr::filter(!field_name %in% c("ass_date", "ass_level", "ass_plus",
                                         "ass_delta", "ass_faculty", "ass_specialty",
                                         "ass_rotator", "ass_cc_quart"))

      # Define category patterns
      categories <- list(
        list(
          key = "cc_inbasket",
          name = "CC: Inbasket Management",
          pattern = "^ass_cc_inb_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_documentation",
          name = "CC: Documentation",
          pattern = "^ass_cc_doc_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_intern_semi",
          name = "CC: Intern Semi-Annual",
          pattern = "^ass_cc_int_semi_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_pgy2_semi",
          name = "CC: PGY2 Semi-Annual",
          pattern = "^ass_cc_pgy2_semi_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_pgy3_semi",
          name = "CC: PGY3 Semi-Annual",
          pattern = "^ass_cc_pgy3_semi_",
          type = "numeric_scale"
        ),
        list(
          key = "day_assessments",
          name = "Day Assessments",
          pattern = "^ass_day_",
          type = "numeric_scale"
        ),
        list(
          key = "consults",
          name = "Consults",
          pattern = "^ass_cons_",
          type = "numeric_scale"
        ),
        list(
          key = "inpatient_intern",
          name = "Inpatient: Intern",
          pattern = "^ass_int_ip_",
          type = "numeric_scale"
        ),
        list(
          key = "inpatient_resident",
          name = "Inpatient: Resident",
          pattern = "^ass_res_ip_",
          type = "numeric_scale"
        ),
        list(
          key = "bridge",
          name = "Bridge Clinic",
          pattern = "^ass_bridge_",
          type = "numeric_scale"
        ),
        list(
          key = "observations",
          name = "Direct Observations",
          pattern = "^ass_obs_",
          type = "observation"
        )
      )

      # Populate field lists for each category
      categories_with_fields <- lapply(categories, function(cat) {
        cat$fields <- ass_fields %>%
          dplyr::filter(grepl(cat$pattern, field_name)) %>%
          dplyr::pull(field_name)

        cat$field_info <- ass_fields %>%
          dplyr::filter(grepl(cat$pattern, field_name))

        cat
      })

      # Only return categories that have fields
      categories_with_fields[sapply(categories_with_fields, function(x) length(x$fields) > 0)]
    })

    # Filter data for current resident
    resident_assessments <- reactive({
      req(rdm_data(), record_id())

      rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          tolower(redcap_repeat_instrument) == "assessment"
        )
    })

    # Track selected category
    selected_category <- reactiveVal(NULL)

    # Render category buttons with counts
    output$category_buttons <- renderUI({
      req(assessment_categories(), resident_assessments())

      cats <- assessment_categories()
      res_data <- resident_assessments()

      buttons <- lapply(cats, function(cat) {
        # Count distinct assessments with any data in this category
        count <- res_data %>%
          dplyr::select(dplyr::any_of(cat$fields)) %>%
          dplyr::filter(dplyr::if_any(dplyr::everything(),
                                     ~!is.na(.) & as.character(.) != "" & as.character(.) != "0")) %>%
          nrow()

        # Create button
        button_class <- "btn assessment-category-btn"
        if (!is.null(selected_category()) && selected_category() == cat$key) {
          button_class <- paste(button_class, "active btn-primary")
        } else {
          button_class <- paste(button_class, "btn-outline-primary")
        }

        actionButton(
          ns(paste0("cat_", cat$key)),
          paste0(cat$name, " (", count, ")"),
          class = button_class
        )
      })

      div(
        class = "d-flex flex-wrap",
        buttons
      )
    })

    # Handle category button clicks
    observe({
      cats <- assessment_categories()
      if (is.null(cats)) return()

      lapply(cats, function(cat) {
        observeEvent(input[[paste0("cat_", cat$key)]], {
          selected_category(cat$key)
        })
      })
    })

    # Render visualization for selected category
    output$viz_output <- renderUI({
      req(selected_category(), assessment_categories(), resident_assessments())

      cat_key <- selected_category()
      cats <- assessment_categories()
      cat_info <- cats[[which(sapply(cats, function(x) x$key == cat_key))]]
      res_data <- resident_assessments()

      # Filter to assessments with data in this category
      category_data <- res_data %>%
        dplyr::select(record_id, ass_date, ass_faculty, dplyr::any_of(cat_info$fields)) %>%
        dplyr::filter(dplyr::if_any(dplyr::all_of(cat_info$fields),
                                   ~!is.na(.) & as.character(.) != "" & as.character(.) != "0"))

      if (nrow(category_data) == 0) {
        return(
          div(
            class = "alert alert-info assessment-viz-container",
            icon("info-circle", class = "me-2"),
            strong("No data available for this category yet."),
            br(),
            sprintf("You have %d total assessments, but none have data in the %s fields.",
                   nrow(res_data), cat_info$name)
          )
        )
      }

      # Create visualization based on category type
      if (cat_info$type == "numeric_scale") {
        create_scale_visualization(category_data, cat_info, ns)
      } else if (cat_info$type == "observation") {
        create_observation_visualization(category_data, cat_info, ns)
      } else {
        div(
          class = "alert alert-warning assessment-viz-container",
          icon("wrench", class = "me-2"),
          sprintf("Visualization for %s is under development.", cat_info$name)
        )
      }
    })
  })
}

#' Create scale visualization
#' @keywords internal
create_scale_visualization <- function(data, cat_info, ns) {

  # Pivot data to long format
  viz_data <- data %>%
    dplyr::select(record_id, ass_date, ass_faculty, dplyr::all_of(cat_info$fields)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cat_info$fields),
      names_to = "field",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value), value != "", value != "0")

  if (nrow(viz_data) == 0) {
    return(
      div(
        class = "alert alert-warning assessment-viz-container",
        icon("info-circle", class = "me-2"),
        "No valid data found for visualization."
      )
    )
  }

  # Get field labels from cat_info
  field_labels <- setNames(
    cat_info$field_info$field_label,
    cat_info$field_info$field_name
  )

  # Add readable field names
  viz_data <- viz_data %>%
    dplyr::mutate(
      field_label = field_labels[field],
      # Try to convert to numeric for summarization
      value_numeric = suppressWarnings(as.numeric(value))
    )

  # Create summary by field
  summary_data <- viz_data %>%
    dplyr::filter(!is.na(value_numeric)) %>%
    dplyr::group_by(field_label) %>%
    dplyr::summarise(
      mean_score = mean(value_numeric, na.rm = TRUE),
      median_score = median(value_numeric, na.rm = TRUE),
      n_assessments = dplyr::n_distinct(record_id, ass_date),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(mean_score))

  # Create plotly chart if we have numeric data
  if (nrow(summary_data) > 0) {
    plot_obj <- plotly::plot_ly(
      data = summary_data,
      x = ~mean_score,
      y = ~reorder(field_label, mean_score),
      type = "bar",
      orientation = "h",
      marker = list(color = "#667eea"),
      text = ~paste0("Mean: ", round(mean_score, 2),
                    "<br>Median: ", round(median_score, 2),
                    "<br>N: ", n_assessments),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = list(text = paste("Average Scores -", cat_info$name), x = 0),
        xaxis = list(title = "Average Score"),
        yaxis = list(title = ""),
        margin = list(l = 250)
      )

    chart_output <- plotly::renderPlotly({ plot_obj })
  } else {
    chart_output <- NULL
  }

  # Create data table
  display_data <- viz_data %>%
    dplyr::select(ass_date, ass_faculty, field_label, value) %>%
    dplyr::arrange(desc(ass_date))

  table_output <- DT::renderDT({
    DT::datatable(
      display_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      colnames = c("Date", "Faculty", "Assessment Item", "Rating")
    )
  })

  # Return combined UI
  div(
    class = "assessment-viz-container",
    h5(paste("Total assessments in this category:", nrow(data))),

    if (!is.null(chart_output)) {
      tagList(
        h6(class = "mt-3", "Summary Chart"),
        plotly::plotlyOutput(ns("category_plot"), height = "400px")
      )
    },

    h6(class = "mt-4", "Detailed Data"),
    DT::DTOutput(ns("category_table"))
  )
}

#' Create observation visualization
#' @keywords internal
create_observation_visualization <- function(data, cat_info, ns) {
  div(
    class = "alert alert-info assessment-viz-container",
    icon("info-circle", class = "me-2"),
    "Observation visualization coming soon. For now, use the detailed data table.",
    br(), br(),
    sprintf("You have %d observation assessments.", nrow(data))
  )
}
