# ============================================================================
# COMPLETION CHECKLIST MODULE
# Displays what modules are complete/incomplete
# ============================================================================

#' Completion Checklist UI
#'
#' @param id Module namespace ID
#' @param show_details Logical, whether to show details column
#' @export
mod_completion_checklist_ui <- function(id, show_details = TRUE) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("checklist_display"))
  )
}

#' Completion Checklist Server
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive containing app data
#' @param record_id Reactive containing resident ID
#' @param period Reactive containing period number
#' @param show_details Logical, whether to show details
#' @export
mod_completion_checklist_server <- function(id, rdm_data, record_id, period, show_details = TRUE) {
  moduleServer(id, function(input, output, session) {

    # Fresh data fetch from REDCap for this record (to get recently submitted data)
    fresh_rdm_data <- reactive({
      req(record_id())

      # Fetch fresh data for this specific record
      tryCatch({
        result <- REDCapR::redcap_read(
          redcap_uri = app_config$redcap_url,
          token = app_config$rdm_token,
          records = record_id(),
          raw_or_label = "raw"
        )
        if (result$success && nrow(result$data) > 0) {
          # Organize into forms structure like rdm_data
          fresh_data <- result$data

          # Create all_forms structure
          all_forms <- list()

          # s_eval form
          s_eval_rows <- fresh_data %>% dplyr::filter(redcap_repeat_instrument == "s_eval")
          if (nrow(s_eval_rows) > 0) all_forms$s_eval <- s_eval_rows

          # ilp form
          ilp_rows <- fresh_data %>% dplyr::filter(redcap_repeat_instrument == "ilp")
          if (nrow(ilp_rows) > 0) all_forms$ilp <- ilp_rows

          # milestone form
          ms_rows <- fresh_data %>% dplyr::filter(redcap_repeat_instrument == "milestone_selfevaluation_c33c")
          if (nrow(ms_rows) > 0) all_forms$milestone_selfevaluation_c33c <- ms_rows

          # scholarship form
          schol_rows <- fresh_data %>% dplyr::filter(redcap_repeat_instrument == "scholarship")
          if (nrow(schol_rows) > 0) all_forms$scholarship <- schol_rows

          return(list(all_forms = all_forms))
        }
        return(NULL)
      }, error = function(e) NULL)
    })

    # Merged data - prefer fresh, fallback to rdm_data
    merged_data <- reactive({
      req(rdm_data())

      fresh <- fresh_rdm_data()
      if (!is.null(fresh) && !is.null(fresh$all_forms)) {
        # Merge fresh data into rdm_data structure
        merged <- rdm_data()

        # Replace forms with fresh versions if available
        for (form_name in names(fresh$all_forms)) {
          if (!is.null(fresh$all_forms[[form_name]])) {
            merged$all_forms[[form_name]] <- fresh$all_forms[[form_name]]
          }
        }

        return(merged)
      }

      # Fallback to original
      rdm_data()
    })

    # Extract period number from various formats - as a reactive
    period_num <- reactive({
      req(period())

      if (is.numeric(period())) {
        period()
      } else if (is.list(period()) && "period_number" %in% names(period())) {
        # period() is a list from active_period() with period_number field
        period()$period_number
      } else if (is.character(period())) {
        # period() is a period name string
        period_map <- c(
          "Entering Residency" = 7,
          "Mid Intern" = 1,
          "End Intern" = 2,
          "Mid PGY2" = 3,
          "End PGY2" = 4,
          "Mid PGY3" = 5,
          "Graduating" = 6
        )
        period_map[period()]
      } else {
        # Fallback - try to coerce to numeric
        as.numeric(period())
      }
    })

    # Get completion status (use merged data with fresh fetch)
    completion_status <- reactive({
      req(merged_data(), record_id(), period_num())

      get_period_completion_status(merged_data(), record_id(), period_num())
    })

    # Render checklist
    output$checklist_display <- renderUI({
      req(completion_status(), period_num())

      status_df <- completion_status()
      required_modules <- get_required_modules(period_num())

      # Calculate overall progress
      completed <- sum(status_df$complete)
      total <- nrow(status_df)
      percentage <- if (total > 0) round((completed / total) * 100) else 0

      # Required modules progress
      required_complete <- sum(status_df$complete & status_df$module_id %in% required_modules)
      required_total <- length(required_modules)

      tagList(
        # Progress bar
        div(
          class = "mb-4",
          h5("Overall Progress"),
          div(
            class = "progress",
            style = "height: 30px;",
            div(
              class = paste0("progress-bar ", if (percentage == 100) "bg-success" else "bg-info"),
              role = "progressbar",
              style = paste0("width: ", percentage, "%;"),
              `aria-valuenow` = percentage,
              `aria-valuemin` = "0",
              `aria-valuemax` = "100",
              tags$strong(paste0(completed, " of ", total, " modules (", percentage, "%)"))
            )
          ),
          tags$p(
            class = "mt-2 mb-0",
            icon("star", class = "text-warning"),
            " Required: ", required_complete, " of ", required_total, " complete"
          )
        ),

        # Checklist items
        div(
          class = "list-group",
          lapply(1:nrow(status_df), function(i) {
            row <- status_df[i, ]
            is_required <- row$module_id %in% required_modules

            icon_element <- if (row$complete) {
              icon("check-circle", class = "text-success")
            } else if (is_required) {
              icon("circle", class = "text-danger")
            } else {
              icon("circle", class = "text-muted")
            }

            div(
              class = paste("list-group-item d-flex justify-content-between align-items-center",
                           if (row$complete) "list-group-item-success" else ""),
              div(
                icon_element,
                " ",
                tags$strong(row$module_title),
                if (is_required) {
                  tags$span(
                    icon("star", class = "text-warning ms-2"),
                    title = "Required"
                  )
                }
              ),
              if (show_details) {
                tags$span(
                  class = if (row$complete) "text-success" else "text-muted",
                  tags$small(row$details)
                )
              }
            )
          })
        )
      )
    })

    # Return reactive completion data for parent module
    return(list(
      completion_status = completion_status,
      is_complete = reactive({
        req(completion_status())
        all(completion_status()$complete)
      }),
      required_complete = reactive({
        req(completion_status(), period_num())
        status_df <- completion_status()
        required_modules <- get_required_modules(period_num())
        all(status_df$complete[status_df$module_id %in% required_modules])
      }),
      percentage = reactive({
        req(completion_status())
        calculate_completion_percentage(completion_status())
      })
    ))
  })
}
