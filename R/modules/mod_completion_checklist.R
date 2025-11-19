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

    # Get completion status
    completion_status <- reactive({
      req(rdm_data(), record_id(), period())

      period_num <- if (is.numeric(period())) {
        period()
      } else {
        # Convert period name to number if needed
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
      }

      get_period_completion_status(rdm_data(), record_id(), period_num)
    })

    # Render checklist
    output$checklist_display <- renderUI({
      req(completion_status())

      status_df <- completion_status()
      required_modules <- get_required_modules(period())

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
        req(completion_status(), period())
        status_df <- completion_status()
        required_modules <- get_required_modules(period())
        all(status_df$complete[status_df$module_id %in% required_modules])
      }),
      percentage = reactive({
        req(completion_status())
        calculate_completion_percentage(completion_status())
      })
    ))
  })
}
