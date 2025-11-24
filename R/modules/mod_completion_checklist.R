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
    # Prominent warning about final submission requirement
    div(
      class = "alert alert-warning mb-4",
      style = "border-left: 5px solid #ff9800; background-color: #fff3e0;",
      h4(
        class = "alert-heading",
        icon("exclamation-triangle"),
        " Important: Final Submission Required"
      ),
      p(
        class = "mb-0",
        tags$strong("Your self-assessment is NOT complete until you click the Final Submit button below."),
        br(),
        "Please complete all required sections (marked with ",
        icon("star", class = "text-warning"),
        "), then click the Final Submit button at the bottom of this page to officially submit your evaluation."
      )
    ),

    uiOutput(ns("checklist_display")),

    # Final submission section
    uiOutput(ns("final_submission_section"))
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

    # Check if already submitted
    is_already_submitted <- reactive({
      req(merged_data(), record_id(), period_num())

      if (is.null(merged_data()$all_forms$s_eval)) {
        return(FALSE)
      }

      s_eval_data <- merged_data()$all_forms$s_eval %>%
        dplyr::filter(
          record_id == !!record_id(),
          redcap_repeat_instrument == "s_eval",
          redcap_repeat_instance == period_num()
        )

      if (nrow(s_eval_data) == 0) {
        return(FALSE)
      }

      # Check if s_eval_complete is marked as complete (2)
      !is.na(s_eval_data$s_eval_complete[1]) && s_eval_data$s_eval_complete[1] == "2"
    })

    # Render final submission section
    output$final_submission_section <- renderUI({
      req(completion_status(), period_num())

      status_df <- completion_status()
      required_modules <- get_required_modules(period_num())

      required_status <- status_df %>%
        dplyr::filter(module_id %in% required_modules)

      all_required_complete <- all(required_status$complete)
      already_submitted <- is_already_submitted()

      if (already_submitted) {
        # Show submission confirmation
        div(
          class = "alert alert-success mt-4",
          style = "border-left: 5px solid #4caf50;",
          h4(
            class = "alert-heading",
            icon("check-circle"),
            " Self-Assessment Submitted"
          ),
          p(
            "Your self-assessment was successfully submitted on ",
            tags$strong(format(Sys.Date(), "%B %d, %Y")),
            "."
          ),
          p(
            class = "mb-0",
            "Your coach will review your submission. Thank you for completing your self-assessment!"
          )
        )
      } else if (all_required_complete) {
        # Show final submit button
        div(
          class = "card mt-4",
          style = "border: 2px solid #4caf50; background-color: #f1f8e9;",
          div(
            class = "card-body text-center",
            h4(
              icon("check-circle", class = "text-success"),
              " All Required Sections Complete!"
            ),
            p(
              class = "mb-3",
              "You have completed all required sections. Click the button below to officially submit your self-assessment."
            ),
            p(
              class = "text-muted mb-3",
              tags$small(
                icon("info-circle"),
                " Clicking this button will mark your evaluation as complete and record the submission date in REDCap."
              )
            ),
            actionButton(
              ns("final_submit"),
              "Final Submit",
              icon = icon("paper-plane"),
              class = "btn btn-success btn-lg",
              style = "min-width: 200px;"
            )
          )
        )
      } else {
        # Show incomplete message
        incomplete_modules <- required_status %>%
          dplyr::filter(!complete) %>%
          dplyr::pull(module_title)

        div(
          class = "alert alert-info mt-4",
          h5(
            icon("info-circle"),
            " Complete Required Sections to Submit"
          ),
          p("The following required sections must be completed before final submission:"),
          tags$ul(
            lapply(incomplete_modules, function(mod) {
              tags$li(tags$strong(mod))
            })
          )
        )
      }
    })

    # Handle final submission
    observeEvent(input$final_submit, {
      req(record_id(), period_num())

      # Confirm submission
      showModal(modalDialog(
        title = "Confirm Final Submission",
        icon("question-circle", class = "text-warning fa-2x"),
        p(
          tags$strong("Are you sure you want to submit your self-assessment?")
        ),
        p("This will:"),
        tags$ul(
          tags$li("Mark your evaluation as complete"),
          tags$li("Record today's date as the submission date"),
          tags$li("Notify your coach that your assessment is ready for review")
        ),
        p(
          class = "text-muted",
          tags$small("Note: You can still make changes after submission if needed.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("confirm_final_submit"),
            "Yes, Submit",
            class = "btn btn-success",
            icon = icon("check")
          )
        )
      ))
    })

    # Confirmed final submission
    observeEvent(input$confirm_final_submit, {
      req(record_id(), period_num())

      # Submit to REDCap
      tryCatch({
        # Build submission data
        submission_data <- list(
          record_id = record_id(),
          redcap_repeat_instrument = "s_eval",
          redcap_repeat_instance = period_num(),
          s_e_date = format(Sys.Date(), "%Y-%m-%d"),
          s_eval_complete = "2"  # 2 = Complete in REDCap
        )

        # Convert to data frame for REDCap import
        import_df <- as.data.frame(submission_data, stringsAsFactors = FALSE)

        # Submit to REDCap
        result <- REDCapR::redcap_write(
          ds_to_write = import_df,
          redcap_uri = app_config$redcap_url,
          token = app_config$rdm_token
        )

        removeModal()

        if (result$success) {
          showModal(modalDialog(
            title = "Submission Successful!",
            icon("check-circle", class = "text-success fa-3x"),
            h4("Your self-assessment has been submitted."),
            p(
              "Submission date: ",
              tags$strong(format(Sys.Date(), "%B %d, %Y"))
            ),
            p("Your coach will be notified and will review your assessment."),
            p("Thank you for completing your self-assessment!"),
            footer = actionButton(
              session$ns("close_success"),
              "Close",
              class = "btn btn-primary"
            ),
            easyClose = FALSE
          ))

          # Trigger data refresh
          observeEvent(input$close_success, {
            removeModal()
            # Force refresh of merged_data by invalidating fresh_rdm_data
            session$reload()
          })

        } else {
          showModal(modalDialog(
            title = "Submission Error",
            icon("exclamation-triangle", class = "text-danger fa-2x"),
            p(
              tags$strong("There was an error submitting your self-assessment.")
            ),
            p("Error: ", result$status_message),
            p("Please try again or contact support if the problem persists."),
            footer = modalButton("Close")
          ))
        }

      }, error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Submission Error",
          icon("exclamation-triangle", class = "text-danger fa-2x"),
          p(
            tags$strong("An unexpected error occurred during submission.")
          ),
          p("Error: ", e$message),
          p("Please try again or contact support."),
          footer = modalButton("Close")
        ))
      })
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
      }),
      is_submitted = is_already_submitted
    ))
  })
}
