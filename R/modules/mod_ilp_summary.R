# ============================================================================
# ILP SUMMARY MODULE
# Displays comprehensive summary of submitted data (Individualized Learning Plan)
# ============================================================================

#' ILP Summary UI
#'
#' @param id Module namespace ID
#' @export
mod_ilp_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Your Individualized Learning Plan (ILP)", class = "mb-4"),
    p(class = "lead text-muted", "Summary of your submitted self-assessment data"),
    uiOutput(ns("ilp_summary_display"))
  )
}

#' ILP Summary Server
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive containing app data
#' @param record_id Reactive containing resident ID
#' @param period Reactive containing period number/name
#' @param data_dict Data dictionary
#' @param session_data Optional reactive containing session-submitted data (overrides rdm_data)
#' @export
mod_ilp_summary_server <- function(id, rdm_data, record_id, period, data_dict, session_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # Topic label mappings
    topic_labels <- c(
      "1" = "Abdominal pain", "2" = "Acid base", "3" = "ACS", "4" = "AKI",
      "5" = "AMS", "6" = "Anticoagulation", "7" = "Cirrhosis", "8" = "CHF",
      "9" = "Diabetes", "10" = "Dyspnea", "11" = "Electrolytes", "12" = "GI bleed",
      "13" = "Hypo/hyperNa", "14" = "MSK", "15" = "Onc emergencies", "16" = "Pancreatitis",
      "17" = "Pneumonia", "18" = "Shock", "19" = "SSTI", "20" = "Substance use",
      "21" = "Syncope", "22" = "Transfusion", "23" = "Other"
    )

    # Learning style label mappings
    learn_labels <- c(
      "1" = "Case discussion", "2" = "Small group", "3" = "Direct observation",
      "4" = "Simulation", "5" = "Standardized patients", "6" = "Lectures",
      "7" = "Mentoring", "8" = "Online modules", "9" = "Specific rotation",
      "10" = "Reading", "11" = "Self-directed", "12" = "Other"
    )

    # Get period number
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
        # Fallback
        as.numeric(period())
      }
    })

    # Fresh data fetch from REDCap for this record (to get recently submitted data)
    fresh_data <- reactive({
      req(record_id())

      # Fetch fresh data for this specific record
      tryCatch({
        result <- REDCapR::redcap_read(
          redcap_uri = app_config$redcap_url,
          token = app_config$rdm_token,
          records = record_id(),
          raw_or_label = "raw"
        )
        if (result$success) result$data else NULL
      }, error = function(e) NULL)
    })

    # Get all form data for this period - prefer fresh data, fallback to rdm_data
    s_eval_data <- reactive({
      req(record_id(), period_num())

      # Try fresh data first
      fresh <- fresh_data()
      if (!is.null(fresh) && nrow(fresh) > 0) {
        s_eval_rows <- fresh %>%
          dplyr::filter(
            redcap_repeat_instrument == "s_eval",
            redcap_repeat_instance == period_num()
          )
        if (nrow(s_eval_rows) > 0) return(dplyr::slice(s_eval_rows, 1))
      }

      # Fallback to original rdm_data
      if (is.null(rdm_data()$all_forms$s_eval)) return(NULL)

      rdm_data()$all_forms$s_eval %>%
        dplyr::filter(
          record_id == !!record_id(),
          redcap_repeat_instrument == "s_eval",
          redcap_repeat_instance == period_num()
        ) %>%
        dplyr::slice(1)
    })

    ilp_data <- reactive({
      req(record_id(), period_num())

      # Try fresh data first
      fresh <- fresh_data()
      if (!is.null(fresh) && nrow(fresh) > 0) {
        ilp_rows <- fresh %>%
          dplyr::filter(
            redcap_repeat_instrument == "ilp",
            redcap_repeat_instance == period_num()
          )
        if (nrow(ilp_rows) > 0) return(dplyr::slice(ilp_rows, 1))
      }

      # Fallback to original rdm_data
      if (is.null(rdm_data()$all_forms$ilp)) return(NULL)

      rdm_data()$all_forms$ilp %>%
        dplyr::filter(
          record_id == !!record_id(),
          redcap_repeat_instrument == "ilp",
          redcap_repeat_instance == period_num()
        ) %>%
        dplyr::slice(1)
    })

    milestone_data <- reactive({
      req(record_id(), period_num())

      # Try fresh data first
      fresh <- fresh_data()
      if (!is.null(fresh) && nrow(fresh) > 0) {
        ms_rows <- fresh %>%
          dplyr::filter(
            redcap_repeat_instrument == "milestone_selfevaluation_c33c",
            redcap_repeat_instance == period_num()
          )
        if (nrow(ms_rows) > 0) return(dplyr::slice(ms_rows, 1))
      }

      # Fallback to original rdm_data
      if (is.null(rdm_data()$all_forms$milestone_selfevaluation_c33c)) return(NULL)

      rdm_data()$all_forms$milestone_selfevaluation_c33c %>%
        dplyr::filter(
          record_id == !!record_id(),
          redcap_repeat_instrument == "milestone_selfevaluation_c33c",
          redcap_repeat_instance == period_num()
        ) %>%
        dplyr::slice(1)
    })

    scholarship_data <- reactive({
      req(rdm_data(), record_id())

      if (is.null(rdm_data()$all_forms$scholarship)) return(NULL)

      rdm_data()$all_forms$scholarship %>%
        dplyr::filter(record_id == !!record_id())
    })

    # Render summary
    output$ilp_summary_display <- renderUI({
      req(period_num())

      config <- get_period_structure(period_num())

      # Build summary sections based on what modules are in this period
      sections <- list()

      # Scholarship Section
      if ("scholarship" %in% config$modules) {
        schol <- scholarship_data()
        sections$scholarship <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-primary text-white",
            h5(class = "mb-0", icon("graduation-cap"), " Scholarship")
          ),
          div(
            class = "card-body",
            if (!is.null(schol) && nrow(schol) > 0) {
              tagList(
                p(tags$strong("Activities Recorded: "), nrow(schol)),
                tags$ul(
                  lapply(1:min(5, nrow(schol)), function(i) {
                    type <- switch(as.character(schol$schol_type[i]),
                      "1" = "QI Project",
                      "2" = "Patient Safety",
                      "3" = "Research",
                      "4" = "Presentation",
                      "5" = "Publication",
                      "6" = "Education",
                      "7" = "Committee",
                      "Activity"
                    )
                    tags$li(type)
                  })
                ),
                if (nrow(schol) > 5) {
                  p(class = "text-muted", "... and ", nrow(schol) - 5, " more")
                }
              )
            } else {
              p(class = "text-muted", "No scholarship activities recorded")
            }
          )
        )
      }

      # Career Planning Section
      if ("career_planning" %in% config$modules) {
        s_eval <- s_eval_data()
        sections$career <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-info text-white",
            h5(class = "mb-0", icon("briefcase-medical"), " Wellness & Career Planning")
          ),
          div(
            class = "card-body",
            if (!is.null(s_eval) && nrow(s_eval) > 0) {
              tagList(
                # Wellness
                if (!is.na(s_eval$s_e_well[1]) && nzchar(s_eval$s_e_well[1])) {
                  div(
                    tags$strong("Wellness:"),
                    p(s_eval$s_e_well[1])
                  )
                },
                # Career Path
                div(
                  tags$strong("Career Paths Selected:"),
                  {
                    career_fields <- grep("^s_e_career_path___", names(s_eval), value = TRUE)
                    selected <- career_fields[!is.na(s_eval[1, career_fields]) & s_eval[1, career_fields] == "1"]
                    if (length(selected) > 0) {
                      tags$ul(
                        lapply(selected, function(f) {
                          code <- sub("s_e_career_path___", "", f)
                          label <- switch(code,
                            "1" = "Primary Care",
                            "2" = "Subspecialty/Fellowship",
                            "3" = "Hospitalist",
                            "4" = "Clinical Research",
                            "5" = "Basic Science",
                            "6" = "Clinical Educator",
                            "7" = "Administration",
                            "8" = "Other",
                            "Unknown"
                          )
                          tags$li(label)
                        })
                      )
                    } else {
                      p(class = "text-muted", "None selected")
                    }
                  }
                )
              )
            } else {
              p(class = "text-muted", "Not completed")
            }
          )
        )
      }

      # Program Feedback Section
      if ("program_feedback" %in% config$modules) {
        s_eval <- s_eval_data()
        sections$feedback <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-success text-white",
            h5(class = "mb-0", icon("comments"), " Program Feedback")
          ),
          div(
            class = "card-body",
            if (!is.null(s_eval) && nrow(s_eval) > 0) {
              tagList(
                if (!is.na(s_eval$s_e_prog_plus[1]) && nzchar(s_eval$s_e_prog_plus[1])) {
                  div(
                    tags$strong("Plus (What's Working):"),
                    p(s_eval$s_e_prog_plus[1]),
                    hr()
                  )
                },
                if (!is.na(s_eval$s_e_prog_delta[1]) && nzchar(s_eval$s_e_prog_delta[1])) {
                  div(
                    tags$strong("Delta (What to Improve):"),
                    p(s_eval$s_e_prog_delta[1]),
                    hr()
                  )
                },
                if (!is.na(s_eval$s_e_progconf[1]) && nzchar(s_eval$s_e_progconf[1])) {
                  div(
                    tags$strong("Conference Feedback:"),
                    p(s_eval$s_e_progconf[1]),
                    hr()
                  )
                },
                if (!is.na(s_eval$s_e_progfeed[1]) && nzchar(s_eval$s_e_progfeed[1])) {
                  div(
                    tags$strong("Broader Issues:"),
                    p(s_eval$s_e_progfeed[1])
                  )
                }
              )
            } else {
              p(class = "text-muted", "No feedback provided")
            }
          )
        )
      }

      # Assessment Review Section (s_e_plus and s_e_delta - resident's reflection on feedback)
      if ("assessment_review" %in% config$modules) {
        s_eval <- s_eval_data()
        sections$assessment_review <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-secondary text-white",
            h5(class = "mb-0", icon("clipboard-check"), " Assessment Review")
          ),
          div(
            class = "card-body",
            if (!is.null(s_eval) && nrow(s_eval) > 0) {
              tagList(
                if (!is.na(s_eval$s_e_plus[1]) && nzchar(s_eval$s_e_plus[1])) {
                  div(
                    tags$strong("Reflections on Plus Feedback:"),
                    p(s_eval$s_e_plus[1]),
                    hr()
                  )
                },
                if (!is.na(s_eval$s_e_delta[1]) && nzchar(s_eval$s_e_delta[1])) {
                  div(
                    tags$strong("Reflections on Delta Feedback:"),
                    p(s_eval$s_e_delta[1])
                  )
                },
                if ((is.na(s_eval$s_e_plus[1]) || !nzchar(s_eval$s_e_plus[1])) &&
                    (is.na(s_eval$s_e_delta[1]) || !nzchar(s_eval$s_e_delta[1]))) {
                  p(class = "text-muted", "No reflections recorded")
                }
              )
            } else {
              p(class = "text-muted", "Not completed")
            }
          )
        )
      }

      # Learning Section
      if ("learning" %in% config$modules) {
        s_eval <- s_eval_data()
        sections$learning <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-warning",
            h5(class = "mb-0", icon("book"), " Learning & Development")
          ),
          div(
            class = "card-body",
            if (!is.null(s_eval) && nrow(s_eval) > 0) {
              tagList(
                div(
                  tags$strong("Topics for Development:"),
                  {
                    topic_fields <- grep("^s_e_topic_sel___", names(s_eval), value = TRUE)
                    selected <- topic_fields[!is.na(s_eval[1, topic_fields]) & s_eval[1, topic_fields] == "1"]
                    if (length(selected) > 0) {
                      tags$ul(
                        lapply(selected, function(f) {
                          code <- sub("s_e_topic_sel___", "", f)
                          label <- if (code %in% names(topic_labels)) topic_labels[code] else paste("Topic", code)
                          tags$li(label)
                        })
                      )
                    } else {
                      p(class = "text-muted", "None selected")
                    }
                  }
                ),
                hr(),
                div(
                  tags$strong("Desired Learning Experiences:"),
                  {
                    learn_fields <- grep("^s_e_learn_style___", names(s_eval), value = TRUE)
                    selected <- learn_fields[!is.na(s_eval[1, learn_fields]) & s_eval[1, learn_fields] == "1"]
                    if (length(selected) > 0) {
                      tags$ul(
                        lapply(selected, function(f) {
                          code <- sub("s_e_learn_style___", "", f)
                          label <- if (code %in% names(learn_labels)) learn_labels[code] else paste("Style", code)
                          tags$li(label)
                        })
                      )
                    } else {
                      p(class = "text-muted", "None selected")
                    }
                  }
                )
              )
            } else {
              p(class = "text-muted", "Not completed")
            }
          )
        )
      }

      # Milestone Section
      if ("milestone_self_eval" %in% config$modules) {
        milestone <- milestone_data()
        sections$milestone <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-danger text-white",
            h5(class = "mb-0", icon("tasks"), " Milestone Self-Evaluation")
          ),
          div(
            class = "card-body",
            if (!is.null(milestone) && nrow(milestone) > 0) {
              milestone_fields <- c(
                paste0("rep_pc", 1:6, "_self"),
                paste0("rep_mk", 1:3, "_self"),
                paste0("rep_sbp", 1:3, "_self"),
                paste0("rep_pbl", 1:2, "_self"),
                paste0("rep_prof", 1:4, "_self"),
                paste0("rep_ics", 1:3, "_self")
              )
              completed <- sum(!is.na(milestone[1, milestone_fields]))
              tagList(
                p(tags$strong("Milestones Rated: "), completed, " of ", length(milestone_fields)),
                p(class = "text-success", icon("check-circle"), " Self-evaluation complete")
              )
            } else {
              p(class = "text-muted", "Not completed")
            }
          )
        )
      }

      # ILP/Goals Section
      if ("ilp_generation" %in% config$modules) {
        ilp <- ilp_data()
        sections$ilp <- div(
          class = "card mb-3",
          div(
            class = "card-header bg-dark text-white",
            h5(class = "mb-0", icon("bullseye"), " Goals (ILP)")
          ),
          div(
            class = "card-body",
            if (!is.null(ilp) && nrow(ilp) > 0) {
              tagList(
                # PC/MK Goal
                if (!is.na(ilp$goal_pcmk[1])) {
                  div(
                    tags$strong("PC/MK Goal: "),
                    ilp$goal_pcmk[1],
                    if (!is.na(ilp$how_pcmk[1]) && nzchar(ilp$how_pcmk[1])) {
                      p(class = "ms-3", tags$em("How: "), ilp$how_pcmk[1])
                    },
                    hr()
                  )
                },
                # SBP/PBLI Goal
                if (!is.na(ilp$goal_sbppbl[1])) {
                  div(
                    tags$strong("SBP/PBLI Goal: "),
                    ilp$goal_sbppbl[1],
                    if (!is.na(ilp$how_sbppbl[1]) && nzchar(ilp$how_sbppbl[1])) {
                      p(class = "ms-3", tags$em("How: "), ilp$how_sbppbl[1])
                    },
                    hr()
                  )
                },
                # PROF/ICS Goal
                if (!is.na(ilp$goal_subcomp_profics[1])) {
                  div(
                    tags$strong("PROF/ICS Goal: "),
                    ilp$goal_subcomp_profics[1],
                    if (!is.na(ilp$how_profics[1]) && nzchar(ilp$how_profics[1])) {
                      p(class = "ms-3", tags$em("How: "), ilp$how_profics[1])
                    }
                  )
                }
              )
            } else {
              p(class = "text-muted", "No goals set")
            }
          )
        )
      }

      # Return all sections
      tagList(sections)
    })
  })
}
