# ============================================================================
# USER INTERFACE - DYNAMIC PERIOD-BASED FLOW
# RDM 2.0 Resident Self-Assessment
# ============================================================================

ui <- page_navbar(
  title = "IMSLU Resident Self-Assessment",
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,

  # Header elements (not inside nav_panel)
  header = tagList(
    # Enable shinyjs for loading indicators
    shinyjs::useShinyjs(),

    # Load gmed styles
    gmed::load_gmed_styles(theme = "slucare"),

    # Add stepper CSS
    tags$head(
      tags$style(HTML("
        .stepper-container {
          display: flex;
          justify-content: space-between;
          align-items: center;
          position: relative;
        }
        .stepper-item {
          display: flex;
          flex-direction: column;
          align-items: center;
          flex: 1;
          position: relative;
          cursor: pointer;
          transition: all 0.3s ease;
        }
        .stepper-item:hover .stepper-circle {
          transform: scale(1.1);
        }
        .stepper-circle {
          width: 36px;
          height: 36px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: bold;
          font-size: 0.875rem;
          margin-bottom: 0.5rem;
          transition: all 0.3s ease;
          z-index: 2;
          position: relative;
        }
        .stepper-circle.active {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
        }
        .stepper-circle.completed {
          background: #10b981;
          color: white;
        }
        .stepper-circle.incomplete {
          background: #e5e7eb;
          color: #9ca3af;
          border: 2px solid #d1d5db;
        }
        .stepper-label {
          font-size: 0.75rem;
          text-align: center;
          max-width: 100px;
          line-height: 1.2;
          color: #6b7280;
          font-weight: 500;
        }
        .stepper-label.active {
          color: #667eea;
          font-weight: 600;
        }
        .stepper-line {
          position: absolute;
          top: 18px;
          left: 50%;
          right: -50%;
          height: 2px;
          background: #e5e7eb;
          z-index: 1;
        }
        .stepper-line.completed {
          background: #10b981;
        }
        .stepper-item:last-child .stepper-line {
          display: none;
        }

        /* GMED Card Styling */
        .gmed-card {
          background: #ffffff;
          border: 1px solid #e9ecef;
          box-shadow: 0 2px 8px rgba(0, 61, 92, 0.08);
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          animation: gmedFadeIn 0.3s ease-in;
        }

        .gmed-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(0, 61, 92, 0.12);
        }

        /* Card animations */
        @keyframes gmedFadeIn {
          from {
            opacity: 0;
            transform: translateY(10px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }

        /* Enhanced badge styling for gmed colors */
        .gmed-badge-primary {
          background: linear-gradient(135deg, #003d5c, #0066a1);
          color: white;
          padding: 0.5rem 1rem;
          border-radius: 8px;
          font-weight: 600;
          letter-spacing: 0.5px;
        }

        .gmed-badge-success {
          background: linear-gradient(135deg, #00a651, #28a745);
          color: white;
          padding: 0.5rem 1rem;
          border-radius: 8px;
          font-weight: 600;
          letter-spacing: 0.5px;
        }

        .gmed-badge-info {
          background: linear-gradient(135deg, #2196f3, #4a90a4);
          color: white;
          padding: 0.5rem 1rem;
          border-radius: 8px;
          font-weight: 600;
          letter-spacing: 0.5px;
        }

        .gmed-badge-warning {
          background: linear-gradient(135deg, #ff8c00, #ffa500);
          color: white;
          padding: 0.5rem 1rem;
          border-radius: 8px;
          font-weight: 600;
          letter-spacing: 0.5px;
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
          .gmed-card {
            margin-bottom: 1rem;
          }

          .gmed-card .card-body {
            padding: 1rem !important;
          }
        }
      "))
    )
  ),

  # Access Code Page
  nav_panel(
    title = NULL,
    value = "access",


    # Loading overlay (shown on startup)
    div(
      id = "loading_overlay",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%;
              background: rgba(255,255,255,0.95); z-index: 9999;
              display: flex; align-items: center; justify-content: center;",
      div(
        class = "text-center",
        div(
          class = "spinner-border text-primary mb-3",
          style = "width: 4rem; height: 4rem;",
          role = "status"
        ),
        h3("Loading Application..."),
        p(class = "text-muted", id = "loading_message", "Initializing...")
      )
    ),

    div(
      class = "container mt-5",
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-8 col-md-10",
          div(
            class = "card shadow-lg",
            div(
              class = "card-body p-5",
              # Welcome header
              div(
                class = "text-center mb-4",
                h2(class = "mb-3", icon("user-md", class = "me-2"), "Welcome to the IMSLU Resident Self-Assessment"),
                hr()
              ),

              # Introduction text
              div(
                class = "mb-4",
                p(
                  class = "lead",
                  "Upon entering your access code, you will be asked a series of questions to help you reflect on your progress as a resident and to formulate a draft of your ",
                  tags$strong("Individualized Learning Plan (ILP)"), "."
                ),
                p(
                  class = "text-muted",
                  icon("clock", class = "me-2"),
                  "Please allow yourself approximately 30 minutes to complete this assessment."
                ),
                p(
                  class = "text-muted",
                  icon("save", class = "me-2"),
                  "Please complete this before meeting with your resident coach so they have time to review your answers."
                ),
                p(
                  class = "text-muted mb-0",
                  icon("info-circle", class = "me-2"),
                  "If you do not finish, you can return to this at a later time."
                )
              ),

              # Access code input
              div(
                class = "mt-4",
                h5(class = "text-center mb-3", "Enter Your Access Code"),
                textInput(
                  "access_code_input",
                  NULL,
                  placeholder = "Enter access code",
                  width = "100%"
                ),
                actionButton(
                  "submit_code",
                  "Begin Assessment",
                  class = "btn-lg w-100 mt-3",
                  style = "background: linear-gradient(135deg, #003d5c, #0066a1); color: white; border: none; border-radius: 12px; font-weight: 600; letter-spacing: 0.5px; box-shadow: 0 4px 12px rgba(0, 61, 92, 0.3); transition: all 0.3s ease;",
                  icon = icon("arrow-right")
                ),
                div(
                  id = "access_code_error",
                  class = "alert alert-danger mt-3",
                  style = "display: none;",
                  "Invalid access code. Please try again."
                )
              ),

              # Disclaimer
              hr(class = "mt-4"),
              div(
                class = "text-center mt-3",
                p(
                  class = "small text-muted mb-0",
                  icon("lock", class = "me-1"),
                  tags$strong("Disclaimer:"), " This information is proprietary to the IMSLU Program at SSM Health - Saint Louis University. Authorized access only."
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Introduction Page (always shown)
  nav_panel(
    title = NULL,
    value = "intro",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-11 offset-lg-0",

          # Welcome header
          div(
            class = "mb-4 text-center",
            h2(
              class = "mb-2",
              style = "color: #003d5c;",
              icon("user-md", class = "me-2"),
              uiOutput("resident_name_display_intro", inline = TRUE)
            ),
            p(
              class = "text-muted",
              "Please review your information below and use the checklist to track your progress."
            )
          ),

          # Two-column layout: Resident info + Checklist
          div(
            class = "row g-4 mb-4",

            # Left column: Resident info cards
            div(
              class = "col-lg-5",

              # Training info card
              div(
                class = "gmed-card mb-3",
                style = "border-left: 4px solid #0066a1; border-radius: 12px;",
                div(
                  class = "card-body",
                  div(
                    class = "d-flex align-items-center mb-3",
                    div(
                      class = "me-3",
                      style = "width: 48px; height: 48px; background: linear-gradient(135deg, #003d5c, #0066a1); border-radius: 12px; display: flex; align-items: center; justify-content: center;",
                      icon("graduation-cap", class = "text-white fa-lg")
                    ),
                    div(
                      h5(class = "mb-0", style = "color: #003d5c; font-weight: 600;", "Training Information")
                    )
                  ),
                  div(class = "row g-3",
                    div(class = "col-6",
                      tags$small(class = "text-muted d-block mb-1", "Level"),
                      tags$span(
                        class = "badge",
                        style = "background: linear-gradient(135deg, #2196f3, #4a90a4); font-size: 0.875rem; padding: 0.5rem 0.75rem; border-radius: 8px;",
                        textOutput("resident_level_display_intro_compact", inline = TRUE)
                      )
                    ),
                    div(class = "col-6",
                      tags$small(class = "text-muted d-block mb-1", "Type"),
                      tags$span(
                        class = "badge bg-secondary",
                        style = "font-size: 0.875rem; padding: 0.5rem 0.75rem; border-radius: 8px;",
                        textOutput("resident_track_display_intro_compact", inline = TRUE)
                      )
                    ),
                    div(class = "col-6",
                      tags$small(class = "text-muted d-block mb-1", "Graduation"),
                      tags$div(style = "color: #2c3e50; font-weight: 500;",
                        textOutput("resident_grad_year_display_intro_compact", inline = TRUE)
                      )
                    ),
                    div(class = "col-6",
                      tags$small(class = "text-muted d-block mb-1", "Academic Year"),
                      tags$div(style = "color: #2c3e50; font-weight: 500;",
                        textOutput("resident_academic_year_display_intro_compact", inline = TRUE)
                      )
                    )
                  )
                )
              ),

              # Period info card
              div(
                class = "gmed-card mb-3",
                style = "border-left: 4px solid #00a651; border-radius: 12px;",
                div(
                  class = "card-body",
                  div(
                    class = "d-flex align-items-center mb-3",
                    div(
                      class = "me-3",
                      style = "width: 48px; height: 48px; background: linear-gradient(135deg, #00a651, #28a745); border-radius: 12px; display: flex; align-items: center; justify-content: center;",
                      icon("calendar-check", class = "text-white fa-lg")
                    ),
                    div(
                      h5(class = "mb-0", style = "color: #00a651; font-weight: 600;", "Current Period")
                    )
                  ),
                  div(class = "text-center py-2",
                    tags$span(
                      class = "badge",
                      style = "background: linear-gradient(135deg, #00a651, #28a745); font-size: 1.25rem; padding: 0.75rem 1.5rem; border-radius: 12px; letter-spacing: 0.5px;",
                      textOutput("resident_period_display_intro_compact", inline = TRUE)
                    )
                  ),
                  p(class = "text-muted small text-center mb-0 mt-2",
                    icon("info-circle", class = "me-1"),
                    "This assessment covers your current training period"
                  )
                )
              ),

              # Coach info card
              div(
                class = "gmed-card",
                style = "border-left: 4px solid #4a90a4; border-radius: 12px;",
                div(
                  class = "card-body",
                  div(
                    class = "d-flex align-items-center mb-3",
                    div(
                      class = "me-3",
                      style = "width: 48px; height: 48px; background: linear-gradient(135deg, #4a90a4, #2196f3); border-radius: 12px; display: flex; align-items: center; justify-content: center;",
                      icon("user-friends", class = "text-white fa-lg")
                    ),
                    div(
                      h5(class = "mb-0", style = "color: #4a90a4; font-weight: 600;", "Your Coach")
                    )
                  ),
                  div(
                    div(class = "mb-2",
                      tags$small(class = "text-muted d-block mb-1", "Name"),
                      tags$div(style = "color: #2c3e50; font-weight: 500;",
                        textOutput("resident_coach_display_intro_compact", inline = TRUE)
                      )
                    ),
                    div(
                      tags$small(class = "text-muted d-block mb-1", "Contact"),
                      uiOutput("resident_coach_email_display_intro_compact")
                    )
                  )
                )
              )
            ),

            # Right column: Completion checklist
            div(
              class = "col-lg-7",
              div(
                class = "gmed-card h-100",
                style = "border-radius: 16px; border-top: 8px solid #003d5c;",
                div(
                  class = "card-body p-4",
                  div(
                    class = "d-flex align-items-center mb-4",
                    div(
                      class = "me-3",
                      style = "width: 56px; height: 56px; background: linear-gradient(135deg, #003d5c, #0066a1); border-radius: 14px; display: flex; align-items: center; justify-content: center;",
                      icon("clipboard-check", class = "text-white", style = "font-size: 1.5rem;")
                    ),
                    div(
                      h4(class = "mb-0", style = "color: #003d5c; font-weight: 600;", "Self-Assessment Progress")
                    )
                  ),
                  div(
                    class = "alert mb-3",
                    style = "background: linear-gradient(135deg, #f8f9fa, #e9ecef); border-left: 4px solid #2196f3; border-radius: 8px;",
                    div(
                      class = "d-flex align-items-start",
                      icon("info-circle", class = "me-2 mt-1", style = "color: #2196f3;"),
                      div(
                        tags$strong("Checklist Guide:"),
                        tags$ul(
                          class = "mb-0 mt-2 small",
                          style = "list-style: none; padding-left: 0;",
                          tags$li(
                            class = "mb-1",
                            tags$span(
                              class = "badge me-2",
                              style = "background: linear-gradient(135deg, #ff8c00, #ffa500); font-size: 0.75rem;",
                              icon("star")
                            ),
                            tags$strong("Required"), " - Must complete before submission"
                          ),
                          tags$li(
                            class = "mb-1",
                            tags$span(
                              class = "badge me-2",
                              style = "background: linear-gradient(135deg, #00a651, #28a745); font-size: 0.75rem;",
                              icon("check-circle")
                            ),
                            "Complete - Already submitted"
                          ),
                          tags$li(
                            class = "mb-1",
                            tags$span(
                              class = "badge bg-danger me-2",
                              style = "font-size: 0.75rem;",
                              icon("circle")
                            ),
                            "Incomplete - Required but not done"
                          ),
                          tags$li(
                            tags$span(
                              class = "badge bg-secondary me-2",
                              style = "font-size: 0.75rem;",
                              icon("circle")
                            ),
                            "Optional - Available but not required"
                          )
                        )
                      )
                    )
                  ),
                  mod_completion_checklist_ui("intro_checklist", show_details = TRUE)
                )
              )
            )
          ),

          # Navigation
          div(
            class = "d-flex justify-content-center mt-4",
            actionButton(
              "nav_intro_next",
              "Begin Self-Assessment",
              class = "btn-lg",
              style = "background: linear-gradient(135deg, #003d5c, #0066a1); color: white; border: none; padding: 0.75rem 3rem; border-radius: 12px; font-weight: 600; letter-spacing: 0.5px; box-shadow: 0 4px 12px rgba(0, 61, 92, 0.3); transition: all 0.3s ease;",
              icon = icon("arrow-right"),
              onclick = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 6px 20px rgba(0, 61, 92, 0.4)'"
            )
          )
        )
      )
    )
  ),
  
    # SINGLE DYNAMIC CONTENT PAGE
  # Portfolio page with progress stepper
nav_panel(
  title = NULL,
  value = "portfolio",
  
  # Progress stepper header
  div(
    class = "bg-white border-bottom shadow-sm sticky-top",
    style = "z-index: 1000;",
    div(
      class = "container-fluid py-3",
      uiOutput("progress_stepper")
    )
  ),
  
  # Main content
  div(
    class = "container py-4",
    uiOutput("portfolio_content")
  )
),
  
  # Completion Page
  nav_panel(
    title = NULL,
    value = "complete",
    div(
      class = "container py-5",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",

          # Success message
          div(
            class = "card shadow-lg mb-4",
            div(
              class = "card-body p-5 text-center",
              icon("check-circle", class = "fa-4x text-success mb-4"),
              h2("Self-Assessment Complete!"),
              p(class = "lead mb-4", "Thank you for completing your self-assessment."),
              hr(),
              div(
                class = "text-start mt-4",
                p(
                  icon("save", class = "me-2 text-success"),
                  tags$strong("Your responses have been saved successfully.")
                ),
                p(
                  icon("clipboard-check", class = "me-2 text-primary"),
                  "Review your completion status and Individualized Learning Plan (ILP) below."
                ),
                p(
                  icon("user-friends", class = "me-2 text-info"),
                  class = "mb-0",
                  "Your coach will review your submission prior to your meeting."
                )
              )
            )
          ),

          # Completion checklist
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-success text-white",
              h4(class = "mb-0", icon("clipboard-check"), " Completion Status")
            ),
            div(
              class = "card-body",
              mod_completion_checklist_ui("completion_checklist", show_details = TRUE)
            )
          ),

          # ILP Summary
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4(class = "mb-0", icon("file-medical-alt"), " Your Individualized Learning Plan (ILP)")
            ),
            div(
              class = "card-body",
              mod_ilp_summary_ui("ilp_summary")
            )
          ),

          # Navigation
          div(
            class = "mt-4 text-center",
            actionButton(
              "return_to_start",
              "Return to Introduction",
              class = "btn-primary btn-lg",
              icon = icon("home")
            )
          )
        )
      )
    )
  )
)