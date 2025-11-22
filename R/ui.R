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
                  class = "btn-primary btn-lg w-100 mt-3",
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
          class = "col-lg-10 offset-lg-1",
          
          # Welcome header
          div(
            class = "card mb-4 shadow-sm",
            div(
              class = "card-body",
              h2(
                icon("user-md", class = "me-2"),
                uiOutput("resident_name_display_intro", inline = TRUE)
              ),
              p(
                class = "lead text-muted mb-2",
                "Below is your training information and current period. Please review and verify this information before beginning your self-assessment."
              ),
              div(
                class = "alert alert-info",
                icon("info-circle", class = "me-2"),
                "Use the checklist on the right to track your progress through each section."
              ),
              hr(),
              
              # Resident info grid
              div(
                class = "row g-3",
                div(
                  class = "col-md-4",
                  div(
                    class = "card h-100 border-primary",
                    div(
                      class = "card-header bg-primary text-white",
                      h5(class = "mb-0", icon("graduation-cap"), " Training Information")
                    ),
                    div(
                      class = "card-body",
                      div(class = "mb-3",
                          strong("Current Level:"), br(),
                          tags$span(class = "badge bg-info fs-6", 
                                   textOutput("resident_level_display_intro", inline = TRUE))
                      ),
                      div(class = "mb-3",
                          strong("Program Type:"), br(),
                          tags$span(class = "badge bg-secondary fs-6",
                                   textOutput("resident_track_display_intro", inline = TRUE))
                      ),
                      div(class = "mb-3",
                          strong("Expected Graduation:"), br(),
                          textOutput("resident_grad_year_display_intro", inline = TRUE)
                      ),
                      div(class = "mb-0",
                          strong("Academic Year:"), br(),
                          textOutput("resident_academic_year_display_intro", inline = TRUE)
                      )
                    )
                  )
                ),
                div(
                  class = "col-md-4",
                  div(
                    class = "card h-100 border-success",
                    div(
                      class = "card-header bg-success text-white",
                      h5(class = "mb-0", icon("calendar-check"), " Current Period")
                    ),
                    div(
                      class = "card-body",
                      div(class = "mb-3",
                          strong("Evaluation Period:"), br(),
                          tags$span(class = "badge bg-success fs-5",
                                   textOutput("resident_period_display_intro", inline = TRUE))
                      ),
                      div(
                        class = "alert alert-info mb-0",
                        icon("info-circle", class = "me-2"),
                        "This portfolio review reflects your current training period."
                      )
                    )
                  )
                ),
                div(
                  class = "col-md-4",
                  div(
                    class = "card h-100 border-info",
                    div(
                      class = "card-header bg-info text-white",
                      h5(class = "mb-0", icon("user-friends"), " Your Coach")
                    ),
                    div(
                      class = "card-body",
                      div(class = "mb-3",
                          strong("Coach:"), br(),
                          textOutput("resident_coach_display_intro", inline = TRUE)
                      ),
                      div(class = "mb-0",
                          strong("Contact:"), br(),
                          uiOutput("resident_coach_email_display_intro")
                      )
                    )
                  )
                )
              )
            )
          ),

          # Two-column layout: Resident info + Checklist
          div(
            class = "row g-4 mb-4",

            # Left column: Resident info cards (existing)
            div(
              class = "col-lg-6",
              h4(class = "mb-3", icon("user-circle"), " Your Information"),

              # Training info card (compact)
              div(
                class = "card mb-3 border-primary",
                div(
                  class = "card-header bg-primary text-white",
                  h6(class = "mb-0", icon("graduation-cap"), " Training")
                ),
                div(
                  class = "card-body py-2",
                  div(class = "row",
                    div(class = "col-6 mb-2",
                      tags$small(class = "text-muted", "Level:"), br(),
                      tags$span(class = "badge bg-info",
                               textOutput("resident_level_display_intro_compact", inline = TRUE))
                    ),
                    div(class = "col-6 mb-2",
                      tags$small(class = "text-muted", "Type:"), br(),
                      tags$span(class = "badge bg-secondary",
                               textOutput("resident_track_display_intro_compact", inline = TRUE))
                    ),
                    div(class = "col-6",
                      tags$small(class = "text-muted", "Graduation:"), br(),
                      tags$small(textOutput("resident_grad_year_display_intro_compact", inline = TRUE))
                    ),
                    div(class = "col-6",
                      tags$small(class = "text-muted", "Academic Year:"), br(),
                      tags$small(textOutput("resident_academic_year_display_intro_compact", inline = TRUE))
                    )
                  )
                )
              ),

              # Period info card (compact)
              div(
                class = "card mb-3 border-success",
                div(
                  class = "card-header bg-success text-white",
                  h6(class = "mb-0", icon("calendar-check"), " Current Period")
                ),
                div(
                  class = "card-body",
                  div(class = "text-center",
                    tags$span(class = "badge bg-success fs-5",
                             textOutput("resident_period_display_intro_compact", inline = TRUE))
                  ),
                  p(class = "text-muted small mb-0 mt-2 text-center",
                    "This assessment covers your current training period."
                  )
                )
              ),

              # Coach info card (compact)
              div(
                class = "card border-info",
                div(
                  class = "card-header bg-info text-white",
                  h6(class = "mb-0", icon("user-friends"), " Your Coach")
                ),
                div(
                  class = "card-body py-2",
                  div(class = "mb-2",
                    tags$small(class = "text-muted", "Name:"), br(),
                    textOutput("resident_coach_display_intro_compact", inline = TRUE)
                  ),
                  div(class = "mb-0",
                    tags$small(class = "text-muted", "Contact:"), br(),
                    uiOutput("resident_coach_email_display_intro_compact")
                  )
                )
              )
            ),

            # Right column: Completion checklist
            div(
              class = "col-lg-6",
              div(
                class = "card h-100",
                div(
                  class = "card-header bg-light",
                  h4(class = "mb-0", icon("clipboard-check"), " Self-Assessment Progress")
                ),
                div(
                  class = "card-body",
                  div(
                    class = "alert alert-info mb-3",
                    icon("info-circle", class = "me-2"),
                    tags$strong("How to use this checklist:"),
                    tags$ul(
                      class = "mb-0 mt-2",
                      tags$li(tags$strong(icon("star", class = "text-warning"), " Required"), " - Must be completed before final submission"),
                      tags$li(icon("check-circle", class = "text-success"), " Complete - You've already submitted data"),
                      tags$li(icon("circle", class = "text-danger"), " Incomplete - Required but not yet completed"),
                      tags$li(icon("circle", class = "text-muted"), " Optional - Not required but available")
                    )
                  ),
                  mod_completion_checklist_ui("intro_checklist", show_details = TRUE)
                )
              )
            )
          ),

          # Navigation
          div(
            class = "d-flex justify-content-end mt-4",
            actionButton(
  "nav_intro_next",
  "Begin Self-Assessment",  # CHANGED
  class = "btn-primary btn-lg",
  icon = icon("arrow-right")
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