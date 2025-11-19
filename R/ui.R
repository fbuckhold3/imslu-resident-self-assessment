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
          class = "col-md-6",
          div(
            class = "card shadow",
            div(
              class = "card-body p-5 text-center",
              h2("Resident Self-Assessment Access"),  # CHANGED
              p(class = "text-muted mb-4", "Please enter your access code to begin"),
              textInput(
                "access_code_input",
                NULL,
                placeholder = "Enter access code",
                width = "100%"
              ),
              actionButton(
                "submit_code",
                "Submit",
                class = "btn-primary btn-lg w-100 mt-3"
              ),
              div(
                id = "access_code_error",
                class = "alert alert-danger mt-3",
                style = "display: none;",
                "Invalid access code. Please try again."
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
              p(class = "lead text-muted mb-4", "Welcome to your self-assessment review"),
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
          
          # Period debug (for testing)
          conditionalPanel(
            condition = "input.show_period_debug",
            div(
              class = "card mb-4 border-info",
              div(
                class = "card-header bg-info text-white",
                h5(class = "mb-0", icon("cog"), " Period Detection (Debug)")
              ),
              div(
                class = "card-body",
                verbatimTextOutput("period_debug_info")
              )
            ),
            div(
              class = "card mb-4 border-warning",
              div(
                class = "card-header bg-warning",
                h5(class = "mb-0", icon("wrench"), " Period Override (Testing)")
              ),
              div(
                class = "card-body",
                p(class = "text-muted small", "Override automatic period detection for testing different period flows"),
                selectInput(
                  "period_override",
                  "Force Period:",
                  choices = c(
                    "Use Auto-Detection" = "auto",
                    "Period 7: Entering Residency (July-Sept, Intern)" = "7",
                    "Period 1: Mid Intern (Oct-Jan, PGY1)" = "1",
                    "Period 2: End Intern (Feb-June, PGY1)" = "2",
                    "Period 3: Mid PGY2 (Oct-Jan, PGY2)" = "3",
                    "Period 4: End PGY2 (Feb-June, PGY2)" = "4",
                    "Period 5: Mid PGY3 (Oct-Jan, PGY3)" = "5",
                    "Period 6: Graduating (Feb-June, PGY3)" = "6"
                  ),
                  selected = "auto"
                ),
                div(
                  class = "alert alert-info small mb-0",
                  icon("info-circle", class = "me-2"),
                  "When override is active, the flow will show modules for the selected period."
                )
              )
            )
          ),
          checkboxInput("show_period_debug", "Show Period Debug Info", value = TRUE),

          # Completion Checklist
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-light",
              h4(class = "mb-0", icon("clipboard-check"), " Self-Assessment Progress")
            ),
            div(
              class = "card-body",
              p(class = "text-muted", "Track your progress through the self-assessment modules. Required modules are marked with a star (", icon("star", class = "text-warning"), ")."),
              mod_completion_checklist_ui("intro_checklist", show_details = TRUE)
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
              p(class = "lead", "Thank you for completing your self-assessment."),
              hr(),
              p("Your responses have been saved to REDCap."),
              p("Review your submitted data below.")
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