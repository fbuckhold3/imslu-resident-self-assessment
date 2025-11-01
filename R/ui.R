# ============================================================================
# USER INTERFACE - DYNAMIC PERIOD-BASED FLOW
# RDM 2.0 Resident Self-Assessment
# ============================================================================

ui <- page_navbar(
  title = "Resident Portfolio Review",
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  
  # Access Code Page
  nav_panel(
    title = NULL,
    value = "access",
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
              h2("Resident Portfolio Access"),
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
              p(class = "lead text-muted mb-4", "Welcome to your portfolio review"),
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
          
          # Navigation
          div(
            class = "d-flex justify-content-end mt-4",
            actionButton(
              "nav_intro_next",
              "Begin Portfolio Review",
              class = "btn-primary btn-lg",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
    # SINGLE DYNAMIC CONTENT PAGE
  nav_panel(
    title = NULL,
    value = "portfolio",
    uiOutput("portfolio_content")  # Server renders everything
  ),
  
  # Completion Page
  nav_panel(
    title = NULL,
    value = "complete",
    div(
      class = "container py-5",
      div(
        class = "row justify-content-center",
        div(
          class = "col-md-8 text-center",
          div(
            class = "card shadow-lg",
            div(
              class = "card-body p-5",
              icon("check-circle", class = "fa-4x text-success mb-4"),
              h2("Portfolio Review Complete!"),
              p(class = "lead", "Thank you for completing your portfolio review."),
              hr(),
              p("Your responses have been saved to REDCap."),
              p("You can close this window or return to the beginning."),
              div(
                class = "mt-4",
                actionButton(
                  "return_to_start",
                  "Return to Start",
                  class = "btn-primary btn-lg",
                  icon = icon("home")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Progress indicator
  nav_spacer(),
  nav_item(
    uiOutput("progress_indicator")
  )
)