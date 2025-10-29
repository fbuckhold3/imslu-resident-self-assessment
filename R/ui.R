# ============================================================================
# USER INTERFACE
# RDM 2.0 Resident Self-Assessment
# ============================================================================

ui <- page_navbar(
  title = "Resident Portfolio Review",
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  fillable = FALSE,
  
  # ============================================================================
  # HIDDEN NAVIGATION - CONTROLLED BY SERVER
  # ============================================================================
  
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
  
  # Introduction Page
  nav_panel(
    title = NULL,
    value = "intro",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          
          # Welcome header with comprehensive info
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
              
              # Resident info grid - 3 columns
              div(
                class = "row g-3",
                
                # Column 1: Training info
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
                
                # Column 2: Current evaluation period
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
                
                # Column 3: Coach information
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
          
          # Career goals section - using module
          div(
            class = "card mb-4 shadow-sm",
            div(
              class = "card-header bg-white border-bottom",
              h4(class = "mb-0", icon("bullseye", class = "me-2"), "Career Goals & Planning")
            ),
            div(
              class = "card-body",
              mod_career_goals_ui("career_goals", mode = "edit")
            )
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-end mt-4",
            actionButton(
              "nav_intro_next",
              "Continue to Scholarly Activities",
              class = "btn-primary btn-lg",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
  # Scholarly Activities Page
  nav_panel(
    title = NULL,
    value = "scholarly",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          
          h2("Scholarly Activities"),
          p(class = "lead text-muted", "Review and update your scholarly work"),
          hr(),
          
          # Current activities
          h4("Current Activities"),
          DT::dataTableOutput("scholarly_activities_table"),
          
          div(
            class = "mt-4",
            actionButton(
              "add_scholarly_activity",
              "Add New Activity",
              class = "btn-success",
              icon = icon("plus")
            )
          ),
          
          # Save status
          div(
            class = "mt-2",
            uiOutput("scholarly_save_status")
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-between mt-4 pt-3 border-top",
            actionButton(
              "nav_scholarly_prev",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            ),
            actionButton(
              "nav_scholarly_next",
              "Continue to Well-being",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
  # Well-being Page
  nav_panel(
    title = NULL,
    value = "wellbeing",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          h2("Well-being Check"),
          p(class = "lead text-muted", "How are you doing?"),
          hr(),
          
          # Well-being content placeholder
          div(
            class = "card",
            div(
              class = "card-body",
              p("Well-being assessment content will go here")
            )
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-between mt-4 pt-3 border-top",
            actionButton(
              "nav_wellbeing_prev",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            ),
            actionButton(
              "nav_wellbeing_next",
              "Continue to Evaluations",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
  # Plus/Delta Review Page
  nav_panel(
    title = NULL,
    value = "plusdelta",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          h2("Evaluation Feedback Review"),
          p(class = "lead text-muted", "Review your plus/delta feedback and reflect"),
          hr(),
          
          # Plus/Delta module
          mod_plus_delta_table_ui("plusdelta_review"),
          
          # Reflection input
          div(
            class = "card mt-4",
            div(
              class = "card-header bg-info text-white",
              h4(class = "mb-0", icon("lightbulb"), " Your Reflection")
            ),
            div(
              class = "card-body",
              textAreaInput(
                "plusdelta_reflection",
                "After reviewing this feedback, what are your thoughts?",
                rows = 6,
                width = "100%",
                placeholder = "Reflect on the feedback you've received..."
              )
            )
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-between mt-4 pt-3 border-top",
            actionButton(
              "nav_plusdelta_prev",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            ),
            actionButton(
              "nav_plusdelta_next",
              "Continue to Learning Data",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
  # Learning Data Page
  nav_panel(
    title = NULL,
    value = "learning",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          h2("Learning Data"),
          p(class = "lead text-muted", "Review your learning metrics"),
          hr(),
          
          # Learning data placeholder
          div(
            class = "card",
            div(
              class = "card-body",
              p("Learning data visualization will go here")
            )
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-between mt-4 pt-3 border-top",
            actionButton(
              "nav_learning_prev",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            ),
            actionButton(
              "nav_learning_next",
              "Continue to Milestones",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
  # Milestones Page
  nav_panel(
    title = NULL,
    value = "milestones",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          h2("Milestone Self-Assessment"),
          p(class = "lead text-muted", "Review and update your milestone progress"),
          hr(),
          
          # Milestone module placeholder
          div(
            class = "card",
            div(
              class = "card-body",
              p("Milestone assessment module will go here")
            )
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-between mt-4 pt-3 border-top",
            actionButton(
              "nav_milestones_prev",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            ),
            actionButton(
              "nav_milestones_next",
              "Continue to ILP",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
  ),
  
  # ILP Page
  nav_panel(
    title = NULL,
    value = "ilp",
    div(
      class = "container py-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 offset-lg-1",
          h2("Individualized Learning Plan"),
          p(class = "lead text-muted", "Document your learning goals"),
          hr(),
          
          # ILP content placeholder
          div(
            class = "card",
            div(
              class = "card-body",
              p("ILP module will go here")
            )
          ),
          
          # Navigation
          div(
            class = "d-flex justify-content-between mt-4 pt-3 border-top",
            actionButton(
              "nav_ilp_prev",
              "Previous",
              class = "btn-outline-secondary",
              icon = icon("arrow-left")
            ),
            actionButton(
              "nav_ilp_next",
              "Review & Submit",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          )
        )
      )
    )
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
  
  # Progress indicator (top right)
  nav_spacer(),
  nav_item(
    uiOutput("progress_indicator")
  )
)