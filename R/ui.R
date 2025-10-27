# ============================================================================
# USER INTERFACE
# RDM 2.0 Resident Self-Assessment
# ============================================================================

ui <- fluidPage(
  
  # Theme and JavaScript
  theme = bslib::bs_theme(
    version = 5,
    primary = "#0072B2",
    secondary = "#56B4E9",
    success = "#009E73",
    warning = "#E69F00",
    danger = "#D55E00"
  ),
  
  useShinyjs(),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      /* Main styling */
      .main-header {
        background: linear-gradient(135deg, #0072B2 0%, #005a8c 100%);
        color: white;
        padding: 2rem;
        margin-bottom: 2rem;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .main-header h1 {
        margin: 0;
        font-size: 2rem;
        font-weight: 600;
      }
      
      .main-header p {
        margin: 0.5rem 0 0 0;
        font-size: 1.1rem;
        opacity: 0.95;
      }
      
      /* Access code screen */
      .access-code-container {
        max-width: 500px;
        margin: 4rem auto;
        padding: 2rem;
        background: white;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      /* Resident info banner */
      .resident-banner {
        background: #f8f9fa;
        border-left: 4px solid #0072B2;
        padding: 1rem 1.5rem;
        margin-bottom: 2rem;
        border-radius: 4px;
      }
      
      .resident-banner strong {
        color: #0072B2;
      }
      
      /* Quick link cards */
      .quick-link-card {
        background: white;
        border: 2px solid #e9ecef;
        border-radius: 8px;
        padding: 1.5rem;
        text-align: center;
        cursor: pointer;
        transition: all 0.3s ease;
        height: 150px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        margin-bottom: 1rem;
      }
      
      .quick-link-card:hover {
        border-color: #0072B2;
        box-shadow: 0 4px 12px rgba(0,114,178,0.2);
        transform: translateY(-2px);
      }
      
      .quick-link-card i {
        font-size: 2.5rem;
        color: #0072B2;
        margin-bottom: 0.5rem;
      }
      
      .quick-link-card h5 {
        margin: 0;
        color: #333;
        font-weight: 600;
      }
      
      /* Tab content area */
      .tab-content {
        padding: 2rem;
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        min-height: 400px;
      }
      
      /* Placeholder content */
      .placeholder-content {
        padding: 3rem;
        text-align: center;
        color: #6c757d;
      }
      
      .placeholder-content i {
        font-size: 3rem;
        margin-bottom: 1rem;
        opacity: 0.5;
      }
      
      /* Hide by default */
      .hidden-section {
        display: none;
      }
    "))
  ),
  
  # ============================================================================
  # HEADER
  # ============================================================================
  
  div(
    class = "main-header",
    h1(icon("user-graduate"), "SSM-SLUH Internal Medicine Resident Self-Assessment"),
    p("Track your progress, set goals, and view feedback")
  ),
  
  # ============================================================================
  # ACCESS CODE ENTRY (Shows when NOT authenticated)
  # ============================================================================
  
  div(
    id = "access_code_screen",
    class = "access-code-container",
    
    h3("Welcome!", class = "text-center mb-4"),
    p("Please enter your access code to continue.", 
      class = "text-center text-muted mb-4"),
    
    textInput(
      "access_code_input",
      label = NULL,
      placeholder = "Enter your access code",
      width = "100%"
    ),
    
    actionButton(
      "submit_code",
      "Access My Assessment",
      class = "btn-primary btn-lg w-100",
      icon = icon("arrow-right")
    ),
    
    div(
      id = "access_code_error",
      class = "alert alert-danger mt-3 hidden-section",
      icon("exclamation-triangle"),
      " Invalid access code. Please try again."
    )
  ),
  
  # ============================================================================
  # RESIDENT INFO BANNER (Shows when authenticated)
  # ============================================================================
  
  div(
    id = "resident_info_banner",
    class = "resident-banner hidden-section",
    
    fluidRow(
      column(3, tags$strong("Resident:"), textOutput("resident_name_display", inline = TRUE)),
      column(2, tags$strong("Level:"), textOutput("resident_level_display", inline = TRUE)),
      column(3, tags$strong("Current Period:"), textOutput("current_period_display", inline = TRUE)),
      column(4, 
             actionButton("logout_btn", "Logout", 
                          class = "btn-sm btn-outline-secondary float-end",
                          icon = icon("sign-out-alt"))
      )
    )
  ),
  
  # ============================================================================
  # QUICK LINKS / NAVIGATION CARDS (Shows when authenticated)
  # ============================================================================
  
  div(
    id = "quick_links_section",
    class = "hidden-section",
    
    h4("Quick Navigation", class = "mb-3"),
    
    fluidRow(
      column(3,
             div(
               class = "quick-link-card",
               onclick = "Shiny.setInputValue('nav_to_tab', 'plus_delta', {priority: 'event'});",
               icon("comments", class = "fa-solid"),
               h5("Plus / Delta", class = "mt-2"),
               p("View feedback", class = "text-muted small mb-0")
             )
      ),
      column(3,
             div(
               class = "quick-link-card",
               onclick = "Shiny.setInputValue('nav_to_tab', 'progress', {priority: 'event'});",
               icon("chart-line", class = "fa-solid"),
               h5("My Progress", class = "mt-2"),
               p("View charts", class = "text-muted small mb-0")
             )
      ),
      column(3,
             div(
               class = "quick-link-card",
               onclick = "Shiny.setInputValue('nav_to_tab', 'goals', {priority: 'event'});",
               icon("bullseye", class = "fa-solid"),
               h5("Learning Goals", class = "mt-2"),
               p("Set ILP goals", class = "text-muted small mb-0")
             )
      ),
      column(3,
             div(
               class = "quick-link-card",
               onclick = "Shiny.setInputValue('nav_to_tab', 'scholarship', {priority: 'event'});",
               icon("graduation-cap", class = "fa-solid"),
               h5("Scholarship", class = "mt-2"),
               p("Track projects", class = "text-muted small mb-0")
             )
      )
    )
  ),
  
  # ============================================================================
  # MAIN CONTENT TABS (Shows when authenticated)
  # ============================================================================
  
  div(
    id = "main_content_tabs",
    class = "hidden-section",
    
    navbarPage(
      title = NULL,
      id = "main_tabs",
      
      # TAB 1: Plus/Delta Feedback
tabPanel(
  "Plus / Delta",
  value = "plus_delta",
  icon = icon("comments"),
  
  div(
    class = "tab-content",
    h3("Plus / Delta Feedback", class = "mb-4"),
    
    # Actual gmed module
    gmed::mod_plus_delta_table_ui("resident_plus_delta")
  )
),
      
      # TAB 2: My Progress (Charts)
      tabPanel(
        "My Progress",
        value = "progress",
        icon = icon("chart-line"),
        
        div(
          class = "tab-content",
          h3("My Progress", class = "mb-4"),
          
          div(
            class = "placeholder-content",
            icon("chart-line", class = "fa-solid"),
            h4("Progress Visualization Module"),
            p("This will show milestone progression charts"),
            p(class = "text-muted", "Spider charts, trend lines, and comparisons")
          )
        )
      ),
      
      # TAB 3: Learning Goals (ILP)
      tabPanel(
        "Learning Goals",
        value = "goals",
        icon = icon("bullseye"),
        
        div(
          class = "tab-content",
          h3("Individualized Learning Plan", class = "mb-4"),
          
          div(
            class = "placeholder-content",
            icon("bullseye", class = "fa-solid"),
            h4("ILP Goals Module"),
            p("Set and track your learning goals"),
            p(class = "text-muted", "Based on your milestone assessments")
          )
        )
      ),
      
      # TAB 4: Scholarship
      tabPanel(
        "Scholarship",
        value = "scholarship",
        icon = icon("graduation-cap"),
        
        div(
          class = "tab-content",
          h3("Scholarship Activities", class = "mb-4"),
          
          div(
            class = "placeholder-content",
            icon("graduation-cap", class = "fa-solid"),
            h4("Scholarship Module"),
            p("Track QI projects, presentations, and publications"),
            p(class = "text-muted", "Document your scholarly activities")
          )
        )
      ),
      
      # TAB 5: Self-Assessment (Milestones)
      tabPanel(
        "Self-Assessment",
        value = "self_assessment",
        icon = icon("clipboard-check"),
        
        div(
          class = "tab-content",
          h3("Milestone Self-Assessment", class = "mb-4"),
          
          div(
            class = "placeholder-content",
            icon("clipboard-check", class = "fa-solid"),
            h4("Self-Assessment Entry Module"),
            p("Complete your milestone self-evaluation"),
            p(class = "text-muted", "Rate yourself across all competencies")
          )
        )
      )
    )
  )
)