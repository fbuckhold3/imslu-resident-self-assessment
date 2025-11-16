# IMSLU Resident Self-Assessment Application

**SSM-SLUH Internal Medicine Resident Self-Assessment System**  
Version: RDM 2.0  
Last Updated: November 2025

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Architecture](#architecture)
3. [Period Structure & Module Matrix](#period-structure--module-matrix)
4. [User Completion Checklist](#user-completion-checklist)
5. [REDCap Field Mappings](#redcap-field-mappings)
6. [Module Implementation Status](#module-implementation-status)
7. [Data Flow](#data-flow)
8. [Technical Dependencies](#technical-dependencies)
9. [Development Patterns](#development-patterns)

---

## System Overview

### Purpose
Comprehensive resident self-assessment application that transitions from legacy system (imres.self.assessment) to RDM 2.0 database structure. Supports period-based evaluations across 7 residency periods from entering residency through graduation.

### Stakeholders
- **Residents**: Complete self-assessments, track progress, set goals
- **Faculty/Coaches**: Review assessments, provide feedback
- **Program Administrators**: Monitor resident progress, aggregate data

### Key Features
- Dynamic period detection based on PGY year and date
- Modular architecture with reusable components
- Integration with ACGME milestone standards
- REDCap-based data storage with complex repeating instruments
- Interactive visualizations with national benchmark comparisons

---

## Architecture

### High-Level Design

```
Authentication (Access Code)
    ↓
Period Detection (Academic Year + PGY)
    ↓
Module Configuration (Registry-based)
    ↓
Dynamic Content Rendering (Period-specific modules)
```

### Repository Structure

**Main Application**: `imslu-resident-self-assessment`
- Shiny application entry point
- Period-specific workflows
- REDCap data entry forms

**Shared Package**: `gmed`
- Reusable visualization functions
- Data processing utilities
- REDCap API integration
- Cross-application components

### Design Principles

1. **Separation of Concerns**
   - `gmed` package: Visualization and data processing
   - Application code: Entry forms and workflows
   
2. **Modular Registry Pattern**
   - Modules self-register with configuration
   - Dynamic loading based on period
   - Consistent UI/server patterns

3. **Data-Driven Configuration**
   - Period detection uses resident data
   - Module display based on registry
   - Field mappings from data dictionary

---

## Period Structure & Module Matrix

### Period Definitions

| Period # | Name | PGY Level | Timing |
|----------|------|-----------|--------|
| 7 | Entering Residency | Intern | July-Sept |
| 1 | Mid Intern | Intern | Nov-Jan |
| 2 | End Intern | Intern | Apr-Jun |
| 3 | Mid PGY2 | PGY2 | Nov-Jan |
| 4 | End PGY2 | PGY2 | Apr-Jun |
| 5 | Mid PGY3 | PGY3 | Nov-Jan |
| 6 | Graduation | PGY3 | Feb-Jun |

### Module × Period Matrix

| Module | P1 | P2 | P3 | P4 | P5 | P6 | P7 |
|--------|----|----|----|----|----|----|-----|
| **Scholarship** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Wellness & Career Planning** | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Program Feedback** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Assessment Review** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Learning & Development** | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Milestone Self-Evaluation** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **ILP Generation** | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Graduation Data** | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ |
| **Board Preparation** | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ |
| **Skills Review** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| **Learning Styles & Topics** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| **Goals** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| **Concerns** | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |

**Legend**: ✅ Module appears in period | ❌ Module not shown

### Implementation Notes

- **Period 7 (Entering Residency)**: Filtered by academic year (July 1 start), unique modules for incoming interns
- **Periods 1-5 (Standard)**: Core self-assessment modules with career planning
- **Period 6 (Graduation)**: Transition-focused with board prep and graduation data
- All periods include **Milestone Self-Evaluation** as a core component

---

## User Completion Checklist

### Purpose
This checklist helps residents track their progress through the self-assessment and ensure all required sections are completed before final submission. The checklist is dynamic - sections shown depend on the current period.

---

### Periods 1-5: Standard Self-Evaluation Checklist

**Before You Start:**
- [ ] Review Plus/Delta feedback from faculty
- [ ] Review your previous period's goals and career planning

**Required Sections:**

1. **Scholarship** ⭐ Required
   - [ ] Added all scholarly activities since last review
   - [ ] Included complete citations where applicable
   - [ ] Marked Patient Safety Review participation (if applicable)
   - [ ] Marked Root Cause Analysis participation (if applicable)

2. **Wellness and Career Planning** ⭐ Required
   - [ ] Selected career path(s) you're considering
   - [ ] Selected fellowship interests (if applicable)
   - [ ] Indicated track interest (Primary Care, Hospitalist, PROMOTE)
   - [ ] Listed topics to discuss with mentor

3. **Program Feedback** ⭐ Required
   - [ ] Provided Plus feedback (what's working well)
   - [ ] Provided Delta feedback (what could be improved)
   - [ ] Added conference feedback (if applicable)
   - [ ] Added general program feedback

4. **Assessment Review**
   - [ ] Reviewed all Plus feedback from faculty
   - [ ] Reviewed all Delta feedback from faculty
   - [ ] Added your reflections on the feedback received

5. **Learning & Development**
   - [ ] Selected top 3 topics you feel least confident about
   - [ ] Selected preferred learning experiences
   - [ ] Reviewed previous period's selections

6. **Milestone Self-Evaluation** ⭐ Required
   - [ ] Completed ratings for all 23 milestone subcompetencies
   - [ ] Added descriptions for high ratings (Level 4-5) where required
   - [ ] Reviewed spider plot visualization
   - [ ] Compared your ratings to national benchmarks

7. **ILP Generation (Goals)** ⭐ Required
   - [ ] Selected PC/MK subcompetency goal
   - [ ] Selected target level and specific milestone description
   - [ ] Described HOW you will achieve this goal
   - [ ] Selected SBP/PBLI subcompetency goal
   - [ ] Selected target level and specific milestone description
   - [ ] Described HOW you will achieve this goal
   - [ ] Selected PROF/ICS subcompetency goal
   - [ ] Selected target level and specific milestone description
   - [ ] Described HOW you will achieve this goal
   - [ ] Reviewed previous period's goals and progress

**Final Check:**
- [ ] All required sections (⭐) are complete
- [ ] All text entries are professional and spell-checked
- [ ] Career planning reflects your current thinking
- [ ] Goals are specific, measurable, and achievable
- [ ] Ready to submit

---

### Period 6: Graduation Checklist

**Before You Start:**
- [ ] Review Plus/Delta feedback from faculty
- [ ] Consider your transition plans

**Required Sections:**

1. **Scholarship** ⭐ Required
   - [ ] Added all scholarly activities since last review
   - [ ] Included complete citations where applicable
   - [ ] Marked Patient Safety Review participation (if applicable)
   - [ ] Marked Root Cause Analysis participation (if applicable)

2. **Graduation Data** ⭐ Required
   - [ ] Indicated what you'll be doing in July (Primary Care, Hospitalist, Fellowship, Other)
   - [ ] Selected fellowship type (if applicable)
   - [ ] Indicated practice type (Academic vs Community)
   - [ ] Indicated practice location
   - [ ] Provided future email address
   - [ ] Provided future phone number
   - [ ] Completed fellowship location details (if applicable)

3. **Program Feedback** ⭐ Required
   - [ ] Provided Plus feedback (what's working well)
   - [ ] Provided Delta feedback (what could be improved)
   - [ ] Added conference feedback (if applicable)
   - [ ] Added general program feedback

4. **Assessment Review**
   - [ ] Reviewed all Plus feedback from faculty
   - [ ] Reviewed all Delta feedback from faculty
   - [ ] Added your reflections on the feedback received

5. **Board Preparation** ⭐ Required
   - [ ] Indicated Step 3 completion status
   - [ ] Set Step 3 date (if not completed)
   - [ ] Indicated MKSAP completion percentage
   - [ ] Noted any board exam concerns
   - [ ] Documented previous help received (if applicable)

6. **Milestone Self-Evaluation** ⭐ Required
   - [ ] Completed ratings for all 23 milestone subcompetencies
   - [ ] Added descriptions for high ratings (Level 4-5) where required
   - [ ] Reviewed spider plot visualization
   - [ ] Compared your ratings to national benchmarks

**Final Check:**
- [ ] All required sections (⭐) are complete
- [ ] Contact information is current and accurate
- [ ] Board preparation plans are clear
- [ ] Transition plans are documented
- [ ] Ready to submit

---

### Period 7: Entering Residency Checklist

**Before You Start:**
- [ ] Review the skills assessment categories
- [ ] Think about your medical school preparation
- [ ] Consider areas where you'd like additional support

**Required Sections:**

1. **Skills Review** ⭐ Required
   - [ ] Rated preparedness for team communication (Q1)
   - [ ] Rated preparedness for interdisciplinary communication (Q2)
   - [ ] Rated preparedness for student teaching (Q3)
   - [ ] Rated preparedness for procedure consent (Q4)
   - [ ] Rated preparedness for personal organization (Q5)
   - [ ] Rated preparedness for orders/prescriptions (Q6)
   - [ ] Rated preparedness for evidence-based lookup (Q7)
   - [ ] Rated preparedness for patient presentation (Q8)
   - [ ] Rated preparedness for documentation (Q9)
   - [ ] Rated preparedness for handoffs (Q10)
   - [ ] Rated preparedness for urgent/emergent care (Q11)
   - [ ] Rated preparedness for bad news delivery (Q12)
   - [ ] Rated preparedness for asking for help (Q13)
   - [ ] Rated preparedness for ICU management (Q14)
   - [ ] Rated preparedness for inpatient management (Q15)
   - [ ] Rated preparedness for clinic management (Q16)
   - [ ] Rated preparedness for calling consults (Q17)

2. **Learning Styles and Topics** ⭐ Required
   - [ ] Selected top 3 topics you feel least confident about
   - [ ] Specified "Other" topic if applicable
   - [ ] Selected preferred learning experiences
   - [ ] Specified "Other" learning style if applicable

3. **Goals** ⭐ Required
   - [ ] Written Goal #1 for internship
   - [ ] Written Goal #2 for internship (optional but recommended)
   - [ ] Written Goal #3 for internship (optional but recommended)
   - [ ] Indicated if faculty member helped with goals
   - [ ] Provided faculty member name and email (if applicable)

4. **Concerns and Questions** ⭐ Required
   - [ ] Listed any concerns about entry into residency
   - [ ] Noted specific areas where you'd like coaching
   - [ ] Identified questions for your mentor

5. **Milestone Self-Evaluation** ⭐ Required
   - [ ] Completed ratings for all 23 milestone subcompetencies
   - [ ] Added descriptions for high ratings (Level 4-5) where required
   - [ ] Reviewed spider plot visualization
   - [ ] Compared your ratings to national benchmarks

**Final Check:**
- [ ] All required sections (⭐) are complete
- [ ] Skills assessment honestly reflects your preparation
- [ ] Goals are specific and achievable
- [ ] Concerns are clearly articulated
- [ ] Learning preferences are identified
- [ ] Ready to submit

---

### Implementation Notes for Developers

**Checklist Display Logic:**
```r
# In UI, create collapsible checklist at top of page
div(class = "checklist-container",
  h3("Self-Assessment Checklist"),
  p("Use this checklist to track your progress. All sections marked with ⭐ are required."),
  
  # Dynamic checklist based on period
  uiOutput("dynamic_checklist"),
  
  # Progress indicator
  uiOutput("progress_indicator")
)

# In server, track completion
observe({
  period <- current_period()
  
  # Calculate completion percentage
  total_required <- length(get_required_sections(period))
  completed <- sum(section_completion_status())
  
  output$progress_indicator <- renderUI({
    div(class = "progress-bar",
      style = sprintf("width: %d%%;", (completed/total_required)*100),
      sprintf("%d of %d required sections complete", completed, total_required)
    )
  })
})
```

**Section Completion Detection:**
```r
# For each module, track if required fields have data
is_scholarship_complete <- reactive({
  # Check if at least one scholarship entry exists for current period
  scholarship_data <- rdm_data()$scholarship
  current_entries <- scholarship_data %>%
    filter(record_id == current_record_id())
  
  return(nrow(current_entries) > 0)
})

is_career_planning_complete <- reactive({
  # Check if career path has been selected
  s_eval_data <- get_current_period_data("s_eval")
  
  career_fields <- grep("^s_e_career_path___", names(s_eval_data), value = TRUE)
  any_selected <- any(s_eval_data[1, career_fields] == "1", na.rm = TRUE)
  
  return(any_selected)
})

is_milestones_complete <- reactive({
  # Check if all 23 milestone fields have values
  milestone_data <- get_current_period_data("milestone_selfevaluation_c33c")
  
  required_fields <- c(
    paste0("rep_pc", 1:6, "_self"),
    paste0("rep_mk", 1:3, "_self"),
    paste0("rep_sbp", 1:3, "_self"),
    paste0("rep_pbl", 1:2, "_self"),
    paste0("rep_prof", 1:4, "_self"),
    paste0("rep_ics", 1:3, "_self")
  )
  
  all_complete <- all(!is.na(milestone_data[1, required_fields]))
  
  return(all_complete)
})
```

**Final Submission Check:**
```r
# Before allowing final submission
validate_completion <- function(period) {
  required_sections <- get_required_sections(period)
  completion_status <- sapply(required_sections, function(section) {
    check_function <- paste0("is_", section, "_complete")
    if (exists(check_function)) {
      return(do.call(check_function, list()))
    }
    return(FALSE)
  })
  
  if (!all(completion_status)) {
    incomplete <- required_sections[!completion_status]
    showModal(modalDialog(
      title = "Incomplete Sections",
      paste("Please complete the following required sections before submitting:",
            paste(incomplete, collapse = ", ")),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

# In submit button observer
observeEvent(input$final_submit, {
  if (validate_completion(current_period())) {
    # Proceed with submission
    submit_self_assessment()
  }
})
```

**Visual Checklist Component:**
```r
# Render checklist with completion status
render_checklist_item <- function(label, is_complete, is_required = FALSE) {
  icon <- if (is_complete) {
    icon("check-circle", class = "text-success")
  } else if (is_required) {
    icon("circle", class = "text-danger")
  } else {
    icon("circle", class = "text-muted")
  }
  
  required_star <- if (is_required) "⭐" else ""
  
  div(class = "checklist-item",
    icon,
    span(label, class = if (is_complete) "completed" else ""),
    span(required_star, class = "required-indicator")
  )
}
```

---

## REDCap Field Mappings

### Form Organization

#### Repeating Instruments
- **Pattern 1 - OVERWRITE (Fixed Instances)**: `s_eval`, `ilp`, `milestone_entry`, `milestone_selfevaluation_c33c`
  - Instance number = period number
  - Updates overwrite existing data for that period
  
- **Pattern 2 - ADDITIVE (Continuous)**: `scholarship`
  - Each submission creates new instance
  - Instance numbers increment automatically

#### Non-Repeating Instruments
- `resident_data`: Core resident information
- `assessment`: Faculty evaluations (separate repeating structure)

---

### Module Field Mappings

#### 1. Scholarship Module
**Form**: `scholarship` (repeating, ADDITIVE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Project type | `schol_type` | dropdown | 1=QI, 2=Patient Safety, 3=Research, 4=Presentation, 5=Publication, 6=Education, 7=Committee |
| Patient Safety Review | `schol_ps` | yesno | 0/1 |
| Root Cause Analysis | `schol_rca` | yesno | 0/1 |
| QI Description | `schol_qi` | notes | Text |
| Research Description | `schol_res` | notes | Text |
| Research Mentor | `schol_res_mentor` | notes | Text |
| Research Status | `schol_res_status` | dropdown | 1=Finished, 2=Ongoing, 3=Hiatus, 4=Abandoned |
| Presentation Flag | `schol_pres` | yesno | 0/1 |
| Presentation Type | `schol_pres_type` | dropdown | 1=IM Program, 2=SSM, 3=Local, 4=Regional, 5=National |
| Presentation Conference | `schol_pres_conf` | notes | Text |
| Full Citation | `schol_cit` | notes | Text |
| Published Flag | `schol_pub` | yesno | 0/1 |
| Committee Name | `schol_comm` | notes | Text |
| Committee Type | `schol_comm_type` | dropdown | 1=IM Program, 2=IM Dept, 3=Hospital, 4=SSM, 5=Other |
| Committee Other | `schol_comm_other` | notes | Text |
| Division | `schol_div` | dropdown | 1=Allergy, 2=Cardio, 3=Endo, 4=GI, 5=GIM, 6=Geri, 7=Hem/Onc, 8=ID, 9=Nephro, 10=Pall, 11=Pulm/CC, 12=Rheum, 13=Other |

**Notes**:
- Uses ADDITIVE pattern - each submission creates new instance
- Multiple entries allowed per period
- `schol_ps` and `schol_rca` trigger completion badges

---

#### 2. Wellness & Career Planning Module
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| **Career Path** | `s_e_career_path` | checkbox | 1=Primary Care, 2=Subspecialty, 3=Hospitalist, 4=Clinical Research, 5=Basic Science, 6=Clinical Educator, 7=Administration, 8=Other |
| Career Other | `s_e_career_oth` | text | Text |
| **Fellowship Interest** | `s_e_fellow` | checkbox | 1=Allergy, 2=Cardio, 3=Endo, 4=GI, 5=Geri, 6=Hem/Onc, 7=ID, 8=Nephro, 9=Pall, 10=Pulm/CC, 11=Rheum, 12=Other |
| Fellowship Other | `s_e_fellow_oth` | text | Text |
| Track Pursuit | `s_e_track` | yesno | 0/1 |
| **Track Type** | `s_e_track_type` | checkbox | 1=Primary Care Track, 2=Hospitalist Track, 3=PROMOTE Track |
| Discussion Topics | `s_e_discussion` | notes | Text |

**Checkbox Field Handling**:
- Checkbox fields stored as separate columns with `___` suffix
- Example: `s_e_career_path___1`, `s_e_career_path___2`, etc.
- Values: 0 (unchecked) or 1 (checked)
- Must use `raw_or_label = "raw"` in REDCap export to get 0/1 values

**Notes**:
- Uses OVERWRITE pattern - instance = period number
- Wellness component (free text) not yet implemented in field mapping
- Displays previous period's career planning for continuity

---

#### 3. Program Feedback Module
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Plus Feedback | `s_e_prog_plus` | notes | Text |
| Delta Feedback | `s_e_prog_delta` | notes | Text |
| Conference Feedback | `s_e_progconf` | notes | Text |
| General Program Feedback | `s_e_progfeed` | notes | Text |

**Notes**:
- All fields are free-text feedback areas
- Uses OVERWRITE pattern - instance = period number

---

#### 4. Assessment Review (Plus/Delta)
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Plus Feedback Review | `s_e_plus` | notes | Text |
| Delta Feedback Review | `s_e_delta` | notes | Text |

**Data Sources**:
- Assessment data comes from `assessment` form (faculty evaluations)
- Fields: `ass_plus`, `ass_delta`, `ass_date`, `ass_faculty`, `ass_level`
- Aggregated and displayed by period for resident review

**Notes**:
- Display-only in most cases (shows faculty feedback)
- Resident can add reflections in `s_e_plus` and `s_e_delta`

---

#### 5. Learning & Development Module
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| **Topic Selection** | `s_e_topic_sel` | checkbox | 1=Abdominal pain, 2=Acid base, 3=ACS, 4=AKI, 5=AMS, 6=Anticoag, 7=Cirrhosis, 8=CHF, 9=Diabetes, 10=Dyspnea, 11=Lytes, 12=GI bleed, 13=Hypo/hyperNa, 14=MSK, 15=Onc emergencies, 16=Pancreatitis, 17=Pneumonia, 18=Shock, 19=SSTI, 20=Substance use, 21=Syncope, 22=Transfusion, 23=Other |
| Topic Other | `s_e_topic_oth` | text | Text |
| **Learning Style** | `s_e_learn_style` | checkbox | 1=Case discussion, 2=Small group, 3=Direct obs, 4=Simulation, 5=Standardized pts, 6=Lectures, 7=Mentoring, 8=Online modules, 9=Specific rotation, 10=Reading, 11=Self-directed, 12=Other |
| Learning Other | `s_e_learn_oth` | text | Text |

**Notes**:
- Checkbox fields: Use `raw_or_label = "raw"` to get 0/1 values
- Stores as `s_e_topic_sel___1`, `s_e_topic_sel___2`, etc.
- Displays previous period selections for continuity

---

#### 6. Milestone Self-Evaluation Module
**Form**: `milestone_selfevaluation_c33c` (repeating, OVERWRITE pattern)

**Format**: REP Milestones (legacy 5-point scale)

| Competency | Field Pattern | Example Fields |
|------------|---------------|----------------|
| Patient Care | `rep_pc{1-6}_self` | `rep_pc1_self`, `rep_pc2_self` |
| Medical Knowledge | `rep_mk{1-3}_self` | `rep_mk1_self`, `rep_mk2_self` |
| Systems-Based Practice | `rep_sbp{1-3}_self` | `rep_sbp1_self`, `rep_sbp2_self` |
| Practice-Based Learning | `rep_pbl{1-2}_self` | `rep_pbl1_self`, `rep_pbl2_self` |
| Professionalism | `rep_prof{1-4}_self` | `rep_prof1_self`, `rep_prof2_self` |
| Interpersonal Skills | `rep_ics{1-3}_self` | `rep_ics1_self`, `rep_ics2_self` |

**Description Fields** (optional, for higher ratings):
- Pattern: `{field_name}_desc`
- Example: `rep_pc1_self_desc`, `rep_pc2_self_desc`

**Metadata Fields**:
- `prog_mile_date_self`: Date of entry (date_mdy)
- `prog_mile_period_self`: Period (7=Entering, 1-6=Standard periods)

**Values**: 
- 1-5 scale (1=Novice, 2=Advanced Beginner, 3=Competent, 4=Proficient, 5=Expert)
- Aligns with ACGME milestone 9-point scale via mapping

**Notes**:
- Uses gmed package's `mod_milestone_entry` module
- Spider plot visualization with national benchmarks
- Interactive plotly charts with hover details
- OVERWRITE pattern - instance = period number

---

#### 7. ILP Generation (Goals) Module
**Form**: `ilp` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| ILP Date | `ilp_date` | text | date_mdy |
| **PC/MK Goal** | `goal_pcmk` | dropdown | 1=PC1, 2=PC2, 3=PC3, 4=PC4, 5=PC5, 6=PC6, 7=MK1, 8=MK2, 9=MK3 |
| PC/MK Target Level | `goal_level_pcmk` | dropdown | 1-5 |
| PC/MK Level Row | `goal_level_r_pcmk` | dropdown | 1-15 (specific row within level) |
| PC/MK How | `how_pcmk` | notes | Text |
| **SBP/PBLI Goal** | `goal_sbppbl` | dropdown | 1=SBP1, 2=SBP2, 3=SBP3, 4=PBLI1, 5=PBLI2 |
| SBP/PBLI Target Level | `goal_level_sbppbl` | dropdown | 1-5 |
| SBP/PBLI Level Row | `goal_r_sbppbl` | dropdown | 1-15 |
| SBP/PBLI How | `how_sbppbl` | notes | Text |
| **PROF/ICS Goal** | `goal_subcomp_profics` | dropdown | 1=PROF1, 2=PROF2, 3=PROF3, 4=PROF4, 5=ICS1, 6=ICS2, 7=ICS3 |
| PROF/ICS Target Level | `goal_level_profics` | dropdown | 1-5 |
| PROF/ICS Level Row | `goal_r_profics` | dropdown | 1-15 |
| PROF/ICS How | `how_profics` | notes | Text |

**Notes**:
- Three domains: PC/MK, SBP/PBLI, PROF/ICS
- Each domain requires: subcompetency selection, target level, specific row, and "how" plan
- Uses milestone table data from data dictionary
- OVERWRITE pattern - instance = period number
- Previous period goals displayed for review

---

#### 8. Graduation Data Module
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Step 3 Completed | `s_e_step3` | yesno | 0/1 |
| Step 3 Score Sent | `s_e_step3_contact` | yesno | 0/1 |
| Next Position | `s_e_grad_next` | dropdown | 1=Primary Care, 2=Hospitalist, 3=Fellowship, 4=Other |
| Next Position Other | `s_e_grad_next_othe` | text | Text |
| **Fellowship Type** | `s_e_grad_fellow` | checkbox | 1=Allergy, 2=Cardio, 3=Endo, 4=GI, 5=Geri, 6=Hem/Onc, 7=ID, 8=Nephro, 9=Pall, 10=Pulm/CC, 11=Rheum, 12=Other |
| Fellowship Other | `s_e_grad_fellow_oth` | text | Text |
| Practice Type | `s_e_grad_where` | dropdown | 1=Academic, 2=Community |
| Practice Location | `s_e_grad_loc` | dropdown | 1=SSM-SLUCare, 2=SSM, 3=St. Louis, 4=Other |
| Location Other | `s_e_grad_loc_other` | text | Text |
| SLU Fellowship | `s_e_grad_fellow_loc` | yesno | 0/1 |
| Fellowship Location | `s_e_grad_fellow_loc_else` | text | Text |
| Future Email | `s_e_grad_email` | text | Email |
| Future Phone | `s_e_grad_phone` | text | Text |

**Notes**:
- Period 6 only
- Tracks transition plans and contact information
- Checkbox fields use `___` suffix pattern

---

#### 9. Board Preparation Module
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Step 3 Completed | `s_e_step3` | yesno | 0/1 |
| Step 3 Date Set | `s_e_step3_date_set` | yesno | 0/1 |
| Step 3 Scheduled Date | `s_e_step3_date` | text | date_mdy |
| Board Concerns | `s_e_board_concern` | yesno | 0/1 |
| Previous Help | `s_e_board_help` | yesno | 0/1 |
| Help Discussion | `s_e_board_discu` | notes | Text |
| MKSAP Completion | `s_e_mksap_comp` | dropdown | 1=0-25%, 2=26-50%, 3=55-75%, 4=76-99%, 5=Completed/Repeating |

**Notes**:
- Period 6 only
- Focuses on board exam preparation
- Tracks MKSAP progress and support needs

---

#### 10. Skills Review Module (Period 7)
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Preparedness Q1-17 | `s_e_prep_{1-17}` | radio | 1=Not at all, 2=Slightly, 3=Moderately, 4=Very, 5=Extremely prepared |

**Preparedness Topics** (17 questions):
1. Team communication
2. Interdisciplinary communication
3. Student teaching
4. Procedure consent
5. Personal organization
6. Orders/prescriptions
7. Evidence-based lookup
8. Patient presentation
9. Documentation
10. Handoffs
11. Urgent/emergent care
12. Bad news delivery
13. Asking for help
14. ICU management
15. Inpatient management
16. Clinic management
17. Calling consults

**Notes**:
- Period 7 (Entering Residency) only
- Assesses incoming intern preparedness
- 5-point scale for each skill area

---

#### 11. Learning Styles & Topics Module (Period 7)
**Form**: `s_eval` (repeating, OVERWRITE pattern)

**Fields**: Same as Learning & Development module
- `s_e_topic_sel` (checkbox)
- `s_e_topic_oth` (text)
- `s_e_learn_style` (checkbox)
- `s_e_learn_oth` (text)

**Notes**:
- Shared field names with Learning module
- Period 7 context (entering residency focus)

---

#### 12. Goals Module (Period 7)
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Faculty Assistance | `s_e_fac_assist` | yesno | 0/1 |
| Faculty Name | `s_e_fac_member` | text | Text |
| Faculty Email | `s_e_fac_email` | text | Email |
| Goal 1 | `s_e_ume_goal1` | notes | Text |
| Goal 2 | `s_e_ume_goal2` | notes | Text |
| Goal 3 | `s_e_ume_goal3` | notes | Text |

**Notes**:
- Period 7 only
- Free-form goal entry (not milestone-based like ILP)
- Optional faculty mentor tracking

---

#### 13. Concerns Module (Period 7)
**Form**: `s_eval` (repeating, OVERWRITE pattern)

| Field Purpose | REDCap Field Name | Type | Values |
|---------------|-------------------|------|--------|
| Concerns/Comments | `s_e_ume_concern` | notes | Text |

**Notes**:
- Period 7 only
- Free-text area for incoming intern concerns

---

## Module Implementation Status

### ✅ Complete (Implemented & Tested)

1. **Scholarship** (`mod_scholarship_wrapper`)
   - Multi-step entry workflow
   - Category-based display (QI, Research, Presentations, etc.)
   - Completion badges (Patient Safety, RCA)
   - ADDITIVE pattern submission

2. **Wellness & Career Planning** (`mod_career_planning_wrapper`)
   - Career path checkboxes
   - Fellowship selection
   - Track pursuit
   - Previous period display

3. **Assessment Review** (`mod_assessment_wrapper`)
   - Plus/Delta feedback aggregation
   - Period-based filtering
   - Faculty attribution

4. **Program Feedback** (`mod_program_feedback`)
   - Four feedback areas (Plus, Delta, Conference, General)
   - Free-text entry

5. **Milestone Self-Evaluation** (`mod_milestone_entry` from gmed)
   - REP milestone format
   - Spider plot visualization
   - National benchmark comparison
   - Description fields for high ratings

6. **ILP Generation** (`mod_goals_wrapper`)
   - Three domain goal setting
   - Milestone table integration
   - Previous goal review
   - "How" planning for each goal

### 🚧 In Progress

7. **Learning & Development** (`mod_learning`)
   - Topic selection (checkbox)
   - Learning style preferences (checkbox)
   - Previous period review
   - ⚠️ *Needs verification of checkbox handling*

### ⏳ Planned (Not Yet Implemented)

8. **Graduation Data** (`mod_graduation_data`)
   - Next position tracking
   - Contact information
   - Fellowship details

9. **Board Preparation** (`mod_board_prep`)
   - Step 3 tracking
   - MKSAP progress
   - Concern documentation

10. **Skills Review** (`mod_skills_review`)
    - 17-question preparedness assessment
    - Period 7 specific

11. **Learning Styles & Topics** (`mod_learning_styles`)
    - May be same as Learning module
    - Period 7 context

12. **Goals** (`mod_goals`)
    - Free-form goal entry (different from ILP)
    - Faculty mentor tracking
    - Period 7 specific

13. **Concerns** (`mod_concerns`)
    - Incoming intern concerns
    - Period 7 specific

---

## Data Flow

### 1. Authentication Flow
```
User enters access code
    ↓
Validate against resident_data.access_code
    ↓
Load resident record (record_id, grad_yr, type)
    ↓
Store in session
```

### 2. Period Detection Flow
```
Get resident data (grad_yr, type)
    ↓
Calculate current period using gmed::calculate_pgy_and_period()
    ↓
Return period_number, period_name, pgy_year, is_valid
    ↓
Load period configuration (modules, titles)
```

### 3. Data Loading Flow
```
Call gmed::load_rdm_complete()
    ↓
Export all forms from REDCap (raw_or_label = "raw")
    ↓
Filter to active residents (res_archive != 1)
    ↓
Process assessment data (dates, levels, aggregation)
    ↓
Create milestone workflow (detection + processing)
    ↓
Return comprehensive data structure
```

### 4. Module Rendering Flow
```
Get period config → modules list
    ↓
For each module:
    ↓
    Lookup in module registry
    ↓
    Call UI function with namespace
    ↓
    Initialize server with data
```

### 5. Submission Flow

**OVERWRITE Pattern** (s_eval, ilp, milestone_entry):
```
Calculate instance = period_number
    ↓
Build field list
    ↓
Call submit_overwrite_data()
    ↓
Overwrites existing data for that instance
```

**ADDITIVE Pattern** (scholarship):
```
Get next available instance
    ↓
Build field list
    ↓
Call submit_additive_data()
    ↓
Creates new instance
```

### 6. Visualization Flow
```
Load historical data → Calculate medians
    ↓
Get current period data → Self-evaluation scores
    ↓
Call gmed::create_milestone_spider_plotly()
    ↓
Display interactive spider plot
```

---

## Technical Dependencies

### R Packages

**Core Framework**:
- `shiny` - Web application framework
- `shinyjs` - JavaScript interactions
- `htmltools` - HTML generation

**Data Manipulation**:
- `data.table` - High-performance data operations (preferred over dplyr)
- `purrr` - Functional programming utilities
- `dplyr` - Data manipulation (legacy, being phased out where data.table is more efficient)

**Visualization**:
- `plotly` - Interactive charts (spider plots, etc.)
- `ggplot2` - Static graphics (if needed)

**REDCap Integration**:
- `httr` - HTTP requests to REDCap API
- `jsonlite` - JSON parsing

**Internal Packages**:
- `gmed` - Shared visualization and data processing functions
  - Milestone functions
  - Spider plot generation
  - REDCap data loading
  - Period calculations

### External Services

**REDCap Database**:
- URL: Configured in app_config
- API Token: Stored in .Renviron
- Database: RDM 2.0

**Data Dictionary**:
- Field definitions
- Choice labels
- Branching logic
- Form structure

---

## Development Patterns

### Module Structure

**File Naming**:
- Wrapper modules: `R/modules/wrappers/mod_{module}_wrapper.R`
- Standalone modules: `R/modules/mod_{module}.R`
- Configuration: `R/config/{purpose}_config.R`

**Module Pattern**:
```r
# UI Function
mod_module_wrapper_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # UI elements with ns() namespace
  )
}

# Server Function
mod_module_wrapper_server <- function(id, rdm_data, record_id, period, data_dict) {
  moduleServer(id, function(input, output, session) {
    
    # Server logic
    # - Reactive expressions
    # - Observers
    # - Output rendering
    # - Submission handlers
    
  })
}
```

### Registry Pattern

**Module Registration** (`R/config/module_registry.R`):
```r
get_module_functions <- function(module_id, data_dict = NULL) {
  registry <- list(
    module_id = list(
      ui = "mod_module_ui",
      server = "mod_module_server",
      title = "Module Title",
      icon = "fa-icon"
    )
  )
  return(registry[[module_id]])
}
```

### Checkbox Field Handling

**Critical Pattern**:
```r
# REDCap Export
data <- gmed::load_rdm_complete(
  rdm_token = token,
  redcap_url = url,
  raw_or_label = "raw"  # MUST use "raw" for checkboxes
)

# Checkbox fields come as:
# field_name___1 = 0 or 1
# field_name___2 = 0 or 1
# etc.

# To get selected values:
selected_values <- names(data)[grepl("^field_name___", names(data)) & data == 1]
selected_values <- gsub("field_name___", "", selected_values)
```

### Data Dictionary Handling

**Standardization** (handles both API and CSV formats):
```r
standardize_dict_names <- function(dict) {
  name_mapping <- list(
    field_name = c("field_name", "Variable / Field Name", "Variable...Field.Name"),
    field_label = c("field_label", "Field Label", "Field.Label"),
    choices = c("select_choices_or_calculations", 
                "Choices, Calculations, OR Slider Labels")
  )
  
  for (standard_name in names(name_mapping)) {
    possible_names <- name_mapping[[standard_name]]
    for (poss_name in possible_names) {
      if (poss_name %in% names(dict)) {
        names(dict)[names(dict) == poss_name] <- standard_name
        break
      }
    }
  }
  
  return(dict)
}
```

### Submission Patterns

**OVERWRITE Submission**:
```r
submit_overwrite_data(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  form_data = fields,
  instrument_name = "s_eval",
  period = period,
  level = pgy_year,
  review_type = "scheduled"
)
```

**ADDITIVE Submission**:
```r
submit_additive_data(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  form_data = fields,
  instrument_name = "scholarship"
)
```

### Code Style Preferences

1. **Use data.table/purrr** over dplyr where computationally advantageous
2. **Complete function code** in documentation (copy/paste ready)
3. **Concrete implementation steps** over conceptual explanations
4. **Pragmatic approach**: Get it working, then refine
5. **No "imres" references**: Only use gmed package

---

## Future Enhancements

### Planned Features
- [ ] Complete all Period 6 modules (Graduation Data, Board Prep)
- [ ] Complete all Period 7 modules (Skills Review, Goals, Concerns)
- [ ] Wellness component for Career Planning module
- [ ] Enhanced AI theme identification for program feedback
- [ ] Cross-application reusability expansion

### Technical Debt
- [ ] Remove all legacy "imres" package references
- [ ] Complete migration to data.table where appropriate
- [ ] Standardize all checkbox field handling
- [ ] Add comprehensive error handling
- [ ] Implement automated testing

---

## Quick Reference

### Key Functions (gmed package)

- `gmed::load_rdm_complete()` - Load all REDCap data
- `gmed::calculate_pgy_and_period()` - Determine period from resident data
- `gmed::create_milestone_spider_plotly()` - Generate milestone spider plots
- `gmed::get_evaluation_dictionary()` - Load data dictionary
- `gmed::mod_milestone_entry_ui/server()` - Milestone entry module

### Key Configuration Files

- `R/config/period_config.R` - Period definitions and module lists
- `R/config/module_registry.R` - Module UI/server function mappings
- `R/config/field_mappings.R` - REDCap field name mappings
- `R/globals.R` - Global configuration and initialization

### Important Constants

- **Academic Year Start**: July 1
- **Period Count**: 7 (0-7, with 7 = Entering Residency)
- **Milestone Scale**: 1-5 (REP) or 1-9 (ACGME)
- **REDCap Instance = Period Number** (for OVERWRITE forms)

---

## Final Submission Checklist

### Before You Click Submit

Regardless of which period you're completing, always verify:

#### Content Quality
- [ ] All text entries are professionally written
- [ ] Spelling and grammar have been checked
- [ ] Free-text responses are thoughtful and specific
- [ ] Citations are complete and properly formatted (scholarship)
- [ ] All required fields (⭐) have been completed

#### Accuracy
- [ ] Career planning reflects your current interests
- [ ] Contact information is current (graduation period)
- [ ] Milestone self-ratings are honest and accurate
- [ ] Goals are realistic and achievable

#### Completeness
- [ ] Reviewed the period-specific checklist above
- [ ] All required modules show as complete
- [ ] Progress indicator shows 100%
- [ ] No error messages or warnings displayed

#### Review
- [ ] Reviewed Plus/Delta feedback from faculty
- [ ] Compared current responses to previous period (if applicable)
- [ ] Goals align with milestone self-evaluation
- [ ] Learning needs match identified areas for growth

### Common Issues Before Submission

**❌ Incomplete Scholarship Entries**
- Make sure each entry has: Type, Description, Status
- Publications need full citations
- Presentations need venue/conference

**❌ Vague Career Planning**
- Don't leave checkboxes all unchecked
- Be specific about fellowship interests
- Indicate if you're unsure (it's okay!)

**❌ Empty Feedback Fields**
- Even if no major concerns, provide constructive feedback
- Balance Plus and Delta comments
- Be specific with conference feedback

**❌ Incomplete Milestone Ratings**
- All 23 subcompetencies must be rated
- High ratings (4-5) need description text
- Review spider plot for obvious outliers

**❌ Unrealistic or Vague Goals**
- Goals should be measurable and specific
- "How" plan should include concrete steps
- Three domain goals required (PC/MK, SBP/PBLI, PROF/ICS)

### Technical Submission Notes

**For Developers:**

```r
# Final validation before submission
perform_final_validation <- function(period, rdm_data, record_id) {
  
  validation_results <- list(
    errors = character(),
    warnings = character(),
    passed = FALSE
  )
  
  # 1. Check all required modules are complete
  required_modules <- get_required_modules(period)
  for (module in required_modules) {
    if (!check_module_completion(module, rdm_data, record_id, period)) {
      validation_results$errors <- c(
        validation_results$errors,
        sprintf("Module '%s' is incomplete", module)
      )
    }
  }
  
  # 2. Check data quality issues
  
  # Milestone completeness
  milestone_complete <- check_all_milestones_rated(rdm_data, record_id, period)
  if (!milestone_complete) {
    validation_results$errors <- c(
      validation_results$errors,
      "Not all milestone subcompetencies have been rated"
    )
  }
  
  # High milestone ratings need descriptions
  missing_descriptions <- check_milestone_descriptions(rdm_data, record_id, period)
  if (length(missing_descriptions) > 0) {
    validation_results$warnings <- c(
      validation_results$warnings,
      sprintf("High ratings without descriptions: %s", 
              paste(missing_descriptions, collapse = ", "))
    )
  }
  
  # 3. Check for empty required text fields
  empty_required_fields <- check_required_text_fields(period, rdm_data, record_id)
  if (length(empty_required_fields) > 0) {
    validation_results$errors <- c(
      validation_results$errors,
      sprintf("Required fields are empty: %s", 
              paste(empty_required_fields, collapse = ", "))
    )
  }
  
  # 4. Check goals have "how" plans
  if (period %in% 1:5) {
    goals_complete <- check_goals_complete(rdm_data, record_id, period)
    if (!goals_complete) {
      validation_results$errors <- c(
        validation_results$errors,
        "All three goal domains need target and 'how' plan"
      )
    }
  }
  
  # 5. Check career planning has at least one selection
  if (period %in% 1:5) {
    career_selected <- check_career_planning(rdm_data, record_id, period)
    if (!career_selected) {
      validation_results$warnings <- c(
        validation_results$warnings,
        "No career path selected - consider indicating your interests"
      )
    }
  }
  
  # Set passed flag
  validation_results$passed <- length(validation_results$errors) == 0
  
  return(validation_results)
}

# Display validation results to user
show_validation_results <- function(validation_results) {
  
  if (validation_results$passed && length(validation_results$warnings) == 0) {
    showModal(modalDialog(
      title = "Ready to Submit!",
      icon("check-circle", class = "text-success fa-3x"),
      h4("All validation checks passed."),
      p("Your self-assessment is complete and ready for submission."),
      footer = tagList(
        modalButton("Review Again"),
        actionButton("confirm_submit", "Submit Now", class = "btn-success")
      )
    ))
  } else if (validation_results$passed && length(validation_results$warnings) > 0) {
    showModal(modalDialog(
      title = "Ready with Warnings",
      icon("exclamation-triangle", class = "text-warning fa-3x"),
      h4("You can submit, but please review these items:"),
      tags$ul(
        lapply(validation_results$warnings, function(w) tags$li(w))
      ),
      footer = tagList(
        modalButton("Review Again"),
        actionButton("confirm_submit", "Submit Anyway", class = "btn-warning")
      )
    ))
  } else {
    showModal(modalDialog(
      title = "Cannot Submit - Errors Found",
      icon("times-circle", class = "text-danger fa-3x"),
      h4("Please fix these issues before submitting:"),
      tags$ul(
        lapply(validation_results$errors, function(e) tags$li(e))
      ),
      if (length(validation_results$warnings) > 0) {
        tagList(
          h4("Warnings:"),
          tags$ul(
            lapply(validation_results$warnings, function(w) tags$li(w))
          )
        )
      },
      footer = modalButton("OK")
    ))
  }
}

# In submit button handler
observeEvent(input$submit_self_assessment, {
  
  validation <- perform_final_validation(
    period = current_period(),
    rdm_data = app_data(),
    record_id = current_record_id()
  )
  
  show_validation_results(validation)
  
})

# After user confirms
observeEvent(input$confirm_submit, {
  removeModal()
  
  # Proceed with actual submission
  submit_result <- submit_all_self_assessment_data(
    record_id = current_record_id(),
    period = current_period()
  )
  
  if (submit_result$success) {
    showModal(modalDialog(
      title = "Submission Complete!",
      icon("check-circle", class = "text-success fa-3x"),
      h4("Your self-assessment has been successfully submitted."),
      p("Thank you for completing your self-assessment."),
      p("Your coach will review your submission."),
      footer = actionButton("close_app", "Close", class = "btn-primary")
    ))
  } else {
    showModal(modalDialog(
      title = "Submission Failed",
      icon("times-circle", class = "text-danger fa-3x"),
      h4("There was an error submitting your self-assessment."),
      p(submit_result$error_message),
      p("Please try again or contact support if the problem persists."),
      footer = modalButton("OK")
    ))
  }
})
```

---

**Last Updated**: November 15, 2025  
**Maintainer**: Fred (Development Lead)  
**Repository**: imslu-resident-self-assessment  
**Shared Package**: gmed

