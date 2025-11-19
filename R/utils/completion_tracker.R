# ============================================================================
# COMPLETION TRACKER UTILITIES
# Functions to check if modules have data submitted
# ============================================================================

#' Check if Scholarship module is complete
#'
#' @param rdm_data App data structure from load_rdm_complete()
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status (TRUE/FALSE) and count
#' @export
check_scholarship_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$scholarship)) {
    return(list(complete = FALSE, count = 0, details = "No scholarship data"))
  }

  schol_data <- rdm_data$all_forms$scholarship %>%
    dplyr::filter(record_id == !!record_id)

  count <- nrow(schol_data)

  list(
    complete = count > 0,
    count = count,
    details = if (count > 0) paste(count, "activities recorded") else "No activities"
  )
}

#' Check if Career Planning module is complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status and details
#' @export
check_career_planning_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$s_eval)) {
    return(list(complete = FALSE, details = "No s_eval data"))
  }

  s_eval_data <- rdm_data$all_forms$s_eval %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == period
    )

  if (nrow(s_eval_data) == 0) {
    return(list(complete = FALSE, details = "Not started"))
  }

  # Check if any career path checkbox is selected
  career_fields <- grep("^s_e_career_path___", names(s_eval_data), value = TRUE)
  has_career <- any(!is.na(s_eval_data[1, career_fields]) & s_eval_data[1, career_fields] == "1")

  # Check if wellness is filled
  has_wellness <- !is.na(s_eval_data$s_e_well[1]) && nzchar(s_eval_data$s_e_well[1])

  list(
    complete = has_career || has_wellness,
    details = if (has_career && has_wellness) {
      "Career path and wellness recorded"
    } else if (has_career) {
      "Career path recorded (wellness optional)"
    } else if (has_wellness) {
      "Wellness recorded (career path recommended)"
    } else {
      "Not started"
    }
  )
}

#' Check if Program Feedback module is complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status and details
#' @export
check_program_feedback_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$s_eval)) {
    return(list(complete = FALSE, details = "No s_eval data"))
  }

  s_eval_data <- rdm_data$all_forms$s_eval %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == period
    )

  if (nrow(s_eval_data) == 0) {
    return(list(complete = FALSE, details = "Not started"))
  }

  # Check if any feedback fields have content
  has_plus <- !is.na(s_eval_data$s_e_prog_plus[1]) && nzchar(s_eval_data$s_e_prog_plus[1])
  has_delta <- !is.na(s_eval_data$s_e_prog_delta[1]) && nzchar(s_eval_data$s_e_prog_delta[1])
  has_conf <- !is.na(s_eval_data$s_e_progconf[1]) && nzchar(s_eval_data$s_e_progconf[1])
  has_feed <- !is.na(s_eval_data$s_e_progfeed[1]) && nzchar(s_eval_data$s_e_progfeed[1])

  complete_count <- sum(has_plus, has_delta, has_conf, has_feed)

  list(
    complete = complete_count > 0,
    details = paste(complete_count, "of 4 feedback areas completed")
  )
}

#' Check if Assessment Review module is complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status and details
#' @export
check_assessment_review_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$s_eval)) {
    return(list(complete = FALSE, details = "No s_eval data"))
  }

  s_eval_data <- rdm_data$all_forms$s_eval %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == period
    )

  if (nrow(s_eval_data) == 0) {
    return(list(complete = FALSE, details = "Not reviewed"))
  }

  # Check if resident has added reflections
  has_plus_reflection <- !is.na(s_eval_data$s_e_plus[1]) && nzchar(s_eval_data$s_e_plus[1])
  has_delta_reflection <- !is.na(s_eval_data$s_e_delta[1]) && nzchar(s_eval_data$s_e_delta[1])

  # Also check if there's assessment data to review
  assessment_count <- 0
  if (!is.null(rdm_data$assessment)) {
    assessment_count <- rdm_data$assessment %>%
      dplyr::filter(record_id == !!record_id) %>%
      nrow()
  }

  list(
    complete = has_plus_reflection || has_delta_reflection,
    details = if (assessment_count == 0) {
      "No assessments to review"
    } else if (has_plus_reflection || has_delta_reflection) {
      paste(assessment_count, "assessments reviewed with reflections")
    } else {
      paste(assessment_count, "assessments available (not reflected on)")
    }
  )
}

#' Check if Learning module is complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status and details
#' @export
check_learning_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$s_eval)) {
    return(list(complete = FALSE, details = "No s_eval data"))
  }

  s_eval_data <- rdm_data$all_forms$s_eval %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == period
    )

  if (nrow(s_eval_data) == 0) {
    return(list(complete = FALSE, details = "Not started"))
  }

  # Check if any topic selections
  topic_fields <- grep("^s_e_topic_sel___", names(s_eval_data), value = TRUE)
  has_topics <- any(!is.na(s_eval_data[1, topic_fields]) & s_eval_data[1, topic_fields] == "1")

  # Check if any learning style selections
  learn_fields <- grep("^s_e_learn_style___", names(s_eval_data), value = TRUE)
  has_styles <- any(!is.na(s_eval_data[1, learn_fields]) & s_eval_data[1, learn_fields] == "1")

  list(
    complete = has_topics || has_styles,
    details = if (has_topics && has_styles) {
      "Topics and learning styles selected"
    } else if (has_topics) {
      "Topics selected"
    } else if (has_styles) {
      "Learning styles selected"
    } else {
      "Not started"
    }
  )
}

#' Check if Milestone Self-Evaluation module is complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status and details
#' @export
check_milestone_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$milestone_selfevaluation_c33c)) {
    return(list(complete = FALSE, details = "No milestone data"))
  }

  milestone_data <- rdm_data$all_forms$milestone_selfevaluation_c33c %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "milestone_selfevaluation_c33c",
      redcap_repeat_instance == period
    )

  if (nrow(milestone_data) == 0) {
    return(list(complete = FALSE, details = "Not started"))
  }

  # Check if all 23 milestone fields have values
  milestone_fields <- c(
    paste0("rep_pc", 1:6, "_self"),
    paste0("rep_mk", 1:3, "_self"),
    paste0("rep_sbp", 1:3, "_self"),
    paste0("rep_pbl", 1:2, "_self"),
    paste0("rep_prof", 1:4, "_self"),
    paste0("rep_ics", 1:3, "_self")
  )

  completed_fields <- sum(!is.na(milestone_data[1, milestone_fields]))
  total_fields <- length(milestone_fields)

  list(
    complete = completed_fields == total_fields,
    details = paste(completed_fields, "of", total_fields, "milestones rated")
  )
}

#' Check if ILP Generation (Goals) module is complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with status and details
#' @export
check_ilp_complete <- function(rdm_data, record_id, period) {
  if (is.null(rdm_data$all_forms$ilp)) {
    return(list(complete = FALSE, details = "No ILP data"))
  }

  ilp_data <- rdm_data$all_forms$ilp %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "ilp",
      redcap_repeat_instance == period
    )

  if (nrow(ilp_data) == 0) {
    return(list(complete = FALSE, details = "No goals set"))
  }

  # Check if all three goal domains are filled
  has_pcmk <- !is.na(ilp_data$goal_pcmk[1]) && !is.na(ilp_data$how_pcmk[1]) && nzchar(ilp_data$how_pcmk[1])
  has_sbppbl <- !is.na(ilp_data$goal_sbppbl[1]) && !is.na(ilp_data$how_sbppbl[1]) && nzchar(ilp_data$how_sbppbl[1])
  has_profics <- !is.na(ilp_data$goal_subcomp_profics[1]) && !is.na(ilp_data$how_profics[1]) && nzchar(ilp_data$how_profics[1])

  goal_count <- sum(has_pcmk, has_sbppbl, has_profics)

  list(
    complete = goal_count == 3,
    details = paste(goal_count, "of 3 goal domains completed")
  )
}

#' Get completion status for all modules in a period
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return Data frame with module, status, and details
#' @export
get_period_completion_status <- function(rdm_data, record_id, period) {
  # Get the modules for this period
  config <- get_period_structure(period)
  modules <- config$modules
  module_titles <- config$module_titles

  # Initialize results
  results <- data.frame(
    module_id = character(),
    module_title = character(),
    complete = logical(),
    details = character(),
    stringsAsFactors = FALSE
  )

  for (module_id in modules) {
    # Get completion status based on module type
    status <- switch(module_id,
      scholarship = check_scholarship_complete(rdm_data, record_id, period),
      career_planning = check_career_planning_complete(rdm_data, record_id, period),
      program_feedback = check_program_feedback_complete(rdm_data, record_id, period),
      assessment_review = check_assessment_review_complete(rdm_data, record_id, period),
      learning = check_learning_complete(rdm_data, record_id, period),
      milestone_self_eval = check_milestone_complete(rdm_data, record_id, period),
      ilp_generation = check_ilp_complete(rdm_data, record_id, period),
      # Default for modules not yet implemented
      list(complete = FALSE, details = "Module not yet implemented")
    )

    results <- rbind(results, data.frame(
      module_id = module_id,
      module_title = module_titles[[module_id]],
      complete = status$complete,
      details = status$details,
      stringsAsFactors = FALSE
    ))
  }

  results
}

#' Calculate overall completion percentage
#'
#' @param completion_status Data frame from get_period_completion_status()
#' @return Numeric percentage (0-100)
#' @export
calculate_completion_percentage <- function(completion_status) {
  if (nrow(completion_status) == 0) return(0)

  completed <- sum(completion_status$complete)
  total <- nrow(completion_status)

  round((completed / total) * 100)
}

#' Get required modules for a period
#'
#' According to CLAUDE.md, these modules are required (marked with ⭐)
#'
#' @param period Period number
#' @return Character vector of required module IDs
#' @export
get_required_modules <- function(period) {
  if (period == 7) {
    # Period 7: Entering Residency
    c("skills_review", "learning_styles", "goals", "concerns", "milestone_self_eval")
  } else if (period == 6) {
    # Period 6: Graduation
    c("scholarship", "graduation_data", "program_feedback", "board_prep", "milestone_self_eval")
  } else {
    # Periods 1-5: Standard
    c("scholarship", "career_planning", "program_feedback", "milestone_self_eval", "ilp_generation")
  }
}

#' Check if all required modules are complete
#'
#' @param rdm_data App data structure
#' @param record_id Resident record ID
#' @param period Period number
#' @return List with overall_complete status and details
#' @export
check_required_complete <- function(rdm_data, record_id, period) {
  required_modules <- get_required_modules(period)
  all_status <- get_period_completion_status(rdm_data, record_id, period)

  required_status <- all_status %>%
    dplyr::filter(module_id %in% required_modules)

  incomplete_required <- required_status %>%
    dplyr::filter(!complete)

  list(
    overall_complete = nrow(incomplete_required) == 0,
    required_count = nrow(required_status),
    complete_count = sum(required_status$complete),
    incomplete_modules = incomplete_required$module_title
  )
}
