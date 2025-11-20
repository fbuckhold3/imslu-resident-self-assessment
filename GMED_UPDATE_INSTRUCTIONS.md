# How to Fix Career Planning Display Issue

## Problem

The previous career planning and track data wasn't displaying because the app was using an outdated version of the `gmed` package.

## Solution

**Good news:** The `gmed` package on GitHub already has the correct implementations!

The functions `display_career_planning()` and `display_wellness()` are already fixed in the repository at:
https://github.com/fbuckhold3/gmed

They correctly use:
- Lowercase `"s_eval"` for repeat instrument filtering
- `redcap_repeat_instance` for period-based filtering
- Proper data structure handling for `rdm_data$all_forms$s_eval`

## Quick Fix: Install Latest gmed from GitHub

### Option 1: Run the Installation Script

From R or RStudio:
```r
source("install_gmed_from_github.R")
```

Or from command line:
```bash
Rscript install_gmed_from_github.R
```

### Option 2: Manual Installation

In R or RStudio:
```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install latest gmed from GitHub
devtools::install_github("fbuckhold3/gmed", force = TRUE, upgrade = "never")
```

### After Installation

1. Restart R session / Shiny app
2. The career planning display should now work correctly
3. Previous period's data will show with:
   - Career paths
   - Fellowship interests
   - Program tracks
   - Discussion topics

## What Was Already Fixed in gmed

The gmed repository already contains the correct implementation in `R/display_career_planning.R`:

### Key Fixes (Already in GitHub Version)

**1. Correct Case for Repeat Instrument** (Lines 105, 352)
```r
redcap_repeat_instrument == "s_eval"  # lowercase, not "S Eval"
```

**2. Correct Field for Period Filtering** (Lines 106, 353)
```r
redcap_repeat_instance == prev_period  # not s_e_period
```

**3. Flexible Data Structure Handling** (Lines 19-23, 323-327)
```r
# Handles both rdm_data$all_forms$s_eval AND flat rdm_data
if (!is.null(rdm_data$all_forms) && !is.null(rdm_data$all_forms$s_eval)) {
  s_eval_data <- rdm_data$all_forms$s_eval
} else {
  s_eval_data <- rdm_data
}
```

**4. Robust Period Type Handling** (Lines 83-95, 330-342)
```r
# Handles numeric, list (with period_number), and character period types
current_period_num <- if (is.numeric(current_period)) {
  current_period
} else if (is.list(current_period) && "period_number" %in% names(current_period)) {
  current_period$period_number
} else if (is.character(current_period)) {
  period_map <- c(
    "Entering Residency" = 7, "Mid Intern" = 1, "End Intern" = 2,
    "Mid PGY2" = 3, "End PGY2" = 4, "Mid PGY3" = 5, "Graduating" = 6
  )
  period_map[current_period]
} else {
  as.numeric(current_period)
}
```

## Verification

After installing the updated gmed package, verify the functions are working:

```r
# Load the package
library(gmed)

# Check function exists
exists("display_career_planning", where = asNamespace("gmed"))
exists("display_wellness", where = asNamespace("gmed"))

# Verify package version
packageVersion("gmed")
```

The app wrapper in `R/modules/wrappers/mod_career_planning_wrapper.R` already correctly calls these functions:

```r
output$previous_career_display <- renderUI({
  req(rdm_data(), period(), record_id())

  gmed::display_career_planning(
    rdm_data = rdm_data(),
    record_id = record_id(),
    current_period = period(),
    data_dict = data_dict
  )
})
```

## Troubleshooting

### Issue: Still not displaying after update

**Check 1: Verify gmed version and source**
```r
packageVersion("gmed")

# Check where package is installed from
packageDescription("gmed")$GithubRepo
packageDescription("gmed")$GithubUsername
```

**Check 2: Force reinstall from GitHub**
```r
remove.packages("gmed")
devtools::install_github("fbuckhold3/gmed", force = TRUE)
```

**Check 3: Restart R session completely**
- Close and reopen R/RStudio
- Reload all packages
- Restart the Shiny app

**Check 4: Verify data structure**
```r
# In your app, add debugging:
observe({
  req(rdm_data())
  message("Data structure: ", paste(names(rdm_data()), collapse = ", "))

  if (!is.null(rdm_data()$all_forms)) {
    message("all_forms available: ", paste(names(rdm_data()$all_forms), collapse = ", "))

    if ("s_eval" %in% names(rdm_data()$all_forms)) {
      message("s_eval rows: ", nrow(rdm_data()$all_forms$s_eval))
    }
  }
})
```

**Check 5: Verify REDCap data has previous period**
```r
# Make sure there IS data from previous period
observe({
  req(rdm_data(), record_id(), period())

  prev_period <- period() - 1

  prev_data <- rdm_data()$all_forms$s_eval %>%
    dplyr::filter(
      record_id == !!record_id(),
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == prev_period
    )

  message("Previous period (", prev_period, ") has ", nrow(prev_data), " rows")
})
```

### Issue: Function not found error

This means gmed isn't properly installed. Try:
```r
# Completely remove and reinstall
remove.packages("gmed")
.rs.restartR()  # If in RStudio

devtools::install_github("fbuckhold3/gmed")
library(gmed)
```

### Issue: Old version still loading

R may be caching the old package:
```r
# Clear package cache
detach("package:gmed", unload = TRUE)
remove.packages("gmed")

# Clear R environment
rm(list = ls())

# Reinstall
devtools::install_github("fbuckhold3/gmed", force = TRUE)

# Restart R session
.rs.restartR()  # RStudio only
# Or close and reopen R
```

## Summary

**The fix is simple:** Install the latest gmed from GitHub.

The GitHub repository at `https://github.com/fbuckhold3/gmed` already contains all the necessary fixes in `R/display_career_planning.R`.

No code changes are needed in the `imslu-resident-self-assessment` app - the wrapper module correctly calls `gmed::display_career_planning()` and `gmed::display_wellness()`.

Just run:
```r
devtools::install_github("fbuckhold3/gmed", force = TRUE)
```

Then restart your R session and Shiny app.
