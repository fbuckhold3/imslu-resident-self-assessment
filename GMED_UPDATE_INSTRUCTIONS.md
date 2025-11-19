# Instructions to Update gmed Package

## What Was Fixed

The `display_career_planning()` function in the gmed package had three critical issues that prevented it from working:

### 1. Wrong case for repeat instrument filter
- **Before**: `redcap_repeat_instrument == "S Eval"`
- **After**: `redcap_repeat_instrument == "s_eval"`
- **Line**: 162, 220

### 2. Wrong field name for period filtering
- **Before**: `s_e_period == previous_period_code`
- **After**: `redcap_repeat_instance == previous_period_num`
- **Lines**: 163, 221
- **Why**: REDCap uses `redcap_repeat_instance` to track which instance of a repeating form, not a custom `s_e_period` field

### 3. Inflexible data structure handling
- **Before**: Only accepted flat data frame
- **After**: Handles both `rdm_data` directly OR `rdm_data$all_forms$s_eval`
- **Lines**: 9-13
- **Why**: Different apps may structure the data differently

### 4. Improved period type handling
- **Added**: Robust handling of numeric, list (with `period_number`), and character period types
- **Lines**: 142-159
- **Why**: Different functions return period in different formats

## Files to Update in gmed Repository

The function is typically located at:
```
gmed/R/display_career_planning.R
```

Or it might be in:
```
gmed/R/rdm_display_functions.R
```

Or check:
```
gmed/R/utils_rdm.R
```

## Step-by-Step Instructions

### 1. Clone/Navigate to gmed Repository

```bash
# If you haven't cloned it yet:
cd /home/user
git clone [YOUR_GMED_REPO_URL] gmed

# Or if you already have it:
cd /home/user/gmed
```

### 2. Create a New Branch

```bash
git checkout -b fix/display-career-planning
```

### 3. Find the Function File

```bash
# Search for the function
grep -r "display_career_planning" R/
```

### 4. Replace the Function

Open the file containing `display_career_planning()` and replace the entire function with the code from:
```
/home/user/display_career_planning_FIXED.R
```

Key changes to make:
- Line ~142: Change `"S Eval"` to `"s_eval"`
- Line ~143: Change `s_e_period == display_period` to `redcap_repeat_instance == display_period`
- Line ~220: Change `"S Eval"` to `"s_eval"`
- Line ~221: Change `s_e_period == previous_period_code` to `redcap_repeat_instance == previous_period_num`
- Lines ~9-13: Add data structure handling
- Lines ~142-159: Update period type conversion

### 5. Update Documentation (if needed)

Check the function's roxygen documentation at the top and ensure it mentions:
```r
#' @param rdm_data Data frame containing REDCap data OR data structure with $all_forms$s_eval
#' @param current_period Current period number (1-7) or list with period_number
```

### 6. Test the Changes

```R
# In R console
devtools::load_all()
devtools::test()  # If you have tests
devtools::check()  # Check package integrity
```

### 7. Commit and Push

```bash
git add R/display_career_planning.R  # Or whatever file you updated
git commit -m "Fix display_career_planning to use correct REDCap field names

- Change repeat instrument filter from 'S Eval' to 's_eval' (lowercase)
- Use redcap_repeat_instance instead of non-existent s_e_period field
- Add flexible data structure handling for rdm_data formats
- Improve period type conversion (numeric, list, character)
- Resolves data loading issues in imslu-resident-self-assessment app"

git push -u origin fix/display-career-planning
```

### 8. Create Pull Request

1. Go to your gmed repository on GitHub
2. Click "Compare & pull request" for your new branch
3. Title: `Fix display_career_planning REDCap field name issues`
4. Description:
   ```
   ## Problem
   The `display_career_planning()` function was using incorrect REDCap field names:
   - Wrong case: "S Eval" instead of "s_eval"
   - Wrong field: `s_e_period` instead of `redcap_repeat_instance`

   This caused the function to return no data when called from apps.

   ## Solution
   - Fixed repeat instrument filter to lowercase "s_eval"
   - Changed period filtering to use `redcap_repeat_instance`
   - Added flexible data structure handling
   - Improved period type conversion

   ## Testing
   Tested in imslu-resident-self-assessment app with:
   - Record ID: 94
   - Period 5 (Mid PGY3)
   - Successfully displays period 4 career planning data
   ```

5. Assign reviewers and submit

### 9. After Merge: Update Your App

Once the PR is merged to gmed main branch:

```bash
# In your imslu-resident-self-assessment repo
cd /home/user/imslu-resident-self-assessment

# Update gmed package
R -e "devtools::install_github('YOUR_ORG/gmed')"

# Or if using renv:
renv::install("YOUR_ORG/gmed")
```

Then you can switch back from the inline implementation to using the gmed function:

```r
# In mod_career_planning_wrapper.R, line ~187
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

## Quick Reference: What to Search/Replace

If you want to do a quick find/replace approach:

**Find**:
```r
redcap_repeat_instrument == "S Eval"
```
**Replace with**:
```r
redcap_repeat_instrument == "s_eval"
```

**Find**:
```r
s_e_period == display_period
```
**Replace with**:
```r
redcap_repeat_instance == display_period
```

**Find**:
```r
s_e_period == previous_period_code
```
**Replace with**:
```r
redcap_repeat_instance == previous_period_num
```

**And add at the beginning** (after line 8):
```r
  # Handle different data structure formats
  if (!is.null(rdm_data$all_forms) && !is.null(rdm_data$all_forms$s_eval)) {
    s_eval_data <- rdm_data$all_forms$s_eval
  } else {
    s_eval_data <- rdm_data
  }

  # Filter to this record
  record_data <- s_eval_data %>%
    dplyr::filter(record_id == !!record_id)
```

**And replace the period conversion section** (around line 135-145) with:
```r
  # Convert period to number - handle all types
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

## Summary

The fixed function is in `/home/user/display_career_planning_FIXED.R`. Use it to replace the function in your gmed package, following the steps above. The key fixes ensure the function uses the correct REDCap field names (`s_eval` and `redcap_repeat_instance`) instead of the incorrect ones (`S Eval` and `s_e_period`).
