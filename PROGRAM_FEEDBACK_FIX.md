# Program Feedback Fix: Handle Mismatched Instance Numbers

## Problem

The program feedback module was not displaying previous period data. Investigation revealed that the underlying issue is **instance numbers don't always match period numbers** in the REDCap data.

### Example from actual data:
- **Instance 4**: `s_e_period = "End PGY2"` (Period 4) ✓ Correct
- **Instance 5**: `s_e_period = ""` (empty) ✗ Problem
- **Instance 8**: `s_e_period = "Mid PGY3"` (Should be Period 5, but stored as instance 8) ✗ Mismatch

The old code filtered by `redcap_repeat_instance` assuming it matched the period number, which failed when instance numbers were incorrect.

## Solution

Changed the filtering logic to use the **`s_e_period` field value** instead of instance numbers:

```r
# OLD (Broken):
prev_data <- s_eval_data %>%
  filter(
    record_id == !!record_id,
    redcap_repeat_instrument == "s_eval",
    redcap_repeat_instance == prev_period  # Assumes instance = period
  )

# NEW (Fixed):
prev_data <- s_eval_data %>%
  filter(
    record_id == !!record_id,
    redcap_repeat_instrument == "s_eval",
    !is.na(s_e_period),
    s_e_period == prev_period_name  # Uses period name from s_e_period field
  )
```

## Changes Made

### 1. Created new gmed function: `display_program_feedback()`
**File**: `/home/user/gmed/R/display_program_feedback.R`

- Displays previous period program feedback with styled cards
- Filters by `s_e_period` field instead of instance number
- Shows all four feedback types (Plus, Delta, Conference, Broader Issues)
- Returns NULL if no previous period data exists

### 2. Updated Program Feedback Module
**File**: `/home/user/imslu-resident-self-assessment/R/modules/mod_program_feedback.R`

**Changes**:
- Added `uiOutput("previous_feedback_display")` to show previous period feedback
- Added `output$previous_feedback_display` renderUI that calls `gmed::display_program_feedback()`
- Fixed data loading observer to:
  - Convert period to number correctly (handles reactive, list, numeric, character)
  - Map period number to period name using lookup table
  - Filter by `s_e_period` field value instead of instance
  - Load CURRENT period data (not "most recent")
- Added period mapping constants for reliable conversion

### 3. Updated reinstall script
**File**: `/home/user/imslu-resident-self-assessment/reinstall_local_gmed.R`

- Added check for `display_program_feedback()` function

## Period Name Mappings

```r
period_num_to_name <- c(
  "7" = "Entering Residency",
  "1" = "Mid Intern",
  "2" = "End Intern",
  "3" = "Mid PGY2",
  "4" = "End PGY2",
  "5" = "Mid PGY3",
  "6" = "Graduating"
)
```

## Testing

To test the fix:

1. Reinstall gmed package:
   ```r
   source("reinstall_local_gmed.R")
   ```

2. Restart R session

3. Run the app and navigate to a period where you have previous period data

4. Check that:
   - Previous period feedback displays correctly
   - Current period data loads into the form fields
   - Save functionality still works

## Future Considerations

### Should fix other display functions too

The same issue affects:
- `gmed::display_career_planning()` (lines 102-107 in display_career_planning.R)
- `gmed::display_wellness()` (lines 361-366 in display_career_planning.R)

Both still filter by `redcap_repeat_instance` instead of `s_e_period`. Consider updating these as well for consistency and robustness.

### Root cause investigation

Investigate why instance numbers don't match period numbers:
- Is the submission code setting wrong instances?
- Is there legacy data with incorrect instances?
- Should we enforce instance = period in submission code?

## Related Files

- `/home/user/gmed/R/display_career_planning.R` - Contains display_career_planning and display_wellness (both need similar fix)
- `/home/user/gmed/R/period_mapping.R` - Period mapping functions
- Period configuration constants duplicated in multiple places (consider centralizing)
