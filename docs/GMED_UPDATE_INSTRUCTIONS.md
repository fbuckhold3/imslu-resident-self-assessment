# Complete gmed Package Update Instructions

## Files to Copy to gmed Repo

All files are in: `/home/user/imslu-resident-self-assessment/docs/`

### 1. Add CC Completion Module
**Copy:** `GMED_mod_cc_completion.R`
**To:** `/Users/fredbuckhold/Documents/GitHub/gmed/R/mod_cc_completion.R`

This is a brand new file - just copy it as-is.

### 2. Update Assessment Wrapper
**Copy:** `GMED_UPDATED_mod_assessment_viz_wrapper.R`
**To:** `/Users/fredbuckhold/Documents/GitHub/gmed/R/mod_assessment_viz_wrapper.R`

This **replaces** the existing file completely.

### 3. Fix Detail Viz
**Reference:** `GMED_DETAIL_VIZ_FIX.md`
**Edit:** `/Users/fredbuckhold/Documents/GitHub/gmed/R/mod_assessment_detail_viz.R`

Follow the instructions in the .md file to make 3 specific changes:
- Fix case-sensitive filter
- Improve sparse data handling
- Add data counts to category buttons

---

## Step-by-Step Process

### On Your Mac:

```bash
cd ~/Documents/GitHub/gmed

# 1. Copy the CC completion module
cp ~/path/to/imslu-resident-self-assessment/docs/GMED_mod_cc_completion.R R/mod_cc_completion.R

# 2. Copy the updated wrapper (backs up original first)
cp R/mod_assessment_viz_wrapper.R R/mod_assessment_viz_wrapper.R.backup
cp ~/path/to/imslu-resident-self-assessment/docs/GMED_UPDATED_mod_assessment_viz_wrapper.R R/mod_assessment_viz_wrapper.R

# 3. Open detail viz file to make the fixes
# Open in RStudio or your editor:
# R/mod_assessment_detail_viz.R
# Follow instructions in GMED_DETAIL_VIZ_FIX.md
```

### In RStudio (gmed project):

```r
# Update documentation and NAMESPACE
devtools::document()

# Check for any issues
devtools::check()

# Install the updated package
devtools::install()
```

### Commit to gmed repo:

```bash
cd ~/Documents/GitHub/gmed

git add R/mod_cc_completion.R
git add R/mod_assessment_viz_wrapper.R
git add R/mod_assessment_detail_viz.R
git add NAMESPACE  # If updated by devtools::document()

git commit -m "Add CC completion module and fix assessment visualizations

- Add mod_cc_completion: Track continuity clinic assessments by quarter/year
- Update mod_assessment_viz_wrapper: Include CC completion option
- Fix mod_assessment_detail_viz: Handle sparse data and case-sensitive filter

Improvements:
- CC completion shows 4 quarters/year across 3 academic years
- Detail viz now case-insensitive for redcap_repeat_instrument
- Category buttons show data counts
- Better handling of sparse assessment fields"

git push
```

---

## Update THIS App (imslu-resident-self-assessment)

After gmed is updated and installed, update the wrapper in this app:

**Edit:** `R/modules/wrappers/mod_assessment_wrapper.R`

**Add this** after the `resident_name` reactive (around line 40):

```r
# Get resident data for CC completion
resident_info_data <- reactive({
  req(rdm_data(), record_id())

  app_data <- rdm_data()

  app_data$residents %>%
    dplyr::filter(record_id == !!record_id()) %>%
    dplyr::slice(1)
})
```

**Update the gmed wrapper call** (around line 60) to include new parameters:

```r
gmed::mod_assessment_viz_wrapper_server(
  "viz_wrapper",
  rdm_data = combined_assessment_questions,
  rdm_data_raw = combined_assessment_questions,
  record_id = record_id,
  data_dict = data_dict,
  include_questions = TRUE,
  include_cc_completion = TRUE,           # ← ADD THIS
  resident_name = resident_name,
  resident_data = resident_info_data      # ← ADD THIS
)
```

---

## Testing

### In the self-assessment app:

1. **Reinstall gmed** (if not done):
   ```r
   devtools::install("~/Documents/GitHub/gmed")
   ```

2. **Restart R session** in the self-assessment app

3. **Run the app** and login with record_id 94

4. **Go to Assessment Review page** - should see:
   - ✅ Plus/Delta feedback (working already)
   - ✅ Assessment charts (working already)
   - ✅ **CC Completion Status** (NEW - accordion with years/quarters)
   - ✅ **Detailed Assessment Analysis** (FIXED - buttons with counts, visualizations showing)
   - ✅ Questions/Conference (working already)

---

## What Each Fix Does

### CC Completion Module
- Shows 4 quarters per academic year
- Tracks last 3 years
- Green badges for completed quarters with evaluator/date
- Yellow badges for pending quarters
- Accordion UI (current year expanded by default)

### Detail Viz Fix
- **Case fix**: Now finds data with lowercase "assessment"
- **Sparse data**: Shows categories even with partial data
- **Data counts**: Buttons show "(5)" to indicate 5 entries
- **Better messages**: "21 total assessments, 5 in this category"

---

## Troubleshooting

**If CC completion doesn't show:**
- Check that `include_cc_completion = TRUE` in wrapper call
- Check that `resident_info_data` reactive is defined
- Check browser console for errors

**If detail viz still shows "No data":**
- Verify the filter fix was applied (look for `tolower()`)
- Check that `source_form` column exists in combined data
- Test with record_id 94 which has known data

**If package install fails:**
- Run `devtools::document()` first
- Check for syntax errors in modified files
- Run `devtools::check()` to see specific errors

---

## Quick Reference

**gmed files to modify:**
1. `R/mod_cc_completion.R` (NEW)
2. `R/mod_assessment_viz_wrapper.R` (REPLACE)
3. `R/mod_assessment_detail_viz.R` (EDIT 3 sections)

**This app files to modify:**
1. `R/modules/wrappers/mod_assessment_wrapper.R` (ADD 2 sections)

**Commands:**
```r
# In gmed
devtools::document()
devtools::install()

# In this app
# Restart R, then run app
```
