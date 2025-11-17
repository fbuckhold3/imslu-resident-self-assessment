# gmed Detail Viz Bug Fix Summary

## Problem

The detail viz module (`mod_assessment_detail_viz.R`) has two bugs:

### Bug 1: Incorrect Counting
**Current:** Counts individual field values after pivot (10 values = 5 fields × 2 rows)
**Should:** Count distinct assessments with any data in the category (2 assessments)

**Example:**
- Record 94 has 21 assessments total
- Only 2 assessments have CC Inbasket data
- Button shows "CC: Inbasket (10)" ← WRONG
- Should show "CC: Inbasket (2)" ← CORRECT

### Bug 2: Visualization Not Rendering
When clicking a category button, nothing displays (even though data exists).

---

## Root Cause

In `mod_assessment_detail_viz.R`, the button counting logic:

**Current (WRONG):**
```r
# Counts AFTER pivoting - counts individual field values
viz_data <- filtered_data %>%
  select(any_of(cat_info$fields)) %>%
  pivot_longer(...) %>%
  filter(!is.na(value), value != "")

category_count <- nrow(viz_data)  # ← Counts 10 field values
```

**Should be (CORRECT):**
```r
# Count BEFORE pivoting - counts distinct assessments
category_count <- filtered_data %>%
  select(any_of(cat_info$fields)) %>%
  filter(if_any(everything(), ~!is.na(.) & . != "" & . != "0")) %>%
  nrow()  # ← Counts 2 assessments
```

---

## Fix Needed

### Location: `gmed/R/mod_assessment_detail_viz.R`

### Change 1: Fix Button Counting (around line 80-100)

Find where category buttons are rendered in `renderUI`, change from:
```r
# WRONG - counts after pivot
viz_data <- ... %>% pivot_longer() %>% filter()
category_count <- nrow(viz_data)
```

To:
```r
# CORRECT - count assessments with any data in category fields
category_count <- filtered_data %>%
  select(any_of(cat_info$fields)) %>%
  filter(if_any(everything(), ~!is.na(.) & . != "" & . != "0")) %>%
  nrow()
```

### Change 2: Fix Visualization Rendering

In `create_scale_viz` function, ensure it:
1. Properly handles sparse data
2. Actually renders the chart when data exists
3. Shows helpful message when truly no data

Check that the function returns the plotly chart wrapped in proper div/tagList.

---

## Test Case

**Data:** Record 94, CC: Inbasket category

**Expected Results:**
- Button shows: "CC: Inbasket (2)"
- Click button → shows bar chart with 5 fields
- Chart shows values (3, 4) for the 2 assessments
- Data table below chart shows the 10 individual values

**Current (BROKEN) Results:**
- Button shows: "CC: Inbasket (10)" ← WRONG COUNT
- Click button → nothing displays ← BROKEN RENDERING

---

## Files to Modify

1. `gmed/R/mod_assessment_detail_viz.R`
   - Fix button counting logic
   - Fix visualization rendering in `create_scale_viz`

---

## After Fix

Once gmed is updated:
1. Reinstall gmed: `devtools::install("~/Documents/GitHub/gmed")`
2. Restart R in assessment app
3. Test with record 94
4. Should see correct counts and working visualizations
