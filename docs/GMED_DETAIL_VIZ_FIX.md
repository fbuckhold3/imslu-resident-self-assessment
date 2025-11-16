# Detail Viz Fix for Sparse Assessment Data

## Problem

The detail viz has two main issues:
1. **Case-sensitive filter**: Looking for "Assessment" but data has "assessment"
2. **Sparse data handling**: Each assessment row only fills a few relevant fields

## Fix for mod_assessment_detail_viz.R

### 1. Fix the Filter (Line ~120 in renderUI section)

**CHANGE FROM:**
```r
filtered_data <- rdm_data() %>%
  dplyr::filter(
    record_id == !!record_id(),
    !is.na(redcap_repeat_instrument),
    redcap_repeat_instrument == "Assessment"  # ← PROBLEM: Capital A
  )
```

**CHANGE TO:**
```r
filtered_data <- rdm_data() %>%
  dplyr::filter(
    record_id == !!record_id(),
    !is.na(redcap_repeat_instrument),
    tolower(redcap_repeat_instrument) == "assessment"  # ← FIX: Case-insensitive
  )
```

### 2. Improve Sparse Data Handling in create_scale_viz

**FIND the create_scale_viz function** and update the data filtering section:

**CHANGE FROM:**
```r
# Current code that filters and might return empty
viz_data <- filtered_data %>%
  dplyr::select(dplyr::one_of(c("record_id", "ass_date", cat_info$fields))) %>%
  tidyr::pivot_longer(
    cols = -c(record_id, ass_date),
    names_to = "field",
    values_to = "value"
  ) %>%
  dplyr::filter(!is.na(value), value != "") %>%  # ← Removes sparse data
  dplyr::mutate(value = as.numeric(value))
```

**CHANGE TO:**
```r
# Better handling: keep partial data and show what exists
viz_data <- filtered_data %>%
  dplyr::select(dplyr::any_of(c("record_id", "ass_date", cat_info$fields))) %>%
  tidyr::pivot_longer(
    cols = -dplyr::any_of(c("record_id", "ass_date")),
    names_to = "field",
    values_to = "value"
  ) %>%
  dplyr::filter(!is.na(value), value != "", value != "0") %>%  # Keep non-zero values
  dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
  dplyr::filter(!is.na(value))  # Remove values that couldn't convert to numeric

# If still no data, return helpful message
if (nrow(viz_data) == 0) {
  return(div(
    class = "alert alert-info",
    icon("info-circle", class = "me-2"),
    strong("No data available for this category yet."),
    br(),
    sprintf("This resident has %d total assessments, but none have filled the %s fields.",
            nrow(filtered_data), cat_info$name)
  ))
}
```

### 3. Add Data Count to Category Buttons

**In the renderUI that creates category buttons** (around line ~80):

**CHANGE FROM:**
```r
actionButton(
  ns(paste0("cat_", cat_key)),
  cat_info$name,
  class = button_class
)
```

**CHANGE TO:**
```r
# Count how many assessments have data for this category
category_count <- filtered_data() %>%
  dplyr::select(dplyr::any_of(cat_info$fields)) %>%
  tidyr::pivot_longer(everything(), values_to = "value") %>%
  dplyr::filter(!is.na(value), value != "") %>%
  nrow()

actionButton(
  ns(paste0("cat_", cat_key)),
  paste0(cat_info$name, " (", category_count, ")"),  # ← Show count
  class = button_class
)
```

## Summary of Changes

1. **Case-insensitive filter**: Use `tolower()` to handle "assessment" vs "Assessment"
2. **Better empty data messages**: Show total assessments vs category-specific data
3. **Data counts on buttons**: Show how many entries exist for each category
4. **Improved filtering**: Use `dplyr::any_of()` instead of `one_of()` for better NA handling

## Testing

After making these changes, test with:
- Resident 94 (has 21 assessments with sparse data)
- Click on each category button
- Should see:
  - Counts on buttons showing available data
  - Visualizations for categories with data
  - Helpful messages for categories without data

## Files to Update in gmed

1. `/Users/fredbuckhold/Documents/GitHub/gmed/R/mod_assessment_detail_viz.R`

Make the changes above, then:
```r
devtools::document()
devtools::install()
```
