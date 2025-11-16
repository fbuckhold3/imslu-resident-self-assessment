# CC Completion Status Implementation Guide

## Overview
Add a continuity clinic assessment completion tracker to the gmed package for reuse across all apps.

## Implementation Steps

### 1. Add Module to gmed Package

**File**: `gmed/R/mod_cc_completion.R`
- Copy code from: `/docs/gmed_module_cc_completion.R`
- This creates the reusable CC completion visualization module

**What it does:**
- Displays CC assessments organized by academic year
- Shows 4 quarters per year with completion status
- Color-coded: Green (completed) vs Yellow (pending)
- Shows evaluator name and date for completed assessments
- Accordion UI - expands current year by default

### 2. Update gmed Assessment Wrapper

**File**: `gmed/R/mod_assessment_viz_wrapper.R`
- Reference code from: `/docs/gmed_wrapper_update.R`
- Add `include_cc_completion` parameter (default TRUE)
- Add `resident_data` parameter for year calculation
- Call `mod_cc_completion_server()` in appropriate location

**Location in wrapper:**
```
Plus/Delta Table
↓
Assessment Charts
↓
**CC Completion Status** ← ADD HERE
↓
Detailed Assessment Viz
↓
Questions Viz (if included)
```

### 3. Update THIS App's Wrapper

**File**: `imslu-resident-self-assessment/R/modules/wrappers/mod_assessment_wrapper.R`
- Reference code from: `/docs/this_app_wrapper_update.R`
- Add `resident_info_data` reactive
- Pass it to gmed wrapper as `resident_data` parameter
- Set `include_cc_completion = TRUE`

### 4. Export the New Module (gmed package)

**File**: `gmed/NAMESPACE`
Add:
```r
export(mod_cc_completion_ui)
export(mod_cc_completion_server)
```

Or if using roxygen2, rebuild with:
```r
devtools::document()
```

### 5. Reinstall gmed Package

After making changes to gmed:
```r
# In gmed repo directory
devtools::install()

# Or from this app
devtools::install("~/path/to/gmed")
```

## Data Requirements

The module expects assessment data with these columns:
- `record_id` - Resident identifier
- `redcap_repeat_instrument` - Form type ("assessment")
- `ass_cc_quart` - Quarter number (1, 2, 3, or 4)
- `ass_date` - Assessment date
- `ass_faculty` - Evaluator name
- `ass_level` - Resident level (optional)

## Visual Design

**Completed Quarter:**
```
✓ Quarter 1                    [Green background]
Completed by Dr. Smith on Oct 15, 2024
```

**Pending Quarter:**
```
○ Quarter 2                    [Yellow background]
Not yet completed
```

**Academic Year Accordion:**
```
▼ Academic Year 2024-2025 (3 of 4 completed)
  [Quarter cards displayed]

▶ Academic Year 2023-2024 (4 of 4 completed)
  [Collapsed]

▶ Academic Year 2022-2023 (2 of 4 completed)
  [Collapsed]
```

## Benefits

1. **Reusable** - Any app using gmed can include CC completion
2. **Read-only** - Perfect for resident self-assessment viewing
3. **Clear visual status** - Easy to see completion at a glance
4. **Academic year aware** - Automatically calculates relevant years
5. **Flexible** - Can be toggled on/off per app via `include_cc_completion`

## Testing

After implementation, test with:
- Resident with complete quarters (should show green)
- Resident with partial completion (mix of green/yellow)
- Resident with no CC assessments (all yellow)
- Different academic years

## Next Steps

After CC completion is working:
1. Update gmed detail viz to handle sparse assessment data
2. Test all assessment visualizations together
3. Move on to other module fixes
