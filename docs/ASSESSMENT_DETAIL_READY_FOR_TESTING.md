# Custom Assessment Detail Visualization - Ready for Testing

**Status**: ✅ **Code Complete - Ready for User Testing**

---

## What Was Built

A custom assessment detail visualization module that:
- Dynamically detects assessment categories from data dictionary
- Correctly counts distinct assessments (not field values)
- Displays interactive category buttons with accurate counts
- Renders plotly charts showing average scores by assessment item
- Shows detailed data tables with all assessment entries
- Handles empty categories gracefully
- Supports 11 predefined assessment types

---

## Critical Bug Fixed

**Problem**: Original implementation created render functions but never registered them with the Shiny session output object, causing:
- Blank visualization areas when buttons clicked
- "Output not found" errors
- Charts and tables not displaying

**Solution**: Refactored to:
1. Separate data processing (`create_scale_visualization_data`) from UI (`create_scale_visualization_ui`)
2. Register `output$category_plot` and `output$category_table` in main server function
3. Create reactive `current_category_data` to prepare data for both outputs
4. Properly wire outputs to UI elements

---

## Files Modified

### `/R/modules/mod_assessment_detail_custom.R` (Complete Rewrite)
**Lines**: 442 (up from initial 419)

**Key Sections**:
1. **UI Function** (lines 6-58)
   - Custom CSS for button styling
   - Card layout with header
   - Category button container
   - Visualization output container

2. **Server Function** (lines 67-317)
   - Dynamic category detection from data_dict (11 categories)
   - Assessment filtering (case-insensitive)
   - Button rendering with counts
   - Category selection tracking
   - **Output registration** (NEW - lines 254-282):
     ```r
     output$category_plot <- plotly::renderPlotly({...})
     output$category_table <- DT::renderDT({...})
     output$viz_output <- renderUI({...})
     ```

3. **Helper Functions** (lines 319-450)
   - `create_scale_visualization_data()` - Data processing and plot creation
   - `create_scale_visualization_ui()` - UI generation
   - `create_observation_visualization()` - Placeholder for observations

### `/R/modules/wrappers/mod_assessment_wrapper.R` (Updated)
**Integration**: Lines 101-109 initialize custom detail viz alongside gmed components

### `/R/globals.R` (Updated)
**Line 63**: Added `source("R/modules/mod_assessment_detail_custom.R")`

---

## Assessment Categories Implemented

1. **CC: Inbasket Management** (`^ass_cc_inb_`)
2. **CC: Documentation** (`^ass_cc_doc_`)
3. **CC: Intern Semi-Annual** (`^ass_cc_int_semi_`)
4. **CC: PGY2 Semi-Annual** (`^ass_cc_pgy2_semi_`)
5. **CC: PGY3 Semi-Annual** (`^ass_cc_pgy3_semi_`)
6. **Day Assessments** (`^ass_day_`)
7. **Consults** (`^ass_cons_`)
8. **Inpatient: Intern** (`^ass_int_ip_`)
9. **Inpatient: Resident** (`^ass_res_ip_`)
10. **Bridge Clinic** (`^ass_bridge_`)
11. **Direct Observations** (`^ass_obs_`) - Placeholder UI only

---

## How It Works

### 1. Category Detection
```r
# Scans data_dict for assessment fields
# Matches patterns like ^ass_cons_ for consults
# Only shows categories with actual fields in database
```

### 2. Assessment Counting
```r
# Counts DISTINCT assessments (rows) with data
count <- res_data %>%
  select(any_of(cat$fields)) %>%
  filter(if_any(everything(), ~!is.na(.) & . != "")) %>%
  nrow()  # Correct: counts assessments, not field values
```

### 3. Visualization Generation
```r
# When category selected:
# 1. Filter assessments to those with category data
# 2. Pivot to long format (field → value pairs)
# 3. Convert to numeric, calculate means/medians
# 4. Create plotly horizontal bar chart
# 5. Create DT table with raw data
# 6. Register outputs with session
```

### 4. Output Registration (The Fix!)
```r
# Server properly registers outputs:
output$category_plot <- renderPlotly({...})    # Chart
output$category_table <- renderDT({...})       # Table
output$viz_output <- renderUI({...})           # Container

# UI references these by ID:
plotlyOutput(ns("category_plot"))
DTOutput(ns("category_table"))
```

---

## Testing Instructions

**See**: `docs/TESTING_CUSTOM_ASSESSMENT_DETAIL.md` for comprehensive testing checklist

**Quick Start**:
1. Restart R session
2. Run app: `shiny::runApp()`
3. Login with record_id **94** (test resident with good data)
4. Navigate to "Assessment Review" page
5. Scroll to "Detailed Assessment Analysis"
6. Click category buttons (Consults, Inpatient: Resident, CC categories)
7. Verify charts and tables display

**Expected for Record 94**:
- Consults: 6 assessments
- Inpatient: Resident: 9 assessments
- CC Inbasket: 2 assessments
- CC Documentation: 2 assessments

---

## Known Limitations

### 1. Label Decoding Not Yet Implemented
**Issue**: Some fields show raw numeric values (1, 2, 3) instead of labels ("Met expectation", "Exceeded")

**Impact**: Medium - data is correct but less readable

**Fix**: Next iteration will parse `select_choices_or_calculations` from data_dict

### 2. Observation Sub-types Not Categorized
**Issue**: Observations have multiple types (physical exam, presentations, write-up, etc.) encoded in field name suffixes

**Impact**: Low - placeholder UI shows for now

**Fix**: Next iteration will detect and group observation sub-types

### 3. Mixed Raw/Label Data
**Issue**: REDCap sometimes returns labels despite `raw_or_label = "raw"` setting

**Impact**: Medium - visualization handles both, but labels aren't ideal for charting

**Status**: Being investigated

---

## Next Steps

### If Testing Passes ✅
1. Document which categories work perfectly
2. Note any data display quirks
3. Extract reusable portions to gmed package
4. Update gmed `mod_assessment_detail_viz.R` with fixes
5. Test in other apps that use gmed

### If Testing Reveals Issues ⚠️
1. Document specific failures in testing guide
2. Check browser console for JavaScript errors
3. Check R console for server errors
4. Verify data structure with diagnostics
5. Iterate on fixes

### Future Enhancements 🚀
1. **Label Decoding**: Parse choices from data_dict
   ```r
   # Extract: "1, Met | 2, Exceeded | 3, Far exceeded"
   # Create lookup: c("1" = "Met", "2" = "Exceeded", ...)
   # Apply to display_data before rendering
   ```

2. **Observation Sub-types**:
   ```r
   # Detect: ass_obs_pe (physical exam), ass_obs_pres (presentation)
   # Group by suffix
   # Create accordion or tabs for each type
   ```

3. **Trend Visualization**:
   ```r
   # Add timeline view showing scores over time
   # Plotly line chart by assessment date
   # Show improvement/decline trends
   ```

4. **Export Functionality**:
   ```r
   # Download button for filtered data
   # Excel export with charts
   # PDF report generation
   ```

---

## Git Status

**Branch**: `claude/posit-connect-setup-01LPF7w2ANaZpaecRogaJmpu`

**Commits**:
1. `c4588fb` - Fix output registration in custom assessment detail viz (CRITICAL)
2. `071b51d` - Add comprehensive testing guide

**Ready to Push**: Yes (after user testing confirms it works)

---

## Developer Notes

### Why This Approach?

**Build Here First**:
- Faster iteration (no package rebuild)
- Easy debugging with browser()
- Can test with real app data
- User can see progress immediately

**Extract to gmed Later**:
- Once stable and tested
- Reusable across multiple apps
- Maintains gmed as visualization library
- This app becomes reference implementation

### Code Quality Checklist
- [x] Proper namespace usage with `ns()`
- [x] Reactive expressions for data flow
- [x] Error handling for empty data
- [x] Graceful degradation (empty categories)
- [x] Clear separation of concerns (data/UI)
- [x] Output registration in correct scope
- [x] DRY principle (helper functions)
- [x] Commented code sections

---

## Questions for User After Testing

1. **Accuracy**: Do the assessment counts match your expectations?
2. **Performance**: Does category switching feel responsive?
3. **Usability**: Are the visualizations helpful and clear?
4. **Data**: Which fields show as numbers vs. labels?
5. **Bugs**: Any console errors or crashes?
6. **Features**: What additional views would be useful?

---

**Last Updated**: November 17, 2025
**Status**: Code complete, awaiting user testing
**Next Milestone**: Extract to gmed package after successful testing
