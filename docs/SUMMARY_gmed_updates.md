# Summary: gmed Package Updates

## What Needs to Happen

### In gmed Repo (new chat):

1. **Add new file**: `R/mod_cc_completion.R`
   - Copy content from: `imslu-resident-self-assessment/docs/GMED_mod_cc_completion.R`

2. **Replace file**: `R/mod_assessment_viz_wrapper.R`
   - Copy content from: `imslu-resident-self-assessment/docs/GMED_UPDATED_mod_assessment_viz_wrapper.R`

3. **Edit file**: `R/mod_assessment_detail_viz.R`
   - Make 3 specific changes per: `imslu-resident-self-assessment/docs/GMED_DETAIL_VIZ_FIX.md`
   - Change 1: Line ~120: `redcap_repeat_instrument == "Assessment"` → `tolower(redcap_repeat_instrument) == "assessment"`
   - Change 2: Improve sparse data handling in `create_scale_viz` function
   - Change 3: Add data counts to category buttons in renderUI

4. **Update documentation**:
   ```r
   devtools::document()
   ```

5. **Commit and push**:
   ```bash
   git add R/mod_cc_completion.R R/mod_assessment_viz_wrapper.R R/mod_assessment_detail_viz.R NAMESPACE man/
   git commit -m "Add CC completion and fix assessment visualizations"
   git push
   ```

---

### In imslu-resident-self-assessment Repo (this chat):

**After gmed is updated**, edit `R/modules/wrappers/mod_assessment_wrapper.R`:

1. **Add** after line 40 (after `resident_name` reactive):
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

2. **Update** gmed wrapper call at line 60 - add two parameters:
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

3. **Install updated gmed**:
```r
devtools::install("~/Documents/GitHub/gmed")
```

4. **Test the app**:
   - Restart R session
   - Run app with record_id 94
   - Check Assessment Review page

---

## Expected Results

After all updates:

✅ **CC Completion Status** - New section showing quarters by academic year
✅ **Detailed Assessment Analysis** - Buttons show data counts, visualizations display
✅ **All existing components** - Plus/Delta, Charts, Questions still work

---

## Reference Files

All reference files in: `imslu-resident-self-assessment/docs/`
- `GMED_mod_cc_completion.R` - New module code
- `GMED_UPDATED_mod_assessment_viz_wrapper.R` - Replacement wrapper
- `GMED_DETAIL_VIZ_FIX.md` - 3 edits for detail viz
- `GMED_UPDATE_INSTRUCTIONS.md` - Full detailed guide

---

## Quick Workflow

1. **New chat with gmed repo** → Make the 3 file changes → Document → Commit → Push
2. **Back to this chat** → Update wrapper → Install gmed → Test
3. **Done!**
