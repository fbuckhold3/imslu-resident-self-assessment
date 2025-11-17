# Testing Custom Assessment Detail Visualization

## What Was Fixed

**Critical Bug**: The plotly chart and data table outputs were not registered with the Shiny session, causing them to not display when category buttons were clicked.

**Solution**: Refactored to properly register `output$category_plot` and `output$category_table` in the server function.

---

## Testing Checklist

### Setup
1. **Restart R Session** in Positron (Session → Restart R)
2. **Run the app**: `shiny::runApp()`
3. **Login with test resident**: Use access code for **record_id 94** (has good assessment data)

---

### Test 1: Category Buttons Display

**Navigate to**: Assessment Review page

**Expected Results**:
- [ ] Section titled "Detailed Assessment Analysis" appears
- [ ] Multiple category buttons display with counts in parentheses
- [ ] Example buttons you should see:
  - CC: Inbasket Management (2)
  - CC: Documentation (2)
  - Consults (6)
  - Inpatient: Resident (9)
  - Day Assessments (some number)

**What to Check**:
- Counts should represent **distinct assessments**, not field values
- Buttons should have nice styling (rounded, gradient on hover)
- No JavaScript errors in browser console (F12)

---

### Test 2: Consults Category

**Action**: Click the "Consults" button

**Expected Results**:
- [ ] Button becomes active (purple gradient background, white text)
- [ ] **Summary Chart** appears showing horizontal bar chart
  - Each bar represents an assessment item (e.g., "Reasoning process", "Patient care")
  - Bars show average scores
  - Hover shows mean, median, and count
- [ ] **Detailed Data** table appears below chart
  - Columns: Date, Faculty, Assessment Item, Rating
  - Sortable and searchable
  - Shows 10 rows per page by default
  - Should have ~6 assessment records visible

**What to Check**:
- Chart renders without errors
- Table shows data
- No console errors
- Assessments are from different dates/faculty

---

### Test 3: Inpatient: Resident Category

**Action**: Click the "Inpatient: Resident" button

**Expected Results**:
- [ ] Previous category button returns to outline style
- [ ] New button becomes active
- [ ] Summary chart updates with new data (~9 assessments)
- [ ] Table updates with inpatient-specific fields

**What to Check**:
- Smooth transition between categories
- Chart properly re-renders
- Table data changes completely (not mixed with previous category)

---

### Test 4: CC Categories

**Action**: Click "CC: Inbasket Management" or "CC: Documentation"

**Expected Results**:
- [ ] Chart shows continuity clinic assessment items
- [ ] Table shows ~2 assessments
- [ ] Fields specific to CC evaluations (not consult or inpatient fields)

---

### Test 5: Empty Category

**Action**: Click a category with 0 count (if any exist)

**Expected Results**:
- [ ] No crash
- [ ] Info message displays:
  - "No data available for this category yet"
  - Shows total assessments vs. category-specific data
  - Example: "You have 21 total assessments, but none have data in the Day Assessments fields"

---

### Test 6: Data Accuracy

**Action**: Pick one assessment from the table

**Verification Steps**:
1. Note the date and faculty name
2. Check against raw data (optional):
```r
app_data <- load_app_data()
app_data$all_forms$assessment %>%
  filter(record_id == 94, !is.na(ass_date)) %>%
  select(ass_date, ass_faculty, starts_with("ass_cons_")) %>%
  print(n = 20)
```

**Expected Results**:
- [ ] Dates match
- [ ] Faculty names match
- [ ] Ratings are reasonable (1-5 or text labels)

---

### Test 7: Multiple Residents

**Action**: Log out and log in with different record_id (if available)

**Expected Results**:
- [ ] Category counts change based on that resident's data
- [ ] Some categories may show (0)
- [ ] No data from previous resident appears

---

## Known Issues to Document

### Issue 1: Mixed Raw/Label Data
**Symptom**: Some ratings show as numbers (1, 2, 3), others as text ("Met your expectation", "Exceeded")

**Status**: Expected behavior - will be fixed in next iteration with label decoding

**What to Note**:
- Which fields show numbers vs. text
- Example: Consults might show "1", "2", "3" while others show labels

---

### Issue 2: Observation Fields Not Categorized
**Symptom**: Observation category button may not appear or show (0)

**Status**: Placeholder - full implementation coming

**Expected**:
- Message: "Observation visualization coming soon"
- Or button doesn't appear at all

---

## Success Criteria

✅ **Module is working correctly if**:
1. All category buttons display with correct counts
2. Clicking any button shows visualization
3. Charts render without console errors
4. Tables display assessment data
5. Switching categories updates both chart and table
6. Empty categories show helpful message instead of crashing

❌ **Report these as bugs**:
1. "Output category_plot not found" error
2. Chart area blank after clicking button
3. Table doesn't display
4. Counts are way off (10x the expected number)
5. Categories mix data from different types
6. App crashes when clicking buttons

---

## Debugging Tips

### If visualizations don't show:

**Check 1**: Browser console (F12 → Console)
```
Look for:
- "Output category_plot not found" → Output registration bug (should be fixed)
- "Cannot read property 'data' of undefined" → Data processing error
- Plotly errors → Chart configuration issue
```

**Check 2**: R Console
```
Look for:
- "Error in filter" → Data filtering issue
- "object 'field_label' not found" → Data dictionary problem
- "non-numeric argument to binary operator" → Label/raw data mixing
```

**Check 3**: Data structure
```r
# In R console while app is running:
# Access via browser console or add browser() in server
combined_data <- rdm_data()
table(combined_data$redcap_repeat_instrument)
# Should show: assessment, questions, possibly faculty_evaluation
```

---

## After Testing

**Document Results**:
1. Which categories work perfectly ✅
2. Which have minor issues ⚠️
3. Which are completely broken ❌
4. Console errors encountered
5. Data accuracy concerns

**Next Steps**:
- If all tests pass → Extract to gmed package
- If some fail → Debug and iterate
- If major failures → Revisit approach

---

## Test Record Details

**Record 94 Expected Data** (as of last check):
- Total assessments: 21
- Consults: 6 assessments
- Inpatient (Resident): 9 assessments
- CC Inbasket: 2 assessments
- CC Documentation: 2 assessments
- Some Day assessments
- Possibly Bridge, Observations

**If counts differ significantly**, may indicate:
- Data updated in REDCap
- Filtering logic issue
- Case sensitivity in redcap_repeat_instrument filter
