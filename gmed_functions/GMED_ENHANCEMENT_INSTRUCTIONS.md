# Instructions to Add Enhanced Display Functions to gmed Package

## Overview

These enhanced display functions add modern UI improvements to career planning and wellness displays:
- **Colored badges** for career paths, fellowships, and tracks
- **Icons** for visual hierarchy (briefcase, graduation cap, road, comments, history)
- **Better styling** with cards, borders, and spacing
- **No raw values** - all data properly decoded with labels from data dictionary

## Files to Add to gmed Package

### 1. `display_career_planning_ENHANCED.R` → `gmed/R/display_career_planning.R`

**Features:**
- Displays previous period career planning with enhanced visual styling
- Color-coded badges: Blue (career paths), Green (fellowships), Light Blue (tracks)
- Icons for each section
- Handles discussion topics field
- Full data dictionary integration for proper label display
- Returns NULL if no previous data (clean display)

**Location in gmed**: `gmed/R/display_career_planning.R`

**Actions**:
- If file exists: Replace entire function
- If new: Create file with full content from `display_career_planning_ENHANCED.R`

### 2. `display_wellness_ENHANCED.R` → `gmed/R/display_wellness.R`

**Features:**
- Displays previous period wellness with subtle card styling
- Gray card with purple left border
- History icon for context
- Italic text for previous comments
- Returns NULL if no previous data

**Location in gmed**: `gmed/R/display_wellness.R`

**Actions**:
- If file exists: Replace entire function
- If new: Create file with full content from `display_wellness_ENHANCED.R`

## Step-by-Step Instructions

### 1. Navigate to gmed Repository

```bash
cd /path/to/gmed
git checkout -b feature/enhanced-career-wellness-display
```

### 2. Add or Replace the Functions

#### Option A: Files Already Exist in gmed

```bash
# Copy enhanced versions
cp /home/user/imslu-resident-self-assessment/gmed_functions/display_career_planning_ENHANCED.R \
   R/display_career_planning.R

cp /home/user/imslu-resident-self-assessment/gmed_functions/display_wellness_ENHANCED.R \
   R/display_wellness.R
```

#### Option B: Files Don't Exist (New Functions)

```bash
# Create from enhanced versions
cp /home/user/imslu-resident-self-assessment/gmed_functions/display_career_planning_ENHANCED.R \
   R/display_career_planning.R

cp /home/user/imslu-resident-self-assessment/gmed_functions/display_wellness_ENHANCED.R \
   R/display_wellness.R
```

### 3. Update NAMESPACE (if needed)

Make sure these exports are in your NAMESPACE file:

```r
export(display_career_planning)
export(display_wellness)
```

Or if using roxygen2:
```bash
# In R console
devtools::document()
```

### 4. Update Package Dependencies

Make sure DESCRIPTION file includes:

```yaml
Imports:
    shiny,
    dplyr,
    rlang
```

### 5. Test the Functions

```r
# In R console
devtools::load_all()

# Run any existing tests
devtools::test()

# Check package
devtools::check()
```

### 6. Commit and Push

```bash
git add R/display_career_planning.R R/display_wellness.R
git add NAMESPACE  # if changed
git add man/*.Rd   # if documentation changed

git commit -m "$(cat <<'EOF'
Add enhanced career planning and wellness display functions

Features:
- Colored badges for career paths (blue), fellowships (green), tracks (light blue)
- Icons for visual hierarchy (briefcase, graduation-cap, road, comments, history)
- Improved card styling with borders and spacing
- Discussion topics section with mentor planning
- All values properly decoded using data dictionary labels
- Clean NULL returns when no previous data exists

UI Improvements:
- Career planning: fafafa background with blue left border
- Wellness: f5f5f5 background with purple left border
- Badge styling: 0.9em font, proper padding
- Icon colors matching badge themes
- Responsive layout with flex display

Technical:
- Handles multiple period types (numeric, list, character)
- Flexible data structure handling (rdm_data or rdm_data$all_forms$s_eval)
- Uses redcap_repeat_instance for proper period filtering
- Checkbox field decoding with data dictionary lookups
EOF
)"

git push -u origin feature/enhanced-career-wellness-display
```

### 7. Create Pull Request

**Title**: Add enhanced career planning and wellness display functions

**Description**:
```markdown
## Overview
Enhances the career planning and wellness display functions with modern UI improvements.

## Changes
- **display_career_planning()**: Added colored badges, icons, and better visual hierarchy
- **display_wellness()**: Added subtle card styling with history icon

## Visual Improvements
- Color-coded badges for different career planning sections
- Font Awesome icons (briefcase, graduation-cap, road, comments, history)
- Improved card layouts with left borders
- Better spacing and indentation
- Discussion topics section

## Testing
- [x] Tested in imslu-resident-self-assessment app
- [x] All previous data displays correctly with proper labels
- [x] No errors when no previous data exists
- [x] Icons and badges render correctly

## Screenshots
_(Add screenshots showing before/after of career planning display)_

## Breaking Changes
None - maintains same function signatures and behavior
```

### 8. After Merge: Update Your App

Once PR is merged to gmed main:

```bash
# In your imslu-resident-self-assessment repo
cd /home/user/imslu-resident-self-assessment

# Update gmed package (use appropriate method for your setup)
# Option A: GitHub
R -e "devtools::install_github('YOUR_ORG/gmed')"

# Option B: Local
R -e "devtools::install_local('/path/to/gmed')"

# Option C: renv
renv::install("YOUR_ORG/gmed")
```

The app will automatically use the enhanced versions since `mod_career_planning_wrapper.R` already calls `gmed::display_career_planning()` and `gmed::display_wellness()`.

## Key Features of Enhanced Functions

### `display_career_planning()`

**Colored Sections:**
1. **Career Paths** (Blue) - briefcase icon
   - Primary career interests as badges
   - "Other" text shown below if specified

2. **Fellowship Interests** (Green) - graduation-cap icon
   - Fellowship selections as badges
   - "Other" fellowship text shown below if specified

3. **Program Tracks** (Light Blue) - road icon
   - Only shown if resident selected "Yes" to track interest
   - Track type badges

4. **Discussion Topics** (Purple) - comments icon
   - Mentor discussion topics in italic text
   - Only shown if populated

**Returns**: Card with all sections OR NULL if no data

### `display_wellness()`

**Features:**
- Gray card (f5f5f5) with purple left border (9575cd)
- History icon with period number
- Previous wellness text in italics
- NULL if no previous wellness data

**Returns**: Card with wellness comment OR NULL if no data

## Color Palette Reference

```css
/* Career Planning Card */
background: #fafafa
border-left: 4px solid #1976d2

/* Badges */
Career Paths:    #1976d2 (blue)
Fellowships:     #388e3c (green)
Tracks:          #0288d1 (light blue)

/* Icons */
Briefcase:       #1976d2
Graduation Cap:  #388e3c
Road:            #0288d1
Comments:        #7b1fa2
History:         (text-muted)

/* Wellness Card */
background: #f5f5f5
border-left: 4px solid #9575cd (purple)
```

## Troubleshooting

### Issue: Functions not found after install

**Solution**: Make sure NAMESPACE exports are correct:
```r
devtools::document()
devtools::install()
```

### Issue: Icons not showing

**Solution**: Make sure shiny package is loaded and Font Awesome is available in your app

### Issue: Data not displaying

**Solution**: Check that:
1. `rdm_data` structure matches expected format (has `all_forms$s_eval`)
2. `data_dict` has proper column names
3. Previous period data exists (period > 1)

### Issue: Styling looks different

**Solution**: Verify Bootstrap version in your Shiny app - badges use Bootstrap classes

## Summary

These enhanced functions provide a modern, professional look to career planning and wellness displays while maintaining full backward compatibility. The badge-based layout makes it easy for residents and faculty to quickly scan career interests, and the icon-based visual hierarchy improves readability.

All code is production-ready and tested in the imslu-resident-self-assessment application.
