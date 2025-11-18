# Assessment Detail Category Module - gmed Integration

## Overview

The **Assessment Detail Category** module provides interactive category-based assessment visualization with filtered Plus/Delta feedback. This module is designed to be reusable across multiple applications through the gmed package.

## Features

- **Category Selection**: Interactive buttons for different assessment types (CC, Day Assessments, Consults, Observations, etc.)
- **Dynamic Counts**: Shows number of assessments in each category
- **Plotly Visualizations**: Interactive bar charts showing average scores by field
- **Observation Sub-types**: Drill-down capability for direct observation assessments
- **Filtered Plus/Delta Table**: Shows feedback only for selected category
- **Responsive Design**: Bootstrap styling with animations

## Installation in gmed Package

### Step 1: Copy Module to gmed

```bash
# Copy the module file to gmed package
cp docs/GMED_mod_assessment_detail_category.R \
   /Users/fredbuckhold/Documents/GitHub/gmed/R/mod_assessment_detail_category.R
```

### Step 2: Document and Build

```r
# In gmed package directory
devtools::document()
devtools::install()
```

### Step 3: Export Check

Verify the module is exported in `NAMESPACE`:

```r
export(mod_assessment_detail_category_ui)
export(mod_assessment_detail_category_server)
```

## Usage in Applications

### Basic Implementation

```r
# In your app's UI
mod_assessment_detail_category_ui("assessment_detail")

# In your app's server
mod_assessment_detail_category_server(
  "assessment_detail",
  rdm_data = app_data,
  record_id = reactive(current_record_id),
  data_dict = data_dictionary
)
```

### Integration with Assessment Wrapper

Update `mod_assessment_wrapper.R` to use the gmed module:

```r
# UI
mod_assessment_wrapper_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # gmed assessment visualizations
    gmed::mod_assessment_viz_wrapper_ui(ns("viz_wrapper")),

    # gmed category detail module
    gmed::mod_assessment_detail_category_ui(ns("detail_category"))
  )
}

# Server
mod_assessment_wrapper_server <- function(id, rdm_data, record_id, period, data_dict) {
  moduleServer(id, function(input, output, session) {

    # Prepare combined data...
    combined_data <- reactive({
      # Your data preparation code
    })

    # gmed assessment wrapper
    gmed::mod_assessment_viz_wrapper_server(
      "viz_wrapper",
      rdm_data = combined_data,
      record_id = record_id,
      data_dict = data_dict
    )

    # gmed category detail
    gmed::mod_assessment_detail_category_server(
      "detail_category",
      rdm_data = combined_data,
      record_id = record_id,
      data_dict = data_dict
    )
  })
}
```

## Data Requirements

### Required Data Structure

The `rdm_data` reactive must contain assessment data with:

**Required Columns:**
- `record_id`: Resident ID
- `redcap_repeat_instrument`: Must contain "assessment" (case-insensitive)
- `ass_date`: Assessment date
- `ass_faculty`: Faculty name
- Assessment category fields (e.g., `ass_day_*`, `ass_cc_*`, `ass_obs_*`)

**Optional Columns (for Plus/Delta table):**
- `ass_level`: Assessment level (e.g., Intern, PGY2)
- `ass_specialty`: Rotation/specialty
- `ass_plus`: Plus feedback text
- `ass_delta`: Delta feedback text

**Observation-specific:**
- `ass_obs_type`: Observation type code (1-13)

### Data Dictionary Structure

Must contain fields with:
- `field_name`: REDCap field name
- `field_label`: Human-readable label
- `field_type`: Field type (e.g., "radio", "text", "notes")

## Assessment Categories

The module auto-detects these categories based on field patterns:

| Category Key | Display Name | Field Pattern | Type |
|--------------|--------------|---------------|------|
| `cc_inbasket` | CC: Inbasket Management | `^ass_cc_inb_` | numeric_scale |
| `cc_documentation` | CC: Documentation | `^ass_cc_doc_` | numeric_scale |
| `cc_intern_semi` | CC: Intern Semi-Annual | `^ass_cc_int_semi_` | numeric_scale |
| `cc_pgy2_semi` | CC: PGY2 Semi-Annual | `^ass_cc_pgy2_semi_` | numeric_scale |
| `cc_pgy3_semi` | CC: PGY3 Semi-Annual | `^ass_cc_pgy3_semi_` | numeric_scale |
| `day_assessments` | Day Assessments | `^ass_day_` | numeric_scale |
| `consults` | Consults | `^ass_cons_` | numeric_scale |
| `inpatient_intern` | Inpatient: Intern | `^ass_int_ip_` | numeric_scale |
| `inpatient_resident` | Inpatient: Resident | `^ass_res_ip_` | numeric_scale |
| `bridge` | Bridge Clinic | `^ass_bridge_` | numeric_scale |
| `observations` | Direct Observations | `^ass_obs_` | observation |

## Observation Sub-types

When "Direct Observations" category is selected, sub-types are detected from `ass_obs_type` field:

| Code | Name |
|------|------|
| 1 | Clinical Decision Making |
| 2 | Advance Care Planning |
| 3 | Educational Session |
| 4 | Physical Exam |
| 5 | Presentation |
| 6 | Written H&P |
| 7 | Daily Notes |
| 8 | Patient Discharge |
| 9 | Patient / Family Counseling |
| 10 | Supervision of Intern/Acting Intern |
| 11 | Procedure |
| 12 | Multi-D Rounds |
| 13 | Emergency Condition (Rapid/Code) |

## Customization Options

### Custom Title

```r
gmed::mod_assessment_detail_category_ui(
  "detail",
  title = "Your Custom Title"
)
```

### Adding New Categories

To add a new category pattern, edit the `assessment_categories` reactive in the module:

```r
list(
  key = "your_category",
  name = "Your Category Display Name",
  pattern = "^ass_your_prefix_",
  type = "numeric_scale"  # or "observation"
)
```

### Custom Observation Types

Update the `obs_type_names` mapping in the `observation_types` reactive:

```r
obs_type_names <- c(
  "1" = "Clinical Decision Making",
  # Add your custom types
  "14" = "Your Custom Type"
)
```

## Dependencies

### R Packages Required:
- `shiny`
- `dplyr`
- `tidyr`
- `plotly`
- `DT`
- `htmltools`

### External Resources:
- Bootstrap CSS (for styling)
- Animate.css (for animations)
- Font Awesome (for icons)

## Troubleshooting

### No Categories Showing

**Problem**: Category buttons don't appear

**Solution**: Check that:
1. Data dictionary contains assessment fields with `^ass_` prefix
2. Assessment data has `redcap_repeat_instrument = "assessment"`
3. At least one assessment has data in category fields

### Plus/Delta Table Not Appearing

**Problem**: Feedback table doesn't show

**Solution**: Ensure `ass_plus` and `ass_delta` columns exist in data and contain non-empty values

### Observation Sub-types Not Detected

**Problem**: No observation type buttons

**Solution**: Check that `ass_obs_type` field exists and contains values 1-13

### Charts Not Rendering

**Problem**: Plotly charts don't display

**Solution**:
1. Verify field values are numeric
2. Check that at least one field has non-zero, non-NA values
3. Ensure plotly package is installed

## Testing Checklist

- [ ] Category buttons display with correct counts
- [ ] Clicking category shows visualization
- [ ] Plotly charts render correctly
- [ ] Plus/Delta table appears below chart
- [ ] Table filters to selected category
- [ ] Observation sub-types work (if applicable)
- [ ] Observation charts and tables display
- [ ] Responsive design works on different screen sizes
- [ ] Animations run smoothly

## Migration from Local Module

If converting from `mod_assessment_detail_custom.R`:

1. Replace local import:
   ```r
   # OLD
   source("R/modules/mod_assessment_detail_custom.R")
   mod_assessment_detail_custom_ui(...)

   # NEW
   gmed::mod_assessment_detail_category_ui(...)
   ```

2. Update server call:
   ```r
   # OLD
   mod_assessment_detail_custom_server(...)

   # NEW
   gmed::mod_assessment_detail_category_server(...)
   ```

3. Remove local module file (optional - keep for reference)

## Version History

- **v1.0.0** (2025-01): Initial gmed module creation
  - Category-based visualization
  - Filtered Plus/Delta table
  - Observation sub-type support
  - Interactive plotly charts

## Support

For issues or questions:
- Check gmed package documentation
- Review this integration guide
- Test with sample data first

## Future Enhancements

Planned features:
- [ ] Configurable category definitions via parameter
- [ ] Export charts to PDF/PNG
- [ ] Comparison across residents (program director view)
- [ ] Time-series trend visualization
- [ ] Custom color schemes
