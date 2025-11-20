#!/bin/bash

# ============================================================================
# Script to Update gmed Package with CC Completion and Detail Viz Fixes
# Run this from: ~/Documents/GitHub/imslu-resident-self-assessment
# ============================================================================

echo "=========================================="
echo "Updating gmed Package"
echo "=========================================="
echo ""

# Paths
ASSESSMENT_APP_DIR="$HOME/Documents/GitHub/imslu-resident-self-assessment"
GMED_DIR="$HOME/Documents/GitHub/gmed"

# Check if in correct directory
if [ ! -d "$ASSESSMENT_APP_DIR/docs" ]; then
  echo "Error: Must run this script from the assessment app directory"
  echo "Current directory: $(pwd)"
  echo "Expected: $ASSESSMENT_APP_DIR"
  exit 1
fi

# Check if gmed directory exists
if [ ! -d "$GMED_DIR" ]; then
  echo "Error: gmed directory not found at $GMED_DIR"
  exit 1
fi

echo "Step 1: Copying CC completion module..."
cp "$ASSESSMENT_APP_DIR/docs/GMED_mod_cc_completion.R" "$GMED_DIR/R/mod_cc_completion.R"
echo "  ✓ Copied to: $GMED_DIR/R/mod_cc_completion.R"

echo ""
echo "Step 2: Copying updated assessment wrapper..."
cp "$GMED_DIR/R/mod_assessment_viz_wrapper.R" "$GMED_DIR/R/mod_assessment_viz_wrapper.R.backup"
echo "  ✓ Backed up original to: mod_assessment_viz_wrapper.R.backup"
cp "$ASSESSMENT_APP_DIR/docs/GMED_UPDATED_mod_assessment_viz_wrapper.R" "$GMED_DIR/R/mod_assessment_viz_wrapper.R"
echo "  ✓ Copied to: $GMED_DIR/R/mod_assessment_viz_wrapper.R"

echo ""
echo "Step 3: Detail viz fix instructions..."
echo "  ⚠️  Manual edit required: $GMED_DIR/R/mod_assessment_detail_viz.R"
echo "  📖 See instructions in: $ASSESSMENT_APP_DIR/docs/GMED_DETAIL_VIZ_FIX.md"
echo ""
echo "  You need to make 3 changes:"
echo "    1. Fix case-sensitive filter (Assessment -> assessment)"
echo "    2. Improve sparse data handling"
echo "    3. Add data counts to category buttons"

echo ""
echo "=========================================="
echo "Files copied successfully!"
echo "=========================================="
echo ""
echo "Next steps:"
echo ""
echo "1. Edit the detail viz file:"
echo "   open $GMED_DIR/R/mod_assessment_detail_viz.R"
echo "   (Follow instructions in GMED_DETAIL_VIZ_FIX.md)"
echo ""
echo "2. In R/RStudio, from gmed directory:"
echo "   cd $GMED_DIR"
echo "   R"
echo "   > devtools::document()"
echo "   > devtools::install()"
echo ""
echo "3. Commit and push gmed changes:"
echo "   cd $GMED_DIR"
echo "   git add R/mod_cc_completion.R R/mod_assessment_viz_wrapper.R R/mod_assessment_detail_viz.R"
echo "   git commit -m 'Add CC completion and fix assessment viz'"
echo "   git push"
echo ""
echo "4. Then update this assessment app wrapper:"
echo "   (Instructions in: docs/GMED_UPDATE_INSTRUCTIONS.md)"
echo ""
