# Milestone Images

## Purpose
These images are the ACGME milestone subcompetency visualizations used in the milestone self-evaluation module.

## Source
Downloaded from: `fbuckhold3/gmed` repository
Path: `inst/www/milestones/`
URL: https://github.com/fbuckhold3/gmed/tree/main/inst/www/milestones

## Why Local Copy?
The gmed package's `milestone_module.R` has a bug where it looks for images in the wrong package ("imres" instead of "gmed"). To ensure images display correctly when deployed to Posit Connect Cloud, we maintain a local copy here.

The module's image path fallback order is:
1. `www/milestones/{image}` ✅ **This is where our local copies are found**
2. `milestones/{image}`
3. `system.file("www", {image}, package="imres")` ❌ Wrong package name
4. GitHub URL fallback ❌ Wrong repository URL

## Files (21 total)

### Patient Care (6)
- pc1.png - History
- pc2.png - Physical Examination
- pc3.png - Differential Diagnosis
- pc4.png - Diagnostic Testing
- pc5.png - Therapeutic Management
- pc6.png - Consultative Services

### Medical Knowledge (3)
- mk1.png - Foundational Knowledge
- mk2.png - Subspecialty Knowledge
- mk3.png - Teaching

### Systems-Based Practice (3)
- sbp1.png - Health System Management
- sbp2.png - Value-Based Care
- sbp3.png - Population Health

### Practice-Based Learning & Improvement (2)
- pbli1.png - Evidence-Based Practice
- pbli2.png - Quality Improvement

### Professionalism (4)
- prof1.png - Accountability
- prof2.png - Humanism
- prof3.png - Self-Awareness
- prof4.png - Professional Growth

### Interpersonal & Communication Skills (3)
- ics1.png - Communication with Patients
- ics2.png - Communication with Healthcare Team
- ics3.png - Communication in Difficult Situations

## Maintenance
If the gmed package is updated, these images should be refreshed from the source repository to ensure they stay in sync.

## Future Fix
Ideally, the gmed package should be updated to:
1. Use correct package name ("gmed" instead of "imres")
2. Properly register resource paths with `addResourcePath()` in package initialization
3. Update GitHub fallback URL to point to gmed repository

Until then, this local copy ensures reliable image display across all deployment environments.
