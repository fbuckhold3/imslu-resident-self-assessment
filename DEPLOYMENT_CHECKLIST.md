# Posit Connect Cloud Deployment Checklist

## Pre-Deployment Verification

### ✅ Files Created
- [x] `manifest.json` - Package dependencies and configuration
- [x] `.rscignore` - Files to exclude from deployment
- [x] `generate_manifest.R` - Script to regenerate manifest with current package versions
- [x] `DEPLOYMENT.md` - Comprehensive deployment documentation

### ✅ Dependencies Verified

**CRAN Packages** (11 packages):
- [x] shiny, shinyjs, bslib, DT
- [x] dplyr, tidyr, purrr, lubridate
- [x] plotly, ggplot2, httr, jsonlite, data.table

**GitHub Packages** (1 package):
- [x] gmed (`fbuckhold3/gmed`)

### ✅ Configuration Files
- [x] `app.R` - Entry point exists
- [x] `R/globals.R` - Configuration loaded
- [x] `.gitignore` - Excludes .Renviron (correct)

---

## Deployment Steps

### Step 1: Local Verification (Optional but Recommended)
```r
# Update manifest with current package versions
source("generate_manifest.R")

# Test app locally
shiny::runApp()
```

### Step 2: Deploy to Posit Connect Cloud

**Option A: Using rsconnect (Recommended)**
```r
library(rsconnect)

# First time only: connect your account
rsconnect::connectUser(
  server = "cloud.posit.co",
  account = "your-account-name"
)

# Deploy
rsconnect::deployApp(
  appDir = ".",
  appName = "imslu-resident-self-assessment",
  appTitle = "IMSLU Resident Self-Assessment",
  forceUpdate = TRUE
)
```

**Option B: Using Posit Connect Cloud UI**
1. Log into Posit Connect Cloud
2. Click "Publish" → "New Content" → "Shiny Application"
3. Upload project directory or connect Git repository
4. Connect Cloud will detect `manifest.json` automatically

### Step 3: Configure Environment Variables

**CRITICAL - App will not run without these!**

In Posit Connect Cloud → Your App → Settings → Vars:

| Variable | Required | Example Value |
|----------|----------|---------------|
| `RDM_TOKEN` | ✅ YES | `YOUR_REDCAP_API_TOKEN` |
| `FAC_TOKEN` | ❌ No | `YOUR_FACULTY_TOKEN` |
| `DEBUG_MODE` | ❌ No | `true` or `false` |
| `USE_FAKE_MILESTONES` | ❌ No | `true` or `false` |

**After adding variables, RESTART the application!**

### Step 4: Test Deployed Application

- [ ] Application loads without errors
- [ ] Authentication works (test with an access code)
- [ ] REDCap data loads successfully
- [ ] Modules render correctly
- [ ] Data submission works
- [ ] No console errors (check browser developer tools)

---

## Troubleshooting Quick Reference

### Error: "RDM_TOKEN not found"
→ Set `RDM_TOKEN` environment variable in Connect Cloud
→ Restart application

### Error: "Cannot install gmed package"
→ Verify `fbuckhold3/gmed` is accessible
→ If private, configure GITHUB_PAT in environment variables

### Error: "Package not found"
→ Update package versions in `manifest.json`
→ Run `generate_manifest.R` locally to get current versions

### Error: SSL certificate errors
→ Already handled in `R/globals.R` (SSL verification disabled)
→ Check REDCap URL is accessible

---

## Post-Deployment

### Security
- [ ] Verify environment variables are set correctly
- [ ] Test with limited access codes
- [ ] Monitor logs for authentication failures

### Performance
- [ ] Check application load time
- [ ] Verify REDCap API response time
- [ ] Monitor concurrent user capacity

### Monitoring
- [ ] Set up error notifications (if available)
- [ ] Check logs regularly
- [ ] Monitor for package update alerts

---

## Quick Commands

```r
# View deployment status
rsconnect::deployments()

# Show deployment logs
rsconnect::showLogs()

# Redeploy after changes
rsconnect::deployApp(appDir = ".", forceUpdate = TRUE)

# Terminate deployment
rsconnect::terminateApp(appName = "imslu-resident-self-assessment")
```

---

## Important Notes

1. **manifest.json vs renv**: Posit Connect Cloud uses `manifest.json`, not `renv.lock`
2. **GitHub Package**: The `gmed` package from `fbuckhold3/gmed` must be accessible
3. **Environment Variables**: Must be configured in Connect Cloud UI, not `.Renviron`
4. **SSL Configuration**: Already handled in code for institutional certificates
5. **File Exclusions**: `.rscignore` prevents unnecessary files from being deployed

---

## Support

- **Full Documentation**: See `DEPLOYMENT.md`
- **Architecture**: See `CLAUDE.md`
- **Posit Docs**: https://docs.posit.co/connect/cloud/
- **rsconnect Guide**: https://rstudio.github.io/rsconnect/

---

**Ready to Deploy!** ✅

All files are configured and ready for Posit Connect Cloud deployment.
