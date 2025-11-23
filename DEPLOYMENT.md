# Posit Connect Cloud Deployment Guide

## Overview
This guide covers deploying the IMSLU Resident Self-Assessment Application to Posit Connect Cloud.

---

## Prerequisites

### 1. Posit Connect Cloud Account
- Access to Posit Connect Cloud
- Appropriate permissions to deploy applications

### 2. GitHub Access
- The `gmed` package repository must be accessible: `fbuckhold3/gmed`
- If private, you'll need to configure GitHub credentials in Connect Cloud

### 3. REDCap API Tokens
You will need the following environment variables configured in Posit Connect Cloud:
- `RDM_TOKEN` - **REQUIRED** - REDCap API token for RDM 2.0 database
- `FAC_TOKEN` - *Optional* - REDCap API token for faculty data
- `DEBUG_MODE` - *Optional* - Set to "true" or "false" (default: "true")
- `USE_FAKE_MILESTONES` - *Optional* - Set to "true" or "false" (default: "true")

---

## Deployment Files

### ✅ manifest.json
Created and configured with:
- All CRAN package dependencies
- GitHub package dependency for `gmed` (fbuckhold3/gmed)
- R platform version specification
- Entry point: `app.R`

### ✅ app.R
Entry point for the Shiny application (already exists)

### ✅ R/ directory
All application code (already exists)

---

## Step-by-Step Deployment

### Step 1: Verify Local Setup

Before deploying, ensure the app runs locally:

```r
# Check that gmed package is installed
library(gmed)

# Verify environment variables
Sys.getenv("RDM_TOKEN")  # Should return your token

# Run the app
shiny::runApp()
```

### Step 2: Prepare for Deployment

#### Option A: Using rsconnect package (Recommended)

1. **Install rsconnect**:
   ```r
   install.packages("rsconnect")
   ```

2. **Configure your Posit Connect Cloud account**:
   ```r
   library(rsconnect)

   # Connect to Posit Cloud
   rsconnect::connectUser(
     server = "cloud.posit.co",
     account = "your-account-name"
   )
   ```

   Follow the prompts to authenticate.

3. **Deploy the application**:
   ```r
   library(rsconnect)

   # Deploy from the app directory
   rsconnect::deployApp(
     appDir = ".",
     appName = "imslu-resident-self-assessment",
     appTitle = "IMSLU Resident Self-Assessment",
     forceUpdate = TRUE,
     launch.browser = FALSE
   )
   ```

#### Option B: Using Posit Connect Cloud UI

1. Log into Posit Connect Cloud
2. Click "Publish" → "New Content"
3. Select "Shiny Application"
4. Choose "Upload" method
5. Upload the entire project directory (or connect to Git repository)
6. Connect Cloud will automatically detect `manifest.json`

### Step 3: Configure Environment Variables

**CRITICAL**: After deployment, you must configure environment variables:

1. Navigate to your deployed application in Connect Cloud
2. Go to "Settings" → "Vars"
3. Add the following environment variables:

| Variable Name | Value | Required |
|---------------|-------|----------|
| `RDM_TOKEN` | Your REDCap RDM API token | ✅ Yes |
| `FAC_TOKEN` | Your REDCap FAC API token | ❌ No |
| `DEBUG_MODE` | `true` or `false` | ❌ No (default: true) |
| `USE_FAKE_MILESTONES` | `true` or `false` | ❌ No (default: true) |

4. **Save** the configuration
5. **Restart** the application for changes to take effect

### Step 4: Verify Deployment

1. Access the deployed URL
2. Test authentication with an access code
3. Verify data loads from REDCap
4. Check that all modules render correctly
5. Test a complete workflow (entry → submission)

---

## GitHub Package Considerations

### Public Repository
If `fbuckhold3/gmed` is **public**:
- No additional configuration needed
- Connect Cloud will automatically install from GitHub

### Private Repository
If `fbuckhold3/gmed` is **private**, you need to configure GitHub credentials:

#### Method 1: GitHub Personal Access Token (PAT)

1. Create a GitHub PAT with `repo` scope:
   - Go to GitHub → Settings → Developer settings → Personal access tokens
   - Generate new token with `repo` access

2. Configure in Connect Cloud:
   - Go to your content → Settings → Vars
   - Add: `GITHUB_PAT` = your-personal-access-token

3. Update deployment to use PAT:
   ```r
   # In Connect Cloud, this will be automatically picked up
   Sys.setenv(GITHUB_PAT = "your-token")
   remotes::install_github("fbuckhold3/gmed")
   ```

#### Method 2: SSH Deploy Key

1. Generate SSH key pair
2. Add public key to `fbuckhold3/gmed` repository as deploy key
3. Configure private key in Connect Cloud settings

---

## Common Issues and Solutions

### Issue 1: "RDM_TOKEN not found"
**Symptom**: App crashes on startup with token error

**Solution**:
- Ensure `RDM_TOKEN` is set in Connect Cloud environment variables
- Restart the application after adding variables

### Issue 2: "Cannot install gmed package"
**Symptom**: Deployment fails during package installation

**Solutions**:
- Verify `fbuckhold3/gmed` repository is accessible
- If private, configure GITHUB_PAT (see above)
- Check that gmed package has valid DESCRIPTION file

### Issue 3: SSL Certificate Errors
**Symptom**: REDCap API calls fail with SSL errors

**Solution**:
- The app already includes SSL config in `R/globals.R`:
  ```r
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  ```
- This should handle institutional certificate issues
- If still failing, check REDCap URL is accessible from Connect Cloud

### Issue 4: "Package version not found"
**Symptom**: Specific package version in manifest.json not available

**Solution**:
- Update `manifest.json` with current available versions
- Run locally to check installed versions:
  ```r
  installed.packages()[c("shiny", "plotly", "DT"), "Version"]
  ```

### Issue 5: Missing Dependencies
**Symptom**: App loads but features don't work

**Solution**:
- Verify all packages from `gmed` are available
- Check `gmed` package dependencies in its DESCRIPTION file
- Manually add missing packages to `manifest.json`

---

## Updating the Deployment

### Update Application Code

```r
# After making changes to your app
rsconnect::deployApp(
  appDir = ".",
  appName = "imslu-resident-self-assessment",
  forceUpdate = TRUE
)
```

### Update Environment Variables

1. Go to Connect Cloud → Your App → Settings → Vars
2. Modify variables
3. Click "Restart" to apply changes

### Update Package Versions

1. Update `manifest.json` with new versions
2. Redeploy the application
3. Connect Cloud will install updated packages

---

## Testing Checklist

Before going live, verify:

- [ ] Application loads without errors
- [ ] Authentication works (access code validation)
- [ ] REDCap data loads successfully
- [ ] All periods (1-7) display correctly
- [ ] Scholarship module works (ADDITIVE pattern)
- [ ] Career Planning module works (OVERWRITE pattern)
- [ ] Milestone self-evaluation renders with spider plot
- [ ] ILP/Goals module works
- [ ] Program feedback saves correctly
- [ ] Environment variables are configured
- [ ] No console errors in browser developer tools
- [ ] Responsive design works on mobile/tablet
- [ ] Data submission succeeds
- [ ] REDCap receives data correctly

---

## Maintenance

### Regular Updates
- Monitor for package updates on CRAN
- Check `gmed` package for updates
- Update `manifest.json` as needed

### Monitoring
- Check Connect Cloud logs regularly
- Monitor for authentication failures
- Track REDCap API usage

### Security
- Rotate REDCap API tokens periodically
- Review access logs
- Keep packages updated for security patches

---

## Support Resources

- **Posit Connect Cloud Docs**: https://docs.posit.co/connect/cloud/
- **rsconnect Package**: https://rstudio.github.io/rsconnect/
- **Shiny Deployment Guide**: https://shiny.posit.co/r/deploy.html
- **REDCap API Docs**: Check your institutional REDCap instance

---

## Quick Reference Commands

```r
# Install deployment tools
install.packages("rsconnect")

# Check package versions
installed.packages()[, c("Package", "Version")]

# List current deployments
rsconnect::deployments()

# Deploy
rsconnect::deployApp(
  appDir = ".",
  appName = "imslu-resident-self-assessment",
  forceUpdate = TRUE
)

# Show deployment logs
rsconnect::showLogs()

# Terminate deployment
rsconnect::terminateApp(appName = "imslu-resident-self-assessment")
```

---

## Contact

For deployment issues specific to this application:
- Check CLAUDE.md for application architecture
- Review R/globals.R for configuration details
- Consult period_config.R for module structure

---

**Last Updated**: 2025-11-23
**Deployment Target**: Posit Connect Cloud
**Minimum R Version**: 4.3.0
