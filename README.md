# Longitudinal trajectories of depressive symptoms in children are influenced by baseline inflammation and HIV status
Analysis code for a longitudinal cohort study involving 1,301 participants in Uganda.

### PROJECT SUMMARY
The primary aim for the study was to determine whether the trajectories of depressive symptoms in children and adults may be influenced by HIV status or baseline inflammation.

### ASSOCIATED PUBLICATION
TBC

### CONTACT
For any questions about the code, please contact the lead investigator: Dr Arish Mudra Rakshasa-Loots ([arish.mrl@ed.ac.uk](mailto:arish.mrl@ed.ac.uk)).

### DATA DICTIONARY
Variable name | Description
--- | ---
_Basic_ | 
`PID` | Participant ID
`group` | Participant group (Children or Caregivers)
`timepoint` | Timepoint of study visit, in months (baseline = 0)
`HIV.status` | HIV status group (Participants with HIV, Participants exposed to HIV, or Participants without HIV)
_Demographic_ | 
`age` | Participant age in years
`sex` | Self-identified sex
_Clinical_ | 
`CRP` | Baseline blood C-reactive protein (CRP) in mg/L
`Glu` | Baseline blood glucose in mmol/L
`PHQ` | PHQ-9 score at relevant study visit



### GENERAL APPROACH
Trajectories of PHQ-9 scores in participants were estimated using latent growth curve modelling (`growth` in package `lavaan`). Conditional LGCM was then conducted to determine the main effects of HIV status and baseline CRP concentrations, and the interaction of these predictors. Analyses were run separately for children (n = 862) and adults (n = 439).

### DESCRIPTION OF SCRIPTS
Scripts are described below in the order in which they are run, though they are fairly independent (except `01-loaddata` is always run first).

`00-packages.R` loads the required packages, and installs them if they are not already installed.

`01-loaddata.R` loads the processed and anonymised data, assigns variables to be factors or numeric, and explores the missingness and distributions of relevant variables.

`02-demos.R` calculates the median follow-up duration for all participants, and produces publication-ready demographic characteristic tables for children and adults separately using the `gt_summary` package.

`03-traj-plots.R` produces a variety of visualisations of PHQ-9 score trajectories, including overall trajectories, and trajectories stratified by HIV status and/or CRP group (low/high/moderate).

`04-lgm-unconditional.R` fits unconditional linear growth curve models to data from children and adults separately, and extracts model fit measures for these two groups.

`05-lgm-CRP-cont.R` fits conditional linear growth curve models to data from children and adults separately, with baseline CRP and HIV status as predictors, and also includes an interaction term for these two predictors. Predicted values for intercept and slope for both groups are also estimated and exported.

`06-lgm-interaction-plots.R` plots the predicted values for intercept and slope estimated in `05-lgm-CRP-cont.R` against baseline CRP concentrations and stratified by HIV status, as an additional way of visualising interactions between these variables.
