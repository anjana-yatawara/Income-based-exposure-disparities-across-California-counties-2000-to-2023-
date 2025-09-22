# Income‑based exposure disparities across California counties (2000–2023)

This repository contains code and data to reproduce the analysis and figures for:

> Ko, K., Rodriguez, C., and Yatawara, A. **Income‑based exposure disparities across California counties, 2000–2023: a generalizable statistical framework.**

The workflow builds population‑weighted daily and annual exposures by income group for PM2.5, NO2, O3, SO2, and CO, computes absolute and relative disparities, and fits mixed‑effects trend models on the daily low minus high series.

## Repository structure

/code
01_download_data.* # Download AQS daily summaries, Census/ACS, HDPulse
02_build_panels.* # County‑day aggregation, population joins
03_income_groups.* # Fixed 2019–2023 percentile thresholds (30/70 by default)
04_exposure_disparities.* # Daily and annual group means, disparities, sign shares
05_models_trends.* # Mixed‑effects with seasonal harmonics and ARMA residuals
06_figures_tables.* # Figures for daily series and %‑days panels, Tables 1–7
07_sensitivity.* # Thresholds, population interpolation, medians, wildfire
/data_raw/ # Unmodified downloads (or scripts to fetch on the fly)
/data_derived/ # Cleaned panels and outputs written by scripts
/output/ # Figures and tables for manuscript and supplement
/paper/ # Manuscript sources


Files are provided in R or Python as appropriate. Choose the path that matches your environment.

## Data sources

- **AQS daily summaries** (EPA AirData) for pollutant daily series  
- **Census/ACS** for county population (2010–2023)  
- **NIMHD HDPulse** for county median household income (reference period 2019–2023)  

Exact endpoints and field names are documented in `code/01_download_data.*`. Please review data use terms at the source sites.

## Quick start

1. Clone the repository:
   ```bash
   git clone https://github.com/anjana-yatawara/Income-based-exposure-disparities-across-California-counties-2000-to-2023-.git
   cd Income-based-exposure-disparities-across-California-counties-2000-to-2023-
2. Create a clean environment.
install.packages(c("tidyverse","lubridate","readr","sf","lme4","nlme","broom","ggplot2"))
# optional: renv::init() to snapshot versions
3. source("code/01_download_data.R")
source("code/02_build_panels.R")
source("code/03_income_groups.R")
source("code/04_exposure_disparities.R")
source("code/05_models_trends.R")
source("code/06_figures_tables.R")
# optional sensitivities
source("code/07_sensitivity.R")
