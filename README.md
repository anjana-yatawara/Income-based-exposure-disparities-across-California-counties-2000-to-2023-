# Income-based exposure disparities across California counties (2000-2023)

This repository contains code and data to reproduce the analysis and figures for:

> Ko, K., Rodriguez, C., and Yatawara, A. **Income-based exposure disparities across California counties, 2000-2023: a generalizable statistical framework.**

The workflow builds population-weighted daily and annual exposures by income group for PM2.5, NO2, O3, SO2, and CO, computes absolute and relative disparities, and fits mixed-effects trend models on the daily low minus high series. Scripts are designed so that a new state or a different socioeconomic stratifier can be substituted with minimal changes.

---

## Repository structure

    /code
      01_download_data.*         # Download AQS daily summaries, Census or ACS, HDPulse
      02_build_panels.*          # County-day aggregation, population joins
      03_income_groups.*         # Fixed 2019-2023 percentile thresholds (30/70 by default)
      04_exposure_disparities.*  # Daily and annual group means, disparities, sign shares
      05_models_trends.*         # Mixed-effects with seasonal harmonics and ARMA residuals
      06_figures_tables.*        # Figures for daily series and percent-days panels, Tables 1-7
      07_sensitivity.*           # Thresholds, population interpolation, medians, wildfire checks
    /data_raw/                   # Unmodified downloads or scripts to fetch on the fly
    /data_derived/               # Cleaned panels and outputs written by scripts
    /output/                     # Figures and tables for manuscript and supplement
    /paper/                      # Manuscript sources (optional)

Files are provided in R or Python. The asterisk indicates .R or .py counterparts.

---

## Data sources

- EPA AQS daily summaries for pollutant daily series  
- U.S. Census and ACS for county population (2010-2023)  
- NIMHD HDPulse for county median household income (reference period 2019-2023)

Exact endpoints, field names, and query parameters are declared in `code/01_download_data.*`. Review data use terms at the source sites.

---

## Set up

1) Clone the repo

    git clone https://github.com/anjana-yatawara/Income-based-exposure-disparities-across-California-counties-2000-to-2023-.git
    cd Income-based-exposure-disparities-across-California-counties-2000-to-2023-

2) Create an environment

R path

    install.packages(c(
      "tidyverse","lubridate","readr","sf",
      "lme4","nlme","broom","ggplot2","data.table"
    ))
    # optional: renv::init() to snapshot versions

Python path

    python -m venv venv
    source venv/bin/activate            # on Windows: venv\Scripts\activate
    pip install pandas numpy scipy statsmodels matplotlib pyjanitor

3) Configure project-level options

- Set an environment variable if you want to cache downloads, for example  
  `export AQS_CACHE=data_raw/aqs_cache`  
- If using API-backed downloads, supply keys in a `.env` file or through your shell when required by your local script variants.

---

## Quick start

Run the full pipeline in R

    Rscript code/01_download_data.R
    Rscript code/02_build_panels.R
    Rscript code/03_income_groups.R
    Rscript code/04_exposure_disparities.R
    Rscript code/05_models_trends.R
    Rscript code/06_figures_tables.R
    # optional sensitivities
    Rscript code/07_sensitivity.R


Outputs are written to `/data_derived` and `/output`.

---

## Reproducing key manuscript objects

- Table 1: annual exposures by income group within five time windows  
- Table 2: annual disparity metrics for PM2.5 (Appendix contains the others)  
- Table 3: mixed-effects trend estimates with ARMA residual selection  
- Figures 2, 4, 6, 8, 10: daily exposure series by group with sign-share overlay  
- Figures 3, 5, 7, 9, 11: percent of days per year with low greater than high

Run `code/06_figures_tables.*` to export all tables and figures into `/output`.

---


## Pollutant definitions and units

Daily statistic: AQS daily mean concentration for all five pollutants.  
Units: PM2.5 in µg/m³, NO2 and SO2 in ppb, O3 and CO in ppm.  
Exact field names are set in `code/02_build_panels.*`.

---

## Reuse and extensibility

- To apply the framework to another state, update the geographic layer and data pulls in `01_download_data.*` and `02_build_panels.*`.  
- To stratify by poverty or race and ethnicity instead of income, replace the grouping variable in `03_income_groups.*` and propagate through `04_exposure_disparities.*`.

---

## License

- Code: MIT License  
- Figures and text: CC BY 4.0

See `LICENSE` files where applicable.

---

## How to cite

If you use this code or data, cite the paper and this repository.

    Ko K, Rodriguez C, Yatawara A. Income-based exposure disparities across California counties, 2000-2023. 2025. GitHub repository: anjana-yatawara/Income-based-exposure-disparities-across-California-counties-2000-to-2023-

A BibTeX entry will be added upon publication.

---

## Contact

Questions and issues: open a GitHub issue or email the corresponding author at ayatawara@csub.edu.
