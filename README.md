# oral-disease-gbd-analysis
Code and workflow for analyzing global burden of oral diseases in adults aged 70+ using GBD 2021 data, including trend analysis and Bayesian forecasting.

## Project Overview
This repository contains the code used in the manuscript:  
**Burden of Oral Diseases in Adults Aged 70 Years and Older: Time-Trend Analysis of Global Burden of Disease Study 2021**

This repository contains reproducible code for all analyses in the manuscript, including:
1. GBD 2021 data extraction & cleaning (for adults aged 70+)
2. Descriptive analysis (burden metrics: incidence/prevalence/DALYs, stratified by sex/region/disease type)
3. Age-standardization (using 2021 global population weights)
4. Joinpoint regression (trend change detection: APC/AAPC)
5. Decomposition analysis (drivers of burden change: risk/population growth/age structure)
6. **SDI & Cross-country Inequality Analysis**:
   - Spearman correlation (SDI vs. oral disease ASRs)
   - Cross-country inequality quantification (Slope Index of Inequality [SII], Concentration Index)
7. Bayesian Age-Period-Cohort (BAPC) modeling (2022–2040 burden projection)

## File Structure
```
oral-disease-gbd-analysis/
├── scripts/                  # Core analysis code
│   ├── 01_gbd_data_cleaning.R  # Extract GBD 2021 data (70+ population) & clean; add SDI grouping
│   ├── 02_age_standardization.R # Calculate ASIR/ASPR/ASDR (2021 global population weights)
│   ├── 03_descriptive_analysis.R # Compute burden metrics & generate basic visualizations
│   ├── 04_joinpoint_regression.R # Trend analysis (APC/AAPC calculation)
│   ├── 05_decomposition_analysis.R # Decompose burden changes (risk/population/age structure)
│   ├── 06_sdi_inequality_analysis.R # SDI grouping + Spearman correlation + Cross-country inequality (SII/Concentration Index)
│   └── 07_bapc_forecasting.R     # BAPC model (2022–2040 projections)
└── README.md                 # Repository documentation
```

## Requirements
### Software
- R (≥4.5.0)
- Joinpoint Regression Program (4.9.1.0)
- Microsoft Excel 2019 (data extraction)
- Adobe Illustrator CC 2024 (figure editing)

### R Packages
# Core analytical packages (key to study logic)
- `BAPC` (v0.0.36): Core package for Bayesian Age-Period-Cohort modeling (2040 projections)
- `INLA`: Auxiliary for Bayesian modeling (compatible with BAPC)
- `nordpred`: Auxiliary for population prediction/cohort analysis
- `ineq`: Calculation of Concentration Index (cross-country inequality analysis)
- `mgcv`: Lorenz curve fitting and spline functions (inequality analysis core)
- `epitools`: Calculation of epidemiological metrics (disease burden analysis)
- `car`: Heteroscedasticity diagnosis (regression robustness test)
- `MASS`: Robust regression (SDI-disease burden association analysis)

# General data processing packages
- `tidyverse`: Includes dplyr/tidyr/readr/purrr/stringr (data cleaning & format conversion)
- `data.table`: Efficient processing of large-scale GBD datasets
- `reshape2`/`reshape`: Data reshaping (wide/long format conversion)
- `readxl`/`openxlsx`: Reading/writing Excel-format data
- `broom`: Tidying regression results (visualization/table output)

# Visualization & spatial analysis packages
- `ggplot2`: Basic chart plotting (trend plots, correlation plots)
- `ggsci`: Academic-style color palettes (chart beautification)
- `patchwork`/`cowplot`: Multi-plot composition (result figure combination)
- `ggmap`/`maps`/`terra`/`sf`/`tmaptools`: Spatial analysis (global disease burden mapping)
- `ggbrace`: Bracket annotations for charts (visualization auxiliary)

# Auxiliary tools
- `splines`: Spline function fitting (Lorenz curve/regression models)

## Data Source
All raw data are from the **Global Burden of Disease Study 2021** (GBD 2021), accessed via the official GBD data visualization platform:  
https://vizhub.healthdata.org/gbd-results/

- **Population**: Adults aged 70+ (stratified by 5-year age intervals, sex)
- **Geographic grouping**: 5 SDI regions (low/low-middle/middle/high-middle/high), 21 global regions, 204 countries/territories
- **Metrics extracted**: Incident cases, prevalent cases, DALYs (and corresponding age-specific rates); **SDI values for each country/region**

*Note: Raw GBD data are not included in this repository (licensing restrictions). Users can replicate data extraction via the link above (select metrics: oral diseases, population: 70+, stratification: sex/region/age; add SDI as a grouping variable).*


## How to Replicate Analyses
1. **Extract GBD 2021 data**: Use the GBD platform to download data (including SDI values) and save to a local `data/` folder (create this folder locally, not in the GitHub repo).
2. **Run scripts**
3. **Joinpoint regression**: For `04_joinpoint_regression.R`, ensure Joinpoint software (v4.9.1.0) is installed and linked to R.

## Contact
For questions about the code or analysis:  
[Qianqian Feng] | [f3033644734@163.com]
