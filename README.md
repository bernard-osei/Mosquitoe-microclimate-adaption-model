# Divergence of satellite-derived temperature metrics from mosquito resting-site temperatures of Plasmodium falciparum: evidence from indoor Anopheles populations in Ghana

**Authors:** Osei Bernard, Awoka Rubby, Naandam Samuel, Sally Jahn, Debbie Shackleton, Thomas Churcher, Michael Osae

**Affiliations:**
- Department of Mathematics, University of Cape Coast, Ghana
- Biotechnology and Nuclear Agriculture Research Institute, Ghana Atomic Energy Commission, Ghana
- Imperial College London, UK

**Status:** Work in progress
---
## Overview
This repository contains all data and analysis code for a study investigating whether mosquito resting-site temperatures (MRST)
diverge from satellite-derived temperatures (SDT) and community indoor temperatures (CIT), and 
what the downstream consequences are for estimating the extrinsic incubation period (EIP) of *Plasmodium falciparum* in Ghana.

Field data were collected from 431 *Anopheles coluzzii*, *An. gambiae*, and *An. funestus* mosquitoes sampled across 
six villages in the Coastal Savanna and Forest ecological zones of southern Ghana. The study applies Bayesian spatio-temporal
models within the INLA framework to quantify thermal differences between temperature sources, assess mosquito behavioural positioning,
and evaluate how MRST and SDT influence EIP estimation and mosquito survival across multiple EIP formulations and mortality functions.
---
## Repository Structure

```
.
├── data/
│   ├── raw_diagnostics.xlsx          # Raw field data (mosquito resting-site measurements)
│   ├── mean_EIP.rds                  # mSOS posterior EIP lookup table (17–30°C)
│   └── simulated_sat_v3.csv          # Simulated satellite temperature dataset
│
├── script/
│   ├── 01_EDR.R            # Data import, variable coding, missing value imputation
│   ├── 02_temperature_models.R       # INLA temperature association models (CIT and SDT)
│   ├── 03_temperature_difference.R   # INLA temperature difference models
│   ├── 04_height_model_analysis.R            # INLA behavioural positioning (height-from-ground) models
│   ├── 05_simple_Bayesian_reg.R           # INLA temperature association without spatial-temporal effects
│   ├── 06_EIP.R                 # EIP estimation across three models and three temperature sources and probability of surviving the EIP
│   └── functions.R                   # Custom utility functions
│
├── figures/
│   ├── main/                         # Figures included in the main manuscript
│   └── supplementary/                # Figures included in the supplementary information
│
├── table_results/         # Temperature simulation outputs
│
├── EIP_HMTP_DTR-main      # model functions to facilitate the estimation of EIP
│
└── README.md
```

---

## Data

Field data were collected across six villages in three ecological zones of Ghana during September–October 2011 (minor rainy season) and November 2011–January 2012 (dry season). At each mosquito resting site, the following variables were recorded:

| Variable | Description |
|---|---|
| `temp` | Mosquito resting-site temperature (MRST, °C) |
| `temp_indoor` | Average community indoor temperature (CIT, °C) |
| `sat_temp` | Mean satellite-derived temperature (SDT, °C) from ERA5-Land |
| `sat_temp_min` | Minimum daily SDT (°C) |
| `sat_temp_max` | Maximum daily SDT (°C) |
| `rh` | Relative humidity at the resting site (%) |
| `li` | Light intensity at the resting site (lux) |
| `ws` | Wind speed at the resting site (m/s) |
| `height_from_ground` | Resting height above ground (cm) |
| `dist_from_nearest_wall` | Distance from the nearest wall (m) |
| `resting_surface` | Surface material category (R1–R4) |
| `species` | Mosquito species (*An. coluzzii*, *An. gambiae*, *An. funestus*) |
| `sp_elisa` | Sporozoite ELISA status (positive/negative) |
| `village` | Study village |
| `ecological_zone` | Forest / Coastal Savanna / Forest-Savanna transition |

Satellite-derived climate metrics were extracted from the **ERA5-Land reanalysis dataset** at 1° spatial resolution (~10 km), matched to sampling dates.

---

## Methods Summary
### Statistical models
All Bayesian models were fitted using the **INLA** framework (`R-INLA` package). Four model types were implemented:

1. **Temperature association models** — Bayesian regression of MRST on CIT and three SDT metrics, with and without spatial-temporal random effects
2. **Temperature difference models** — Bayesian regression of pairwise MRST–CIT and MRST–SDT differences on relative humidity, light intensity, and species
3. **Behavioural positioning models** — Bayesian regression of resting height on CIT, surface type, species, light intensity, and sporozoite ELISA status
4. **EIP and survival models** — Three EIP formulations (mSOS, degree-day, Brière) crossed with three mortality functions (Martens 2, Neil, Mordecai) across three temperature sources (raw SDT, simulated MRST, observed MRST)

### EIP models
| Model | Reference |
|---|---|
| Mechanistic model of sporogony (mSOS) | Stopard et al. (2021); Suh et al. (2024) |
| Degree-day | Detinova (1962); Macdonald (1957) |
| Brière thermal performance curve | Johnson et al. (2015); Mordecai et al. (2013) |

### Mortality functions
| Function | Reference |
|---|---|
| Martens 2 (primary) | Martens (1997) |
| Neil (sensitivity) | — |
| Mordecai quadratic (sensitivity) | Mordecai et al. (2013) |

---

## Requirements

All analyses were conducted in **R**. The following packages are required:

```r
# Bayesian modelling
install.packages("INLA", repos = c(getOption("repos"),
  INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)

# Data manipulation
install.packages(c("tidyverse", "readxl", "janitor", "lubridate", "zoo", "rio"))

# Visualisation
install.packages(c("ggplot2", "patchwork", "RColorBrewer", "ggnewscale"))

# Modelling utilities
install.packages(c("deSolve", "zipfR", "GoFKernel", "foreach", "doParallel"))
```

R version 4.0 or later is recommended.

---

## Reproducing the Analysis

Clone the repository and run the scripts in numbered order from the `script/` directory:

Each script saves its outputs to the `figures/` or `table_results/` directories. Runtime for the INLA models is approximately 5–15 minutes per model on a standard laptop.

---
## Key Findings

- Mosquito resting-site temperatures were systematically warmer than satellite-derived estimates across all six villages, with mean SDT ranging 24.1–27.9°C compared with 25.8–32.5°C for observed MRST
- Relative humidity was the primary driver of the MRST–SDT thermal divergence, with the offset exceeding 5°C under dry conditions (~50% RH)
- Using SDT overestimated the mSOS EIP by a mean of 2.06 days, with larger overestimates under the degree-day (2.86 days) and Brière (4.31 days) formulations
- Resting height was negatively associated with indoor temperature and differed significantly by surface type, with mosquitoes resting lower on textile surfaces and higher on man-made hard surfaces
- Survival probability remained consistently low (≤ 0.44) and was more sensitive to mortality function choice than to EIP model choice

---

## Citation

If you use this code or data, please cite:

> Osei Bernard, Awoka Rubby, Naandam Samuel, Sally Jahn, Debbie Shackleton, Thomas Churcher, Michael Osae.
> *Divergence of satellite-derived temperature metrics from mosquito resting-site temperatures of Plasmodium falciparum:
>  evidence from indoor Anopheles populations in Ghana.* [Journal name, year — to be updated upon acceptance]

---
## Contact

**Corresponding author:** Osei Bernard
**Email:** [bernard.osei002@stu.ucc.edu.gh] <!-- replace with your actual email -->
**Institution:** Department of Mathematics, University of Cape Coast / Biotechnology and Nuclear Agriculture Research Institute, Ghana Atomic Energy Commission

---

## Licence

This repository is made available under the [MIT Licence](LICENSE). The field data are made available for research and non-commercial use. 
Please contact the corresponding author before using the data in a new publication.

---
