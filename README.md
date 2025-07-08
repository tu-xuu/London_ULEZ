# London Ultra Low Emission Zone (ULEZ) Analysis

This repository contains the implementation of **Weather Normalization (WN)** method and the **Augmented Synthetic Control Method (ASCM)** to evaluate the impact of the London ULEZ policy on air quality.

---

## Weather Normalization

The main script for weather normalization is `Main_code.R`, which includes:

- Data loading  
- Environment setup  
- Model training and prediction using the WN method

### Input Data:

- **Observation data**: located in the `data/` directory, e.g., `Avg1_UT.csv`, containing original air quality time series  
- **Meteorological data**: `London_MET_2000_2024.Rdata`, which includes NOAA site-level observations and ERA5 reanalysis data

### Supporting Scripts:

- `Time_processing.R`: fills missing time components and standardizes date-time formats  
- `RM_code.R`: contains the core methodology of Tuan’s weather normalization (see `Main_code.R` for usage details)

---

## Augmented Synthetic Control Method (ASCM)

The script `ASCM_NO2.R` implements the ASCM to estimate the **causal impact** of the ULEZ policy on weather-normalized NO₂ concentrations.

### Input Data:

- Located in `data/ASCM_NO2.Rdata`
- Contains preprocessed WN data from:
  - ~30 control sites across the UK  
  - 2 treated series: average **Urban Background (UB)** and **Urban Traffic (UT)** from Central London

The output includes time-varying ATT (average treatment effect on the treated), confidence intervals, and model fit statistics.

---

This combined WN-ASCM framework integrates and extends previous methodologies, enabling broader applicability across various evaluation scenarios.  

---
## References

- Tuan V. Vu et al. (2019):  
  [Weather Normalization](https://github.com/tuanvvu/Air_quality_trend_analysis)

- Eli Ben-Michael et al. (2020):  
  [Augmented Synthetic Control Method (augsynth)](https://github.com/ebenmichael/augsynth)

---

## Contact

For questions or collaborations, feel free to open an issue or contact the maintainer.
