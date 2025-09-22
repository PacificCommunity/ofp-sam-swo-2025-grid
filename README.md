# SWO 2025 Grid Results

Download SWO 2025 assessment report:

- **Stock assessment of swordfish in the southwest Pacific Ocean: 2025**\
  **[WCPFC-SC21-2025/SA-WP-05](https://meetings.wcpfc.int/node/26681)**

Download SWO 2025 diagnostic model:

- Clone the **[swo-2025-diagnostic](https://github.com/PacificCommunity/ofp-sam-swo-2025-diagnostic)** repository or download as **[main.zip](https://github.com/PacificCommunity/ofp-sam-swo-2025-diagnostic/archive/refs/heads/main.zip)** file

Download SWO 2025 grid results:

- Clone the **[swo-2025-grid](https://github.com/PacificCommunity/ofp-sam-swo-2025-grid)** repository or download as **[main.zip](https://github.com/PacificCommunity/ofp-sam-swo-2025-grid/archive/refs/heads/main.zip)** file

## Grid of ensemble models

The SWO 2025 assessment used a structural uncertainty grid with 360 models:

Axis                             | Levels | Options
-------------------------------- | ------ | ---------------------------------------------------------------------
CPUE                             |      2 | **AU & NZ**, AU & PICT
Steepness                        |      3 | 0.7, **0.8**, 0.9
Proportion recruitment by region |      2 | **1:3**, 1:4
Movement                         |      3 | **Diagnostic**, Halve from 1 to 2, Halve from 2 to 1
Data weighting                   |      5 | **Francis**, Halve weight, Double weight, Halve length, Double length
Natural mortality                |      2 | **Estimated**, Fixed

## Grid results

The grid requires substantial storage and memory: the 360 `Report.sso` files take around 40 GB on a hard drive, and the 360 r4ss model objects would also take 40 GB in memory if loaded into one R session.

This challenge was solved by stripping down each r4ss model object to include only the elements required for the grid analysis. The `model_list.rds` file (5 MB) in the `model_list` folder contains a list of 360 r4ss model objects, where each grid model contains the following elements:

```
annual_time_series
derived_quants
CoVar (only one line, the correlation between SBlatest/SBmsy and Flatest/Fmsy)
Dynamic_Bzero
log_det_hessian
maximum_gradient_component
```

The [TAF](TAF) analysis starts from the `model_list` R object and the following tables of interest can be found inside the [data](TAF/data), [output](TAF/output), and [report](TAF/report) folders:

Table            |  Rows | Content
---------------- | ----- | -------------------------------------------------------------
conv.csv         |   360 | Convergence diagnostics: Hessian and gradient
refpts.csv       |   360 | Reference points: Frecent/Fmsy, SBrecent/SBmsy, etc.
refpts_table.csv |    18 | Table 4 in the SWO 2025 stock assessment report
tseries.csv      | 25920 | Time series from grid models: SB, Rec, SB/SBmsy, F/Fmsy, etc.

## Incorporating structural and estimation uncertainty

The last two rows in the `refpts_table.csv` incorporate both structural and estimation uncertainty about the key reference points Frecent/Fmsy and SBrecent/SBmsy.

The TAF analysis evaluates the estimation uncertainty in each model for the time series (`tseries_est.rds`) and reference points (`refpts_est.rds`), using Monte Carlo simulations.

## Running the TAF analysis

To run the TAF analysis of the grid results, first install TAF and r4ss if they are not already installed.

```
install.packages("TAF")
install_github("r4ss/r4ss")
```

Start R, make sure the TAF folder is the working directory, and then run:

```
library(TAF)
taf.boot()
source.all()
```
