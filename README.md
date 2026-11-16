# SRBC Signal2Noise

A Shiny app that predicts stream biodiversity from water-quality conditions at
Susquehanna River Basin Commission (SRBC) monitoring stations. Six generalized
additive models (`mgcv`) link field-sonde and lab measurements to three diversity
indices (Hill's N1, Hill's N2 and Margalef) for benthic macroinvertebrates (115
samples) and fish (50 samples) collected 2008–2017.

Built at Harrisburg University by Pranita, Sri, Emily, Ziyuan and Igor (2019);
modernized in 2026.

## Run it

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "mgcv", "testthat", "withr"))
shiny::runApp()        # from this folder
```

Tested with R 4.5.2, shiny 1.12.0, bslib 0.9.0, plotly 4.11.0, DT 0.34.0 and mgcv 1.9-3.
Startup takes about two seconds because the fitted models are cached in
`data/model_cache.rds`. If the data, the k-grid file or the installed mgcv
version changes, the app refits once (about 20 s) and rewrites the cache.
To force a refit, run `Rscript scripts/build_models.R`.

## Pages

| Page | What it does |
|---|---|
| **Overview** | Headline numbers and a clickable station map. Clicking a station loads its average conditions into the scenario. |
| **Biotic Response** | Predicted indices with 95% confidence intervals, the percentile among sampled sites, an extrapolation warning, a comparison plot, and a scenario log you can download as CSV. |
| **Sensitivity** | One-at-a-time response curves for every driver, plus a tornado chart ranking the drivers. |
| **Model Diagnostics** | Deviance explained, R², GCV, smooth-term tests, observed-vs-fitted and residual plots, and a comparison of all six models. |
| **Data** | A filterable table of the modeling data with CSV download, plus notes on the data. |
| **Help** | Quick start, index definitions, modeling method, data caveats and credits. |

The Scenario sidebar (assemblage and sliders) is shared by every page.

## Layout

```
app.R                 UI + server entry point
R/                    auto-sourced by Shiny
  config.R            constants: predictors, metrics, outliers, palette
  data_prep.R         2019 data pipeline (rename, impute, join, drop outliers)
  models.R            GAM formulas, fitting, cache
  predict.R           predictions + CIs, envelope check, sensitivity helpers
  ui_helpers.R        theme, sidebar, plot styling
  mod_*.R             one Shiny module per page
data/
  DataAll.RData       source data (allData2 = fish, allData3 + allDatat = macro)
  Gam_Grid_outlier_removal.csv   fish k-grid search (GCV for 279,936 combinations)
  model_cache.rds     fitted models + prepared data (generated)
  messages.csv        items shown under the bell icon
scripts/build_models.R  force a refit
tests/testthat/       model parity + server/module tests
www/                  logos, favicon, styles.css
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

The tests check that predictions at the default settings match the 2019 app
(for example macro Hill's N1 = 8.8906 and fish Hill's N1 = 11.9237), along with
the fit statistics, the sensitivity helpers, and every page's server logic via
`shiny::testServer()`.

## Changes from the 2019 version

* **Fixed a startup crash.** readr ≥ 2.0 names the unnamed first CSV column `...1`
  instead of `X1`, so `result_out$X1 <- NULL` stopped removing it. The row index
  was then used as the spline basis size `k`, and fitting failed.
* **Faster startup: about 34 s down to about 2 s.** The data pipeline used to run
  twice (`ui_scripts.R` duplicated `global.R`). It also built an unused
  1.68-million-row `expand.grid` (≈107 MB) twice and refit all six GAMs on every
  launch. Models are now cached.
* **Filled the empty pages.** Sensitivity Analysis and Help were blank; they now
  have real content, and Model Diagnostics and Data pages are new.
* **Uncertainty.** Predictions show 95% CIs, their percentile among sampled
  sites, and warnings when the combined conditions are outside the data or the
  prediction is below an index's possible minimum.
* **Interactive station map.** It replaces two static PNGs, which were byte-identical,
  so the "fish" image actually showed macroinvertebrate data.
* **Modern UI.** bslib/Bootstrap 5 replaces shinydashboard, shinyjs and magick.
  The non-functional search box is gone, and "Bionic" is renamed "Biotic".
* **Robust data prep.** Outliers are now matched by station and timestamp
  instead of by row number.
* **Clean-up.** Removed the stale packrat library (R 3.5.3), the RStudio Connect
  `manifest.json`, the root `.RData` workspace dump, and the Visual Studio
  `.rxproj` file (all still in the first git commit).

## Known data issues

**Heavy mean imputation.** Missing predictor values were replaced with column
means, as in 2019. For many predictors this affects a large share of samples:

* Macroinvertebrates (of 115): Flow 52, Turbidity 50, Alkalinity 46, Oxygen 40,
  pH 32, and the other sonde readings 23.
* Fish (of 50): Flow 34, Oxygen 24, Turbidity 18, and the other sonde readings 10.

23 macroinvertebrate and 10 fish samples have no sonde readings at all. Those
samples all sit at the mean, which weakens what the models can learn about those
drivers. The Data page has an *Imputed* column that flags them.

**Coded lab columns.** In `DataAll.RData`, the lab columns **Oxygen**, **Flow** and **Alkalinity** hold
values of about 400–2,400. That is the same range as the lab `pH ` column, which
cannot be a real pH, so these look like factor-level codes created when the
source spreadsheet was converted to numbers. The models use the values as
stored, so the app marks those sliders as relative indices. Rebuilding
`DataAll.RData` from the raw SRBC export and refitting
(`Rscript scripts/build_models.R`) would fix this.

## License

Apache License 2.0. See [LICENSE](LICENSE).
