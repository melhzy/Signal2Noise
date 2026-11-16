# Help page and shared explanatory content ----------------------------------------

imputation_summary <- function(app, taxa) {
  d <- app$data[[taxa]]
  vars <- TAXA_PREDICTORS[[taxa]]
  imp <- strsplit(d$Imputed, ", ", fixed = TRUE)
  n <- vapply(vars, function(v) sum(vapply(imp, function(x) v %in% x, logical(1))), integer(1))
  n <- sort(n[n > 0], decreasing = TRUE)
  sprintf("%s (of %d): %s", TAXA[[taxa]], nrow(d),
          paste(sprintf("%s %d", predictor_label(names(n)), n), collapse = ", "))
}

help_data_notes <- function(app) {
  tags$ul(
    class = "mb-0",
    tags$li(HTML(paste(
      "<strong>Coded lab columns.</strong> In <code>data/DataAll.RData</code>, the lab columns",
      "Oxygen, Flow and Alkalinity hold values from about 400 to 2,400, the same range as the",
      "lab <code>pH</code> column, which cannot be a measured pH. They are most likely factor",
      "level codes created when the source spreadsheet was converted to numbers. The models",
      "use these values as given, so read those three sliders as relative indices rather",
      "than mg/L or cfs. Rebuilding the data from the raw SRBC export would fix this."))),
    tags$li(tags$strong("Missing values. "),
            "Missing predictor values were replaced with the column mean before modeling",
            "(as in the 2019 analysis). This affects many samples, which therefore sit exactly",
            "at the mean (the dense tick in the Sensitivity rugs; see the Imputed column on the",
            "Data page). Imputed values per predictor:",
            tags$ul(tags$li(imputation_summary(app, "macro")),
                    tags$li(imputation_summary(app, "fish")))),
    tags$li(tags$strong("Outliers. "),
            "Two fish samples (station 2029 on 2013-06-17; station 1669 on 2014-07-22) and one",
            "macroinvertebrate sample (station 2029 on 2013-06-17) were excluded as outliers."),
    tags$li(tags$strong("Sample size. "),
            "The models use 115 macroinvertebrate and 50 fish samples, so",
            "confidence intervals are wide and stream order 1 has a single sample."),
    tags$li(tags$strong("Turbidity "),
            "includes small negative sonde readings (down to −0.6), which are kept as recorded.")
  )
}

mod_help_ui <- function(id, app) {
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)),
    card(
      card_header(icon("compass"), " Quick start"),
      tags$ol(
        tags$li("Choose ", tags$strong("Macroinvertebrates"), " or ", tags$strong("Fish"),
                " at the top of the Scenario sidebar."),
        tags$li("Move the sliders to describe stream conditions, or click a station on the",
                " Overview map to load its average conditions. ",
                tags$em("Reset to typical conditions"), " restores the defaults."),
        tags$li(tags$strong("Biotic Response:"), " predicted indices with 95% confidence",
                " intervals, the percentile among sampled sites, and an extrapolation warning",
                " when the combined conditions are unlike any sampled site. Save scenarios to",
                " the log to compare them and download them as CSV."),
        tags$li(tags$strong("Sensitivity:"), " response curves for every driver and a",
                " tornado chart ranking drivers by how much they move the prediction."),
        tags$li(tags$strong("Model Diagnostics:"), " fit statistics, smooth-term tests",
                " and residual plots."),
        tags$li(tags$strong("Data:"), " browse, filter and download the modeling data.")
      )
    ),
    card(
      card_header(icon("seedling"), " Diversity indices"),
      tags$dl(
        tags$dt("Hill's N1"),
        tags$dd(HTML(paste(
          "exp(H&prime;), where H&prime; = &minus;&Sigma; p<sub>i</sub> ln p<sub>i</sub> is",
          "Shannon entropy. The effective number of common taxa: a community of N1 equally",
          "abundant taxa would have the same entropy."))),
        tags$dt("Hill's N2"),
        tags$dd(HTML(paste(
          "1 / &Sigma; p<sub>i</sub><sup>2</sup> (inverse Simpson). The effective number of",
          "dominant taxa; it weights abundant taxa more than N1."))),
        tags$dt("Margalef's index"),
        tags$dd("(S − 1) / ln N, where S is the number of taxa and N the number of",
                " individuals. Richness adjusted for sampling effort."),
        tags$dd(class = "text-muted small mb-0", HTML(paste(
          "Higher values mean more diverse communities. p<sub>i</sub> is the proportion of",
          "individuals in taxon i.")))
      )
    ),
    card(
      card_header(icon("square-root-variable"), " How predictions are made"),
      tags$p(HTML(paste(
        "Each index is modeled with a generalized additive model (<code>mgcv::gam</code>,",
        "Gaussian family, GCV smoothing selection). Every water-quality driver enters as a",
        "smooth function, so responses can be non-linear."))),
      tags$ul(
        tags$li(tags$strong("Macroinvertebrates:"), " smooths of pH, water temperature,",
                " turbidity, specific conductance, dissolved O₂, lab oxygen, flow and",
                " alkalinity (adaptive smoothers for several), plus stream order as a",
                " categorical term."),
        tags$li(tags$strong("Fish:"), " smooths of seven drivers; each smooth's basis size",
                " (k = 3 to 8) was chosen by a grid search over all 279,936 combinations,",
                " minimizing GCV.")
      ),
      tags$p("Confidence intervals are ±1.96 standard errors of the fitted mean, conditional",
             " on the estimated smoothing parameters. They describe uncertainty in the average",
             " response, not the spread of individual samples, which is wider."),
      tags$p(class = "mb-0 text-muted small",
             "The models describe associations in observational monitoring data. They do not",
             " establish that changing one driver would cause the predicted change.")
    ),
    card(
      card_header(icon("database"), " Data notes"),
      help_data_notes(app)
    ),
    card(
      card_header(icon("users"), " Credits"),
      tags$p("Signal2Noise was built at Harrisburg University by Pranita, Sri, Emily, Ziyuan and",
             " Igor (2019) using Susquehanna River Basin Commission (SRBC) monitoring data."),
      tags$p(class = "mb-0 small text-muted",
             "2026 update: modern bslib interface, uncertainty intervals, sensitivity",
             " analysis, diagnostics, station map, scenario log, cached models and tests.")
    )
  )
}
