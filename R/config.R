# Signal2Noise: shared constants ---------------------------------------------
# Programmed by Pranita, Sri, Emily, Ziyuan, and Igor (2019); modernized 2026.

APP_TITLE <- "SRBC Signal2Noise"

PATHS <- list(
  data  = file.path("data", "DataAll.RData"),
  grid  = file.path("data", "Gam_Grid_outlier_removal.csv"),
  cache = file.path("data", "model_cache.rds"),
  news  = file.path("data", "messages.csv")
)

TAXA <- c(macro = "Macroinvertebrates", fish = "Fish")
TAXA_ICON <- c(macro = "bug", fish = "fish")

METRICS <- data.frame(
  id    = c("N1", "N2", "Margalef"),
  label = c("Hill's N1", "Hill's N2", "Margalef"),
  short = c(
    "Effective number of common taxa, exp(Shannon H')",
    "Effective number of dominant taxa, 1 / Simpson's sum p²",
    "Richness adjusted for sample size, (S - 1) / ln(N)"
  ),
  icon  = c("seedling", "layer-group", "list-ol"),
  floor = c(1, 1, 0),   # smallest value the index can take

  stringsAsFactors = FALSE
)

# Continuous predictors used by the GAMs. `id` keeps the original 2019 input
# ids (ph_macro, watertemp_fish, ...). `coded` flags columns whose stored values
# look like factor level codes rather than measurements (see Help > Data notes).
PREDICTORS <- data.frame(
  var   = c("pH", "WaterTemp", "Turbidity", "SpCond", "O2Dis",
            "Oxygen", "Flow", "Alkalinity"),
  id    = c("ph", "watertemp", "turbidity", "spcond", "o2dis",
            "oxygen", "flow", "alkl"),
  label = c("pH", "Water temperature (°C)", "Turbidity",
            "Specific conductance", "Dissolved O₂ (mg/L)",
            "Oxygen (lab)", "Flow", "Alkalinity"),
  group = c(rep("Field sonde", 5), rep("Lab & hydrology", 3)),
  coded = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

TAXA_PREDICTORS <- list(
  macro = PREDICTORS$var,                 # + StreamOrder (factor)
  fish  = setdiff(PREDICTORS$var, "Alkalinity")
)

CODED_NOTE <- paste(
  "Stored values (roughly 400 to 2,400) look like category codes rather than",
  "measurements. Treat this slider as a relative index; see Help > Data notes."
)

# Observations removed as outliers in the 2019 analysis (originally by row
# position: fish rows 30 and 37, macro row 53). Matched here by station and
# timestamp; timestamps are UTC, matching the stored tz ("GMT").
OUTLIERS <- list(
  fish  = data.frame(StationID = c(2029, 1669),
                     when = c("2013-06-17 20:00", "2014-07-22 00:00")),
  macro = data.frame(StationID = 2029, when = "2013-06-17 20:00")
)

PALETTE <- list(
  primary  = "#0B6E99",
  accent   = "#D9822B",
  observed = "#8A99A8",
  grid     = "#E6EBF0",
  ink      = "#1F2A37",
  order    = c("1" = "#9FC5E8", "2" = "#6FA8DC", "3" = "#3D85C6",
               "4" = "#0B6E99", "5" = "#E69138", "6" = "#B45F06")
)

metric_label <- function(id) METRICS$label[match(id, METRICS$id)]
predictor_label <- function(var) {
  ifelse(var == "StreamOrder", "Stream order",
         PREDICTORS$label[match(var, PREDICTORS$var)])
}
input_id <- function(var, taxa) {
  if (var == "StreamOrder") return(paste0("stro_", taxa))
  paste0(PREDICTORS$id[match(var, PREDICTORS$var)], "_", taxa)
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
