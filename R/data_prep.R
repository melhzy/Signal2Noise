# Data preparation -------------------------------------------------------------
# Mirrors the 2019 pipeline in scripts/global.R (git history) step for step:
# rename lab columns, keep samples with a diversity score, mean-impute missing
# predictors, join station attributes (macro), and drop the flagged outliers.
# tests/testthat/test-models.R checks the predictions against the 2019 values.

LAB_RENAMES <- c(
  "Oxygen (mg/l)"     = "Oxygen",
  "Potassium (mg/l)"  = "Potassium",
  "Ammonia (mg/l)"    = "Ammonia",
  "Nitrate-N (mg/l)"  = "Nitrate",
  "Nitrite (mg/l)"    = "Nitrite",
  "Alkalinity (mg/l)" = "Alkalinity",
  "Flow (cfs)"        = "Flow"
)

rename_columns <- function(df, map) {
  hit <- names(df) %in% names(map)
  names(df)[hit] <- unname(map[names(df)[hit]])
  df
}

# Names of the predictors that are missing (and will be imputed) in each row.
imputed_label <- function(df, vars) {
  miss <- is.na(as.matrix(df[vars]))
  apply(miss, 1, function(r) paste(vars[r], collapse = ", "))
}

# Replace NAs with the column mean, as the original loop did.
impute_mean <- function(df, vars) {
  for (v in vars) {
    miss <- is.na(df[[v]])
    df[[v]][miss] <- mean(df[[v]], na.rm = TRUE)
  }
  df
}

drop_outliers <- function(df, outliers) {
  key <- paste(df$StationID, format(df$DateTime, "%Y-%m-%d %H:%M", tz = "UTC"))
  bad <- paste(outliers$StationID, outliers$when)
  stopifnot("outlier rows not found in data" = all(bad %in% key))
  df[!key %in% bad, , drop = FALSE]
}

station_table <- function(allDatat) {
  st <- allDatat[!duplicated(allDatat$StationID),
                 c("StationID", "Station.Name.x", "StreamOrder", "Longitude", "Latitude")]
  names(st)[2] <- "StationName"
  st$StationName <- as.character(st$StationName)
  st[order(st$StationID), ]
}

prepare_fish <- function(allData2) {
  # The 2019 code applied every lab rename except Alkalinity to the fish table.
  df <- rename_columns(allData2, LAB_RENAMES[names(LAB_RENAMES) != "Alkalinity (mg/l)"])
  df <- df[!is.na(df$Hills_N1), ]
  df$Imputed <- imputed_label(df, TAXA_PREDICTORS$fish)
  df <- impute_mean(df, TAXA_PREDICTORS$fish)
  df <- drop_outliers(df, OUTLIERS$fish)
  out <- data.frame(
    StationID   = df$StationID,
    StationName = as.character(df$StationName),
    DateTime    = df$DateTime,
    df[TAXA_PREDICTORS$fish],
    N1          = df$Hills_N1,
    N2          = df$Hills_N2,
    Margalef    = df$margalef,
    Imputed     = df$Imputed,
    check.names = FALSE
  )
  rownames(out) <- NULL
  out
}

prepare_macro <- function(allData3, allDatat) {
  df <- rename_columns(allData3, LAB_RENAMES)
  df <- df[!is.na(df$Hills_N1_macro), ]
  df$Imputed <- imputed_label(df, TAXA_PREDICTORS$macro)
  df <- impute_mean(df, TAXA_PREDICTORS$macro)
  # Responses Hills_N1/N2 come from the station table (".y" after the join),
  # Margalef from the sample table, exactly as in the 2019 models.
  df <- merge(df, allDatat, by = c("DateTime", "StationID"), all = TRUE)
  df <- drop_outliers(df, OUTLIERS$macro)
  out <- data.frame(
    StationID   = df$StationID,
    StationName = as.character(df$Station.Name.x),
    DateTime    = df$DateTime,
    StreamOrder = df$StreamOrder,
    df[TAXA_PREDICTORS$macro],
    N1          = df$Hills_N1_macro.y,
    N2          = df$Hills_N2_macro.y,
    Margalef    = df$margalef_macro,
    Imputed     = df$Imputed,
    check.names = FALSE
  )
  rownames(out) <- NULL
  out
}

load_raw_data <- function(path = PATHS$data) {
  env <- new.env()
  load(path, envir = env)
  as.list(env)[c("allData2", "allData3", "allDatat")]
}
