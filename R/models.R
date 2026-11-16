# GAM specification, fitting and caching ----------------------------------------
# Six generalized additive models (mgcv): {macroinvertebrates, fish} x
# {Hill's N1, Hill's N2, Margalef}. Specifications are unchanged from 2019.

CACHE_SCHEMA <- 3L

# Fish basis dimensions came from a grid search over k = 3..8 for each of the
# seven smooths (data/Gam_Grid_outlier_removal.csv, column V1 = GCV score).
best_fish_k <- function(path = PATHS$grid) {
  grid <- utils::read.csv(path, row.names = 1)
  best <- unlist(grid[which.min(grid$V1), paste0("x", 1:7)])
  stats::setNames(as.integer(best), TAXA_PREDICTORS$fish)
}

model_formula <- function(taxa, response, fish_k = NULL) {
  rhs <- if (taxa == "macro") {
    c("s(pH, k = 10)",
      "s(WaterTemp, k = 10)",
      "s(Turbidity, k = 10, bs = 'ad')",
      "s(SpCond, k = 15, bs = 'ad')",
      "s(O2Dis, k = 10, bs = 'ad')",
      "s(Oxygen, k = 10, bs = 'ad')",
      "s(Flow, k = 15)",
      "s(Alkalinity, k = 15, bs = 'ad')",
      "StreamOrder")
  } else {
    sprintf("s(%s, k = %d)", names(fish_k), fish_k)
  }
  # Global env keeps the cached model objects small (no captured frames).
  stats::as.formula(paste(response, "~", paste(rhs, collapse = " + ")),
                    env = globalenv())
}

fit_models <- function(data, fish_k) {
  sapply(names(TAXA), function(taxa) {
    sapply(METRICS$id, function(metric) {
      mgcv::gam(model_formula(taxa, metric, fish_k),
                data = data[[taxa]], na.action = stats::na.omit)
    }, simplify = FALSE)
  }, simplify = FALSE)
}

build_app_data <- function() {
  raw <- load_raw_data()
  data <- list(
    macro = prepare_macro(raw$allData3, raw$allDatat),
    fish  = prepare_fish(raw$allData2)
  )
  fish_k <- best_fish_k()
  list(
    data     = data,
    stations = station_table(raw$allDatat),
    fish_k   = fish_k,
    models   = fit_models(data, fish_k),
    built    = Sys.time()
  )
}

cache_fingerprint <- function() {
  list(
    schema = CACHE_SCHEMA,
    mgcv   = as.character(utils::packageVersion("mgcv")),
    inputs = unname(tools::md5sum(c(PATHS$data, PATHS$grid)))
  )
}

# Fitting takes ~10-30 s, so fitted models are cached in data/model_cache.rds
# and refit only when the data, grid file or mgcv version changes.
load_app_data <- function(rebuild = FALSE, cache = PATHS$cache) {
  # Register mgcv's S3 methods (summary.gam, predict.gam, ...) before any cached
  # model is used; otherwise summary() silently dispatches to summary.glm.
  loadNamespace("mgcv")
  fp <- cache_fingerprint()
  if (!rebuild && file.exists(cache)) {
    obj <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (!is.null(obj) && identical(obj$fingerprint, fp)) return(obj)
    message("Model cache is stale or unreadable; refitting models.")
  }
  obj <- build_app_data()
  obj$fingerprint <- fp
  tryCatch(saveRDS(obj, cache),
           error = function(e) message("Could not write model cache: ", conditionMessage(e)))
  obj
}

model_summary_row <- function(model, taxa, metric) {
  s <- summary(model)
  data.frame(
    Assemblage   = TAXA[[taxa]],
    Metric       = metric_label(metric),
    n            = s$n,
    `Deviance explained` = s$dev.expl,
    `Adj. R²` = s$r.sq,
    GCV          = model$gcv.ubre,
    check.names  = FALSE
  )
}

smooth_table <- function(model) {
  st <- as.data.frame(summary(model)$s.table)
  data.frame(
    Term      = sub("^s\\((.*)\\)$", "\\1", rownames(st)),
    edf       = st$edf,
    `Ref. df` = st$Ref.df,
    F         = st$F,
    `p-value` = st$`p-value`,
    check.names = FALSE
  )
}
