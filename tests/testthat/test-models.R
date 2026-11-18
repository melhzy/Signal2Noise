# Predictions at the default slider settings, as produced by the 2019 app
# (original scripts/global.R, with the readr column-name crash patched).
baseline <- list(
  macro = c(N1 = 8.890578, N2 = 8.139578, Margalef = 4.088060),
  fish  = c(N1 = 11.92373, N2 = 10.59105, Margalef = 2.031230)
)

test_that("prepared data match the 2019 sample sizes", {
  expect_equal(nrow(app$data$macro), 115)
  expect_equal(nrow(app$data$fish), 50)
  expect_false(anyNA(app$data$macro[TAXA_PREDICTORS$macro]))
  expect_false(anyNA(app$data$fish[TAXA_PREDICTORS$fish]))
  # Imputed flags: 23 macro and 10 fish samples have no sonde readings at all.
  sonde <- "pH, WaterTemp, Turbidity, SpCond, O2Dis"
  expect_equal(sum(startsWith(app$data$macro$Imputed, sonde)), 23)
  expect_equal(sum(startsWith(app$data$fish$Imputed, sonde)), 10)
})

test_that("fish basis dimensions come from the best grid row", {
  expect_equal(unname(app$fish_k), c(5L, 6L, 3L, 5L, 6L, 5L, 4L))
})

test_that("default-scenario predictions reproduce the 2019 app", {
  for (taxa in names(TAXA)) {
    d <- app$data[[taxa]]
    nd <- scenario_frame(default_scenario(d, taxa), taxa, d)
    p <- predict_metrics(app$models[[taxa]], nd, d)
    expect_equal(setNames(p$fit, p$metric), baseline[[taxa]], tolerance = 1e-5,
                 label = paste(taxa, "predictions"))
  }
})

test_that("model fit statistics match the 2019 fits", {
  dev <- sapply(names(TAXA), function(t) sapply(app$models[[t]], function(m) summary(m)$dev.expl))
  expect_equal(round(dev[, "macro"], 3), c(N1 = 0.619, N2 = 0.542, Margalef = 0.659))
  expect_equal(round(dev[, "fish"], 3),  c(N1 = 0.738, N2 = 0.724, Margalef = 0.749))
})

test_that("confidence intervals bracket the fit", {
  d <- app$data$fish
  p <- predict_metrics(app$models$fish, scenario_frame(default_scenario(d, "fish"), "fish", d), d)
  expect_true(all(p$lwr < p$fit & p$fit < p$upr))
  expect_true(all(p$percentile >= 0 & p$percentile <= 100))
})

test_that("sensitivity helpers return well-formed output", {
  d <- app$data$macro
  base <- scenario_frame(default_scenario(d, "macro"), "macro", d)
  rc <- response_curve(app$models$macro$N1, base, d, "pH", n = 25)
  expect_equal(nrow(rc), 25)
  expect_equal(range(rc$x), range(d$pH))
  so <- response_curve(app$models$macro$N1, base, d, "StreamOrder")
  expect_setequal(so$x, as.character(1:6))
  tor <- tornado_data(app$models$macro$N1, base, d, "macro")
  expect_setequal(tor$var, c(TAXA_PREDICTORS$macro, "StreamOrder"))
  expect_true(all(tor$swing >= 0))
})

test_that("envelope check accepts sampled conditions and flags far-out ones", {
  d <- app$data$fish
  inside <- envelope_check(d[1, TAXA_PREDICTORS$fish], d, "fish")
  expect_true(inside$inside)
  far <- d[1, TAXA_PREDICTORS$fish]
  far$pH <- max(d$pH); far$WaterTemp <- min(d$WaterTemp); far$Flow <- max(d$Flow)
  far$O2Dis <- min(d$O2Dis); far$Turbidity <- max(d$Turbidity)
  expect_false(envelope_check(far, d, "fish")$inside)
})

test_that("station scenarios use station means", {
  d <- app$data$macro
  sid <- d$StationID[1]
  sc <- station_scenario(d, sid, "macro")
  expect_equal(sc$pH, mean(d$pH[d$StationID == sid]))
  expect_null(station_scenario(d, -1, "macro"))
})

test_that("cached models work in a fresh session without mgcv attached", {
  # Regression: readRDS() of a gam does not load mgcv, so summary() fell back
  # to summary.glm and crashed the app. load_app_data() must load mgcv.
  withr::local_dir(app_dir)
  rscript <- file.path(R.home("bin"), "Rscript")
  code <- paste(
    "for (f in sort(list.files('R', pattern = '[.]R$', full.names = TRUE))) source(f);",
    "a <- load_app_data();",
    "cat(class(summary(a$models$macro$N1))[1])"
  )
  script <- tempfile(fileext = ".R")
  writeLines(code, script)
  out <- system2(rscript, c("--vanilla", shQuote(script)), stdout = TRUE, stderr = FALSE)
  expect_equal(tail(out, 1), "summary.gam")
})

test_that("the cache alone is enough when the raw data are absent (browser build)", {
  # The Shinylive site ships data/model_cache.rds without DataAll.RData or the
  # grid CSV; load_app_data() must use the cache as is instead of refitting.
  cache <- normalizePath(file.path(app_dir, PATHS$cache))
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "data"))
  file.copy(cache, file.path(dir, PATHS$cache))
  withr::local_dir(dir)
  a <- load_app_data()
  expect_named(a$models, c("macro", "fish"))
  expect_equal(nrow(a$data$fish), 50)
  file.remove(PATHS$cache)
  expect_error(load_app_data(), "Cannot fit models")
})
