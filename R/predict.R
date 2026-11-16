# Prediction, uncertainty and sensitivity helpers -------------------------------

# Slider defaults reproduce the 2019 app: median pH, mean of everything else,
# and stream order = integer part of the mean order.
default_scenario <- function(data, taxa) {
  vars <- TAXA_PREDICTORS[[taxa]]
  vals <- lapply(vars, function(v) if (v == "pH") stats::median(data[[v]]) else mean(data[[v]]))
  names(vals) <- vars
  if (taxa == "macro") {
    vals$StreamOrder <- as.character(as.integer(mean(as.integer(data$StreamOrder))))
  }
  vals
}

# Turn a named list of predictor values into a one-row newdata frame.
scenario_frame <- function(values, taxa, data) {
  vars <- TAXA_PREDICTORS[[taxa]]
  nd <- as.data.frame(lapply(values[vars], as.numeric))
  if (taxa == "macro") {
    nd$StreamOrder <- factor(as.character(values$StreamOrder),
                             levels = levels(data$StreamOrder))
  }
  nd
}

predict_ci <- function(model, newdata, level = 0.95) {
  p <- mgcv::predict.gam(model, newdata, type = "response", se.fit = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  fit <- as.numeric(p$fit)
  se <- as.numeric(p$se.fit)
  data.frame(fit = fit, se = se, lwr = fit - z * se, upr = fit + z * se)
}

predict_metrics <- function(models, newdata, data) {
  rows <- lapply(METRICS$id, function(m) {
    p <- predict_ci(models[[m]], newdata)
    obs <- data[[m]]
    cbind(metric = m, label = metric_label(m), p,
          percentile = 100 * mean(obs <= p$fit),
          obs_median = stats::median(obs))
  })
  do.call(rbind, rows)
}

# Is the scenario inside the cloud of sampled conditions? Compare the distance
# from the scenario to its nearest sample (standardized predictor space) with
# the nearest-neighbour distances among the samples themselves.
envelope_check <- function(newdata, data, taxa, quantile = 0.95) {
  vars <- TAXA_PREDICTORS[[taxa]]
  X <- as.matrix(data[vars])
  mu <- colMeans(X)
  sdv <- apply(X, 2, stats::sd)
  Z <- sweep(sweep(X, 2, mu), 2, sdv, "/")
  z <- (as.numeric(newdata[1, vars]) - mu) / sdv
  d_new <- min(sqrt(colSums((t(Z) - z)^2)))
  D <- as.matrix(stats::dist(Z))
  diag(D) <- Inf
  nn <- apply(D, 1, min)
  threshold <- stats::quantile(nn, quantile, names = FALSE)
  list(distance = d_new, threshold = threshold, inside = d_new <= threshold,
       nearest_typical = stats::median(nn))
}

# One-at-a-time response curve for a single driver.
response_curve <- function(model, base, data, var, n = 120) {
  if (var == "StreamOrder") {
    lv <- levels(data$StreamOrder)
    lv <- lv[lv %in% unique(as.character(data$StreamOrder))]
    nd <- base[rep(1, length(lv)), , drop = FALSE]
    nd$StreamOrder <- factor(lv, levels = levels(data$StreamOrder))
    return(cbind(x = lv, predict_ci(model, nd)))
  }
  rng <- range(data[[var]])
  nd <- base[rep(1, n), , drop = FALSE]
  nd[[var]] <- seq(rng[1], rng[2], length.out = n)
  cbind(x = nd[[var]], predict_ci(model, nd))
}

# Tornado sensitivity: move each driver from its 10th to 90th percentile while
# holding the others at the scenario values. Stream order uses the spread
# across observed orders.
tornado_data <- function(model, base, data, taxa, probs = c(0.1, 0.9)) {
  vars <- TAXA_PREDICTORS[[taxa]]
  base_fit <- predict_ci(model, base)$fit
  rows <- lapply(vars, function(v) {
    q <- stats::quantile(data[[v]], probs, names = FALSE)
    nd <- base[c(1, 1), , drop = FALSE]
    nd[[v]] <- q
    f <- predict_ci(model, nd)$fit
    data.frame(var = v, low_x = q[1], high_x = q[2], at_low = f[1], at_high = f[2])
  })
  if (taxa == "macro") {
    rc <- response_curve(model, base, data, "StreamOrder")
    rows[[length(rows) + 1]] <- data.frame(
      var = "StreamOrder",
      low_x = rc$x[which.min(rc$fit)], high_x = rc$x[which.max(rc$fit)],
      at_low = min(rc$fit), at_high = max(rc$fit))
  }
  out <- do.call(rbind, rows)
  out$label <- predictor_label(out$var)
  out$swing <- abs(out$at_high - out$at_low)
  out$base <- base_fit
  out[order(out$swing), ]
}

station_summary <- function(data, stations) {
  agg <- stats::aggregate(cbind(N1, N2, Margalef) ~ StationID, data = data, FUN = mean)
  agg$samples <- as.vector(table(data$StationID)[as.character(agg$StationID)])
  out <- merge(agg, stations, by = "StationID", all.x = TRUE)
  out$StreamOrder <- as.character(out$StreamOrder)
  out
}

# Mean conditions at a station, clamped to the slider ranges.
station_scenario <- function(data, station_id, taxa) {
  rows <- data[data$StationID == station_id, , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  vals <- lapply(TAXA_PREDICTORS[[taxa]], function(v) mean(rows[[v]]))
  names(vals) <- TAXA_PREDICTORS[[taxa]]
  if (taxa == "macro") vals$StreamOrder <- as.character(rows$StreamOrder[1])
  vals
}
