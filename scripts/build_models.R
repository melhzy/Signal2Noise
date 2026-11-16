# Refit all six GAMs and rewrite data/model_cache.rds.
# Run from the app folder:  Rscript scripts/build_models.R
# (The app also refits automatically when the data, grid file or mgcv version
#  changes; this script forces a rebuild and prints a summary.)

for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)

t0 <- Sys.time()
app <- load_app_data(rebuild = TRUE)
cat(sprintf("Fitted %d models in %.1f s; cache written to %s\n",
            length(unlist(app$models, recursive = FALSE)),
            as.numeric(difftime(Sys.time(), t0, units = "secs")), PATHS$cache))
cat("Fish basis dimensions (k):", paste(names(app$fish_k), app$fish_k, sep = "=", collapse = ", "), "\n\n")

rows <- unlist(lapply(names(TAXA), function(t) {
  lapply(METRICS$id, function(m) model_summary_row(app$models[[t]][[m]], t, m))
}), recursive = FALSE)
print(do.call(rbind, rows), digits = 3, row.names = FALSE)
