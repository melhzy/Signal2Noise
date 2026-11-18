# Build the GitHub Pages site into _site/:
#   _site/index.html  project page (sources in site/)
#   _site/app/        the full Shiny app, compiled with Shinylive to run in the
#                     browser on webR (no R server needed)
# Run from the app folder:  Rscript scripts/build_site.R
# Preview locally:          Rscript -e "httpuv::runStaticServer('_site')"
# Needs install.packages("shinylive"). The first run downloads the Shinylive
# web assets and WebAssembly builds of the app's R packages.

for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)
invisible(load_app_data())  # refreshes data/model_cache.rds if it is stale

out <- "_site"
stage <- file.path(tempdir(), "Signal2Noise")
unlink(c(out, stage), recursive = TRUE)
dir.create(file.path(stage, "data"), recursive = TRUE)

# The browser build ships only what the running app reads: the fitted models
# (with the prepared data inside the cache), not the raw RData or 11 MB grid file.
file.copy(c("app.R", "R", "www"), stage, recursive = TRUE)
file.copy(c(PATHS$cache, PATHS$news), file.path(stage, "data"))

shinylive::export(stage, out, subdir = "app", template_params = list(
  title = "Signal2Noise · live app",
  include_in_head = '<link rel="icon" href="../assets/favicon.jpg">'
))

site_files <- list.files("site", full.names = TRUE)
file.copy(site_files, out, recursive = TRUE)
file.create(file.path(out, ".nojekyll"))  # serve folders as-is (no Jekyll)

size <- sum(file.info(list.files(out, recursive = TRUE, full.names = TRUE))$size)
cat(sprintf("Site written to %s/ (%.1f MB)\n", out, size / 2^20))
