app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(app_dir)
withr::defer(setwd(old_wd), teardown_env())
for (f in sort(list.files(file.path(app_dir, "R"), pattern = "[.]R$", full.names = TRUE))) {
  sys.source(f, envir = globalenv())
}
app <- load_app_data()
