# Exercise the full server function headlessly with shiny::testServer().

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
})

app_obj <- shinyAppDir(app_dir)

macro_inputs <- function(vals) {
  ids <- vapply(names(vals), input_id, character(1), taxa = "macro")
  stats::setNames(vals, ids)
}

test_that("UI pieces render with the expected input ids", {
  expect_s3_class(app_obj, "shiny.appobj")
  side <- as.character(htmltools::renderTags(scenario_sidebar(app))$html)
  for (t in names(TAXA)) {
    for (v in c(TAXA_PREDICTORS[[t]], if (t == "macro") "StreamOrder")) {
      expect_match(side, sprintf('id="%s"', input_id(v, t)), fixed = TRUE)
    }
  }
  expect_no_match(side, 'id="alkl_fish"', fixed = TRUE)
  for (f in list(mod_overview_ui, mod_biotic_ui, mod_sensitivity_ui, mod_diagnostics_ui)) {
    expect_gt(nchar(as.character(htmltools::renderTags(f("x"))$html)), 200)
  }
  for (f in list(mod_data_ui, mod_help_ui)) {
    expect_gt(nchar(as.character(htmltools::renderTags(f("x", app))$html)), 200)
  }
  notes <- as.character(htmltools::renderTags(help_data_notes(app))$html)
  expect_match(notes, "Macroinvertebrates (of 115): Flow 52", fixed = TRUE)
  expect_match(notes, "Fish (of 50): Flow 34", fixed = TRUE)
})

test_that("server computes the scenario and predictions", {
  testServer(app_obj, {
    defaults <- default_scenario(app$data$macro, "macro")
    do.call(session$setInputs, c(list(taxa = "macro"), macro_inputs(defaults)))
    session$elapse(200)
    s <- scenario()
    expect_equal(s$taxa, "macro")
    expect_equal(nrow(s$newdata), 1)
    p <- predict_metrics(s$models, s$newdata, s$data)
    expect_equal(p$fit[p$metric == "N1"], 8.890578, tolerance = 1e-5)

    # Switching assemblage uses the fish sliders.
    fd <- default_scenario(app$data$fish, "fish")
    ids <- vapply(names(fd), input_id, character(1), taxa = "fish")
    do.call(session$setInputs, c(list(taxa = "fish"), stats::setNames(fd, ids)))
    session$elapse(200)
    expect_equal(scenario()$taxa, "fish")
    expect_false("StreamOrder" %in% names(scenario()$newdata))
  })
})

test_that("module servers render without error", {
  d <- app$data$macro
  sc <- reactiveVal(NULL)
  vals <- default_scenario(d, "macro")
  scen <- reactive(list(taxa = "macro", values = vals, data = d, models = app$models$macro,
                        newdata = scenario_frame(vals, "macro", d)))

  testServer(mod_biotic_server, args = list(app = app, scenario = scen), {
    session$setInputs(metrics = METRICS$id)
    expect_match(output$v_N1, "8.89")
    expect_true(nchar(output$position) > 100)
    session$setInputs(save = 1)
    expect_equal(nrow(log()), 1)
    expect_equal(log()$N1, 8.890578, tolerance = 1e-5)
    session$setInputs(clear = 1)
    expect_null(log())
  })

  testServer(mod_sensitivity_server, args = list(app = app, scenario = scen), {
    session$setInputs(metric = "N2", range = "0.10")
    expect_true(nchar(output$curves) > 100)
    expect_true(nchar(output$tornado) > 100)
  })

  testServer(mod_diagnostics_server, args = list(app = app, scenario = scen), {
    session$setInputs(metric = "Margalef")
    expect_match(output$smooths, "Water temperature")
    expect_match(output$all_models, "Fish")
    expect_true(nchar(output$obs_fit) > 100)
  })

  testServer(mod_data_server, args = list(app = app, scenario = scen), {
    expect_match(output$title, "115 samples")
  })

  testServer(mod_overview_server, args = list(app = app, scenario = scen), {
    expect_true(nchar(output$map) > 100)
    expect_match(output$map_title, "Macroinvertebrates")
  })
})
