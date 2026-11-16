# SRBC Signal2Noise ----------------------------------------------------------------
# Predicts stream biodiversity (Hill's N1, Hill's N2, Margalef) for benthic
# macroinvertebrates and fish from water-quality conditions using GAMs.
#
# Programmed by Pranita, Sri, Emily, Ziyuan, and Igor (2019); modernized 2026.
# Run with shiny::runApp() from this folder. Helpers live in R/ (auto-sourced).

library(shiny)
library(bslib)
library(plotly)

app <- load_app_data()   # cached GAMs + prepared data (data/model_cache.rds)
news <- read_news()

ui <- page_navbar(
  id = "nav",
  title = tags$span(class = "brand",
                    tags$img(src = "HU_E3_Favicon.jpg", alt = "", height = 26),
                    APP_TITLE),
  window_title = APP_TITLE,
  theme = app_theme(),
  fillable = FALSE,
  navbar_options = navbar_options(bg = "#0B3C5D", theme = "dark", collapsible = TRUE),
  header = tags$head(
    tags$link(rel = "icon", type = "image/jpeg", href = "HU_E3_Favicon.jpg"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  sidebar = scenario_sidebar(app),
  nav_panel("Overview", value = "overview", icon = icon("house"), mod_overview_ui("overview")),
  nav_panel("Biotic Response", value = "biotic", icon = icon("seedling"), mod_biotic_ui("biotic")),
  nav_panel("Sensitivity", value = "sensitivity", icon = icon("sliders"),
            mod_sensitivity_ui("sensitivity")),
  nav_panel("Model Diagnostics", value = "diagnostics", icon = icon("stethoscope"),
            mod_diagnostics_ui("diagnostics")),
  nav_panel("Data", value = "data", icon = icon("table"), mod_data_ui("data", app)),
  nav_panel("Help", value = "help", icon = icon("circle-question"), mod_help_ui("help", app)),
  nav_spacer(),
  nav_item(news_popover(news)),
  nav_item(tags$a(href = "https://www.harrisburgu.edu", target = "_blank", rel = "noopener",
                  class = "nav-link py-1",
                  tags$img(src = "E3Logo.jpg", alt = "Harrisburg University", height = 30,
                           class = "rounded")))
)

server <- function(input, output, session) {
  taxa <- reactive(input$taxa %||% "macro")

  values <- reactive({
    t <- taxa()
    vars <- c(TAXA_PREDICTORS[[t]], if (t == "macro") "StreamOrder")
    vals <- lapply(vars, function(v) input[[input_id(v, t)]])
    names(vals) <- vars
    req(all(!vapply(vals, is.null, logical(1))))
    vals
  }) |> debounce(120)

  scenario <- reactive({
    t <- taxa()
    v <- values()
    req(setequal(names(v), c(TAXA_PREDICTORS[[t]], if (t == "macro") "StreamOrder")))
    d <- app$data[[t]]
    list(taxa = t, values = v, data = d, models = app$models[[t]],
         newdata = scenario_frame(v, t, d))
  })

  observeEvent(input$reset, {
    t <- taxa()
    apply_scenario(session, t, default_scenario(app$data[[t]], t))
  })

  station_click <- mod_overview_server("overview", app, scenario)
  observeEvent(station_click(), {
    click <- station_click()
    vals <- station_scenario(app$data[[click$taxa]], click$station, click$taxa)
    req(vals)
    apply_scenario(session, click$taxa, vals)
    name <- app$stations$StationName[match(click$station, app$stations$StationID)]
    showNotification(sprintf("Loaded average conditions for %s (station %s).",
                             name %||% "station", click$station),
                     type = "message", duration = 4)
    nav_select("nav", "biotic")
  })

  mod_biotic_server("biotic", app, scenario)
  mod_sensitivity_server("sensitivity", app, scenario)
  mod_diagnostics_server("diagnostics", app, scenario)
  mod_data_server("data", app, scenario)
}

shinyApp(ui, server)
