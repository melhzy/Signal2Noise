# Overview page: headline numbers and an interactive station map ----------------

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "hero mb-3",
        h2("Signal2Noise"),
        p(class = "lead mb-0",
          "Predict stream biodiversity from water-quality conditions at SRBC monitoring",
          "stations. Six generalized additive models link field and lab measurements to",
          "three diversity indices for benthic macroinvertebrates and fish.")),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)),
      card(
        full_screen = TRUE,
        card_header(textOutput(ns("map_title"), inline = TRUE)),
        plotlyOutput(ns("map"), height = "470px"),
        card_footer(class = "small text-muted", textOutput(ns("map_note"), inline = TRUE))
      ),
      card(
        card_header(icon("compass"), " Getting started"),
        tags$ol(class = "steps",
          tags$li(tags$strong("Pick an assemblage"), " (macroinvertebrates or fish) in the",
                  " Scenario sidebar."),
          tags$li(tags$strong("Set conditions"), " with the sliders, or click a station on the",
                  " map to load its average conditions."),
          tags$li(tags$strong("Biotic Response"), " shows predicted Hill's N1, Hill's N2 and",
                  " Margalef with 95% confidence intervals."),
          tags$li(tags$strong("Sensitivity"), " shows which drivers move each index most."),
          tags$li(tags$strong("Model Diagnostics"), " shows how well each model fits.")
        ),
        tags$p(class = "small text-muted mb-0",
               "Predictions describe statistical associations in 2008–2017 monitoring",
               "data, not cause and effect.")
      )
    )
  )
}

mod_overview_server <- function(id, app, scenario) {
  moduleServer(id, function(input, output, session) {
    summary_tbl <- reactive({
      taxa <- scenario()$taxa
      station_summary(app$data[[taxa]], app$stations)
    })

    output$kpis <- renderUI({
      taxa <- scenario()$taxa
      d <- app$data[[taxa]]
      st <- summary_tbl()
      yrs <- range(as.integer(format(d$DateTime, "%Y", tz = "UTC")))
      dev <- sapply(app$models[[taxa]], function(m) summary(m)$dev.expl)
      vb <- function(title, value, ic, ...) {
        value_box(title, value, ..., showcase = icon(ic), showcase_layout = "top right")
      }
      layout_column_wrap(
        width = "220px", class = "mb-3",
        vb("Samples", nrow(d), TAXA_ICON[[taxa]],
           p(TAXA[[taxa]], "surveys used in the models")),
        vb("Stations", nrow(st), "location-dot",
           p(sum(!is.na(st$Latitude)), "with mapped coordinates")),
        vb("Sampling years", paste(yrs, collapse = "–"), "calendar",
           p(length(unique(format(d$DateTime, "%Y"))), "distinct years")),
        vb("Deviance explained", sprintf("%.0f–%.0f%%", 100 * min(dev), 100 * max(dev)),
           "chart-line", p("across the three", tolower(TAXA[[taxa]]), "models"))
      )
    })

    output$map_title <- renderText(paste(TAXA[[scenario()$taxa]], "monitoring stations"))

    output$map_note <- renderText({
      st <- summary_tbl()
      miss <- sum(is.na(st$Latitude))
      paste0("Marker size = mean Hill's N1 at the station; colour = stream order. ",
             "Click a station to load its average conditions into the scenario.",
             if (miss) sprintf(" %d station(s) have no coordinates and are not shown.", miss) else "")
    })

    output$map <- renderPlotly({
      st <- summary_tbl()
      st <- st[!is.na(st$Latitude), ]
      st$hover <- sprintf(
        "<b>%s</b> (ID %s)<br>Stream order %s · %d sample(s)<br>Mean Hill's N1 %s · N2 %s · Margalef %s",
        st$StationName, st$StationID, st$StreamOrder, st$samples,
        fmt(st$N1), fmt(st$N2), fmt(st$Margalef))
      st$StreamOrder <- factor(st$StreamOrder, levels = names(PALETTE$order))
      st$msize <- 9 + 19 * (st$N1 - min(st$N1)) / max(diff(range(st$N1)), 1e-9)
      # Plain lon/lat axes (no basemap: works offline, no WebGL or tile service).
      # A degree of latitude is 1/cos(lat) times longer than one of longitude.
      ratio <- 1 / cos(mean(range(st$Latitude)) * pi / 180)
      p <- plot_ly(
        st, x = ~Longitude, y = ~Latitude, type = "scatter", mode = "markers",
        customdata = ~StationID, color = ~StreamOrder, colors = PALETTE$order,
        marker = list(size = ~msize, line = list(color = "white", width = 1), opacity = 0.9),
        text = ~hover, hoverinfo = "text", source = "station_map"
      ) |>
        style_plot(xlab = "Longitude (°)", ylab = "Latitude (°)", legend = TRUE) |>
        layout(
          yaxis = list(scaleanchor = "x", scaleratio = ratio, title = list(text = "Latitude (°)"),
                       gridcolor = PALETTE$grid, zeroline = FALSE),
          legend = list(title = list(text = "Stream order"), orientation = "v", x = 1.02, y = 1),
          margin = list(l = 60, r = 10, t = 10, b = 50),
          plot_bgcolor = "#F7F9FB"
        )
      event_register(p, "plotly_click")
    })

    # Station id of the latest click, for the app to load into the scenario.
    reactive({
      click <- event_data("plotly_click", source = "station_map", priority = "event")
      req(click, click$customdata)
      list(taxa = isolate(scenario()$taxa), station = click$customdata, at = Sys.time())
    })
  })
}
