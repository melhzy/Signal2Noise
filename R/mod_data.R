# Data page: browse and download the modeling data --------------------------------

mod_data_ui <- function(id, app) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
        tags$span(icon("table"), " ", textOutput(ns("title"), inline = TRUE)),
        downloadButton(ns("download"), "Download CSV", class = "btn-outline-primary btn-sm")
      ),
      DT::DTOutput(ns("table"))
    ),
    card(card_header(icon("circle-info"), " Data notes"), help_data_notes(app))
  )
}

mod_data_server <- function(id, app, scenario) {
  moduleServer(id, function(input, output, session) {
    tbl <- reactive({
      d <- app$data[[scenario()$taxa]]
      d$Date <- format(d$DateTime, "%Y-%m-%d", tz = "UTC")
      d$DateTime <- NULL
      d[c("StationID", "StationName", "Date", setdiff(names(d), c("StationID", "StationName", "Date")))]
    })

    output$title <- renderText(sprintf("Modeling data: %s (%d samples)",
                                       TAXA[[scenario()$taxa]], nrow(tbl())))

    output$table <- DT::renderDT({
      d <- tbl()
      names(d)[names(d) == "N1"] <- "Hill's N1"
      names(d)[names(d) == "N2"] <- "Hill's N2"
      num <- names(d)[vapply(d, is.numeric, logical(1)) & names(d) != "StationID"]
      DT::datatable(d, rownames = FALSE, filter = "top", selection = "none",
                    options = list(pageLength = 10, scrollX = TRUE, autoWidth = FALSE,
                                   dom = "tip")) |>
        DT::formatRound(num, digits = 3)
    })

    output$download <- downloadHandler(
      filename = function() sprintf("signal2noise-%s-data.csv", scenario()$taxa),
      content = function(file) utils::write.csv(tbl(), file, row.names = FALSE)
    )
  })
}
