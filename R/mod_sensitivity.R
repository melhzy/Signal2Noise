# Sensitivity page: one-at-a-time response curves and a tornado ranking ---------

mod_sensitivity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex flex-wrap align-items-end justify-content-between gap-3 mb-1",
        h4(class = "mb-2", textOutput(ns("heading"), inline = TRUE)),
        div(class = "d-flex flex-wrap gap-3",
            selectInput(ns("metric"), "Metric", width = "180px",
                        choices = stats::setNames(METRICS$id, METRICS$label)),
            selectInput(ns("range"), "Tornado swing", width = "230px",
                        choices = c("10th → 90th percentile" = "0.10",
                                    "25th → 75th percentile" = "0.25",
                                    "Minimum → maximum" = "0.00")))),
    p(class = "text-muted",
      "Each panel varies one driver across its sampled range while every other driver",
      "stays at the current scenario (set in the sidebar). Shaded bands are 95% confidence",
      "intervals; the orange line marks the current setting; ticks along the bottom show",
      "where samples were taken."),
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(8, 4)),
      card(full_screen = TRUE,
           card_header(icon("wave-square"), " Response to each driver"),
           plotlyOutput(ns("curves"), height = "640px")),
      card(full_screen = TRUE,
           card_header(icon("ranking-star"), " Which drivers move the prediction most?"),
           plotlyOutput(ns("tornado"), height = "560px"),
           card_footer(class = "small text-muted",
                       "Bars show how far the prediction moves when one driver goes from the",
                       "low to the high end of the chosen range (others held at the scenario).",
                       "Blue: the index rises with the driver; orange: it falls.",
                       "For macroinvertebrates, stream order compares its lowest- and",
                       "highest-scoring orders."))
    )
  )
}

mod_sensitivity_server <- function(id, app, scenario) {
  moduleServer(id, function(input, output, session) {
    output$heading <- renderText(paste("Sensitivity analysis:", TAXA[[scenario()$taxa]]))

    model <- reactive(scenario()$models[[input$metric]])

    output$curves <- renderPlotly({
      s <- scenario()
      m <- model()
      vars <- c(TAXA_PREDICTORS[[s$taxa]], if (s$taxa == "macro") "StreamOrder")
      curves <- lapply(vars, function(v) response_curve(m, s$newdata, s$data, v))
      names(curves) <- vars
      cont <- setdiff(vars, "StreamOrder")
      ylim <- range(unlist(lapply(curves[cont], function(cv) c(cv$lwr, cv$upr))))
      ylim <- ylim + c(-1, 1) * 0.04 * diff(ylim)
      lab <- metric_label(input$metric)

      panels <- lapply(vars, function(v) {
        cv <- curves[[v]]
        if (v == "StreamOrder") {
          cur <- as.character(s$newdata$StreamOrder)
          return(
            plot_ly(cv, x = ~x, y = ~fit, type = "scatter", mode = "markers",
                    error_y = list(type = "data", symmetric = FALSE,
                                   array = ~upr - fit, arrayminus = ~fit - lwr,
                                   color = PALETTE$primary, thickness = 1.5, width = 5),
                    marker = list(size = 9, color = ifelse(cv$x == cur, PALETTE$accent, PALETTE$primary)),
                    hovertemplate = paste0("Stream order %{x}<br>", lab,
                                           " %{y:.2f}<extra></extra>"),
                    showlegend = FALSE) |>
              layout(xaxis = list(title = "Stream order", type = "category"),
                     yaxis = list(title = ""))
          )
        }
        x_now <- s$newdata[[v]][1]
        plot_ly(cv, x = ~x) |>
          add_ribbons(ymin = ~lwr, ymax = ~upr, fillcolor = "rgba(11,110,153,0.15)",
                      line = list(width = 0), hoverinfo = "skip", showlegend = FALSE) |>
          add_lines(y = ~fit, line = list(color = PALETTE$primary, width = 2.2),
                    hovertemplate = paste0(predictor_label(v), " %{x:.3~f}<br>", lab,
                                           " %{y:.2f}<extra></extra>"),
                    showlegend = FALSE) |>
          add_markers(x = s$data[[v]], y = rep(ylim[1], nrow(s$data)), inherit = FALSE,
                      marker = list(symbol = "line-ns-open", size = 7,
                                    color = "rgba(31,42,55,0.35)"),
                      hoverinfo = "skip", showlegend = FALSE) |>
          add_segments(x = x_now, xend = x_now, y = ylim[1], yend = ylim[2], inherit = FALSE,
                       line = list(color = PALETTE$accent, width = 1.5, dash = "dash"),
                       hoverinfo = "skip", showlegend = FALSE) |>
          layout(xaxis = list(title = predictor_label(v), zeroline = FALSE),
                 yaxis = list(title = "", range = ylim))
      })
      ncol <- 3
      subplot(panels, nrows = ceiling(length(panels) / ncol), margin = c(0.04, 0.02, 0.07, 0.07),
              titleX = TRUE, titleY = FALSE) |>
        style_plot(axes = FALSE) |>
        layout(margin = list(l = 85, r = 10, t = 10, b = 40),
               annotations = list(list(text = lab, x = -0.095, y = 0.5, xref = "paper",
                                       yref = "paper", textangle = -90, showarrow = FALSE,
                                       font = list(size = 13))))
    })

    output$tornado <- renderPlotly({
      s <- scenario()
      lo <- as.numeric(input$range)
      tor <- tornado_data(model(), s$newdata, s$data, s$taxa, probs = c(lo, 1 - lo))
      tor$dir <- ifelse(tor$var == "StreamOrder", "Stream order",
                        ifelse(tor$at_high >= tor$at_low, "Rises with driver", "Falls with driver"))
      cols <- c("Rises with driver" = PALETTE$primary, "Falls with driver" = PALETTE$accent,
                "Stream order" = PALETTE$observed)
      xfmt <- function(v, x) ifelse(v == "StreamOrder", paste("order", x), fmt(as.numeric(x), 3))
      tor$hover <- sprintf("<b>%s</b><br>%s → %s<br>%s %s → %s<br>Swing %s",
                           tor$label, xfmt(tor$var, tor$low_x), xfmt(tor$var, tor$high_x),
                           metric_label(input$metric), fmt(tor$at_low), fmt(tor$at_high),
                           fmt(tor$swing))
      tor$label <- factor(tor$label, levels = tor$label)
      plot_ly(tor, y = ~label, x = ~swing, base = ~pmin(at_low, at_high),
              type = "bar", orientation = "h", color = ~dir, colors = cols,
              text = ~hover, hoverinfo = "text", textposition = "none") |>
        style_plot(xlab = metric_label(input$metric), legend = TRUE) |>
        layout(
          barmode = "overlay",
          yaxis = list(title = "", categoryorder = "array", categoryarray = levels(tor$label)),
          shapes = list(list(type = "line", x0 = tor$base[1], x1 = tor$base[1], y0 = 0, y1 = 1,
                             yref = "paper", line = list(color = PALETTE$ink, dash = "dot", width = 1))),
          annotations = list(list(x = tor$base[1], y = 1.02, yref = "paper", showarrow = FALSE,
                                  text = paste("Scenario", fmt(tor$base[1])),
                                  font = list(size = 11))),
          legend = list(orientation = "h", y = -0.15),
          margin = list(l = 10, r = 20, t = 30, b = 60)
        )
    })
  })
}
