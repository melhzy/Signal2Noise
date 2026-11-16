# Biotic Response page: predictions with uncertainty for the current scenario ---

VB_THEMES <- c(N1 = "primary", N2 = "teal", Margalef = "indigo")

metric_value_box <- function(ns, m) {
  row <- METRICS[METRICS$id == m, ]
  value_box(
    title = row$label,
    value = textOutput(ns(paste0("v_", m)), inline = TRUE),
    showcase = icon(row$icon),
    theme = VB_THEMES[[m]],
    uiOutput(ns(paste0("d_", m))),
    tags$p(class = "small opacity-75 mb-0", row$short)
  )
}

mod_biotic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex flex-wrap align-items-center justify-content-between gap-2 mb-2",
        h4(class = "mb-0", textOutput(ns("heading"), inline = TRUE)),
        checkboxGroupInput(ns("metrics"), NULL, inline = TRUE,
                           choices = stats::setNames(METRICS$id, METRICS$label),
                           selected = METRICS$id)),
    uiOutput(ns("envelope")),
    div(class = "vb-grid mb-3",
        lapply(METRICS$id, function(m) {
          conditionalPanel(sprintf("input.metrics && input.metrics.indexOf('%s') > -1", m),
                           ns = ns, metric_value_box(ns, m))
        })),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      card(
        full_screen = TRUE,
        card_header(icon("chart-simple"), " This scenario against sampled sites"),
        plotlyOutput(ns("position"), height = "380px"),
        card_footer(class = "small text-muted",
                    "Grey points are observed values at sampled sites (box = middle 50%).",
                    "The orange diamond is the prediction for the current scenario with its",
                    "95% confidence interval.")
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
          tags$span(icon("table-list"), " Scenario log"),
          div(class = "d-flex gap-1",
              actionButton(ns("save"), "Save scenario", icon = icon("plus"),
                           class = "btn-primary btn-sm"),
              downloadButton(ns("download"), "CSV", class = "btn-outline-secondary btn-sm"),
              actionButton(ns("clear"), NULL, icon = icon("trash-can"),
                           class = "btn-outline-secondary btn-sm", title = "Clear log"))
        ),
        DT::DTOutput(ns("log")),
        card_footer(class = "small text-muted",
                    "Save scenarios to compare what-if conditions side by side, then",
                    "download them as CSV.")
      )
    )
  )
}

mod_biotic_server <- function(id, app, scenario) {
  moduleServer(id, function(input, output, session) {
    preds <- reactive({
      s <- scenario()
      predict_metrics(s$models, s$newdata, s$data)
    })

    output$heading <- renderText(paste("Predicted diversity:", TAXA[[scenario()$taxa]]))

    lapply(METRICS$id, function(m) {
      output[[paste0("v_", m)]] <- renderText({
        p <- preds()
        fmt(p$fit[p$metric == m])
      })
      output[[paste0("d_", m)]] <- renderUI({
        p <- preds()[preds()$metric == m, ]
        floor <- METRICS$floor[METRICS$id == m]
        tagList(
          if (p$fit < floor) {
            tags$p(class = "mb-1 fw-semibold",
                   icon("triangle-exclamation"),
                   sprintf(" Below the index's minimum possible value (%g): the model is extrapolating.",
                           floor))
          },
          tags$p(class = "mb-1", sprintf("95%% CI %s – %s", fmt(p$lwr), fmt(p$upr))),
          tags$p(class = "mb-1",
                 sprintf("At or above %.0f%% of sampled sites (median %s)",
                         p$percentile, fmt(p$obs_median)))
        )
      })
    })

    output$envelope <- renderUI({
      s <- scenario()
      chk <- envelope_check(s$newdata, s$data, s$taxa)
      if (chk$inside) {
        div(class = "envelope-ok small mb-2",
            icon("circle-check"),
            " These conditions resemble sampled sites, so the models are interpolating.")
      } else {
        div(class = "alert alert-warning d-flex gap-2 align-items-start py-2 mb-3", role = "alert",
            icon("triangle-exclamation", class = "mt-1"),
            div(tags$strong("Extrapolation. "),
                "No sampled site has a combination of conditions like this one",
                sprintf("(distance to the nearest sample %.1f vs. a typical %.1f).",
                        chk$distance, chk$nearest_typical),
                "Each slider is within its observed range, but together they fall outside",
                "the data, so treat these predictions with caution."))
      }
    })

    output$position <- renderPlotly({
      s <- scenario()
      sel <- intersect(METRICS$id, input$metrics)
      validate(need(length(sel) > 0, "Select at least one metric above."))
      p <- preds()
      plots <- lapply(seq_along(sel), function(i) {
        m <- sel[i]
        pr <- p[p$metric == m, ]
        lab <- metric_label(m)
        plot_ly() |>
          add_trace(
            type = "box", y = s$data[[m]], x = rep(lab, nrow(s$data)), name = "Sampled sites",
            legendgroup = "obs", showlegend = i == 1,
            boxpoints = "all", jitter = 0.45, pointpos = 0,
            marker = list(color = PALETTE$observed, size = 5, opacity = 0.55),
            line = list(color = PALETTE$observed, width = 1.5),
            fillcolor = "rgba(138,153,168,0.12)", hoverinfo = "y"
          ) |>
          add_trace(
            type = "scatter", mode = "markers", x = lab, y = pr$fit,
            name = "Scenario (95% CI)", legendgroup = "pred", showlegend = i == 1,
            error_y = list(type = "data", symmetric = FALSE,
                           array = pr$upr - pr$fit, arrayminus = pr$fit - pr$lwr,
                           color = PALETTE$accent, thickness = 2.5, width = 10),
            marker = list(symbol = "diamond", size = 15, color = PALETTE$accent,
                          line = list(color = "white", width = 1.5)),
            hovertemplate = sprintf("Prediction %s<br>95%% CI %s – %s<extra></extra>",
                                    fmt(pr$fit), fmt(pr$lwr), fmt(pr$upr))
          ) |>
          layout(yaxis = list(title = lab, gridcolor = PALETTE$grid, zeroline = FALSE),
                 xaxis = list(showticklabels = FALSE))
      })
      subplot(plots, nrows = 1, margin = 0.08, titleY = TRUE) |>
        style_plot(legend = TRUE, axes = FALSE) |>
        layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
               margin = list(l = 70, r = 10, t = 40, b = 10))
    })

    # Scenario log -----------------------------------------------------------
    log <- reactiveVal(NULL)

    observeEvent(input$save, {
      s <- scenario()
      p <- preds()
      row <- data.frame(Saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                        Assemblage = TAXA[[s$taxa]], check.names = FALSE)
      for (v in c(PREDICTORS$var, "StreamOrder")) {
        row[[v]] <- if (v %in% names(s$newdata)) as.character(s$newdata[[v]][1]) else NA
      }
      for (m in METRICS$id) {
        pm <- p[p$metric == m, ]
        row[[m]] <- pm$fit
        row[[paste0(m, "_lwr")]] <- pm$lwr
        row[[paste0(m, "_upr")]] <- pm$upr
      }
      row$Extrapolation <- !envelope_check(s$newdata, s$data, s$taxa)$inside
      log(rbind(log(), row))
      showNotification("Scenario saved to the log.", type = "message", duration = 2)
    })

    observeEvent(input$clear, log(NULL))

    output$log <- DT::renderDT({
      x <- log()
      if (is.null(x)) {
        x <- data.frame(Message = "No saved scenarios yet. Click “Save scenario”.")
        return(DT::datatable(x, rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
      }
      show <- data.frame(
        `#` = seq_len(nrow(x)),
        Assemblage = x$Assemblage,
        `Hill's N1` = sprintf("%s (%s–%s)", fmt(x$N1), fmt(x$N1_lwr), fmt(x$N1_upr)),
        `Hill's N2` = sprintf("%s (%s–%s)", fmt(x$N2), fmt(x$N2_lwr), fmt(x$N2_upr)),
        Margalef = sprintf("%s (%s–%s)", fmt(x$Margalef), fmt(x$Margalef_lwr), fmt(x$Margalef_upr)),
        pH = fmt(as.numeric(x$pH)),
        `Temp (°C)` = fmt(as.numeric(x$WaterTemp), 1),
        `Extrapolated?` = ifelse(x$Extrapolation, "yes", "no"),
        check.names = FALSE
      )
      DT::datatable(show, rownames = FALSE, selection = "none", class = "compact stripe nowrap",
                    options = list(dom = "tp", pageLength = 6, scrollX = TRUE, ordering = FALSE))
    })

    output$download <- downloadHandler(
      filename = function() sprintf("signal2noise-scenarios-%s.csv", format(Sys.Date())),
      content = function(file) {
        x <- log()
        if (is.null(x)) x <- data.frame(Message = "No saved scenarios")
        utils::write.csv(x, file, row.names = FALSE)
      }
    )
  })
}
