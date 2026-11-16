# Model Diagnostics page ---------------------------------------------------------

mod_diagnostics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex flex-wrap align-items-center justify-content-between gap-2 mb-2",
        h4(class = "mb-0", textOutput(ns("heading"), inline = TRUE)),
        radioButtons(ns("metric"), NULL, inline = TRUE,
                     choices = stats::setNames(METRICS$id, METRICS$label))),
    uiOutput(ns("stats")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(full_screen = TRUE, card_header("Observed vs. fitted"),
           plotlyOutput(ns("obs_fit"), height = "340px"),
           card_footer(class = "small text-muted",
                       "Points on the dashed 1:1 line are fitted perfectly.")),
      card(full_screen = TRUE, card_header("Residuals vs. fitted"),
           plotlyOutput(ns("resid"), height = "340px"),
           card_footer(class = "small text-muted",
                       "Look for curvature or funnel shapes, which suggest a misspecified model."))
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(card_header("Smooth terms"),
           tableOutput(ns("smooths")),
           uiOutput(ns("parametric")),
           tags$p(class = "small text-muted mb-0",
                  "edf = effective degrees of freedom (1 means linear; higher means more",
                  "wiggly). p-values test whether the smooth differs from zero.")),
      card(card_header("All six models"),
           tableOutput(ns("all_models")),
           uiOutput(ns("formula")))
    )
  )
}

mod_diagnostics_server <- function(id, app, scenario) {
  moduleServer(id, function(input, output, session) {
    all_summary <- do.call(rbind, unlist(lapply(names(TAXA), function(t) {
      lapply(METRICS$id, function(m) model_summary_row(app$models[[t]][[m]], t, m))
    }), recursive = FALSE))

    model <- reactive(scenario()$models[[input$metric]])

    output$heading <- renderText(paste("Model diagnostics:", TAXA[[scenario()$taxa]]))

    output$stats <- renderUI({
      m <- model()
      s <- summary(m)
      vb <- function(title, value, ic, ...) {
        value_box(title, value, ..., showcase = icon(ic), showcase_layout = "top right")
      }
      layout_column_wrap(
        width = "200px", class = "mb-3",
        vb("Samples", s$n, "vial"),
        vb("Deviance explained", sprintf("%.1f%%", 100 * s$dev.expl), "chart-pie"),
        vb("Adjusted R²", fmt(s$r.sq, 3), "bullseye"),
        vb("GCV score", fmt(m$gcv.ubre, 3), "scale-balanced",
           p(class = "small mb-0", "lower is better"))
      )
    })

    fit_frame <- reactive({
      m <- model()
      d <- scenario()$data
      data.frame(fitted = as.numeric(fitted(m)), observed = m$y,
                 resid = as.numeric(residuals(m, type = "response")),
                 station = d$StationName, date = format(d$DateTime, "%Y-%m-%d", tz = "UTC"))
    })

    hover <- "%{customdata}<br>Observed %{y:.2f}<br>Fitted %{x:.2f}<extra></extra>"

    output$obs_fit <- renderPlotly({
      f <- fit_frame()
      lim <- range(c(f$fitted, f$observed))
      plot_ly(f, x = ~fitted, y = ~observed, type = "scatter", mode = "markers",
              customdata = ~paste(station, date),
              marker = list(color = PALETTE$primary, size = 8, opacity = 0.7,
                            line = list(color = "white", width = 0.5)),
              hovertemplate = hover) |>
        add_segments(x = lim[1], xend = lim[2], y = lim[1], yend = lim[2], inherit = FALSE,
                     line = list(color = PALETTE$accent, dash = "dash"), hoverinfo = "skip") |>
        style_plot(xlab = "Fitted", ylab = "Observed")
    })

    output$resid <- renderPlotly({
      f <- fit_frame()
      plot_ly(f, x = ~fitted, y = ~resid, type = "scatter", mode = "markers",
              customdata = ~paste(station, date),
              marker = list(color = PALETTE$primary, size = 8, opacity = 0.7,
                            line = list(color = "white", width = 0.5)),
              hovertemplate = "%{customdata}<br>Residual %{y:.2f}<br>Fitted %{x:.2f}<extra></extra>") |>
        add_segments(x = min(f$fitted), xend = max(f$fitted), y = 0, yend = 0, inherit = FALSE,
                     line = list(color = PALETTE$accent, dash = "dash"), hoverinfo = "skip") |>
        style_plot(xlab = "Fitted", ylab = "Residual (observed − fitted)")
    })

    output$smooths <- renderTable({
      st <- smooth_table(model())
      st$Term <- predictor_label(st$Term)
      st$`p-value` <- format.pval(st$`p-value`, digits = 2, eps = 1e-4)
      st
    }, digits = 2, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

    output$parametric <- renderUI({
      if (scenario()$taxa != "macro") return(NULL)
      pt <- summary(model())$p.table
      rows <- grepl("^StreamOrder", rownames(pt))
      tags$p(class = "small",
             tags$strong("Stream order"), " enters as a categorical (parametric) term;",
             sprintf("order-level effects relative to order 1 range from %s to %s.",
                     fmt(min(pt[rows, 1])), fmt(max(pt[rows, 1]))))
    })

    output$all_models <- renderTable({
      x <- all_summary
      x$`Deviance explained` <- sprintf("%.1f%%", 100 * x$`Deviance explained`)
      x
    }, digits = 3, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

    output$formula <- renderUI({
      f <- paste(deparse(formula(model()), width.cutoff = 500), collapse = " ")
      tags$details(class = "small mt-2",
                   tags$summary("Model formula (mgcv::gam)"),
                   tags$code(f))
    })
  })
}
