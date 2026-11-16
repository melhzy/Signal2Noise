# Theme, sidebar and plot helpers ------------------------------------------------

app_theme <- function() {
  fonts <- bslib::font_collection("Segoe UI", "system-ui", "-apple-system", "Roboto",
                                  "Helvetica Neue", "Arial", "sans-serif")
  bslib::bs_theme(
    version = 5,
    primary = PALETTE$primary,
    secondary = "#5B6B7B",
    success = "#2E7D5B",
    warning = PALETTE$accent,
    info = "#3D85C6",
    base_font = fonts,
    heading_font = fonts,
    "border-radius" = "0.6rem"
  )
}

slider_step <- function(x) 10^floor(log10(diff(range(x)) / 100))

predictor_slider <- function(var, taxa, data, default) {
  x <- data[[var]]
  step <- slider_step(x)
  lo <- floor(min(x) / step) * step
  hi <- ceiling(max(x) / step) * step
  coded <- PREDICTORS$coded[PREDICTORS$var == var]
  label <- if (coded) {
    tags$span(predictor_label(var), " ",
              bslib::tooltip(shiny::icon("circle-exclamation", class = "text-warning"),
                             CODED_NOTE))
  } else predictor_label(var)
  shiny::sliderInput(input_id(var, taxa), label, min = lo, max = hi,
                     value = default, step = step, ticks = FALSE, width = "100%")
}

slider_accordion <- function(taxa, data) {
  defaults <- default_scenario(data, taxa)
  vars <- TAXA_PREDICTORS[[taxa]]
  groups <- PREDICTORS$group[match(vars, PREDICTORS$var)]
  panels <- lapply(unique(groups), function(g) {
    bslib::accordion_panel(
      g, icon = shiny::icon(if (g == "Field sonde") "water" else "flask"),
      lapply(vars[groups == g], function(v) predictor_slider(v, taxa, data, defaults[[v]]))
    )
  })
  if (taxa == "macro") {
    lv <- levels(data$StreamOrder)
    n <- table(factor(data$StreamOrder, levels = lv))
    panels[[length(panels) + 1]] <- bslib::accordion_panel(
      "Site", icon = shiny::icon("route"),
      shiny::radioButtons(input_id("StreamOrder", taxa), "Stream order",
                          choices = lv, selected = defaults$StreamOrder, inline = TRUE),
      tags$p(class = "small text-muted mb-0",
             sprintf("Samples per order: %s.", paste(sprintf("%s = %d", lv, n), collapse = ", ")),
             "Orders with few samples have wide intervals.")
    )
  }
  do.call(bslib::accordion,
          c(panels, list(open = TRUE, multiple = TRUE, class = "accordion-flush")))
}

scenario_sidebar <- function(app) {
  bslib::sidebar(
    id = "sidebar", title = "Scenario", width = 330, open = "desktop",
    shiny::radioButtons(
      "taxa", "Assemblage",
      choiceNames = list(tagList(shiny::icon("bug"), " Macroinvertebrates"),
                         tagList(shiny::icon("fish"), " Fish")),
      choiceValues = names(TAXA), inline = TRUE
    ),
    shiny::conditionalPanel("input.taxa == 'macro'", slider_accordion("macro", app$data$macro)),
    shiny::conditionalPanel("input.taxa == 'fish'", slider_accordion("fish", app$data$fish)),
    shiny::actionButton("reset", "Reset to typical conditions", icon = shiny::icon("rotate-left"),
                        class = "btn-outline-primary btn-sm w-100"),
    tags$p(class = "small text-muted mb-0",
           "Slider ranges span the sampled conditions. Defaults are the sample",
           "median (pH) or mean (all others), as in the original app.")
  )
}

apply_scenario <- function(session, taxa, values) {
  for (v in names(values)) {
    id <- input_id(v, taxa)
    if (v == "StreamOrder") {
      shiny::updateRadioButtons(session, id, selected = values[[v]])
    } else {
      shiny::updateSliderInput(session, id, value = values[[v]])
    }
  }
}

read_news <- function(path = PATHS$news) {
  if (!file.exists(path)) return(data.frame(from = character(), message = character()))
  x <- suppressWarnings(utils::read.csv(path, check.names = FALSE, strip.white = TRUE,
                                        stringsAsFactors = FALSE))
  names(x) <- trimws(names(x))
  x
}

news_popover <- function(news) {
  items <- if (nrow(news)) {
    tags$ul(class = "list-unstyled mb-0",
            lapply(seq_len(nrow(news)), function(i) {
              tags$li(class = "mb-2", tags$strong(news$from[i]), tags$br(),
                      tags$span(class = "small", news$message[i]))
            }))
  } else tags$p("No updates.")
  bslib::popover(
    tags$a(href = "#", class = "nav-link position-relative", `aria-label` = "Project updates",
           shiny::icon("bell"),
           if (nrow(news)) tags$span(class = "badge rounded-pill bg-warning text-dark news-badge",
                                     nrow(news))),
    items, title = "Project updates", placement = "bottom"
  )
}

# Plotly styling shared by every chart.
# `axes = FALSE` leaves axis settings alone (for subplots, whose panels style
# their own axes; overriding `xaxis` here would only hit the first panel).
style_plot <- function(p, xlab = NULL, ylab = NULL, legend = FALSE, axes = TRUE) {
  axis <- function(title) {
    a <- list(gridcolor = PALETTE$grid, zeroline = FALSE,
              linecolor = "#C9D3DD", ticks = "outside", tickcolor = "#C9D3DD")
    if (!is.null(title)) a$title <- list(text = title, standoff = 8)
    a
  }
  if (axes) p <- plotly::layout(p, xaxis = axis(xlab), yaxis = axis(ylab))
  p |>
    plotly::layout(
      font = list(family = "Segoe UI, system-ui, sans-serif", size = 12, color = PALETTE$ink),
      showlegend = legend,
      legend = list(orientation = "h", y = -0.2),
      margin = list(l = 60, r = 20, t = 30, b = 50),
      hoverlabel = list(bgcolor = "white", bordercolor = "#C9D3DD",
                        font = list(color = PALETTE$ink)),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                              "toggleSpikelines", "hoverCompareCartesian"))
}

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
