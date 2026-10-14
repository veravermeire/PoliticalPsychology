library(shiny)
if (!file.exists("simulation.R")) {
  stop("Save simulation.R in the same folder as app.R before running the app.")
}
source("simulation.R", local = TRUE)

opinion_palette <- grDevices::colorRampPalette(
  c("#2166AC", "#F7F7F7", "#B2182B")
)(201)
opinion_colors <- function(x) {
  opinion_palette[pmax(1L, pmin(201L, round((x + 1) * 100) + 1L))]
}

ui <- fluidPage(
  titlePanel("Opinion dynamics"),
  sidebarLayout(
    sidebarPanel(
      h4("Model settings"),
      sliderInput("n", "People", min = 10, max = 300, value = 100, step = 10),
      selectInput("pattern", "Initial opinions",
                  choices = c("Broad spread" = "broad",
                              "Near the middle" = "middle",
                              "Two groups" = "two_groups"),
                  selected = "two_groups"),
      sliderInput("average_degree", "Target average connections",
                  min = 0, max = 99, value = 10, step = 1),
      sliderInput("homophily", "Preference for similar initial opinions",
                  min = 0, max = 5, value = 0, step = 0.25),
      helpText("0 means no preference. Higher values favor connections between people with similar initial opinions. The network stays fixed."),
      sliderInput("openness", "Openness threshold",
                  min = 0, max = 2, value = 0.4, step = 0.05),
      sliderInput("adjustment", "Adjustment fraction per step",
                  min = 0, max = 1, value = 0.2, step = 0.05),
      numericInput("seed", "Random seed", value = 123,
                   min = 0, max = .Machine$integer.max, step = 1),
      helpText("Settings take effect only when you click Reset / apply. Each run ends after 200 steps, even if opinions stabilize earlier."),
      actionButton("reset", "Reset / apply", class = "btn-primary"),
      tags$hr(),
      radioButtons(
        "node_layout",
        "Node positions",
        choices = c(
          "Move with opinions" = "moving",
          "Fixed" = "fixed"
        ),
        selected = "moving",
        inline = TRUE
      ),
      h4("Playback"),
      actionButton("step_once", "Step once"),
      actionButton("run_pause", "Run"),
      sliderInput("delay", "Delay between steps (milliseconds)",
                  min = 100, max = 1000, value = 250, step = 50),
      tags$hr(),
      downloadButton("download_history", "Opinion history CSV"),
      tags$br(), tags$br(),
      downloadButton("download_network", "Network edges CSV"),
      tags$br(), tags$br(),
      downloadButton("download_settings", "Settings CSV")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tableOutput("metrics"),
      tabsetPanel(
        tabPanel("Current state",
                 fluidRow(
                   column(6, plotOutput("network_plot", height = "800px")),
                   column(6, plotOutput("histogram", height = "300px"))
                 ),
                 helpText("Blue = −1; white = 0; red = +1. Gray lines are all fixed connections, not necessarily eligible influences. Node positions do not change.")),
        tabPanel("History",
                 plotOutput("trajectory_plot", height = "360px"),
                 plotOutput("summary_plot", height = "280px")),
        tabPanel("Rules",
                 h4("One simulation step"),
                 tags$ol(
                   tags$li("Find each person's connected neighbors whose opinion distance is at most the openness threshold."),
                   tags$li("Calculate the average opinion of those eligible neighbors, excluding the person themself."),
                   tags$li("Move the specified fraction toward that average. With no eligible neighbors, do not move."),
                   tags$li("Apply all changes simultaneously using opinions from the previous step.")
                 ),
                 p("Broad spread: uniform between −1 and +1."),
                 p("Near the middle: normal with mean 0 and standard deviation 0.2."),
                 p("Two groups: equal mixture probabilities of normals centered at −0.6 and +0.6, each with standard deviation 0.15. Actual group sizes may differ."),
                 p("Normal draws outside the opinion bounds are redrawn, not clipped."),
                 p("Edges are sampled independently. Their probabilities decrease exponentially with initial opinion distance when similarity preference is positive. Probabilities are calibrated to the target expected average degree; realized average degree can differ."),
                 p("Standard deviation and opinion range measure spread; they are not complete measures of polarization. Stable opinions do not necessarily imply agreement."))
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  
  # Keep the requested degree feasible when the population shrinks.
  observeEvent(input$n, {
    updateSliderInput(session, "average_degree", max = input$n - 1,
                      value = min(isolate(input$average_degree), input$n - 1))
  })
  
  requested_settings <- reactive({
    list(n = input$n, pattern = input$pattern,
         average_degree = min(input$average_degree, input$n - 1),
         homophily = input$homophily, openness = input$openness,
         adjustment = input$adjustment, max_steps = 200, seed = input$seed)
  })
  
  observeEvent(input$reset, {
    running(FALSE)
    tryCatch({
      new_state <- do.call(initialize_model, requested_settings())
      state(new_state)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  }, ignoreNULL = FALSE)
  
  observeEvent(input$step_once, {
    req(state())
    running(FALSE)
    state(step_model(state()))
  })
  
  observeEvent(input$run_pause, {
    req(state())
    if (state()$step >= state()$settings$max_steps) {
      showNotification("Run complete. Click Reset / apply to start again.",
                       type = "message")
    } else {
      running(!running())
    }
  })
  
  # Nonblocking playback; state reads/writes are isolated to prevent a
  # state change from scheduling an extra immediate simulation step.
  observe({
    if (!running()) return(invisible(NULL))
    invalidateLater(input$delay, session)
    isolate({
      s <- state()
      if (!is.null(s)) {
        if (s$step < s$settings$max_steps) {
          s <- step_model(s)
          state(s)
        }
        if (s$step >= s$settings$max_steps) running(FALSE)
      }
    })
  })
  
  observe({
    updateActionButton(session, "run_pause",
                       label = if (running()) "Pause" else "Run")
  })
  
  output$status <- renderText({
    s <- state()
    req(s)
    mode <- if (s$step >= s$settings$max_steps) "Complete" else
      if (running()) "Running" else "Paused"
    pending <- !isTRUE(all.equal(requested_settings(), s$settings))
    paste0("Step ", s$step, " / ", s$settings$max_steps, " | ", mode,
           " | Active seed: ", s$settings$seed,
           if (pending) "\nSettings changed: click Reset / apply to use them." else "")
  })
  
  output$metrics <- renderTable({
    s <- state()
    req(s)
    eligible <- s$adjacency &
      (abs(outer(s$opinions, s$opinions, "-")) <= s$settings$openness)
    data.frame(
      Metric = c("Mean opinion", "Opinion standard deviation", "Opinion range",
                 "Actual average connections", "People with no eligible neighbors"),
      Value = c(sprintf("%.3f", mean(s$opinions)),
                sprintf("%.3f", sd(s$opinions)),
                sprintf("%.3f", diff(range(s$opinions))),
                sprintf("%.2f", mean(rowSums(s$adjacency))),
                as.character(sum(rowSums(eligible) == 0)))
    )
  }, striped = TRUE, rownames = FALSE)
  
  output$network_plot <- renderPlot({
    s <- state()
    req(s)
    
    n <- length(s$opinions)
    angle <- 2 * pi * (seq_len(n) - 1) / n
    
    if (identical(input$node_layout, "moving")) {
      # Horizontal position follows the current opinion.
      # A small, stable vertical offset reduces node overlap.
      x <- s$opinions
      y <- 0.18 * sin(angle)
      
      plot_title <- paste("Opinion-based positions — step", s$step)
    } else {
      # Original fixed circular layout.
      x <- cos(angle)
      y <- sin(angle)
      
      plot_title <- paste("Fixed positions — step", s$step)
    }
    
    moving <- identical(input$node_layout, "moving")
    
    plot_xlim <- c(-1.08, 1.08)
    plot_ylim <- if (moving) c(-0.24, 0.24) else c(-1.08, 1.08)
    
    # In fixed mode, use a square plotting region so the circle fits
    # without creating extra vertical space inside the plot.
    par(
      mar = c(3, 1, 2, 1),
      pty = if (moving) "m" else "s",
      xaxs = "i",
      yaxs = "i"
    )
    
    plot(
      x, y,
      type = "n",
      asp = if (moving) NA else 1,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = plot_xlim,
      ylim = plot_ylim,
      main = plot_title
    )
    
    edges <- network_data(s)
    
    if (nrow(edges)) {
      segments(
        x[edges$from], y[edges$from],
        x[edges$to], y[edges$to],
        col = grDevices::adjustcolor("gray50", alpha.f = 0.18),
        lwd = 0.6
      )
    }
    
    points(
      x, y,
      pch = 21,
      bg = opinion_colors(s$opinions),
      col = "gray30",
      cex = if (n <= 100) 1.2 else 0.8
    )
    
    if (identical(input$node_layout, "moving")) {
      axis(1, at = c(-1, 0, 1), labels = c("−1", "0", "+1"))
      mtext("Current opinion", side = 1, line = 2)
    }
  })
  
  output$trajectory_plot <- renderPlot({
    s <- state()
    req(s)
    h <- s$history
    steps <- seq_len(nrow(h)) - 1L
    par(mar = c(4, 4, 3, 1))
    plot(NA, xlim = c(0, max(1, s$step)), ylim = c(-1, 1),
         xlab = "Step", ylab = "Opinion", main = "Individual opinion trajectories")
    if (nrow(h) > 1) {
      matlines(steps, h, lty = 1, lwd = 1,
               col = grDevices::adjustcolor(opinion_colors(h[1, ]), alpha.f = 0.5))
    } else {
      points(rep(0, ncol(h)), h[1, ], pch = 16,
             col = opinion_colors(h[1, ]))
    }
    abline(h = 0, col = "gray75", lty = 3)
  })
  
  output$summary_plot <- renderPlot({
    s <- state()
    req(s)
    h <- s$history
    steps <- seq_len(nrow(h)) - 1L
    values <- cbind(rowMeans(h), apply(h, 1, sd))
    par(mar = c(4, 4, 3, 1))
    matplot(steps, values, type = if (nrow(h) > 1) "l" else "p",
            lty = 1, pch = 16, lwd = 2, col = c("#333333", "#7B3294"),
            xlim = c(0, max(1, s$step)), ylim = c(-1, 1.1),
            xlab = "Step", ylab = "Value", main = "Mean and opinion spread")
    legend("topleft", c("Mean opinion", "Standard deviation"),
           col = c("#333333", "#7B3294"), lty = 1, lwd = 2, bty = "n")
  })
  
  output$download_history <- downloadHandler(
    filename = function() paste0("opinions_step_", state()$step, ".csv"),
    content = function(file) {
      req(state())
      write.csv(history_data(state()), file, row.names = FALSE)
    }
  )
  output$download_network <- downloadHandler(
    filename = function() "network_edges.csv",
    content = function(file) {
      req(state())
      write.csv(network_data(state()), file, row.names = FALSE)
    }
  )
  output$download_settings <- downloadHandler(
    filename = function() "simulation_settings.csv",
    content = function(file) {
      req(state())
      settings <- state()$settings
      write.csv(data.frame(parameter = names(settings),
                           value = vapply(settings, as.character, character(1))),
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)