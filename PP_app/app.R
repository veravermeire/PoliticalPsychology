# app.R
# Keep your existing simulation.R in the same folder.

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
  
  tags$head(
    tags$style(HTML("
      .statistics-panel {
        background: #ffffff;
        border: 1px solid #e3e7eb;
        border-radius: 8px;
        padding: 18px;
        margin-top: 12px;
      }

      .statistics-panel .statistics-heading {
        font-size: 16px;
        font-weight: 600;
        margin: 0 0 14px 0;
      }

      .statistics-panel #metrics,
      .statistics-panel #metrics table {
        width: 100% !important;
      }

      .statistics-panel #metrics table {
        margin-bottom: 0;
      }

      .statistics-panel #metrics th,
      .statistics-panel #metrics td {
        padding: 10px 12px;
        vertical-align: middle;
      }

      .statistics-panel #metrics th:last-child,
      .statistics-panel #metrics td:last-child {
        text-align: right;
        white-space: nowrap;
      }

      .statistics-panel .statistics-divider {
        border-top: 1px solid #e3e7eb;
        margin: 18px 0 10px 0;
      }

      @media (min-width: 768px) {
        #network-column {
          width: 62%;
        }

        #statistics-column {
          width: 38%;
        }
      }

      /* Hide the scale below each slider. */
      .irs-grid {
        display: none !important;
      }

      /* Preserve the compact slider spacing from your current app. */
      .irs.irs-with-grid {
        height: 40px !important;
      }

      .network-playback {
        margin: 8px 0 16px;
        padding: 14px 10px;
        background: #f8fafc;
        border: 1px solid #e3e7eb;
        border-radius: 8px;
      }

      .playback-main-row,
      .playback-speed-row {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-wrap: wrap;
        gap: 8px;
      }

      .playback-main-row .btn {
        min-width: 100px;
      }

      .playback-speed-row {
        margin-top: 10px;
      }

      .playback-speed-label {
        min-width: 95px;
        text-align: center;
        color: #526170;
        font-size: 13px;
      }

      .network-playback .btn:disabled {
        opacity: 0.45;
        cursor: not-allowed;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Model settings"),
      
      sliderInput(
        "n", "People",
        min = 10, max = 300, value = 100, step = 10
      ),
      selectInput(
        "pattern", "Initial opinions",
        choices = c(
          "Broad spread" = "broad",
          "Near the middle" = "middle",
          "Two groups" = "two_groups"
        ),
        selected = "two_groups"
      ),
      sliderInput(
        "average_degree", "Target average connections",
        min = 0, max = 99, value = 10, step = 1
      ),
      sliderInput(
        "homophily", "Preference for similar initial opinions",
        min = 0, max = 5, value = 0, step = 0.25
      ),
      sliderInput(
        "openness", "Openness threshold",
        min = 0, max = 2, value = 0.4, step = 0.05
      ),
      sliderInput(
        "adjustment", "Adjustment fraction per step",
        min = 0, max = 1, value = 0.2, step = 0.05
      ),
      actionButton("reset", "Reset / apply", class = "btn-primary"),
      tags$hr(),
      radioButtons(
        "node_layout", "Node positions",
        choices = c(
          "Move with opinions" = "moving",
          "Fixed" = "fixed"
        ),
        selected = "moving",
        inline = TRUE
      )
      ),

    
    mainPanel(
      width = 9,
      verbatimTextOutput("status"),
      
      tabsetPanel(
        tabPanel(
          "Current state",
          fluidRow(
            column(
              width = 8,
              id = "network-column",
              div(
                style = "position: relative;",
                
                plotOutput(
                  "network_plot",
                  height = "auto",
                  hover = hoverOpts(
                    id = "network_hover",
                    delay = 50,
                    delayType = "throttle",
                    nullOutside = TRUE
                  )
                ),
                
                uiOutput("node_tooltip")
              ),
              div(
                class = "network-playback",
                div(
                  class = "playback-main-row",
                  actionButton("run_pause", "Run", class = "btn-primary"),
                  actionButton("step_back", "1 step back"),
                  actionButton("step_once", "1 step next")
                ),
                uiOutput("speed_controls")
              )
            ),
            column(
              width = 4,
              id = "statistics-column",
              div(
                class = "statistics-panel",
                h5("Statistical Overview", class = "statistics-heading"),
                tableOutput("metrics"),
                div(class = "statistics-divider"),
                plotOutput("histogram", height = "320px")
              )
            )
          ),
          helpText(
            paste(
              "Blue = −1; white = 0; red = +1.",
              "Gray lines are all fixed connections,",
              "not necessarily eligible influences.",
              "Node positions follow the selected layout."
            )
          )
        ),
        
        tabPanel(
          "History",
          plotOutput("trajectory_plot", height = "360px"),
          plotOutput("summary_plot", height = "280px")
        ),
        
        tabPanel(
          "Rules",
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
          p("Standard deviation and opinion range measure spread; they are not complete measures of polarization. Stable opinions do not necessarily imply agreement."),
          p("The app uses a fixed internal random seed of 123, so unchanged settings reproduce the same starting situation."),
          p("Stepping back pauses playback, restores the previous opinions, and removes subsequent history. Stepping forward recomputes the next step with the same network and settings.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  playback_delay <- reactiveVal(250)
  
  output$node_tooltip <- renderUI({
    s <- state()
    hover <- input$network_hover
    req(s, hover)
    
    n <- length(s$opinions)
    angle <- 2 * pi * (seq_len(n) - 1) / n
    
    # Match the positions used in the network plot.
    if (identical(input$node_layout, "moving")) {
      x <- s$opinions
      y <- 0.18 * sin(angle)
    } else {
      x <- cos(angle)
      y <- sin(angle)
    }
    
    nodes <- data.frame(
      participant_id = seq_len(n),
      x = x,
      y = y,
      current_opinion = s$opinions,
      starting_opinion = as.numeric(s$history[1, ]),
      connections = rowSums(s$adjacency)
    )
    
    # Select the closest node within 12 screen pixels.
    selected <- nearPoints(
      nodes,
      hover,
      xvar = "x",
      yvar = "y",
      threshold = 12,
      maxpoints = 1
    )
    
    if (nrow(selected) == 0L) return(NULL)
    
    # Keep the tooltip inside the plot where possible.
    plot_width <- session$clientData$output_network_plot_width
    plot_height <- session$clientData$output_network_plot_height
    
    if (is.null(plot_width)) plot_width <- 560
    if (is.null(plot_height)) plot_height <- 480
    
    left <- min(
      hover$coords_css$x + 14,
      max(8, plot_width - 230)
    )
    top <- min(
      hover$coords_css$y + 14,
      max(8, plot_height - 130)
    )
    
    div(
      style = paste0(
        "position: absolute;",
        "left: ", max(8, left), "px;",
        "top: ", max(8, top), "px;",
        "width: 220px;",
        "padding: 10px 12px;",
        "background: rgba(255, 255, 255, 0.97);",
        "border: 1px solid #d6dce2;",
        "border-radius: 6px;",
        "box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);",
        "font-size: 13px;",
        "line-height: 1.6;",
        "z-index: 100;",
        "pointer-events: none;"
      ),
      
      tags$strong(
        paste("Participant ID:", selected$participant_id)
      ),
      tags$br(),
      
      paste(
        "Current opinion:",
        sprintf("%.3f", selected$current_opinion)
      ),
      tags$br(),
      
      paste(
        "Starting opinion:",
        sprintf("%.3f", selected$starting_opinion)
      ),
      tags$br(),
      
      paste("Connections:", selected$connections)
    )
  })
  
  # Keep the connections slider feasible for the current population.
  observeEvent(input$n, {
    req(input$n)
    current_degree <- isolate(input$average_degree)
    if (is.null(current_degree)) current_degree <- 10
    
    updateSliderInput(
      session, "average_degree",
      max = input$n - 1,
      value = min(current_degree, input$n - 1)
    )
  })
  
  requested_settings <- reactive({
    req(input$n, input$pattern)
    list(
      n = input$n,
      pattern = input$pattern,
      average_degree = min(input$average_degree, input$n - 1),
      homophily = input$homophily,
      openness = input$openness,
      adjustment = input$adjustment,
      max_steps = 200,
      seed = 123
    )
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
  
  # Exactly one observer for the next-step button.
  observeEvent(input$step_once, {
    req(state())
    running(FALSE)
    state(step_model(state()))
  })
  
  observeEvent(input$step_back, {
    req(state())
    running(FALSE)
    
    s <- state()
    if (s$step <= 0L) return(invisible(NULL))
    
    previous_step <- s$step - 1L
    s$history <- s$history[
      seq_len(previous_step + 1L),
      ,
      drop = FALSE
    ]
    s$opinions <- as.numeric(s$history[nrow(s$history), ])
    s$step <- previous_step
    state(s)
  })
  
  observeEvent(input$run_pause, {
    req(state())
    
    if (running()) {
      running(FALSE)
    } else if (state()$step >= state()$settings$max_steps) {
      showNotification(
        "Run complete. Click Reset / apply to start again.",
        type = "message"
      )
    } else {
      running(TRUE)
    }
  })
  
  output$speed_controls <- renderUI({
    delay <- playback_delay()
    is_running <- running()
    speed_text <- format(
      round(250 / delay, 2),
      trim = TRUE,
      scientific = FALSE
    )
    
    div(
      class = "playback-speed-row",
      actionButton(
        "slow_down", "Slower",
        class = "btn-sm",
        disabled = if (!is_running || delay >= 2000) "disabled" else NULL
      ),
      span(
        paste0("Speed: ", speed_text, "\u00d7"),
        class = "playback-speed-label"
      ),
      actionButton(
        "speed_up", "Faster",
        class = "btn-sm",
        disabled = if (!is_running || delay <= 50) "disabled" else NULL
      )
    )
  })
  
  observeEvent(input$slow_down, {
    req(running())
    playback_delay(min(2000, playback_delay() * 2))
  }, ignoreInit = TRUE)
  
  observeEvent(input$speed_up, {
    req(running())
    playback_delay(max(50, playback_delay() / 2))
  }, ignoreInit = TRUE)
  
  # Nonblocking playback. Isolate state updates so they do not
  # trigger extra immediate iterations of this observer.
  # Stop after four consecutive steps with opinion changes < 0.001.
  observe({
    if (!running()) return(invisible(NULL))
    
    invalidateLater(playback_delay(), session)
    
    isolate({
      s <- state()
      
      if (is.null(s)) {
        running(FALSE)
      } else {
        if (s$step < s$settings$max_steps) {
          s <- step_model(s)
          state(s)
        }
        
        stable <- FALSE
        
        # Five history rows contain four consecutive transitions.
        if (nrow(s$history) >= 5L) {
          recent <- tail(s$history, 5L)
          
          changes <- abs(
            recent[2:5, , drop = FALSE] -
              recent[1:4, , drop = FALSE]
          )
          
          stable <- all(changes < 0.001)
        }
        
        if (stable) {
          running(FALSE)
          showNotification(
            "Stopped: all opinion changes were below 0.001 for four consecutive steps.",
            type = "message",
            duration = 6
          )
        } else if (s$step >= s$settings$max_steps) {
          running(FALSE)
        }
      }
    })
  })
  
  observe({
    updateActionButton(
      session, "run_pause",
      label = if (running()) "Pause" else "Run"
    )
  })
  

  
  output$metrics <- renderTable({
    s <- state()
    req(s)
    eligible <- s$adjacency &
      (abs(outer(s$opinions, s$opinions, "-")) <= s$settings$openness)
    
    data.frame(
      Metric = c(
        "Mean opinion",
        "Opinion standard deviation",
        "Opinion range",
        "Actual average connections",
        "People with no eligible neighbors"
      ),
      Value = c(
        sprintf("%.3f", mean(s$opinions)),
        sprintf("%.3f", sd(s$opinions)),
        sprintf("%.3f", diff(range(s$opinions))),
        sprintf("%.2f", mean(rowSums(s$adjacency))),
        as.character(sum(rowSums(eligible) == 0))
      )
    )
  }, striped = TRUE, rownames = FALSE, width = "100%")
  
  output$histogram <- renderPlot({
    s <- state()
    req(s)
    
    opinion_hist <- hist(
      s$opinions,
      breaks = seq(-1, 1, length.out = 21),
      plot = FALSE,
      include.lowest = TRUE
    )
    
    par(mar = c(4, 4, 3, 1))
    plot(
      opinion_hist,
      freq = TRUE,
      col = opinion_colors(opinion_hist$mids),
      border = "white",
      main = paste("Political opinions — step", s$step),
      xlab = "Political opinion",
      ylab = "Number of people",
      xlim = c(-1, 1),
      ylim = c(0, length(s$opinions)),
      xaxs = "i",
      yaxs = "i",
      xaxt = "n"
    )
    axis(
      side = 1,
      at = c(-1, -0.5, 0, 0.5, 1),
      labels = c("−1", "−0.5", "0", "+0.5", "+1")
    )
  })
  
  output$network_plot <- renderPlot({
    s <- state()
    req(s)
    
    n <- length(s$opinions)
    angle <- 2 * pi * (seq_len(n) - 1) / n
    moving <- identical(input$node_layout, "moving")
    
    if (moving) {
      x <- s$opinions
      y <- 0.18 * sin(angle)
      plot_title <- paste("Opinion-based positions — step", s$step)
      plot_ylim <- c(-0.195, 0.195)
    } else {
      x <- cos(angle)
      y <- sin(angle)
      plot_title <- paste("Fixed positions — step", s$step)
      plot_ylim <- c(-1.06, 1.06)
    }
    
    par(
      mar = c(if (moving) 3 else 0.5, 0.5, 2, 0.5),
      pty = "m",
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
      xlim = c(-1.06, 1.06),
      ylim = plot_ylim,
      main = plot_title
    )
    
    edges <- network_data(s)
    if (nrow(edges) > 0) {
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
    
    if (moving) {
      axis(1, at = c(-1, 0, 1), labels = c("−1", "0", "+1"))
      mtext("Current opinion", side = 1, line = 2)
    }
  }, height = function() {
    if (identical(input$node_layout, "moving")) {
      480
    } else {
      plot_width <- session$clientData$output_network_plot_width
      if (is.null(plot_width) ||
          !is.finite(plot_width) ||
          plot_width <= 0) {
        plot_width <- 560
      }
      min(plot_width, 480)
    }
  })
  
  output$trajectory_plot <- renderPlot({
    s <- state()
    req(s)
    h <- s$history
    steps <- seq_len(nrow(h)) - 1L
    
    par(mar = c(4, 4, 3, 1))
    plot(
      NA,
      xlim = c(0, max(1, s$step)),
      ylim = c(-1, 1),
      xlab = "Step",
      ylab = "Opinion",
      main = "Individual opinion trajectories"
    )
    
    if (nrow(h) > 1) {
      matlines(
        steps, h,
        lty = 1, lwd = 1,
        col = grDevices::adjustcolor(opinion_colors(h[1, ]), alpha.f = 0.5)
      )
    } else {
      points(
        rep(0, ncol(h)), h[1, ],
        pch = 16,
        col = opinion_colors(h[1, ])
      )
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
    matplot(
      steps, values,
      type = if (nrow(h) > 1) "l" else "p",
      lty = 1, pch = 16, lwd = 2,
      col = c("#333333", "#7B3294"),
      xlim = c(0, max(1, s$step)),
      ylim = c(-1, 1.1),
      xlab = "Step",
      ylab = "Value",
      main = "Mean and opinion spread"
    )
    legend(
      "topleft",
      c("Mean opinion", "Standard deviation"),
      col = c("#333333", "#7B3294"),
      lty = 1, lwd = 2, bty = "n"
    )
  })
  
}

shinyApp(ui, server)