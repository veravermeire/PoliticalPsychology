# Opinion dynamics Shiny app.
# Run from this folder (or use shiny::runApp() on this folder).
# simulation.R supplies model logic; anes_data.R supplies survey loading.
# This file defines presentation and session-specific reactive behaviour.

# ---- Dependencies and data ----
library(shiny)

sim_file <- list.files(pattern = "^simulation\\.[rR]$")
if (length(sim_file) == 0) {
  stop("simulation.R not found in the app folder.", call. = FALSE)
}
source(sim_file[1], local = TRUE)
source("anes_data.R", local = TRUE)

# Load the ANES respondents (the csv in data/). If the file is missing the
# app still works in simulation mode and shows a message in the real-data view.
anes_people <- tryCatch(load_anes_people(), error = function(e) NULL)

# ---- Display helpers ----

# Blue = -1, white = 0, red = +1; shared by all opinion displays.
opinion_palette <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(201)

#' Map opinions to the shared blue-white-red palette.
#' @param x Numeric opinion vector, normally in [-1, 1]; NA remains NA.
#' @return Character vector of colours; out-of-range values use endpoint colours.
opinion_colors <- function(x) {
  opinion_palette[pmax(1L, pmin(201L, round((x + 1) * 100) + 1L))]
}

#' Compute matching coordinates for the network plot and hover detection.
#' @param opinions Numeric vector of opinions in [-1, 1], one per person.
#' @param layout "moving" positions people by opinion; otherwise uses a circle.
#' @return List with numeric x and y vectors in the original person order.
node_positions <- function(opinions, layout) {
  n <- length(opinions)
  angle <- 2 * pi * (seq_len(n) - 1) / n
  if (identical(layout, "moving")) {
    list(x = opinions, y = 0.18 * sin(angle))
  } else {
    list(x = cos(angle), y = sin(angle))
  }
}

#' Format proportions as whole percentages for the model explanation.
#' @param x Numeric vector of proportions, normally in [0, 1].
#' @return Character vector with one percentage string per input value.
pct <- function(x) sprintf("%.0f%%", 100 * x)

# Playback delays are in milliseconds; the default corresponds to 1x speed.
playback_settings <- list(default_delay = 250, min_delay = 50, max_delay = 2000)

# ---- Styling ----
app_css <- "
  /* Statistics panel */
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

  /* Column widths of the current-state tab */
  @media (min-width: 768px) {
    #network-column    { width: 62%; }
    #statistics-column { width: 38%; }
  }

  /* Hide the scale below each slider and keep slider spacing compact */
  .irs-grid { display: none !important; }
  .irs.irs-with-grid { height: 40px !important; }

  /* Playback panel */
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
  .playback-main-row .btn { min-width: 100px; }
  .playback-speed-row { margin-top: 10px; }
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
"

# ---- User interface ----
ui <- fluidPage(
  titlePanel("Opinion dynamics"),
  tags$head(tags$style(HTML(app_css))),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Model settings"),
      
      # Switch between the simulated population and the real survey data
      selectInput(
        "data_source", "Data source",
        choices = c(
          "Simulated opinions" = "sim",
          "ANES 2024 survey (real data, static)" = "anes"
        ),
        selected = "sim"
      ),
      
      # Starting pattern of the simulation (also what real data is compared with)
      selectInput(
        "pattern", "Simulated initial opinions",
        choices = c(
          "Broad spread" = "broad",
          "Near the middle" = "middle",
          "Two groups" = "two_groups"
        ),
        selected = "two_groups"
      ),
      numericInput("seed", "Random seed", value = 123, min = 0, step = 1),
      
      # Everything below only matters for the simulation
      conditionalPanel(
        condition = "input.data_source == 'sim'",
        sliderInput("n", "People", min = 10, max = 300, value = 100, step = 10),
        sliderInput("average_degree", "Target average connections",
                    min = 0, max = 99, value = 10, step = 1
        ),
        tags$hr(),
        sliderInput("identity_strength", "Party identity strength",
                    min = 0, max = 1, value = 0.5, step = 0.05
        ),
        sliderInput("perceived_polarization", "Perceived polarization (start)",
                    min = 0, max = 2, value = 1, step = 0.05
        ),
        sliderInput("congenial_media", "Congenial media exposure",
                    min = 0, max = 1, value = 0.75, step = 0.05
        ),
        actionButton("reset", "Reset / apply", class = "btn-primary"),
        tags$hr(),
        radioButtons("node_layout", "Node positions",
                     choices = c("Move with opinions" = "moving", "Fixed" = "fixed"),
                     selected = "moving", inline = TRUE
        ),
        tags$hr(),
        downloadButton("download_history", "Download opinion history (CSV)",
                       class = "btn-sm"
        )
      )
    ),
    mainPanel(
      width = 9,
      
      # ---- Simulation view ----
      conditionalPanel(
        condition = "input.data_source == 'sim'",
        verbatimTextOutput("status"),
        tabsetPanel(
          tabPanel(
            "Current state",
            fluidRow(
              column(
                width = 8, id = "network-column",
                div(
                  style = "position: relative;",
                  plotOutput(
                    "network_plot",
                    height = "auto",
                    hover = hoverOpts(
                      id = "network_hover", delay = 50,
                      delayType = "throttle", nullOutside = TRUE
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
                width = 4, id = "statistics-column",
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
                "Blue = \u22121; white = 0; red = +1.",
                paste0(
                  "Gray lines are fixed connections; how much influence they ",
                  "carry can differ by person and party."
                ),
                "Node positions follow the selected layout."
              )
            )
          ),
          tabPanel(
            "History",
            plotOutput("trajectory_plot", height = "360px"),
            plotOutput("summary_plot", height = "280px")
          ),
          
          # Rules tab (built in the server from the model constants)
          tabPanel("Rules", uiOutput("rules"))
        )
      ),
      
      # ---- Real-data view ----
      conditionalPanel(
        condition = "input.data_source == 'anes'",
        h4("Where people fall on the opinion continuum"),
        p(paste0(
          "Each dot is one person, placed by opinion (left = liberal, right = ",
          "conservative) and coloured as in the simulation, but with no connections. ",
          "The top panel shows the survey respondents; the bottom panel shows the ",
          "simulated starting population chosen in the sidebar, with the same number ",
          "of people. Dashed lines mark each group's average opinion."
        )),
        plotOutput("cmp_continuum", height = "480px"),
        tags$small(paste0(
          "Survey answers are on a 7-point scale, so respondents really sit at only ",
          "7 positions. To show them as a continuum like the simulation, each dot is ",
          "spread randomly by up to one scale step either side of its true answer ",
          "(and randomly up or down); this spread is for display only and carries no ",
          "meaning, so do not read exact values off the plot. The dashed averages ",
          "use the true answers. Real groups are party sides (leaners included); ",
          "simulated groups are the model's two fixed parties."
        ))
      )
    )
  )
)

# ---- Server ----

#' Connect the UI to simulation playback and static survey comparisons.
#' @param input Shiny reactive inputs whose IDs are defined in ui.
#' @param output Shiny output registry for the plots, tables, and controls.
#' @param session Current Shiny session, including plot dimensions and updates.
#' @return No application data; registers reactive outputs and observers.
#' @details Requires the sourced model helpers and loaded anes_people (or NULL).
#'   Simulation state and playback controls are private to each session.
server <- function(input, output, session) {
  # Each session owns its simulation state and playback controls.
  state <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  playback_delay <- reactiveVal(playback_settings$default_delay)
  
  # ---- Settings and (re)initialisation ----
  
  # Keep the connections slider feasible for the current population
  observeEvent(input$n, {
    req(input$n)
    current_degree <- isolate(input$average_degree)
    if (is.null(current_degree)) {
      current_degree <- 10
    }
    updateSliderInput(session, "average_degree",
                      max = input$n - 1,
                      value = min(current_degree, input$n - 1)
    )
  })
  
  # Settings currently chosen in the sidebar (names match initialize_model)
  requested_settings <- reactive({
    req(
      input$n, input$pattern, !is.na(input$seed),
      !is.null(input$average_degree),
      !is.null(input$identity_strength),
      !is.null(input$perceived_polarization),
      !is.null(input$congenial_media)
    )
    list(
      n                      = input$n,
      pattern                = input$pattern,
      average_degree         = min(input$average_degree, input$n - 1),
      identity_strength      = input$identity_strength,
      perceived_polarization = input$perceived_polarization,
      congenial_media        = input$congenial_media,
      max_steps              = 200,
      seed                   = input$seed
    )
  })
  
  # "Reset / apply" and startup run
  observeEvent(input$reset,
               {
                 running(FALSE)
                 tryCatch(
                   {
                     settings <- requested_settings()
                     new_state <- do.call(initialize_model, settings)
                     new_state$applied_inputs <- settings # Detect unapplied sidebar changes.
                     state(new_state)
                   },
                   error = function(e) {
                     showNotification(conditionMessage(e), type = "error", duration = 8)
                   }
                 )
               },
               ignoreNULL = FALSE
  )
  
  # ---- Playback controls ----
  
  observeEvent(input$step_once, {
    req(state())
    running(FALSE)
    state(step_model(state()))
  })
  
  observeEvent(input$step_back, {
    req(state())
    running(FALSE)
    state(back_model(state()))
  })
  
  observeEvent(input$run_pause, {
    req(state())
    if (running()) {
      running(FALSE)
    } else if (state()$step >= state()$settings$max_steps) {
      showNotification("Run complete. Click Reset / apply to start again.", type = "message")
    } else {
      running(TRUE)
    }
  })
  
  output$speed_controls <- renderUI({
    delay <- playback_delay()
    is_running <- running()
    speed_text <- format(
      round(playback_settings$default_delay / delay, 2),
      trim = TRUE, scientific = FALSE
    )
    
    slower_disabled <- !is_running || delay >= playback_settings$max_delay
    faster_disabled <- !is_running || delay <= playback_settings$min_delay
    
    div(
      class = "playback-speed-row",
      actionButton("slow_down", "Slower",
                   class = "btn-sm",
                   disabled = if (slower_disabled) "disabled" else NULL
      ),
      span(paste0("Speed: ", speed_text, "\u00d7"), class = "playback-speed-label"),
      actionButton("speed_up", "Faster",
                   class = "btn-sm",
                   disabled = if (faster_disabled) "disabled" else NULL
      )
    )
  })
  
  observeEvent(input$slow_down,
               {
                 req(running())
                 playback_delay(min(playback_settings$max_delay, playback_delay() * 2))
               },
               ignoreInit = TRUE
  )
  
  observeEvent(input$speed_up,
               {
                 req(running())
                 playback_delay(max(playback_settings$min_delay, playback_delay() / 2))
               },
               ignoreInit = TRUE
  )
  
  # Non-blocking playback. State updates are isolated so they do not trigger
  # extra immediate runs of this observer. Playback stops when opinions and
  # perceptions both changed by < 0.001 for four consecutive steps.
  observe({
    if (running()) {
      invalidateLater(playback_delay(), session)
      
      isolate({
        current_state <- state()
        if (is.null(current_state)) {
          running(FALSE)
        } else {
          current_state <- step_model(current_state)
          state(current_state)
          
          if (model_is_stable(current_state)) {
            running(FALSE)
            showNotification(
              paste0(
                "Stopped: all opinion and perception changes were below ",
                "0.001 for four consecutive steps."
              ),
              type = "message", duration = 6
            )
          } else if (current_state$step >= current_state$settings$max_steps) {
            running(FALSE)
          }
        }
      })
    }
  })
  
  observe({
    updateActionButton(session, "run_pause",
                       label = if (running()) "Pause" else "Run"
    )
  })
  
  # ---- Simulation outputs ----
  
  output$metrics <- renderTable(
    {
      current_state <- state()
      req(current_state)
      
      data.frame(
        Metric = c(
          "Mean opinion",
          "Opinion standard deviation",
          "Opinion range",
          "Distance between party means",
          "Mean perceived polarization",
          "Actual average connections"
        ),
        Value = c(
          sprintf("%.3f", mean(current_state$opinions)),
          sprintf("%.3f", sd(current_state$opinions)),
          sprintf("%.3f", diff(range(current_state$opinions))),
          sprintf("%.3f", actual_party_distance(current_state$opinions, current_state$party)),
          sprintf("%.3f", mean(current_state$perceived)),
          sprintf("%.2f", mean(rowSums(current_state$adjacency)))
        )
      )
    },
    striped = TRUE,
    rownames = FALSE,
    width = "100%"
  )
  
  output$histogram <- renderPlot({
    current_state <- state()
    req(current_state)
    
    opinion_histogram <- hist(current_state$opinions,
                              breaks = seq(-1, 1, length.out = 21),
                              plot = FALSE, include.lowest = TRUE
    )
    
    par(mar = c(4, 4, 3, 1))
    plot(
      opinion_histogram,
      freq = TRUE,
      col = opinion_colors(opinion_histogram$mids),
      border = "white",
      main = paste("Political opinions - step", current_state$step),
      xlab = "Political opinion",
      ylab = "Number of people",
      xlim = c(-1, 1),
      ylim = c(0, length(current_state$opinions)),
      xaxs = "i", yaxs = "i", xaxt = "n"
    )
    axis(1,
         at = c(-1, -0.5, 0, 0.5, 1),
         labels = c("\u22121", "\u22120.5", "0", "+0.5", "+1")
    )
  })
  
  output$network_plot <- renderPlot(
    {
      current_state <- state()
      req(current_state)
      
      n <- length(current_state$opinions)
      moving_layout <- identical(input$node_layout, "moving")
      positions <- node_positions(current_state$opinions, input$node_layout)
      x <- positions$x
      y <- positions$y
      
      plot_title <- if (moving_layout) {
        paste("Opinion-based positions - step", current_state$step)
      } else {
        paste("Fixed positions - step", current_state$step)
      }
      y_limits <- if (moving_layout) c(-0.195, 0.195) else c(-1.06, 1.06)
      bottom_margin <- if (moving_layout) 3 else 0.5
      aspect_ratio <- if (moving_layout) NA else 1
      
      par(
        mar = c(bottom_margin, 0.5, 2, 0.5),
        pty = "m", xaxs = "i", yaxs = "i"
      )
      
      plot(x, y,
           type = "n", asp = aspect_ratio,
           axes = FALSE, xlab = "", ylab = "",
           xlim = c(-1.06, 1.06), ylim = y_limits, main = plot_title
      )
      
      edges <- network_data(current_state)
      if (nrow(edges) > 0) {
        segments(x[edges$from], y[edges$from], x[edges$to], y[edges$to],
                 col = grDevices::adjustcolor("gray50", alpha.f = 0.18), lwd = 0.6
        )
      }
      
      node_size <- if (n <= 100) 1.2 else 0.8
      points(x, y,
             pch = 21, bg = opinion_colors(current_state$opinions), col = "gray30",
             cex = node_size
      )
      
      if (moving_layout) {
        axis(1, at = c(-1, 0, 1), labels = c("\u22121", "0", "+1"))
        mtext("Current opinion", side = 1, line = 2)
      }
    },
    height = function() {
      if (identical(input$node_layout, "moving")) {
        480
      } else {
        plot_width <- session$clientData$output_network_plot_width
        if (is.null(plot_width) || !is.finite(plot_width) || plot_width <= 0) {
          plot_width <- 560
        }
        min(plot_width, 480)
      }
    }
  )
  
  # ---- Hover tooltip ----
  output$node_tooltip <- renderUI({
    current_state <- state()
    hover <- input$network_hover
    req(current_state, hover)
    
    positions <- node_positions(current_state$opinions, input$node_layout)
    
    nodes <- data.frame(
      participant_id   = seq_along(current_state$opinions),
      x                = positions$x,
      y                = positions$y,
      current_opinion  = current_state$opinions,
      starting_opinion = as.numeric(current_state$history[1, ]),
      party            = current_state$party,
      identity         = current_state$identity,
      perceived        = current_state$perceived,
      congenial        = current_state$congenial,
      connections      = rowSums(current_state$adjacency)
    )
    
    # Select the closest node within 12 screen pixels
    selected <- nearPoints(nodes, hover,
                           xvar = "x", yvar = "y",
                           threshold = 12, maxpoints = 1
    )
    if (nrow(selected) == 0L) {
      return(NULL)
    }
    
    # Keep the tooltip inside the plot where possible
    plot_width <- session$clientData$output_network_plot_width
    plot_height <- session$clientData$output_network_plot_height
    if (is.null(plot_width)) {
      plot_width <- 560
    }
    if (is.null(plot_height)) {
      plot_height <- 480
    }
    
    left <- min(hover$coords_css$x + 14, max(8, plot_width - 230))
    top <- min(hover$coords_css$y + 14, max(8, plot_height - 190))
    
    party_label <- if (selected$party > 0) "+1" else "-1"
    
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
      tags$strong(paste("Participant ID:", selected$participant_id)),
      tags$br(),
      paste("Party:", party_label),
      tags$br(),
      paste("Current opinion:", sprintf("%.3f", selected$current_opinion)),
      tags$br(),
      paste("Starting opinion:", sprintf("%.3f", selected$starting_opinion)),
      tags$br(),
      paste("Identity strength:", sprintf("%.2f", selected$identity)),
      tags$br(),
      paste("Perceived polarization:", sprintf("%.2f", selected$perceived)),
      tags$br(),
      paste("Congenial media:", sprintf("%.2f", selected$congenial)),
      tags$br(),
      paste("Connections:", selected$connections)
    )
  })
  
  output$trajectory_plot <- renderPlot({
    current_state <- state()
    req(current_state)
    opinion_history <- current_state$history
    steps <- seq_len(nrow(opinion_history)) - 1L
    
    par(mar = c(4, 4, 3, 1))
    plot(NA,
         xlim = c(0, max(1, current_state$step)), ylim = c(-1, 1),
         xlab = "Step", ylab = "Opinion",
         main = "Individual opinion trajectories"
    )
    
    if (nrow(opinion_history) > 1) {
      matlines(steps, opinion_history,
               lty = 1, lwd = 1,
               col = grDevices::adjustcolor(opinion_colors(opinion_history[1, ]), alpha.f = 0.5)
      )
    } else {
      points(
        rep(0, ncol(opinion_history)), opinion_history[1, ],
        pch = 16, col = opinion_colors(opinion_history[1, ])
      )
    }
    abline(h = 0, col = "gray75", lty = 3)
  })
  
  output$summary_plot <- renderPlot({
    current_state <- state()
    req(current_state)
    opinion_history <- current_state$history
    steps <- seq_len(nrow(opinion_history)) - 1L
    values <- cbind(rowMeans(opinion_history), apply(opinion_history, 1, sd))
    plot_type <- if (nrow(opinion_history) > 1) "l" else "p"
    
    par(mar = c(4, 4, 3, 1))
    matplot(steps, values,
            type = plot_type,
            lty = 1, pch = 16, lwd = 2,
            col = c("#333333", "#7B3294"),
            xlim = c(0, max(1, current_state$step)), ylim = c(-1, 1.1),
            xlab = "Step", ylab = "Value",
            main = "Mean and opinion spread"
    )
    legend("topleft", c("Mean opinion", "Standard deviation"),
           col = c("#333333", "#7B3294"), lty = 1, lwd = 2, bty = "n"
    )
  })
  
  # Status line: progress, plus a hint if settings changed since the last apply
  output$status <- renderText({
    current_state <- state()
    req(current_state)
    changed <- !identical(current_state$applied_inputs, requested_settings())
    hint <- if (changed) "\nSettings changed - click Reset / apply to use them." else ""
    paste0(
      length(current_state$opinions), " people | step ", current_state$step,
      " of ", current_state$settings$max_steps, hint
    )
  })
  
  # Download the opinion history of the current run
  output$download_history <- downloadHandler(
    filename = function() paste0("opinion_history_step", state()$step, ".csv"),
    content = function(file) {
      write.csv(history_data(state()), file, row.names = FALSE)
    }
  )
  
  # ---- Rules: read MODEL constants so the explanation stays in sync ----
  output$rules <- renderUI({
    tagList(
      h4("One simulation step"),
      tags$ol(
        tags$li(
          paste0(
            "Social influence: each person is pulled toward their connections. ",
            "Same-party connections count fully; other-party connections count less, ",
            "by the factor exp(-", MODEL$identity_effect, " x party identity strength ",
            "- ", MODEL$perception_effect, " x perceived polarization / 2). ",
            "The pull is divided by the number of connections, so someone with mostly ",
            "other-party contacts stays weakly influenced."
          )
        ),
        tags$li(
          paste0(
            "Media: ", pct(MODEL$media_share), " of the total pull comes from media and ",
            pct(1 - MODEL$media_share),
            " from connections. Congenial media exposure is the share ",
            "of media that pulls toward the person's own party message (at +/-",
            MODEL$media_position, "); the rest pulls toward the other party's message, ",
            "discounted by the same receptiveness factor as in step 1."
          )
        ),
        tags$li(
          paste0(
            "Each person moves ", pct(MODEL$adjustment),
            " of the way toward this combined pull."
          )
        ),
        tags$li(
          paste0(
            "Perceived polarization: each person moves ", pct(MODEL$perception_rate),
            " toward new evidence, a blend of the gap between the two parties' average ",
            "opinions among themselves and their connections (unchanged if one party is ",
            "missing locally; ", pct(1 - MODEL$media_share), ") and a media-suggested gap of ",
            MODEL$media_distance, " (", pct(MODEL$media_share), ")."
          )
        ),
        tags$li("All changes are applied simultaneously, using values from the previous step.")
      ),
      p(paste0(
        "Parties are fixed and balanced in size. Crossing opinion 0 does not ",
        "change someone's party."
      )),
      p("Broad spread: uniform between \u22121 and +1."),
      p("Near the middle: normal with mean 0 and standard deviation 0.2."),
      p(paste0(
        "Two groups: normals centred at \u22120.6 (one party) and +0.6 (the other ",
        "party), each with standard deviation 0.15."
      )),
      p("Normal draws outside the opinion bounds are redrawn, not clipped."),
      p(paste0(
        "Connections are random and symmetric: each pair is connected with ",
        "probability (target average connections) / (people - 1). The realized ",
        "average can differ."
      )),
      p(paste0(
        "The three sliders only change how people respond; the same seed gives the ",
        "same starting opinions and connections whatever their values."
      )),
      p(paste0(
        "Standard deviation and opinion range measure spread; they are not ",
        "complete measures of polarization. Stable opinions do not necessarily ",
        "imply agreement."
      )),
      p(paste0(
        "The random seed in the sidebar (default 123) makes runs reproducible: ",
        "unchanged settings reproduce the same starting situation."
      )),
      p(paste0(
        "Stepping back pauses playback, restores the previous opinions and ",
        "perceptions, and removes subsequent history. Stepping forward recomputes ",
        "the next step with the same network and settings."
      ))
    )
  })
  
  # ---- Static survey comparison ----
  
  # Simulated starting population with the same number of people as the survey.
  # Party is drawn first, exactly as initialize_model() does.
  simulated_starting_population <- reactive({
    req(input$pattern, !is.na(input$seed), !is.null(anes_people))
    n <- nrow(anes_people)
    pattern <- input$pattern
    with_seed(input$seed, function() {
      party <- sample(rep(c(-1L, 1L), length.out = n))
      list(party = party, opinions = initial_opinions(n, pattern, party))
    })
  })
  
  # Message shown instead of a plot when the data file is missing
  require_anes_data <- function() {
    validate(need(
      !is.null(anes_people),
      "ANES data file not found in the data/ folder. See the README."
    ))
  }
  
  party_colors <- c(D = "#2166AC", R = "#B2182B") # Democrat/leaner, Republican/leaner
  pattern_labels <- c(
    broad = "broad spread", middle = "near the middle",
    two_groups = "two groups"
  )
  
  output$cmp_continuum <- renderPlot({
    require_anes_data()
    survey_opinions <- anes_people$opinion
    simulated_start <- simulated_starting_population()
    simulated_opinions <- simulated_start$opinions
    simulated_party <- simulated_start$party
    seed <- if (is.na(input$seed)) 1 else input$seed
    
    # Random spread for display only (reproducible with the seed). Each survey
    # answer is spread by up to a full scale step (1/3) either way, with more
    # dots near the true answer, so neighbouring answers blend into a continuum.
    # Dots that would leave the -1..+1 axis are reflected back inside.
    positions <- with_seed(seed + 1, function() {
      step <- 1 / 3
      x <- survey_opinions + runif(length(survey_opinions), -step / 2, step / 2) +
        runif(length(survey_opinions), -step / 2, step / 2)
      x <- ifelse(x > 1, 2 - x, ifelse(x < -1, -2 - x, x))
      list(
        real_x = x,
        real_y = runif(length(survey_opinions), -1, 1),
        sim_y = runif(length(simulated_opinions), -1, 1)
      )
    })
    
    # A missing party has no mean; NA also suppresses its plot label.
    mean_or_na <- function(x) if (length(x)) mean(x) else NA_real_
    survey_means <- c(
      mean_or_na(survey_opinions[anes_people$side %in% "D"]),
      mean_or_na(survey_opinions[anes_people$side %in% "R"])
    )
    simulated_means <- c(
      mean_or_na(simulated_opinions[simulated_party == -1L]),
      mean_or_na(simulated_opinions[simulated_party == 1L])
    )
    
    # Both panels share axes and styling so the visual comparison is consistent.
    # x/y are display coordinates; color_opinions supplies the true opinions.
    # means, labels, and label_colors have two entries, one per party.
    # Draws into the current graphics device; returns no application data.
    draw_panel <- function(x, y, color_opinions, title, means, labels, label_colors) {
      par(mar = c(3.4, 0.5, 2.4, 0.5), xaxs = "i", yaxs = "i")
      plot(NA,
           xlim = c(-1.1, 1.1), ylim = c(-1.2, 1.45), axes = FALSE,
           xlab = "", ylab = "", main = title
      )
      abline(v = means, lty = 2, col = "grey40")
      points(x, y,
             pch = 21, cex = 0.75, lwd = 0.4,
             col = grDevices::adjustcolor("gray30", alpha.f = 0.35),
             bg = grDevices::adjustcolor(opinion_colors(color_opinions), alpha.f = 0.7)
      )
      if (!is.na(means[1])) {
        text(means[1], 1.35, labels[1], pos = 2, cex = 0.85, col = label_colors[1])
      }
      if (!is.na(means[2])) {
        text(means[2], 1.35, labels[2], pos = 4, cex = 0.85, col = label_colors[2])
      }
      axis(1, at = c(-1, 0, 1), labels = c("\u22121", "0", "+1"))
      mtext("\u2190 more liberal        more conservative \u2192",
            side = 1,
            line = 2.1, cex = 0.8
      )
    }
    
    survey_title <- sprintf(
      "ANES 2024 respondents (%s people)",
      format(length(survey_opinions), big.mark = ",")
    )
    simulated_title <- sprintf(
      "Simulated start: %s (%s people)",
      pattern_labels[[input$pattern]],
      format(length(simulated_opinions), big.mark = ",")
    )
    
    par(mfrow = c(2, 1))
    draw_panel(
      positions$real_x, positions$real_y, survey_opinions, survey_title, survey_means,
      c("Democratic side average", "Republican side average"),
      c(party_colors[["D"]], party_colors[["R"]])
    )
    draw_panel(
      simulated_opinions, positions$sim_y, simulated_opinions,
      simulated_title, simulated_means,
      c("Party -1 average", "Party +1 average"),
      c("grey30", "grey30")
    )
  })
}

shinyApp(ui, server)
