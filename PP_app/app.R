# load relevant libraries and sources
library(shiny)
source("simulation.R", local = TRUE)
source("anes_network.R", local = TRUE)

# Load the ANES respondents (the csv in data/). If the file is missing the
# app still works in simulation mode and shows a message in ANES mode.
anes_people <- tryCatch(load_anes_people(), error = function(e) NULL)

# create colour palette for opinions
opinion_palette <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(201)

# map opinion values to colours in palette
opinion_colors <- function(x) {
  opinion_palette[pmax(1L, pmin(201L, round((x + 1) * 100) + 1L))]
}

#### User Interface ####
ui <- fluidPage(titlePanel("Opinion dynamics"), tags$head(
  # Add CSS styling rules to the Shiny App
  tags$style(
    HTML(
      "

    /* Styling of statistics panel */
      .statistics-panel {
        background: #ffffff;
        border: 1px solid #e3e7eb;
        border-radius: 8px;
        padding: 18px;
        margin-top: 12px;
      }

    /* Styling of statistics heading */
      .statistics-panel .statistics-heading {
        font-size: 16px;
        font-weight: 600;
        margin: 0 0 14px 0;
      }

    /* Styling of statistics metrics */
      .statistics-panel #metrics,
      .statistics-panel #metrics table {
        width: 100% !important;
      }

    /* Styling of metrics table - remove bottom margin */
      .statistics-panel #metrics table {
        margin-bottom: 0;
      }

    /* Styling of metrics - gives header */
      .statistics-panel #metrics th,
      .statistics-panel #metrics td {
        padding: 10px 12px;
        vertical-align: middle;
      }

    /* Right-aligns the cell, prevents multiple-line wrapping */
      .statistics-panel #metrics th:last-child,
      .statistics-panel #metrics td:last-child {
        text-align: right;
        white-space: nowrap;
      }

    /* Create divider */
      .statistics-panel .statistics-divider {
        border-top: 1px solid #e3e7eb;
        margin: 18px 0 10px 0;
      }

    /* Allocate proportion of network to statistics column within current_state */
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

    /* Style playback panel */
      .network-playback {
        margin: 8px 0 16px;
        padding: 14px 10px;
        background: #f8fafc;
        border: 1px solid #e3e7eb;
        border-radius: 8px;
      }

    /* Style horizontal and vertical layout */
      .playback-main-row,
      .playback-speed-row {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-wrap: wrap;
        gap: 8px;
      }

    /* Style button size of playback row */
      .playback-main-row .btn {
        min-width: 100px;
      }

    /* Style white space around playback row */
      .playback-speed-row {
        margin-top: 10px;
      }

    /* Assign miminum width of speed-label */
      .playback-speed-label {
        min-width: 95px;
        text-align: center;
        color: #526170;
        font-size: 13px;
      }

    /* Make disabled playback buttons semi-transparent */
      .network-playback .btn:disabled {
        opacity: 0.45;
        cursor: not-allowed;
      }
    "
    )
  )
), 

# Define sidebar controls - Model Settings
sidebarLayout(
  sidebarPanel(
    width = 3,
    h4("Model settings"),

    # Switch between the simulated population and real survey respondents
    selectInput(
      "data_source",
      "Data source",
      choices = c(
        "Simulated opinions" = "sim",
        "ANES 2024 respondents (real data)" = "anes"
      ),
      selected = "sim"
    ),
    
    # Slider for n People
    sliderInput(
      "n",
      "People",
      min = 10,
      max = 300,
      value = 100,
      step = 10
    ),
    
    # Drop-down menu for opinion spread, with default=two_groups (simulation only)
    conditionalPanel(
      condition = "input.data_source == 'sim'",
    selectInput(
      "pattern",
      "Initial opinions",
      choices = c(
        "Broad spread" = "broad",
        "Near the middle" = "middle",
        "Two groups" = "two_groups"
      ),
      selected = "two_groups"
    )),
    
    # Slider for average connections
    sliderInput(
      "average_degree",
      "Target average connections",
      min = 0,
      max = 99,
      value = 10,
      step = 1
    ),
    
    # Slider for preference for similar opinions
    sliderInput(
      "homophily",
      "Preference for similar initial opinions",
      min = 0,
      max = 5,
      value = 0,
      step = 0.25
    ),
    
    # Slider for openness threshold (simulation only)
    conditionalPanel(
      condition = "input.data_source == 'sim'",
    sliderInput(
      "openness",
      "Openness threshold",
      min = 0,
      max = 2,
      value = 0.4,
      step = 0.05
    )),
    
    # Slider for adjustment per step
    sliderInput(
      "adjustment",
      "Adjustment fraction per step",
      min = 0,
      max = 1,
      value = 0.2,
      step = 0.05
    ),
    
    # Real-data controls
    conditionalPanel(
      condition = "input.data_source == 'anes'",
      sliderInput(
        "media_effect",
        "Extra similarity preference from congenial media",
        min = 0, max = 5, value = 1, step = 0.25
      ),
      sliderInput(
        "openness_range",
        "Openness: strong party identity (low end) to weak (high end)",
        min = 0, max = 1, value = c(0.2, 0.6), step = 0.05
      )
    ),

    numericInput("seed", "Random seed", value = 123, min = 0, step = 1),

    # Option for moving or fixed nodes in Network display
    actionButton("reset", "Reset / apply", class = "btn-primary"),
    tags$hr(),
    radioButtons(
      "node_layout",
      "Node positions",
      choices = c("Move with opinions" = "moving", "Fixed" = "fixed"),
      selected = "moving",
      inline = TRUE
    ),
    tags$hr(),
    downloadButton("download_history", "Download opinion history (CSV)", class = "btn-sm")
  ),
  
  # Main panel (tab)
  mainPanel(
    width = 9,
    verbatimTextOutput("status"),
     
    # Create three selectable tabs
    tabsetPanel(
      
      # Current State Tab
      tabPanel("Current state", fluidRow(
        column(
          width = 8,
          id = "network-column",
          div(
            style = "position: relative;",
            
            # Reserves space for generated network-plot with non-fixed height
            plotOutput(
              "network_plot",
              height = "auto",
              
              # configures hover information
              hover = hoverOpts(
                id = "network_hover",
                delay = 50,
                delayType = "throttle",
                nullOutside = TRUE
              )
            ),
            uiOutput("node_tooltip")
            
          ),
          
          # Create playback panel according to CSS styling rules
          div(
            class = "network-playback",
            
            # specify three playback buttons
            div(
              class = "playback-main-row",
              actionButton("run_pause", "Run", class = "btn-primary"),
              actionButton("step_back", "1 step back"),
              actionButton("step_once", "1 step next")
            ),
            uiOutput("speed_controls")
          )
        ),
        
        # Statistics column within current-state
        column(
          width = 4,
          id = "statistics-column",
          
          # Wraps the statistics in previously styled panel
          div(
            class = "statistics-panel",
            h5("Statistical Overview", class = "statistics-heading"),
            tableOutput("metrics"),
            div(class = "statistics-divider"), # adds the styled horizontal divider
            plotOutput("histogram", height = "320px")
          )
        )
      ), 
      
      # Combines and displays text explaining node colors, gray connections and node layout
      helpText(
        paste(
          "Blue = −1; white = 0; red = +1.",
          "Gray lines are all fixed connections,",
          "not necessarily eligible influences.",
          "Node positions follow the selected layout."
        )
      )),
      
      # History Tab
      tabPanel(
        "History",
        plotOutput("trajectory_plot", height = "360px"), # reserve space for trajectory plot
        plotOutput("summary_plot", height = "280px") # reserve space for summary plot
      ),
      
      # Rules Tab
      tabPanel(
        "Rules",
        h4("One simulation step"),
        
        # Creates numbered list of rules
        tags$ol(
          tags$li(
            "Find each person's connected neighbors whose opinion distance is at most the openness threshold."
          ),
          tags$li(
            "Calculate the average opinion of those eligible neighbors, excluding the person themself."
          ),
          tags$li(
            "Move the specified fraction toward that average. With no eligible neighbors, do not move."
          ),
          tags$li(
            "Apply all changes simultaneously using opinions from the previous step."
          )
        ),
        
        # Add paragraphs with further descriptions
        p("Broad spread: uniform between −1 and +1."),
        p("Near the middle: normal with mean 0 and standard deviation 0.2."),
        p(
          "Two groups: equal mixture probabilities of normals centered at −0.6 and +0.6, each with standard deviation 0.15. Actual group sizes may differ."
        ),
        p("Normal draws outside the opinion bounds are redrawn, not clipped."),
        p(
          "Edges are sampled independently. Their probabilities decrease exponentially with initial opinion distance when similarity preference is positive. Probabilities are calibrated to the target expected average degree; realized average degree can differ."
        ),
        p(
          "Standard deviation and opinion range measure spread; they are not complete measures of polarization. Stable opinions do not necessarily imply agreement."
        ),
        p(
          "The random seed in the sidebar (default 123) makes runs reproducible: unchanged settings reproduce the same starting situation."
        ),
        p(
          "Stepping back pauses playback, restores the previous opinions, and removes subsequent history. Stepping forward recomputes the next step with the same network and settings."
        ),
        h4("Real-data mode (ANES 2024)"),
        p("Each person is a respondent from the 2024 American National Election Studies Time Series. Starting opinion is the liberal-conservative self-placement rescaled to -1..+1."),
        p("Openness depends on how important the respondent says their party identity is: the more important, the lower the openness threshold (the range is set by the openness slider)."),
        p("Ties are constructed, not observed: ANES has no information on who knows whom. The probability of a tie decreases with opinion distance, and the preference for similar opinions is stronger between two people who watch more TV news from their own side (Fox shows for Republicans and leaners, MSNBC shows for Democrats and leaners)."),
        p("Pure independents are not included because they have no co-partisan media. Source: American National Election Studies, ANES 2024 Time Series Study (electionstudies.org).")
      )
    )
  )
)
)

#### Server ####
server <- function(input, output, session) {
  
  # Reactive elements
  state <- reactiveVal(NULL)          # storage for the simulation state
  running <- reactiveVal(FALSE)       # playback flag
  playback_delay <- reactiveVal(250)  # storage for playback delay
  
  # Generate Tooltip's HTLM and update when reactive dependencies change
  output$node_tooltip <- renderUI({
    
    s <- state()                      # read current simulation state
    hover <- input$network_hover      # read mouse-hover information
    req(s, hover)                     # stop rendering for unavailable information
    
    n <- length(s$opinions)                   # count n participants
    angle <- 2 * pi * (seq_len(n) - 1) / n    # assign participants evenly spaced angles
    
    # Match the positions used in the network plot.
        # positions for moving plot
    if (identical(input$node_layout, "moving")) {
      x <- s$opinions
      y <- 0.18 * sin(angle)
      
       # positions for fixed plot
    } else {
      x <- cos(angle)
      y <- sin(angle)
      
    }
    
    # creates table with one row per participant for data storage
    nodes <- data.frame(
      participant_id = seq_len(n),
      x = x,
      y = y,
      current_opinion = s$opinions,
      starting_opinion = as.numeric(s$history[1, ]),
      connections = rowSums(s$adjacency)
    )
    
    # Hovering: Select the closest node within 12 screen pixels
    selected <- nearPoints(
      nodes,
      hover,
      xvar = "x",
      yvar = "y",
      threshold = 12,
      maxpoints = 1
    )
    
    # Displays no tooltip if no node is close enough to cursor
    if (nrow(selected) == 0L)
      return(NULL)
    
    # Keep the tooltip inside the plot where possible.
    plot_width <- session$clientData$output_network_plot_width
    plot_height <- session$clientData$output_network_plot_height
    
    if (is.null(plot_width))
      plot_width <- 560
    if (is.null(plot_height))
      plot_height <- 480
    
    left <- min(hover$coords_css$x + 14, max(8, plot_width - 230))
    top <- min(hover$coords_css$y + 14, max(8, plot_height - (if (is.null(s$people)) 130 else 230)))
    
    # Create tooltip container
    div(
      
      # Combine CSS settings into one style string
      style = paste0(
        "position: absolute;",
        "left: ",
        max(8, left),
        "px;",
        "top: ",
        max(8, top),
        "px;",
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
      
      # Fill Tooltip: Participant ID
      tags$strong(paste(
        "Participant ID:", selected$participant_id
      )),
      tags$br(),
      
      # Fill Tooltip: Current opinion
      paste(
        "Current opinion:",
        sprintf("%.3f", selected$current_opinion)
      ),
      tags$br(),
      
      # Fill Tooltip: Starting opinion
      paste(
        "Starting opinion:",
        sprintf("%.3f", selected$starting_opinion)
      ),
      tags$br(),
      
      # Fill Tooltip: n connections
      paste("Connections:", selected$connections),
      
      # Real-data mode: survey attributes of this respondent
      if (!is.null(s$people)) {
        pp <- s$people[selected$participant_id, ]
        tagList(
          tags$br(),
          paste("Party side:", if (is.na(pp$side)) "independent" else
            if (pp$side == "D") "Democrat/leaner" else "Republican/leaner"),
          tags$br(),
          paste("Party identity importance:", sprintf("%.2f", pp$identity)),
          tags$br(),
          paste("Congenial media (0-1):", sprintf("%.2f", pp$media)),
          tags$br(),
          paste("Openness:", sprintf("%.2f", s$settings$openness[selected$participant_id])),
          tags$br(),
          paste("Perceived party distance:",
                if (is.na(pp$perceived_polarization)) "n/a" else
                  sprintf("%.0f of 6", pp$perceived_polarization))
        )
      }
    )
  })
  
  # Keep the connections slider feasible for the current population.
  # Run if population size changes
  observeEvent(input$n, {
    req(input$n)
    current_degree <- isolate(input$average_degree)
    if (is.null(current_degree))
      current_degree <- 10
    
    # set slider's maximum to n-1
    updateSliderInput(
      session,
      "average_degree",
      max = input$n - 1,
      value = min(current_degree, input$n - 1)
    )
  })
  
  # Recalculate settings after changed referenced inputs
  requested_settings <- reactive({
    req(input$n, input$data_source, !is.na(input$seed))
    
    # Settings shared by both modes
    base <- list(
      n = input$n,
      average_degree = min(input$average_degree, input$n - 1),
      homophily = input$homophily,
      adjustment = input$adjustment,
      max_steps = 200,
      seed = input$seed
    )
    
    if (identical(input$data_source, "anes")) {
      c(base, list(
        media_effect = input$media_effect,
        open_min = input$openness_range[1],
        open_max = input$openness_range[2]
      ))
    } else {
      req(input$pattern)
      c(base, list(pattern = input$pattern, openness = input$openness))
    }
  })
  
  # Sensible starting values when switching data source
  observeEvent(input$data_source, {
    if (identical(input$data_source, "anes")) {
      updateSliderInput(session, "average_degree", value = 4)
      updateSliderInput(session, "homophily", value = 1)
    } else {
      updateSliderInput(session, "average_degree", value = 10)
      updateSliderInput(session, "homophily", value = 0)
    }
  }, ignoreInit = TRUE)
  
  # Handle "Reset / apply" and startup runs
  observeEvent(input$reset, {
    running(FALSE)
    tryCatch({
      settings <- requested_settings()
      if (identical(input$data_source, "anes")) {
        if (is.null(anes_people)) {
          stop("ANES data file not found in the data/ folder. See the README.")
        }
        new_state <- do.call(initialize_anes_model,
                             c(list(people = anes_people), settings))
      } else {
        new_state <- do.call(initialize_model, settings)
      }
      # remember which inputs produced this state (used for the status line)
      new_state$applied_inputs <- c(list(source = input$data_source), settings)
      state(new_state)
    }, error = function(e) {
      showNotification(conditionMessage(e),
                       type = "error",
                       duration = 8)
    })
  }, ignoreNULL = FALSE)
  
  # Status line: current mode, progress, and a hint if settings have changed
  output$status <- renderText({
    s <- state()
    req(s)
    mode <- if (identical(s$settings$source, "anes")) {
      "ANES 2024 respondents"
    } else {
      "Simulated opinions"
    }
    current <- c(list(source = input$data_source), requested_settings())
    changed <- !identical(s$applied_inputs, current)
    paste0(
      mode, " | ", length(s$opinions), " people | step ", s$step, " of ",
      s$settings$max_steps,
      if (changed) "\nSettings changed - click Reset / apply to use them." else ""
    )
  })
  
  # Download the opinion history of the current run
  output$download_history <- downloadHandler(
    filename = function() paste0("opinion_history_step", state()$step, ".csv"),
    content = function(file) {
      write.csv(history_data(state()), file, row.names = FALSE)
    }
  )
  
  # Respond to the next-step button.
  observeEvent(input$step_once, {
    req(state())
    running(FALSE)
    state(step_model_any(state()))
  })
  
  # Respond to the step-back button
  observeEvent(input$step_back, {
    req(state())
    running(FALSE)
    
    s <- state()
    if (s$step <= 0L)
      return(invisible(NULL))
    
    previous_step <- s$step - 1L
    s$history <- s$history[seq_len(previous_step + 1L), , drop = FALSE]
    s$opinions <- as.numeric(s$history[nrow(s$history), ])
    s$step <- previous_step
    state(s)
  })
  
  # Respond to the "Run / Pause" button
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
  
  # Generate and update speed-control interface
  output$speed_controls <- renderUI({
    delay <- playback_delay()
    is_running <- running()
    speed_text <- format(round(250 / delay, 2),
                         trim = TRUE,
                         scientific = FALSE)
    
    # group speed controls
    div(
      class = "playback-speed-row",
      actionButton(
        "slow_down",
        "Slower",
        class = "btn-sm",
        disabled = if (!is_running ||
                       delay >= 2000)
          "disabled"
        else
          NULL
      ),
      
      # display relative speed (e.g., Speed: 1x)
      span(paste0("Speed: ", speed_text, "\u00d7"), class = "playback-speed-label"),
      
      # create "faster" button to alter play-back speeds
      actionButton(
        "speed_up",
        "Faster",
        class = "btn-sm",
        disabled = if (!is_running ||
                       delay <= 50)
          "disabled"
        else
          NULL
      )
    )
  })
  
  # Slow playback if clicked
  observeEvent(input$slow_down, {
    req(running())
    playback_delay(min(2000, playback_delay() * 2))
  }, ignoreInit = TRUE)
  
  # Speed up playback if cliked 
  observeEvent(input$speed_up, {
    req(running())
    playback_delay(max(50, playback_delay() / 2))
  }, ignoreInit = TRUE)
  
  # Nonblocking playback. Isolate state updates so they do not
  # trigger extra immediate iterations of this observer.
  # Stop after four consecutive steps with opinion changes < 0.001.
  
  # Advance simulation if playback is active
  observe({
    
    # exit if playback is paused
    if (!running())
      return(invisible(NULL))
    
    # run observed again after current delay (without blocking the app)
    invalidateLater(playback_delay(), session)
    
    # Isolate to prevent extra immediate iterations
    isolate({
      s <- state() # read current simulation state
      
      # stop if no simulation state exists
      if (is.null(s)) { 
        running(FALSE)
      } else {
        if (s$step < s$settings$max_steps) {
          s <- step_model_any(s)
          state(s)
        }
        
        stable <- FALSE # assume non-met stopping condition
        
        # Small-change condition: Ensure enough iterations exist to evaluate four consecutive transitions.
        if (nrow(s$history) >= 5L) {
          recent <- tail(s$history, 5L)
          
          changes <- abs(recent[2:5, , drop = FALSE] -
                           recent[1:4, , drop = FALSE])
          
          stable <- all(changes < 0.001) # stable only if past 4 transitions were < .001
        }
        
        # Stop playback when small-change condition has been met
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
  
  # Update button to Pause or Run depending on running status
  observe({
    updateActionButton(session, "run_pause", label = if (running())
      "Pause"
      else
        "Run")
  })
  
  
  output$metrics <- renderTable({
    s <- state()
    req(s)
    eligible <- s$adjacency &
      (abs(outer(s$opinions, s$opinions, "-")) <= s$settings$openness)
    
    metrics_df <- data.frame(
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
    if (!is.null(s$people)) {
      metrics_df <- rbind(metrics_df, data.frame(
        Metric = "Survey: mean perceived party distance (0-6)",
        Value = sprintf("%.2f", mean(s$people$perceived_polarization, na.rm = TRUE))
      ))
    }
    metrics_df
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
      mar = c(if (moving)
        3
        else
          0.5, 0.5, 2, 0.5),
      pty = "m",
      xaxs = "i",
      yaxs = "i"
    )
    
    plot(
      x,
      y,
      type = "n",
      asp = if (moving)
        NA
      else
        1,
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
        x[edges$from],
        y[edges$from],
        x[edges$to],
        y[edges$to],
        col = grDevices::adjustcolor("gray50", alpha.f = 0.18),
        lwd = 0.6
      )
    }
    
    points(
      x,
      y,
      pch = 21,
      bg = opinion_colors(s$opinions),
      col = "gray30",
      cex = if (n <= 100)
        1.2
      else
        0.8
    )
    
    if (moving) {
      axis(1,
           at = c(-1, 0, 1),
           labels = c("−1", "0", "+1"))
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
        steps,
        h,
        lty = 1,
        lwd = 1,
        col = grDevices::adjustcolor(opinion_colors(h[1, ]), alpha.f = 0.5)
      )
    } else {
      points(rep(0, ncol(h)), h[1, ], pch = 16, col = opinion_colors(h[1, ]))
    }
    abline(h = 0,
           col = "gray75",
           lty = 3)
  })
  
  output$summary_plot <- renderPlot({
    s <- state()
    req(s)
    h <- s$history
    steps <- seq_len(nrow(h)) - 1L
    values <- cbind(rowMeans(h), apply(h, 1, sd))
    
    par(mar = c(4, 4, 3, 1))
    matplot(
      steps,
      values,
      type = if (nrow(h) > 1)
        "l"
      else
        "p",
      lty = 1,
      pch = 16,
      lwd = 2,
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
      lty = 1,
      lwd = 2,
      bty = "n"
    )
  })
  
}

shinyApp(ui, server)
