# load relevant libraries and sources
library(shiny)
source("simulation.R", local = TRUE)
source("anes_data.R", local = TRUE)

# Load the ANES respondents (the csv in data/). If the file is missing the
# app still works in simulation mode and shows a message in the real-data view.
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
    
    # Switch between the simulated population and the real survey data
    selectInput(
      "data_source",
      "Data source",
      choices = c(
        "Simulated opinions" = "sim",
        "ANES 2024 survey (real data, static)" = "anes"
      ),
      selected = "sim"
    ),

    # Starting pattern of the simulation (also the pattern real data is compared with)
    selectInput(
      "pattern",
      "Simulated initial opinions",
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
    # Slider for n People
    sliderInput(
      "n",
      "People",
      min = 10,
      max = 300,
      value = 100,
      step = 10
    ),
    
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
    
    # Slider for openness threshold
    sliderInput(
      "openness",
      "Openness threshold",
      min = 0,
      max = 2,
      value = 0.4,
      step = 0.05
    ),
    
    # Slider for adjustment per step
    sliderInput(
      "adjustment",
      "Adjustment fraction per step",
      min = 0,
      max = 1,
      value = 0.2,
      step = 0.05
    ),
    
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
    )
  ),
  
  # Main panel (tab)
  mainPanel(
    width = 9,
    conditionalPanel(
      condition = "input.data_source == 'sim'",
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
        )
      )
    )
    ),
    conditionalPanel(
      condition = "input.data_source == 'anes'",
      h4("Where people fall on the opinion continuum"),
      p("Each dot is one person, placed by opinion (left = liberal, right = conservative) and coloured as in the simulation, but with no connections. The top panel shows the survey respondents; the bottom panel shows the simulated starting population chosen in the sidebar, with the same number of people. Dashed lines mark each group's average opinion."),
      plotOutput("cmp_continuum", height = "480px"),
      tags$small("Survey answers are on a 7-point scale, so respondents really sit at only 7 positions. To show them as a continuum like the simulation, each dot is spread randomly by up to one scale step either side of its true answer (and randomly up or down); this spread is for display only and carries no meaning, so do not read exact values off the plot. The dashed averages use the true answers. Real groups are party sides (leaners included); simulated groups are the people who start below vs. above 0."),
      tags$hr(),
      h4("Real-world check: how far apart are the two political groups?"),
      p("In the simulation, polarization is the distance between the average opinions of two groups. This page measures the same thing in the 2024 American National Election Studies (ANES) survey, taking the two groups to be Democrats/leaners and Republicans/leaners (opinion = liberal-conservative self-placement, -1 to +1). It then splits respondents into low and high levels of the three factors in the model: party identity strength, perceived polarization (how far apart the respondent places the two parties) and congenial media exposure."),
      fluidRow(
        column(7, plotOutput("cmp_distance", height = "400px")),
        column(5,
               h5("Reference points"),
               tableOutput("cmp_reference"),
               tags$small("Real and simulated groups are defined differently (party side vs. the sign of the starting opinion), so treat the comparison as rough. Run the simulation, then come back to see its current state in the last row."))
      ),
      h5("Who is in each low/high group"),
      tableOutput("cmp_factor_table"),
      tags$small("Bars and brackets are 95% confidence intervals. These are snapshots of different people, not changes over time, so they show associations only, not that a factor causes polarization. Respondents between the low and high cut-offs are left out of those comparisons. Pure independents are excluded because they have no news sources from 'their side'."),
      h5("Distributions of the survey measures"),
      fluidRow(
        column(4, plotOutput("cmp_identity", height = "300px")),
        column(4, plotOutput("cmp_media", height = "300px")),
        column(4, plotOutput("cmp_percpol", height = "300px"))
      ),
      h5("How the survey measures relate (correlations)"),
      tableOutput("cmp_cor"),
      tags$small("Congenial media is the number of regularly watched TV shows from the respondent's own side (Fox shows for Republicans and leaners, MSNBC shows for Democrats and leaners), capped at 3. Source: American National Election Studies, ANES 2024 Time Series Study (electionstudies.org).")
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
    top <- min(hover$coords_css$y + 14, max(8, plot_height - 130))
    
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
      paste("Connections:", selected$connections)
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
    req(input$n, input$pattern, !is.na(input$seed))
    
    # Collects input information
    list(
      n = input$n,
      pattern = input$pattern,
      average_degree = min(input$average_degree, input$n - 1),
      homophily = input$homophily,
      openness = input$openness,
      adjustment = input$adjustment,
      max_steps = 200,
      seed = input$seed
    )
  })
  
  # Handle "Reset / apply" and startup runs
  observeEvent(input$reset, {
    running(FALSE)
    tryCatch({
      new_state <- do.call(initialize_model, requested_settings())
      new_state$applied_inputs <- requested_settings()  # for the status line
      state(new_state)
    }, error = function(e) {
      showNotification(conditionMessage(e),
                       type = "error",
                       duration = 8)
    })
  }, ignoreNULL = FALSE)
  
  # Respond to the next-step button.
  observeEvent(input$step_once, {
    req(state())
    running(FALSE)
    state(step_model(state()))
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
          s <- step_model(s)
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
  

  # Status line: progress, and a hint if settings changed since the last apply
  output$status <- renderText({
    s <- state()
    req(s)
    changed <- !identical(s$applied_inputs, requested_settings())
    paste0(
      length(s$opinions), " people | step ", s$step, " of ", s$settings$max_steps,
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
  
  #### Real data (static comparison) ####
  
  # Simulated starting opinions, same number of people as the survey sample
  sim_start <- reactive({
    req(input$pattern, !is.na(input$seed), !is.null(anes_people))
    with_local_seed(input$seed, initial_opinions(nrow(anes_people), input$pattern))
  })
  
  # Group distance overall and at low/high levels of each factor (computed once)
  factor_dist <- reactive({
    req(!is.null(anes_people))
    factor_distances(anes_people)
  })
  
  # Message shown instead of a plot when the data file is missing
  need_data <- function() {
    validate(need(!is.null(anes_people),
                  "ANES data file not found in the data/ folder. See the README."))
  }
  
  compare_cols <- c("#3D405B", "#F2CC8F")      # high, low level (change colours here)
  anes_cols <- c(D = "#2166AC", R = "#B2182B")  # Democrat/leaner, Republican/leaner
  pattern_names <- c(broad = "broad spread", middle = "near the middle",
                     two_groups = "two groups")
  by_side <- function(x, levels) {
    tab <- table(factor(x, levels = levels), factor(anes_people$side, levels = c("D", "R")))
    t(100 * prop.table(tab, 2))                 # rows = side, columns = category
  }
  side_legend <- function(pos) {
    legend(pos, legend = c("Democrats & leaners", "Republicans & leaners"),
           fill = anes_cols, bty = "n", cex = 0.85)
  }
  ci_text <- function(est, lo, hi) sprintf("%.2f [%.2f, %.2f]", est, lo, hi)
  
  output$cmp_continuum <- renderPlot({
    need_data()
    real <- anes_people$opinion
    sim <- sim_start()
    seed <- if (is.na(input$seed)) 1 else input$seed
    
    # Random spread for display only (reproducible with the seed). Each survey
    # answer is spread by up to a full scale step (1/3) either way, with more
    # dots near the true answer, so neighbouring answers blend into a continuum.
    # Dots that would leave the -1..+1 axis are reflected back inside.
    pos <- with_local_seed(seed + 1, {
      step <- 1 / 3
      x <- real + runif(length(real), -step / 2, step / 2) +
        runif(length(real), -step / 2, step / 2)
      x <- ifelse(x > 1, 2 - x, ifelse(x < -1, -2 - x, x))
      list(real_x = x,
           real_y = runif(length(real), -1, 1),
           sim_y = runif(length(sim), -1, 1))
    })
    
    mean_or_na <- function(x) if (length(x)) mean(x) else NA_real_
    real_means <- c(mean_or_na(real[anes_people$side %in% "D"]),
                    mean_or_na(real[anes_people$side %in% "R"]))
    sim_means <- c(mean_or_na(sim[sim < 0]), mean_or_na(sim[sim > 0]))
    
    draw_panel <- function(x, y, col_value, title, means, labels, label_cols) {
      par(mar = c(3.4, 0.5, 2.4, 0.5), xaxs = "i", yaxs = "i")
      plot(NA, xlim = c(-1.1, 1.1), ylim = c(-1.2, 1.45), axes = FALSE,
           xlab = "", ylab = "", main = title)
      abline(v = means, lty = 2, col = "grey40")
      points(x, y, pch = 21, cex = 0.75, lwd = 0.4,
             col = grDevices::adjustcolor("gray30", alpha.f = 0.35),
             bg = grDevices::adjustcolor(opinion_colors(col_value), alpha.f = 0.7))
      if (!is.na(means[1])) {
        text(means[1], 1.35, labels[1], pos = 2, cex = 0.85, col = label_cols[1])
      }
      if (!is.na(means[2])) {
        text(means[2], 1.35, labels[2], pos = 4, cex = 0.85, col = label_cols[2])
      }
      axis(1, at = c(-1, 0, 1), labels = c("\u22121", "0", "+1"))
      mtext("\u2190 more liberal        more conservative \u2192", side = 1,
            line = 2.1, cex = 0.8)
    }
    
    par(mfrow = c(2, 1))
    draw_panel(pos$real_x, pos$real_y, real,
               sprintf("ANES 2024 respondents (%s people)", format(length(real), big.mark = ",")),
               real_means, c("Democratic side average", "Republican side average"),
               c(anes_cols[["D"]], anes_cols[["R"]]))
    draw_panel(sim, pos$sim_y, sim,
               sprintf("Simulated start: %s (%s people)", pattern_names[[input$pattern]],
                       format(length(sim), big.mark = ",")),
               sim_means, c("Group below 0: average", "Group above 0: average"),
               c("grey30", "grey30"))
  })
  
  output$cmp_distance <- renderPlot({
    need_data()
    fd <- factor_dist()
    overall <- fd[fd$Factor == "All respondents", ]
    f <- fd[fd$Factor != "All respondents", ]
    factors <- unique(f$Factor)
    par(mar = c(4.5, 5, 3.5, 1))
    plot(NA, xlim = c(0.5, 3.5), ylim = c(0, max(f$Upper, overall$Distance) * 1.1),
         xaxt = "n", xlab = "", las = 1,
         ylab = "Distance between group means\n(Republican side minus Democratic side)",
         main = "Real-world group distance at low vs. high\nlevels of each factor")
    axis(1, at = 1:3, labels = factors)
    abline(h = overall$Distance, lty = 2, col = "grey40")
    text(0.5, overall$Distance, "all respondents", adj = c(0, -0.6), cex = 0.8, col = "grey30")
    for (i in seq_along(factors)) {
      for (lv in c("Low", "High")) {
        r <- f[f$Factor == factors[i] & f$Level == lv, ]
        x <- i + if (lv == "Low") -0.15 else 0.15
        arrows(x, r$Lower, x, r$Upper, angle = 90, code = 3, length = 0.05, lwd = 2)
        points(x, r$Distance, pch = 21, cex = 2,
               bg = if (lv == "Low") compare_cols[2] else compare_cols[1])
      }
    }
    legend("bottomright", legend = c("Low", "High"), pch = 21, pt.cex = 1.6,
           pt.bg = c(compare_cols[2], compare_cols[1]), bty = "n")
  })
  
  output$cmp_reference <- renderTable({
    need_data()
    fd <- factor_dist()
    o <- fd[fd$Factor == "All respondents", ]
    start <- sim_start()
    rows <- data.frame(
      Source = c("Real survey (all respondents)",
                 sprintf("Simulated start: %s, %s people", pattern_names[[input$pattern]],
                         format(length(start), big.mark = ","))),
      Distance = c(ci_text(o$Distance, o$Lower, o$Upper),
                   sprintf("%.2f", sim_group_distance(start, start))),
      Groups = c("party side", "sign of starting opinion")
    )
    s <- state()
    if (!is.null(s)) {
      rows <- rbind(rows, data.frame(
        Source = sprintf("Simulation tab: step %d, %s start, %d people", s$step,
                         pattern_names[[s$settings$pattern]], length(s$opinions)),
        Distance = sprintf("%.2f", sim_group_distance(s$history[1, ], s$opinions)),
        Groups = "sign of starting opinion"
      ))
    }
    rows
  }, striped = TRUE, spacing = "s", width = "100%")
  
  output$cmp_factor_table <- renderTable({
    need_data()
    fd <- factor_dist()
    data.frame(
      Factor = fd$Factor, Level = fd$Level, Definition = fd$Definition,
      People = format(fd$People, big.mark = ","),
      `Group distance [95% CI]` = ci_text(fd$Distance, fd$Lower, fd$Upper),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "s", width = "100%")
  
  output$cmp_identity <- renderPlot({
    need_data()
    props <- by_side(round(anes_people$identity * 4), 0:4)
    par(mar = c(6, 4.5, 3, 1))
    barplot(props, beside = TRUE, col = anes_cols, las = 2, cex.names = 0.8,
            names.arg = c("Not at all", "A little", "Moderately", "Very", "Extremely"),
            ylab = "% within party side", main = "Party identity importance")
    side_legend("topleft")
  })
  
  output$cmp_media <- renderPlot({
    need_data()
    props <- by_side(round(anes_people$media * 3), 0:3)
    par(mar = c(6, 4.5, 3, 1))
    barplot(props, beside = TRUE, col = anes_cols, las = 1,
            names.arg = c("0", "1", "2", "3 or more"),
            xlab = "TV shows from own side", ylab = "% within party side",
            main = "Congenial TV news")
    side_legend("topright")
  })
  
  output$cmp_percpol <- renderPlot({
    need_data()
    pp <- anes_people$perceived_polarization
    pp <- pp[!is.na(pp)]
    grp <- factor(ifelse(pp < 0, "<0", as.character(pp)), levels = c("<0", 0:6))
    par(mar = c(6, 4.5, 3, 1))
    barplot(100 * prop.table(table(grp)), col = "#7570B3", las = 1,
            xlab = "Republican minus Democratic Party placement\n(<0: placed the other way round)",
            ylab = "% of people", main = "Perceived party distance")
  })
  
  output$cmp_cor <- renderTable({
    need_data()
    survey_correlations(anes_people)
  }, striped = TRUE, spacing = "s", width = "100%")

}

shinyApp(ui, server)