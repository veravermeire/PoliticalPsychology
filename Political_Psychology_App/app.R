# app.R
# Keep this file and the revised simulation.R in the same folder.
# Install once: install.packages("shiny")
# Start with RStudio's Run App button or shiny::runApp("folder/path").
library(shiny)
source("simulation.R", local = TRUE)

opinion_palette <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(201)
opinion_colors <- function(x) {
  opinion_palette[pmax(1L, pmin(201L, round((x + 1) * 100) + 1L))]
}

ui <- fluidPage(
  titlePanel("Partisan identity and opinion dynamics"),
  tags$head(tags$style(HTML("
    .statistics-panel {border:1px solid #e3e7eb; border-radius:8px; padding:14px;}
    .statistics-panel table {width:100%;}
    .statistics-panel td:last-child {text-align:right; white-space:nowrap;}
    .network-playback {background:#f8fafc; padding:12px; border-radius:8px; text-align:center;}
    .playback-row {display:flex; justify-content:center; align-items:center; flex-wrap:wrap; gap:8px; margin:6px 0;}
    .playback-row .btn {min-width:85px;}
    .irs-grid {display:none !important;}
    .irs.irs-with-grid {height:40px !important;}
  "))),
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Background settings"),
                 sliderInput("n", "People", 10, 300, 100, step = 10),
                 selectInput("pattern", "Initial opinions", choices = c(
                   "Broad spread" = "broad", "Near the middle" = "middle",
                   "Two groups" = "two_groups"), selected = "two_groups"),
                 sliderInput("average_degree", "Target average connections", 0, 99, 10, step = 1),
                 tags$hr(),
                 h4("Behavioral settings"),
                 sliderInput("identity_strength", "Partisan identity strength", 0, 1, 0.5, step = 0.05),
                 helpText("Mean attachment: stronger identity reduces opposing-party influence."),
                 sliderInput("perceived_polarization", "Initial perceived polarization", 0, 1, 0.5, step = 0.1),
                 helpText("Mean estimated distance between parties. Perceptions evolve during the run."),
                 sliderInput("congenial_media", "Congenial media exposure", 0, 1, 0.75, step = 0.05),
                 helpText("Mean share of own-party media; the remainder is opposing-party media."),
                 actionButton("reset", "Reset / apply", class = "btn-primary"),
                 tags$hr(),
                 radioButtons("node_layout", "Node positions", choices = c(
                   "Move with opinions" = "moving", "Fixed" = "fixed"),
                   selected = "moving", inline = TRUE)
    ),
    mainPanel(width = 9,
              textOutput("status"),
              tabsetPanel(
                tabPanel("Current state",
                         fluidRow(
                           column(8,
                                  div(style = "position:relative;",
                                      plotOutput("network_plot", height = "460px", hover = hoverOpts(
                                        "network_hover", delay = 100, delayType = "throttle", nullOutside = TRUE)),
                                      uiOutput("node_tooltip")),
                                  div(class = "network-playback",
                                      div(class = "playback-row",
                                          actionButton("run_pause", "Run", class = "btn-primary"),
                                          actionButton("step_back", "1 step back"),
                                          actionButton("step_once", "1 step next")),
                                      div(class = "playback-row",
                                          actionButton("slow_down", "Slower", class = "btn-sm"),
                                          textOutput("speed_label", inline = TRUE),
                                          actionButton("speed_up", "Faster", class = "btn-sm")))),
                           column(4, div(class = "statistics-panel",
                                         h5("Statistical overview"), tableOutput("metrics"),
                                         plotOutput("histogram", height = "280px")))),
                         helpText("Fill: opinion (blue −1, white 0, red +1). Outline: party (A blue, B red). Size: identity strength. Gray lines: fixed connections with potentially unequal influence.")),
                tabPanel("History",
                         plotOutput("trajectory_plot", height = "340px"),
                         plotOutput("summary_plot", height = "280px")),
                tabPanel("Rules",
                         h4("One simulation step"),
                         tags$ol(
                           tags$li("Connected same-party contacts have influence weight 1. Opposing-party influence declines with the listener's identity strength and perceived party distance."),
                           tags$li("Opinions move toward contacts and media positions. Media is an expected exposure mix, not a fresh random message each step; this avoids sampling jitter."),
                           tags$li("Perceived distance moves toward locally observed party differences and the distance portrayed by media. Missing local outgroup evidence preserves the prior estimate for that component."),
                           tags$li("Both updates use the previous state. Party, identity, media preference, and connections remain fixed.")),
                         h4("Assumptions"),
                         p("Opinion adjustment: 0.20. Media share: 0.25. Perception adjustment: 0.10. Outgroup weight = exp(−1.5 × identity − perceived distance / 2)."),
                         p("Media positions: −0.60 and +0.60. Both party sources portray distance 1.20. Congeniality changes source selection, not extremity or media volume."),
                         p("Slider values set expected population means; individuals vary through beta distributions with concentration 20. Realized means can differ. Endpoint settings are constant across people."),
                         p("Parties are balanced and fixed. Broad initial opinions are uniform on [−1, 1]; middle opinions use a bounded normal (mean 0, SD 0.20). Two groups use party-aligned bounded normals (means −0.60/+0.60, SD 0.15). Draws outside the bounds are redrawn."),
                         p("The random undirected network has no party or opinion homophily. The connection setting specifies expected, not realized, average degree."),
                         p("The internal seed is 123. Changing only behavioral settings preserves the starting opinions, parties, and network, making comparisons easier."),
                         p("Stepping back restores both opinions and perceptions. Runs stop at 200 steps or after four transitions with all opinion and perception changes below 0.001. Small changes are not proof of agreement or equilibrium."),
                         p("Actual polarization here means the absolute distance between party mean opinions, not affective polarization. These rules are illustrative, not empirically calibrated."))
              )
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  playback_delay <- reactiveVal(250)
  
  observeEvent(input$n, {
    degree <- isolate(input$average_degree)
    if (is.null(degree)) degree <- 10
    updateSliderInput(session, "average_degree", max = input$n - 1,
                      value = min(degree, input$n - 1))
  })
  
  requested_settings <- reactive({
    req(input$n, input$pattern)
    # Check availability explicitly; zero is a valid slider setting.
    req(!is.null(input$average_degree), !is.null(input$identity_strength),
        !is.null(input$perceived_polarization), !is.null(input$congenial_media))
    list(n = input$n, pattern = input$pattern,
         average_degree = min(input$average_degree, input$n - 1),
         identity_strength = input$identity_strength,
         perceived_polarization = input$perceived_polarization,
         congenial_media = input$congenial_media, max_steps = 200, seed = 123)
  })
  
  observeEvent(input$reset, {
    running(FALSE)
    tryCatch(state(do.call(initialize_model, requested_settings())),
             error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
  }, ignoreNULL = FALSE)
  
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
      showNotification("Run complete. Reset or step back to continue.")
    } else {
      running(TRUE)
    }
  })
  observeEvent(input$slow_down, {
    playback_delay(min(2000, playback_delay() * 2))
  })
  observeEvent(input$speed_up, {
    playback_delay(max(50, playback_delay() / 2))
  })
  output$speed_label <- renderText(paste0("Speed: ", round(250 / playback_delay(), 2), "×"))
  observe({
    updateActionButton(session, "run_pause", label = if (running()) "Pause" else "Run")
  })
  
  observe({
    if (!running()) return()
    invalidateLater(playback_delay(), session)
    # Without isolation, writing state would retrigger immediate extra ticks.
    # The timer is nonblocking, so hovering and buttons remain responsive.
    isolate({
      s <- state()
      if (is.null(s)) {
        running(FALSE)
      } else {
        s <- step_model(s)
        state(s)
        if (model_is_stable(s)) {
          running(FALSE)
          showNotification("Paused: opinions and perceptions changed by less than 0.001 for four steps.")
        } else if (s$step >= s$settings$max_steps) {
          running(FALSE)
        }
      }
    })
  })
  
  output$status <- renderText({
    s <- state()
    req(s)
    pending <- !isTRUE(all.equal(s$settings, requested_settings(), check.attributes = FALSE))
    paste0("Step ", s$step, " / ", s$settings$max_steps,
           if (pending) " — Settings changed: click Reset / apply." else " — Settings applied.")
  })
  
  node_data <- reactive({
    s <- state()
    req(s)
    n <- length(s$opinions)
    angle <- 2 * pi * (seq_len(n) - 1) / n
    # Share coordinates between plot and hover to prevent mismatched tooltips.
    moving <- identical(input$node_layout, "moving")
    data.frame(id = seq_len(n),
               x = if (moving) s$opinions else cos(angle),
               y = if (moving) 0.18 * sin(angle) else sin(angle))
  })
  
  output$node_tooltip <- renderUI({
    s <- state()
    hover <- input$network_hover
    req(s, hover)
    selected <- nearPoints(node_data(), hover, xvar = "x", yvar = "y",
                           threshold = 12, maxpoints = 1)
    if (!nrow(selected)) return(NULL)
    i <- selected$id[1]
    width <- session$clientData$output_network_plot_width
    if (is.null(width) || !is.finite(width)) width <- 560
    left <- max(8, min(hover$coords_css$x + 14, width - 235))
    top <- max(8, min(hover$coords_css$y + 14, 250))
    div(style = sprintf(paste0("position:absolute;left:%spx;top:%spx;",
                               "width:225px;padding:10px;background:rgba(255,255,255,.97);",
                               "border:1px solid #d6dce2;border-radius:6px;z-index:100;",
                               "pointer-events:none;font-size:12px;line-height:1.5;"), left, top),
        tags$strong(paste("Person", i, "—", if (s$party[i] == -1L) "Party A" else "Party B")),
        tags$br(), paste("Opinion:", sprintf("%.3f", s$opinions[i])),
        tags$br(), paste("Initial opinion:", sprintf("%.3f", s$history[1, i])),
        tags$br(), paste("Identity:", sprintf("%.2f", s$identity[i])),
        tags$br(), paste("Perceived distance:", sprintf("%.2f", s$perceived[i])),
        tags$br(), paste("Congenial exposure:", sprintf("%.2f", s$congenial[i])),
        tags$br(), paste("Connections:", sum(s$adjacency[i, ])))
  })
  
  output$metrics <- renderTable({
    s <- state()
    req(s)
    cross <- s$adjacency & outer(s$party, s$party, "!=")
    w <- influence_weights(s$adjacency, s$party, s$identity, s$perceived, s$constants)
    cross_mean <- if (any(cross)) sprintf("%.3f", mean(w[cross])) else "N/A"
    data.frame(Metric = c("Mean opinion", "Opinion SD", "Actual party distance",
                          "Mean perceived distance", "Mean identity", "Mean congenial exposure",
                          "Average connections", "Cross-party influence weight"),
               Value = c(sprintf("%.3f", mean(s$opinions)), sprintf("%.3f", sd(s$opinions)),
                         sprintf("%.3f", actual_party_distance(s$opinions, s$party)),
                         sprintf("%.3f", mean(s$perceived)), sprintf("%.3f", mean(s$identity)),
                         sprintf("%.3f", mean(s$congenial)),
                         sprintf("%.2f", mean(rowSums(s$adjacency))), cross_mean))
  }, striped = TRUE, rownames = FALSE)
  
  output$histogram <- renderPlot({
    s <- state()
    req(s)
    h <- hist(s$opinions, breaks = seq(-1, 1, length.out = 21), plot = FALSE)
    par(mar = c(4, 4, 3, 1))
    plot(h, col = opinion_colors(h$mids), border = "white",
         xlim = c(-1, 1), ylim = c(0, length(s$opinions)),
         main = "Current opinions", xlab = "Opinion", ylab = "People")
  })
  
  output$network_plot <- renderPlot({
    s <- state()
    req(s)
    nodes <- node_data()
    moving <- identical(input$node_layout, "moving")
    par(mar = c(if (moving) 3.5 else 0.5, 0.5, 2, 0.5))
    plot(nodes$x, nodes$y, type = "n", axes = FALSE, xlab = "", ylab = "",
         xlim = c(-1.08, 1.08), ylim = if (moving) c(-0.21, 0.21) else c(-1.08, 1.08),
         asp = if (moving) NA else 1, main = paste("Social network — step", s$step))
    # Reuse cached edges: the network does not change during a run.
    edges <- network_data(s)
    if (nrow(edges)) segments(nodes$x[edges$from], nodes$y[edges$from],
                              nodes$x[edges$to], nodes$y[edges$to],
                              col = adjustcolor("gray50", alpha.f = 0.18), lwd = 0.6)
    points(nodes$x, nodes$y, pch = 21, bg = opinion_colors(s$opinions),
           col = ifelse(s$party == -1L, "#2166AC", "#B2182B"), lwd = 1.3,
           cex = (if (length(s$opinions) <= 100) 0.9 else 0.65) + 0.6 * s$identity)
    if (moving) {
      axis(1, at = c(-1, 0, 1))
      mtext("Current opinion", side = 1, line = 2.3)
    }
  })
  
  output$trajectory_plot <- renderPlot({
    s <- state()
    req(s)
    steps <- seq_len(nrow(s$history)) - 1L
    colors <- ifelse(s$party == -1L, "#2166AC", "#B2182B")
    par(mar = c(4, 4, 3, 1))
    matplot(steps, s$history, type = if (s$step > 0) "l" else "p",
            lty = 1, pch = 16, col = adjustcolor(colors, alpha.f = 0.40),
            xlim = c(0, max(1, s$step)), ylim = c(-1, 1),
            xlab = "Step", ylab = "Opinion", main = "Individual opinion trajectories")
    abline(h = 0, col = "gray75", lty = 3)
  })
  
  output$summary_plot <- renderPlot({
    s <- state()
    req(s)
    actual <- apply(s$history, 1, actual_party_distance, party = s$party)
    perceived <- rowMeans(s$perception_history)
    par(mar = c(4, 4, 3, 1))
    matplot(seq_along(actual) - 1L, cbind(actual, perceived),
            type = if (s$step > 0) "l" else "p", lty = c(1, 2), pch = c(16, 17),
            col = c("#333333", "#7B3294"), lwd = 2,
            xlim = c(0, max(1, s$step)), ylim = c(0, 2), xlab = "Step", ylab = "Distance",
            main = "Actual versus perceived polarization")
    legend("topright", c("Actual party-mean distance", "Mean perceived distance"),
           col = c("#333333", "#7B3294"), lty = c(1, 2), lwd = 2, bty = "n")
  })
}

shinyApp(ui, server)