# Political Psychology: simulation analysis
# Adjust the source path if needed.
library(ggplot2)
library(patchwork)  # install.packages("patchwork") if needed
source("simulation.R", local = TRUE)

# --- 1. Experimental design and final polarization ---

polarization_metrics <- function(state) {
  initial <- actual_party_distance(state$history[1, ], state$party)
  final <- actual_party_distance(state$opinions, state$party)
  
  data.frame(
    initial_polarization = initial,
    final_polarization = final,
    change_polarization = final - initial,
    final_perceived = mean(state$perceived),
    perceived_actual_gap = mean(state$perceived) - final
  )
}

factorial_design <- function(low = 0.2, high = 0.8) {
  expand.grid(
    identity_strength = c(low, high),
    perceived_polarization = c(low, high),
    congenial_media = c(low, high)
  )
}

run_conditions <- function(experimental_design, seeds = 1:30, n = 100,
                           pattern = "two_groups", average_degree = 10,
                           max_steps = 200) {
  parameter_names <- c("identity_strength", "perceived_polarization", "congenial_media")
  stopifnot(
    is.data.frame(experimental_design), all(parameter_names %in% names(experimental_design)),
    length(seeds) > 0, !anyNA(seeds), !anyDuplicated(seeds)
  )
  
  simulation_results <- vector("list", nrow(experimental_design) * length(seeds))
  result_index <- 0L
  
  for (seed in seeds) {
    for (condition_index in seq_len(nrow(experimental_design))) {
      condition <- experimental_design[condition_index, ]
      state <- simulate_opinions(
        n = n, pattern = pattern, average_degree = average_degree,
        identity_strength = condition$identity_strength,
        perceived_polarization = condition$perceived_polarization,
        congenial_media = condition$congenial_media,
        max_steps = max_steps, seed = seed
      )
      result_index <- result_index + 1L
      simulation_results[[result_index]] <- data.frame(seed = seed, condition[parameter_names],
                                                       polarization_metrics(state))
    }
  }
  do.call(rbind, simulation_results)
}

# Seed is reused across conditions, so use a repeated-measures ANOVA.
run_factorial_anova <- function(simulation_results, outcome = "change_polarization") {
  run_data <- simulation_results
  parameter_names <- c("identity_strength", "perceived_polarization", "congenial_media")
  run_data[parameter_names] <- lapply(run_data[parameter_names], factor)
  run_data$seed <- factor(run_data$seed)
  
  anova_formula <- as.formula(paste0(
    outcome, " ~ identity_strength * perceived_polarization * congenial_media + ",
    "Error(seed / (identity_strength * perceived_polarization * congenial_media))"
  ))
  summary(aov(anova_formula, data = run_data))
}

experimental_design <- factorial_design()
factorial_results <- run_conditions(experimental_design)
print(run_factorial_anova(factorial_results))

# Mean polarization change by condition.
print(aggregate(
  change_polarization ~ identity_strength + perceived_polarization + congenial_media,
  data = factorial_results, FUN = mean
))

# Plot A: identity strength x congenial media.
plot_interaction <- ggplot(
  aggregate(change_polarization ~ identity_strength + congenial_media,
            data = factorial_results, FUN = mean),
  aes(x = factor(identity_strength), y = change_polarization,
      color = factor(congenial_media), group = congenial_media)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(title = "A. Final change", x = "Identity strength",
       y = "Mean change", color = "Media exposure") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 12))

# --- 2. Polarization trajectories ---

get_trajectory <- function(state) {
  party_distances <- apply(state$history, 1, function(opinions) {
    actual_party_distance(opinions, state$party)
  })
  data.frame(step = seq_along(party_distances) - 1L, party_distance = party_distances)
}

# Store each run's full history (steps 0–200).
trajectory_results <- do.call(rbind, lapply(seq_len(nrow(experimental_design)), function(condition_index) {
  condition <- experimental_design[condition_index, ]
  do.call(rbind, lapply(1:30, function(seed) {
    state <- simulate_opinions(
      n = 100, pattern = "two_groups", average_degree = 10,
      identity_strength = condition$identity_strength,
      perceived_polarization = condition$perceived_polarization,
      congenial_media = condition$congenial_media,
      max_steps = 200, seed = seed
    )
    data.frame(get_trajectory(state), seed = seed,
               identity_strength = condition$identity_strength,
               perceived_polarization = condition$perceived_polarization,
               congenial_media = condition$congenial_media)
  }))
}))

trajectory_results$identity_label <- factor(
  trajectory_results$identity_strength, c(0.2, 0.8),
  c("Low identity", "High identity")
)
trajectory_results$media_label <- factor(
  trajectory_results$congenial_media, c(0.2, 0.8),
  c("Low media", "High media")
)
trajectory_results$perceived_label <- factor(
  trajectory_results$perceived_polarization, c(0.2, 0.8),
  c("Low perceived polarization", "High perceived polarization")
)

# Plot B: polarization trajectories.
plot_trajectories <- ggplot(
  trajectory_results,
  aes(step, party_distance, color = identity_label, linetype = media_label)
) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.9) +
  labs(title = "B. Over time", x = "Simulation step",
       y = "Party distance", color = "Identity", linetype = "Media") +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 12))

# Plot C: perceived polarization effect over time (high minus low).
perceived_means <- aggregate(
  party_distance ~ step + perceived_polarization,
  data = trajectory_results, FUN = mean
)
perceived_wide <- reshape(
  perceived_means, idvar = "step", timevar = "perceived_polarization",
  direction = "wide"
)
perceived_wide$effect <- perceived_wide[["party_distance.0.8"]] -
  perceived_wide[["party_distance.0.2"]]

plot_perceived <- ggplot(perceived_wide, aes(step, effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1, color = "#0D9488") +
  labs(title = "C. Perceived polarization", x = "Simulation step",
       y = "Difference (high - low)") +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- 3. Stabilization time ---

# First step after which distance stays within tolerance of its final value.
calculate_stabilization_step <- function(run_data, tolerance = 0.01) {
  run_data <- run_data[order(run_data$step), ]
  distance_from_final <- abs(run_data$party_distance - tail(run_data$party_distance, 1))
  outside_tolerance <- which(distance_from_final > tolerance)
  if (length(outside_tolerance) == 0) return(0)
  run_data$step[max(outside_tolerance)] + 1
}

# Group repeated measurements by simulation run.
simulation_runs <- split(trajectory_results, interaction(
  trajectory_results$seed,
  trajectory_results$identity_strength,
  trajectory_results$perceived_polarization,
  trajectory_results$congenial_media,
  drop = TRUE
))

stabilization_results <- do.call(rbind, lapply(simulation_runs, function(run_data) {
  data.frame(
    seed = run_data$seed[1],
    identity_strength = run_data$identity_strength[1],
    perceived_polarization = run_data$perceived_polarization[1],
    congenial_media = run_data$congenial_media[1],
    stabilization_step = calculate_stabilization_step(run_data)
  )
}))

print(summary(stabilization_results$stabilization_step))
print(aggregate(
  stabilization_step ~ identity_strength + perceived_polarization + congenial_media,
  data = stabilization_results,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
))
print(run_factorial_anova(stabilization_results, outcome = "stabilization_step"))

# Plot D: average stabilization time by condition.
stabilization_means <- aggregate(
  stabilization_step ~ identity_strength + perceived_polarization + congenial_media,
  data = stabilization_results, FUN = mean
)

plot_stabilization <- ggplot(
  stabilization_means,
  aes(x = factor(identity_strength), y = stabilization_step,
      fill = factor(perceived_polarization))
) +
  geom_col(position = "dodge") +
  facet_wrap(~ congenial_media, labeller = as_labeller(c(`0.2` = "Low media", `0.8` = "High media"))) +
  labs(title = "D. Stabilization time", x = "Identity strength",
       y = "Mean steps", fill = "Perceived polarization") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 12),
        strip.text = element_text(size = 10))

# Combine and save the four plots.
combined_plot <- ((plot_interaction | plot_trajectories) /
                    (plot_perceived | plot_stabilization)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.margin = margin(12, 12, 12, 12))
print(combined_plot)

# The exported PNG is larger than the RStudio Plots pane.
ggsave("simulation_results.png", combined_plot,
       width = 16, height = 11, dpi = 300, bg = "white")

# Check whether party distance is nearly unchanged during the final 10 steps.
final_distance_changes <- sapply(simulation_runs, function(run_data) {
  abs(run_data$party_distance[run_data$step == 200] -
        run_data$party_distance[run_data$step == 190])
})
print(summary(final_distance_changes))
print(sum(final_distance_changes > 0.01))

# --- 4. Stabilization tolerance sensitivity ---

tolerances <- c(0.005, 0.01, 0.02)
tolerance_results <- do.call(rbind, lapply(tolerances, function(tolerance) {
  do.call(rbind, lapply(simulation_runs, function(run_data) {
    data.frame(
      tolerance = tolerance,
      seed = run_data$seed[1],
      identity_strength = run_data$identity_strength[1],
      perceived_polarization = run_data$perceived_polarization[1],
      congenial_media = run_data$congenial_media[1],
      stabilization_step = calculate_stabilization_step(run_data, tolerance = tolerance)
    )
  }))
}))

print(aggregate(stabilization_step ~ tolerance,
                data = tolerance_results, FUN = mean))
print(aggregate(stabilization_step ~ tolerance + perceived_polarization,
                data = tolerance_results, FUN = mean))

# --- 5. Network density robustness ---

# Repeat the factorial design with twice as many average connections.
degree20_results <- run_conditions(experimental_design, average_degree = 20)
print(run_factorial_anova(degree20_results))
print(aggregate(
  change_polarization ~ identity_strength + perceived_polarization + congenial_media,
  data = degree20_results, FUN = mean
))

