# PoliticalPsychology: factorial and sensitivity analyses
# Place next to simulation.R. Uses base R only.
source("simulation.R")

polarization_metrics <- function(state) {
  initial <- actual_party_distance(state$history[1, ], state$party)
  final <- actual_party_distance(state$opinions, state$party)
  data.frame(initial_polarization = initial,
             final_polarization = final,
             change_polarization = final - initial,
             final_perceived = mean(state$perceived),
             perception_gap = mean(state$perceived) - final)
}

run_conditions <- function(design, seeds = 1:100, n = 100,
                           pattern = "two_groups", average_degree = 10,
                           max_steps = 200) {
  required <- c("identity_strength", "perceived_polarization", "congenial_media")
  stopifnot(is.data.frame(design), all(required %in% names(design)),
            length(seeds) > 0, !anyNA(seeds), !anyDuplicated(seeds))
  out <- vector("list", nrow(design) * length(seeds))
  k <- 0L
  for (seed in seeds) {
    for (j in seq_len(nrow(design))) {
      d <- design[j, ]
      s <- simulate_opinions(n = n, pattern = pattern,
                             average_degree = average_degree,
                             identity_strength = d$identity_strength,
                             perceived_polarization = d$perceived_polarization,
                             congenial_media = d$congenial_media,
                             max_steps = max_steps, seed = seed)
      k <- k + 1L
      out[[k]] <- data.frame(seed = seed, d[required], polarization_metrics(s))
    }
  }
  do.call(rbind, out)
}

factorial_design <- function(low = 0.2, high = 0.8) {
  expand.grid(identity_strength = c(low, high),
              perceived_polarization = c(low, high),
              congenial_media = c(low, high))
}

factorial_analysis <- function(results) {
  # Seed is a block: each seed is reused across all 8 settings.
  d <- results
  for (nm in c("identity_strength", "perceived_polarization", "congenial_media")) {
    d[[nm]] <- factor(d[[nm]])
  }
  d$seed <- factor(d$seed)
  # Repeated-measures ANOVA: one observation per seed x condition.
  fit <- aov(change_polarization ~ identity_strength *
               perceived_polarization * congenial_media +
               Error(seed / (identity_strength * perceived_polarization * congenial_media)),
             data = d)
  summary(fit)
}

sensitivity_design <- function(variable, values = seq(0, 1, by = 0.1),
                               baseline = c(identity_strength = 0.5,
                                            perceived_polarization = 0.5,
                                            congenial_media = 0.5)) {
  stopifnot(variable %in% names(baseline), all(is.finite(values)),
            all(values >= 0 & values <= 1))
  design <- as.data.frame(as.list(baseline))
  design <- design[rep(1, length(values)), , drop = FALSE]
  design[[variable]] <- values
  rownames(design) <- NULL
  design
}

summarize_sensitivity <- function(results, variable) {
  stopifnot(variable %in% names(results))
  spl <- split(results$change_polarization, results[[variable]])
  data.frame(value = as.numeric(names(spl)),
             mean_change = vapply(spl, mean, numeric(1)),
             sd_change = vapply(spl, sd, numeric(1)),
             n_runs = lengths(spl), row.names = NULL)
}

plot_sensitivity <- function(summary_data, variable) {
  with(summary_data, plot(value, mean_change, type = "b", pch = 19,
                          xlab = variable, ylab = "Mean change in party distance",
                          main = paste("Sensitivity to", variable)))
  abline(h = 0, lty = 2)
}

# Example usage (run manually after sourcing analysis.R):
# design <- factorial_design()
# factorial_results <- run_conditions(design, seeds = 1:100)
# print(factorial_analysis(factorial_results))
# write.csv(factorial_results, "factorial_results.csv", row.names = FALSE)
# variables <- c("identity_strength", "perceived_polarization", "congenial_media")
# for (v in variables) {
#   sensitivity_results <- run_conditions(sensitivity_design(v), seeds = 1:100)
#   summary_data <- summarize_sensitivity(sensitivity_results, v)
#   plot_sensitivity(summary_data, v)
# }
