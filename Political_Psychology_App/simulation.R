# simulation.R
# Base R only. The app sources this file; it can also run independently.

# Fixed rates keep the UI focused on the three substantive mechanisms.
# These values illustrate mechanisms; they are not empirical estimates.
MODEL <- list(adjustment = 0.20, media_share = 0.25,
              perception_rate = 0.10, identity_effect = 1.5,
              perception_effect = 1.0, media_position = 0.60,
              media_distance = 1.20, trait_concentration = 20)

check_number <- function(x, name, lower, upper, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < lower || x > upper || (integer && x != floor(x))) {
    stop(sprintf("%s must be %sbetween %s and %s.", name,
                 if (integer) "an integer " else "", lower, upper),
         call. = FALSE)
  }
}

with_seed <- function(seed, fun) {
  # Reproducibility must not disturb random draws elsewhere in R/Shiny.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = ".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  fun()
}

rnorm_bounded <- function(n, mean, sd) {
  # Redraw rather than clip, avoiding artificial piles at the boundaries.
  means <- rep_len(mean, n)
  out <- rnorm(n, means, sd)
  bad <- which(out < -1 | out > 1)
  while (length(bad)) {
    out[bad] <- rnorm(length(bad), means[bad], sd)
    bad <- which(out < -1 | out > 1)
  }
  out
}

initial_opinions <- function(n, pattern, party) {
  switch(pattern,
         broad = runif(n, -1, 1),
         middle = rnorm_bounded(n, 0, 0.20),
         two_groups = rnorm_bounded(n, 0.60 * party, 0.15),
         stop("Unknown starting pattern.", call. = FALSE))
}

generate_network <- function(n, average_degree) {
  check_number(average_degree, "average_degree", 0, n - 1)
  a <- matrix(FALSE, n, n)
  upper <- upper.tri(a)
  # No homophily: hold tie formation constant while testing receptiveness.
  a[upper] <- runif(sum(upper)) < average_degree / (n - 1)
  a | t(a)
}

trait_from_uniform <- function(u, mean, concentration) {
  if (mean <= 0) return(rep(0, length(u)))
  if (mean >= 1) return(rep(1, length(u)))
  # Shared quantiles preserve relative trait ranks across slider comparisons.
  qbeta(u, mean * concentration, (1 - mean) * concentration)
}

initialize_model <- function(n = 100, pattern = "two_groups",
                             average_degree = 10, identity_strength = 0.5,
                             perceived_polarization = 1,
                             congenial_media = 0.75,
                             max_steps = 200, seed = 123) {
  check_number(n, "n", 2, 1000, TRUE)
  check_number(average_degree, "average_degree", 0, n - 1)
  check_number(identity_strength, "identity_strength", 0, 1)
  check_number(perceived_polarization, "perceived_polarization", 0, 2)
  check_number(congenial_media, "congenial_media", 0, 1)
  check_number(max_steps, "max_steps", 1, 10000, TRUE)
  check_number(seed, "seed", 0, .Machine$integer.max, TRUE)
  if (!is.character(pattern) || length(pattern) != 1L ||
      is.na(pattern) || !pattern %in% c("broad", "middle", "two_groups")) {
    stop("pattern must be broad, middle, or two_groups.", call. = FALSE)
  }
  
  with_seed(seed, function() {
    # Party membership stays fixed; crossing opinion zero does not change it.
    # Balanced parties guarantee that party-distance metrics are defined.
    party <- sample(rep(c(-1L, 1L), length.out = n))
    opinions <- initial_opinions(n, pattern, party)
    adjacency <- generate_network(n, average_degree)
    u <- matrix(runif(n * 3), nrow = n)
    identity <- trait_from_uniform(u[, 1], identity_strength, MODEL$trait_concentration)
    perceived <- 2 * trait_from_uniform(u[, 2], perceived_polarization / 2,
                                        MODEL$trait_concentration)
    congenial <- trait_from_uniform(u[, 3], congenial_media, MODEL$trait_concentration)
    pairs <- which(adjacency & upper.tri(adjacency), arr.ind = TRUE)
    h <- matrix(opinions, nrow = 1)
    ph <- matrix(perceived, nrow = 1)
    colnames(h) <- colnames(ph) <- paste0("person_", seq_len(n))
    
    list(opinions = opinions, party = party, identity = identity,
         perceived = perceived, congenial = congenial,
         adjacency = adjacency,
         edges = data.frame(from = pairs[, 1], to = pairs[, 2]),
         history = h, perception_history = ph, step = 0L,
         constants = MODEL,
         settings = list(n = n, pattern = pattern,
                         average_degree = average_degree, identity_strength = identity_strength,
                         perceived_polarization = perceived_polarization,
                         congenial_media = congenial_media, max_steps = max_steps, seed = seed))
  })
}

outgroup_weights <- function(identity, perceived, constants = MODEL) {
  exp(-constants$identity_effect * identity -
        constants$perception_effect * perceived / 2)
}

influence_weights <- function(adjacency, party, identity, perceived,
                              constants = MODEL) {
  # Row i is the listener: the same connection can carry unequal influence.
  n <- length(party)
  w <- matrix(outgroup_weights(identity, perceived, constants), n, n)
  w[outer(party, party, "==")] <- 1
  w * adjacency
}

update_opinions <- function(opinions, adjacency, party, identity, perceived,
                            congenial, constants = MODEL) {
  w <- influence_weights(adjacency, party, identity, perceived, constants)
  degree <- rowSums(adjacency)
  # Dividing by degree preserves weak receptiveness even with only outgroup
  # neighbors. Normalizing by sum(w) would cancel that discount.
  social <- (as.vector(w %*% opinions) - rowSums(w) * opinions) /
    pmax(degree, 1)
  q <- outgroup_weights(identity, perceived, constants)
  own_message <- constants$media_position * party
  other_message <- -own_message
  
  # Use expected repeated exposure, not one random message per tick.
  # This removes sampling jitter and lets playback approach a stable state.
  # Congeniality changes source mix, not message extremity or exposure volume.
  media <- congenial * (own_message - opinions) +
    (1 - congenial) * q * (other_message - opinions)
  
  result <- opinions + constants$adjustment * (
    (1 - constants$media_share) * social + constants$media_share * media)
  # The update is a convex combination; clipping only guards rounding error.
  pmax(-1, pmin(1, result))
}

update_perceptions <- function(opinions, adjacency, party, perceived,
                               constants = MODEL) {
  local <- adjacency
  diag(local) <- TRUE
  # Include self in local party estimates so cross-party contacts supply
  # evidence even when there are no additional same-party neighbors.
  a <- as.numeric(party == -1L)
  b <- as.numeric(party == 1L)
  count_a <- as.vector(local %*% a)
  count_b <- as.vector(local %*% b)
  mean_a <- as.vector(local %*% (opinions * a)) / pmax(count_a, 1)
  mean_b <- as.vector(local %*% (opinions * b)) / pmax(count_b, 1)
  observed <- perceived
  known <- count_a > 0 & count_b > 0
  # Absence of outgroup evidence is not evidence of zero polarization.
  observed[known] <- abs(mean_a[known] - mean_b[known])
  evidence <- (1 - constants$media_share) * observed +
    constants$media_share * constants$media_distance
  result <- perceived + constants$perception_rate * (evidence - perceived)
  pmax(0, pmin(2, result))
}

step_model <- function(state) {
  if (state$step >= state$settings$max_steps) return(state)
  # Compute BOTH updates before replacing anything: order must not matter.
  next_x <- update_opinions(state$opinions, state$adjacency, state$party,
                            state$identity, state$perceived, state$congenial, state$constants)
  next_p <- update_perceptions(state$opinions, state$adjacency, state$party,
                               state$perceived, state$constants)
  state$opinions <- next_x
  state$perceived <- next_p
  state$step <- state$step + 1L
  # Bind matrices to avoid implicit row labels that spoil exact restoration.
  state$history <- rbind(state$history, matrix(next_x, nrow = 1))
  state$perception_history <- rbind(state$perception_history, matrix(next_p, nrow = 1))
  state
}

back_model <- function(state) {
  if (state$step == 0L) return(state)
  state$step <- state$step - 1L
  keep <- seq_len(state$step + 1L)
  state$history <- state$history[keep, , drop = FALSE]
  state$perception_history <- state$perception_history[keep, , drop = FALSE]
  # Restore all changing attributes; fixed traits and connections stay put.
  state$opinions <- as.numeric(state$history[length(keep), ])
  state$perceived <- as.numeric(state$perception_history[length(keep), ])
  state
}

model_is_stable <- function(state, tolerance = 0.001, transitions = 4L) {
  if (nrow(state$history) <= transitions) return(FALSE)
  small <- function(h) {
    recent <- tail(h, transitions + 1L)
    all(abs(recent[-1, , drop = FALSE] -
              recent[-nrow(recent), , drop = FALSE]) < tolerance)
  }
  # Stable opinions alone do not mean perceptions have finished changing.
  small(state$history) && small(state$perception_history)
}

actual_party_distance <- function(opinions, party) {
  abs(mean(opinions[party == -1L]) - mean(opinions[party == 1L]))
}

simulate_opinions <- function(n = 100, pattern = "two_groups",
                              average_degree = 10, identity_strength = 0.5,
                              perceived_polarization = 1, congenial_media = 0.75,
                              max_steps = 200, seed = 123) {
  state <- initialize_model(n, pattern, average_degree, identity_strength,
                            perceived_polarization, congenial_media, max_steps, seed)
  for (i in seq_len(max_steps)) state <- step_model(state)
  state
}

history_data <- function(state) {
  h <- state$history
  data.frame(step = rep(seq_len(nrow(h)) - 1L, each = ncol(h)),
             person = rep(seq_len(ncol(h)), times = nrow(h)),
             party = rep(state$party, times = nrow(h)),
             opinion = as.vector(t(h)),
             perceived_polarization = as.vector(t(state$perception_history)))
}

network_data <- function(state) state$edges

run_model_tests <- function() {
  a <- matrix(c(FALSE, TRUE, TRUE, FALSE), 2, 2)
  pure_social <- MODEL
  pure_social$media_share <- 0
  weak <- update_opinions(c(0, 0.8), a, c(-1, 1), c(0, 0),
                          c(0, 0), c(0.5, 0.5), pure_social)
  stopifnot(isTRUE(all.equal(weak, c(0.16, 0.64))))
  strong <- update_opinions(c(0, 0.8), a, c(-1, 1), c(1, 1),
                            c(2, 2), c(0.5, 0.5), pure_social)
  stopifnot(strong[1] > 0, strong[1] < weak[1])
  isolated <- matrix(FALSE, 2, 2)
  stopifnot(identical(update_opinions(c(0, 0.8), isolated, c(-1, 1),
                                      c(0, 0), c(0, 0), c(1, 1), pure_social), c(0, 0.8)))
  media_only <- update_opinions(c(0, 0), isolated, c(-1, 1),
                                c(0, 0), c(0, 0), c(1, 1))
  stopifnot(isTRUE(all.equal(media_only, c(-0.03, 0.03))))
  s <- initialize_model(n = 10, max_steps = 8)
  stopifnot(identical(back_model(step_model(s)), s),
            identical(step_model(s), step_model(back_model(step_model(s)))))
  changed <- initialize_model(n = 10, identity_strength = 1,
                              perceived_polarization = 2, congenial_media = 0)
  stopifnot(identical(s$opinions, changed$opinions),
            identical(s$adjacency, changed$adjacency))
  stopifnot(identical(simulate_opinions(n = 10, max_steps = 5),
                      simulate_opinions(n = 10, max_steps = 5)))
  # Corner cases: no ties, complete graph, and slider endpoints.
  cases <- expand.grid(pattern = c("broad", "middle", "two_groups"),
                       degree = c(0, 3, 9), identity = c(0, 1), perceived = c(0, 2), media = c(0, 1),
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    z <- cases[i, ]
    r <- simulate_opinions(10, z$pattern, z$degree, z$identity,
                           z$perceived, z$media, max_steps = 10)
    stopifnot(nrow(r$history) == 11, nrow(r$perception_history) == 11,
              all(is.finite(r$history)), all(abs(r$history) <= 1),
              all(is.finite(r$perception_history)),
              all(r$perception_history >= 0 & r$perception_history <= 2),
              isSymmetric(r$adjacency), all(diag(r$adjacency) == FALSE))
  }
  message("All model tests passed.")
  invisible(TRUE)
}

# Optional standalone checks:
# run_model_tests()
# result <- simulate_opinions()
# write.csv(history_data(result), "opinion_history.csv", row.names = FALSE)


















