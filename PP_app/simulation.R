#### Simulation Script ####

# Check whether data is numeric
check_number <- function(x, name, lower, upper, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < lower || x > upper || (integer && x != floor(x))) {
    stop(sprintf("%s must be %sbetween %s and %s.", name,
                 if (integer) "an integer " else "", lower, upper),
         call. = FALSE)
  }
}

# Sample a normal distribution conditional on being within [-1, 1].
# Preserve each person's assigned group mean when redrawing.
rnorm_bounded <- function(n, mean, sd) {
  means <- rep_len(mean, n)
  sds <- rep_len(sd, n)
  out <- rnorm(n, means, sds)
  bad <- which(out < -1 | out > 1)
  while (length(bad)) {
    out[bad] <- rnorm(length(bad), means[bad], sds[bad])
    bad <- which(out < -1 | out > 1)
  }
  out
}

initial_opinions <- function(n, pattern = "two_groups") {
  switch(pattern,
         broad = runif(n, -1, 1),
         middle = rnorm_bounded(n, 0, 0.2),
         two_groups = {
           # Equal mixture probabilities; realized group sizes may differ.
           centers <- sample(c(-0.6, 0.6), n, replace = TRUE)
           rnorm_bounded(n, centers, 0.15)
         },
         stop("Unknown starting pattern.", call. = FALSE)
  )
}

# A simple undirected graph: no self-loops and no duplicate edges.
# For each possible edge:
#   weight = exp(-homophily * initial opinion distance)
#   probability = min(1, multiplier * weight)
# Calibrate multiplier to the requested EXPECTED average degree.
# Realized average degree varies because edges are sampled independently.
# homophily = 0 gives an Erdos-Renyi random graph.
generate_network <- function(opinions, average_degree = 10, homophily = 0) {
  n <- length(opinions)
  check_number(average_degree, "average_degree", 0, n - 1)
  check_number(homophily, "homophily", 0, 10)
  adjacency <- matrix(FALSE, n, n)
  pairs <- which(upper.tri(adjacency), arr.ind = TRUE)
  target_edges <- n * average_degree / 2
  
  if (target_edges == 0) return(adjacency)
  if (average_degree == n - 1) {
    adjacency[,] <- TRUE
    diag(adjacency) <- FALSE
    return(adjacency)
  }
  
  distance <- abs(opinions[pairs[, 1]] - opinions[pairs[, 2]])
  weights <- exp(-homophily * distance)
  multiplier <- uniroot(
    function(a) sum(pmin(1, a * weights)) - target_edges,
    interval = c(0, 1 / min(weights)), tol = 1e-10
  )$root
  present <- runif(nrow(pairs)) < pmin(1, multiplier * weights)
  selected <- pairs[present, , drop = FALSE]
  if (nrow(selected)) {
    adjacency[selected] <- TRUE
    adjacency[cbind(selected[, 2], selected[, 1])] <- TRUE
  }
  adjacency
}

initialize_model <- function(n = 100, pattern = "two_groups",
                             average_degree = 10, homophily = 0,
                             openness = 0.4, adjustment = 0.2,
                             max_steps = 200, seed = 123) {
  check_number(n, "n", 2, 1000, integer = TRUE)
  check_number(average_degree, "average_degree", 0, n - 1)
  check_number(homophily, "homophily", 0, 10)
  check_number(openness, "openness", 0, 2)
  check_number(adjustment, "adjustment", 0, 1)
  check_number(max_steps, "max_steps", 1, 10000, integer = TRUE)
  check_number(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
  if (!pattern %in% c("broad", "middle", "two_groups")) {
    stop("pattern must be broad, middle, or two_groups.", call. = FALSE)
  }
  
  # Preserve the caller's random-number state while making runs reproducible.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  
  opinions <- initial_opinions(n, pattern)
  adjacency <- generate_network(opinions, average_degree, homophily)
  history <- matrix(opinions, nrow = 1)
  colnames(history) <- paste0("person_", seq_len(n))
  
  list(
    opinions = opinions,
    adjacency = adjacency,
    history = history,
    step = 0L,
    settings = list(n = n, pattern = pattern,
                    average_degree = average_degree, homophily = homophily,
                    openness = openness, adjustment = adjustment,
                    max_steps = max_steps, seed = seed)
  )
}

# Every eligible-neighbor mean is calculated from the OLD opinions.
# A person's own opinion is not included in that mean.
update_opinions <- function(opinions, adjacency, openness = 0.4,
                            adjustment = 0.2) {
  check_number(openness, "openness", 0, 2)
  check_number(adjustment, "adjustment", 0, 1)
  n <- length(opinions)
  if (!is.numeric(opinions) || n < 2 || any(!is.finite(opinions)) ||
      any(abs(opinions) > 1)) {
    stop("opinions must be finite numbers in [-1, 1].", call. = FALSE)
  }
  if (!is.matrix(adjacency) || !all(dim(adjacency) == c(n, n)) ||
      anyNA(adjacency) || !all(adjacency %in% c(0, 1)) ||
      !isSymmetric(adjacency) || any(diag(adjacency) != 0)) {
    stop("adjacency must be a symmetric 0/1 matrix with zero diagonal.",
         call. = FALSE)
  }
  distances <- abs(outer(opinions, opinions, "-"))
  eligible <- (adjacency != 0) & (distances <= openness)
  counts <- rowSums(eligible)
  totals <- as.vector(eligible %*% opinions)
  result <- opinions
  active <- counts > 0
  result[active] <- opinions[active] + adjustment *
    (totals[active] / counts[active] - opinions[active])
  # Protect against tiny floating-point excursions outside the bounds.
  pmax(-1, pmin(1, result))
}

step_model <- function(state) {
  if (state$step >= state$settings$max_steps) return(state)
  state$opinions <- update_opinions(
    state$opinions, state$adjacency,
    state$settings$openness, state$settings$adjustment
  )
  state$step <- state$step + 1L
  state$history <- rbind(state$history, state$opinions)
  state
}

simulate_opinions <- function(n = 100, pattern = "two_groups",
                              average_degree = 10, homophily = 0,
                              openness = 0.4, adjustment = 0.2,
                              max_steps = 200, seed = 123) {
  state <- initialize_model(n, pattern, average_degree, homophily,
                            openness, adjustment, max_steps, seed)
  for (i in seq_len(max_steps)) state <- step_model(state)
  state
}

# Long-format opinion history, including the initial state at step 0.
history_data <- function(state) {
  h <- state$history
  data.frame(
    step = rep(seq_len(nrow(h)) - 1L, each = ncol(h)),
    person = rep(seq_len(ncol(h)), times = nrow(h)),
    opinion = as.vector(t(h))
  )
}

network_data <- function(state) {
  pairs <- which(upper.tri(state$adjacency) & state$adjacency,
                 arr.ind = TRUE)
  data.frame(from = pairs[, 1], to = pairs[, 2])
}

# Tiny deterministic checks: simultaneous update and isolated person.
run_model_tests <- function() {
  a <- matrix(FALSE, 3, 3)
  a[1, 2] <- a[2, 1] <- TRUE
  x <- c(0, 0.2, 0.9)
  stopifnot(isTRUE(all.equal(
    update_opinions(x, a, openness = 0.4, adjustment = 0.2),
    c(0.04, 0.16, 0.9)
  )))
  stopifnot(isTRUE(all.equal(
    update_opinions(x, a, openness = 0.1, adjustment = 0.2), x
  )))
  # Inclusive threshold and full adjustment: both people swap opinions.
  stopifnot(isTRUE(all.equal(
    update_opinions(c(0, 0.4, 0.9), a, 0.4, 1),
    c(0.4, 0, 0.9)
  )))
  s1 <- simulate_opinions(n = 10, average_degree = 3, max_steps = 4)
  s2 <- simulate_opinions(n = 10, average_degree = 3, max_steps = 4)
  stopifnot(identical(s1, s2), nrow(s1$history) == 5,
            all(abs(s1$history) <= 1), isSymmetric(s1$adjacency))
  message("All model tests passed.")
  invisible(TRUE)
}

# Standalone usage (uncomment to run):
# run_model_tests()
# result <- simulate_opinions()
# write.csv(history_data(result), "opinion_history.csv", row.names = FALSE)
# write.csv(network_data(result), "network_edges.csv", row.names = FALSE)
# print(tail(history_data(result)))





















