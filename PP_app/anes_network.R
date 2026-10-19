#### ANES 2024 person-level network ####
# Each node is a real ANES respondent. Opinions, openness and tie propensity
# come from the survey; the ties themselves are CONSTRUCTED (ANES has no real
# ties), using the same homophily logic as simulation.R.
# Requires simulation.R to be sourced first (check_number, generate_network...).

# ------------------------------------------------------------------ data ----
# The app reads the raw ANES 2024 Time Series csv that sits in the data/ folder
# of this repository (any file named data/anes_timeseries*.csv). Only the few
# columns we need are read, so loading takes a couple of seconds.

anes_columns <- c(
  "V240001",   # case id
  "V241227x",  # party ID, 7 points (1 strong Dem ... 7 strong Rep)
  "V241177",   # liberal-conservative self-placement (1-7)
  "V241228",   # importance of party identity (1 extremely ... 5 not at all)
  "V241183",   # Democratic Party placement (1-7)
  "V241184",   # Republican Party placement (1-7)
  "V241600a",  # watches TV news? (0 = no; 0 means the show list was skipped)
  # Fox shows: Hannity, Watters, The Five, Ingraham, Gutfeld, Special Report,
  # Outnumbered
  "V241601b", "V241601c", "V241601f", "V241601g", "V241601h", "V241601k",
  "V241602k",
  # MSNBC shows: Maddow, Last Word, Deadline, All In, 11th Hour
  "V241601d", "V241601e", "V241602m", "V241602n", "V241602p"
)
fox_columns <- c("V241601b", "V241601c", "V241601f", "V241601g", "V241601h",
                 "V241601k", "V241602k")
msnbc_columns <- c("V241601d", "V241601e", "V241602m", "V241602n", "V241602p")

anes_valid <- function(x, lo, hi) ifelse(!is.na(x) & x >= lo & x <= hi, x, NA_real_)

# Count how many of the listed 0/1 "mentioned this show" items were ticked.
# Respondents who said they do not watch TV news (V241600a == 0) were never
# shown the list (coded -1), so their count is 0 instead of missing.
count_mentions <- function(d, cols) {
  m <- as.matrix(d[, cols])
  m[!(m %in% c(0, 1))] <- NA
  no_tv <- d$V241600a %in% 0
  for (j in seq_along(cols)) m[no_tv & is.na(m[, j]), j] <- 0
  rowSums(m)  # NA if any item is missing
}

find_anes_file <- function(dir = "data") {
  files <- list.files(dir, pattern = "^anes_timeseries.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No ANES file found. Put the ANES 2024 Time Series csv in the data/ ",
         "folder (see README).", call. = FALSE)
  }
  files[1]
}

# Read the raw ANES csv and turn it into one row per respondent.
load_anes_people <- function(path = find_anes_file(), media_cap = 3) {
  header <- gsub('"', "", strsplit(readLines(path, n = 1, warn = FALSE), ",")[[1]])
  missing_cols <- setdiff(anes_columns, header)
  if (length(missing_cols)) {
    stop("This ANES file lacks expected columns: ",
         paste(missing_cols, collapse = ", "),
         ". Use the 2024 Time Series Full Release csv.", call. = FALSE)
  }
  d <- read.csv(path, check.names = FALSE,
                colClasses = ifelse(header %in% anes_columns, NA, "NULL"))

  pid  <- anes_valid(d$V241227x, 1, 7)
  side <- ifelse(pid <= 3, "D", ifelse(pid >= 5, "R", NA))
  congenial <- ifelse(side %in% "D", count_mentions(d, msnbc_columns),
               ifelse(side %in% "R", count_mentions(d, fox_columns), NA))
  ideology   <- anes_valid(d$V241177, 1, 7)
  importance <- anes_valid(d$V241228, 1, 5)

  people <- data.frame(
    id = d$V240001,
    pid = pid,
    side = side,                                    # "D", "R" (leaners included)
    opinion = (ideology - 4) / 3,                   # [-1, 1], like the simulation
    identity = (5 - importance) / 4,                # 0 = not important, 1 = extremely
    media = pmin(congenial, media_cap) / media_cap, # 0..1
    # Republican Party minus Democratic Party placement (0..6)
    perceived_polarization = anes_valid(d$V241184, 1, 7) - anes_valid(d$V241183, 1, 7)
  )
  # Keep respondents with an opinion, an identity score and a media score
  # (pure independents have no congenial side, so they drop out here).
  people <- people[complete.cases(people[, c("opinion", "identity", "media")]), ]
  rownames(people) <- NULL
  people
}

# --------------------------------------------------------------- network ----
# Same as generate_network() in simulation.R, except that the homophily of a
# pair is stronger when the two people use more congenial media:
#   h_ij = homophily * (1 + media_effect * mean(media_i, media_j))
# media_effect = 0 gives back the original model.
generate_network_anes <- function(opinions, media, average_degree = 4,
                                  homophily = 1, media_effect = 1) {
  n <- length(opinions)
  check_number(average_degree, "average_degree", 0, n - 1)
  check_number(homophily, "homophily", 0, 10)
  check_number(media_effect, "media_effect", 0, 10)
  adjacency <- matrix(FALSE, n, n)
  pairs <- which(upper.tri(adjacency), arr.ind = TRUE)
  target_edges <- n * average_degree / 2
  if (target_edges == 0) return(adjacency)

  distance <- abs(opinions[pairs[, 1]] - opinions[pairs[, 2]])
  h <- homophily * (1 + media_effect * (media[pairs[, 1]] + media[pairs[, 2]]) / 2)
  weights <- exp(-h * distance)
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

# ---------------------------------------------------------------- update ----
# Like update_opinions() in simulation.R, but openness is a vector: person i
# only listens to neighbours within openness[i] of their own opinion.
update_opinions_het <- function(opinions, adjacency, openness, adjustment = 0.2) {
  check_number(adjustment, "adjustment", 0, 1)
  n <- length(opinions)
  stopifnot(length(openness) == n, all(abs(opinions) <= 1))
  distances <- abs(outer(opinions, opinions, "-"))
  eligible <- (adjacency != 0) & (distances <= openness)  # openness[i] recycles by row
  counts <- rowSums(eligible)
  totals <- as.vector(eligible %*% opinions)
  result <- opinions
  active <- counts > 0
  result[active] <- opinions[active] +
    adjustment * (totals[active] / counts[active] - opinions[active])
  pmax(-1, pmin(1, result))
}

# ----------------------------------------------------------------- model ----
# Returns a state with the same fields as initialize_model() (opinions,
# adjacency, history, step, settings) plus `people`, so the app can treat both
# modes alike. settings$openness is a per-person vector here.
# Identity importance sets openness: the more important the party identity,
# the narrower the range of opinions a person listens to.
initialize_anes_model <- function(people, n = 300, average_degree = 4,
                                  homophily = 1, media_effect = 1,
                                  open_min = 0.2, open_max = 0.6,
                                  adjustment = 0.2, max_steps = 200,
                                  seed = 123) {
  if (is.null(people)) stop("ANES data could not be loaded.", call. = FALSE)
  check_number(n, "n", 2, 1000, integer = TRUE)
  check_number(open_min, "open_min", 0, 2)
  check_number(open_max, "open_max", 0, 2)
  check_number(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
  if (open_min > open_max) stop("open_min must not exceed open_max.", call. = FALSE)

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

  n <- min(n, nrow(people))
  people <- people[sample(nrow(people), n), ]
  rownames(people) <- NULL
  openness <- open_max - (open_max - open_min) * people$identity
  adjacency <- generate_network_anes(people$opinion, people$media,
                                     min(average_degree, n - 1),
                                     homophily, media_effect)
  history <- matrix(people$opinion, nrow = 1)
  colnames(history) <- paste0("person_", seq_len(n))

  list(
    opinions = people$opinion, adjacency = adjacency, history = history,
    step = 0L, people = people,
    settings = list(source = "anes", n = n, average_degree = average_degree,
                    homophily = homophily, media_effect = media_effect,
                    open_min = open_min, open_max = open_max,
                    openness = openness, adjustment = adjustment,
                    max_steps = max_steps, seed = seed)
  )
}

# One step for either mode (simulated or ANES).
step_model_any <- function(state) {
  if (!identical(state$settings$source, "anes")) return(step_model(state))
  if (state$step >= state$settings$max_steps) return(state)
  state$opinions <- update_opinions_het(
    state$opinions, state$adjacency,
    state$settings$openness, state$settings$adjustment
  )
  state$step <- state$step + 1L
  state$history <- rbind(state$history, state$opinions)
  state
}

simulate_anes <- function(people, ...) {
  state <- initialize_anes_model(people, ...)
  for (i in seq_len(state$settings$max_steps)) state <- step_model_any(state)
  state
}

count_clusters <- function(x, gap = 0.05) 1 + sum(diff(sort(x)) > gap)

# Tiny deterministic checks (no data file needed).
run_anes_tests <- function() {
  a <- matrix(FALSE, 3, 3); a[1, 2] <- a[2, 1] <- TRUE
  x <- c(0, 0.2, 0.9)
  # A scalar openness must reproduce update_opinions() from simulation.R.
  stopifnot(isTRUE(all.equal(
    update_opinions_het(x, a, rep(0.4, 3), 0.2), update_opinions(x, a, 0.4, 0.2))))
  # Person 1 is too closed for person 2 (0.2 > 0.1); person 2 can listen to 1.
  stopifnot(isTRUE(all.equal(
    update_opinions_het(x, a, c(0.1, 0.4, 0.4), 0.5), c(0, 0.1, 0.9))))
  # media_effect = 0 with equal media is the original network model.
  set.seed(1); op <- runif(30, -1, 1)
  set.seed(2); g1 <- generate_network_anes(op, rep(0, 30), 4, 1, 0)
  set.seed(2); g2 <- generate_network(op, 4, 1)
  stopifnot(identical(g1, g2))
  message("All ANES model tests passed.")
  invisible(TRUE)
}
