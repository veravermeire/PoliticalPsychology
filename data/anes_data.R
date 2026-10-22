#### ANES 2024 survey data (static real-data view) ####
# Reads the raw ANES 2024 Time Series csv and provides the helpers the app uses
# to compare real survey respondents with the simulated population.
# The survey has no information on who knows whom, so there is no network and
# no opinion dynamics for the real data: it is shown as it is.
# Requires nothing but base R (simulation.R supplies the simulated opinions).

# ---------------------------------------------------------- reading data ----
# The app reads the raw ANES 2024 Time Series csv that sits in the
# Data/anes_dataset/ folder of this repository (any file named
# anes_timeseries*.csv). Only the few
# columns we need are read, so loading takes a couple of seconds.

library(here)

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

find_anes_file <- function(dir = here("Data", "anes_dataset")) {
  files <- list.files(dir, pattern = "^anes_timeseries.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No ANES file found. Put the ANES 2024 Time Series csv in the ",
         "Data/anes_dataset/ folder (see README).", call. = FALSE)
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

# ------------------------------------------------------------ comparison ----
# Run an expression with a fixed random seed, then restore the caller's
# random-number state.
with_local_seed <- function(seed, expr) {
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
  force(expr)
}

# Distance between the average opinions of the Republican side and the
# Democratic side (leaners included) on the -1..+1 scale, with a 95% confidence
# interval (normal approximation for a difference in means).
group_distance <- function(p) {
  r <- p$opinion[p$side %in% "R"]
  d <- p$opinion[p$side %in% "D"]
  if (length(r) < 2 || length(d) < 2) {
    return(c(estimate = NA_real_, lower = NA_real_, upper = NA_real_, n = nrow(p)))
  }
  est <- mean(r) - mean(d)
  se <- sqrt(var(r) / length(r) + var(d) / length(d))
  c(estimate = est, lower = est - 1.96 * se, upper = est + 1.96 * se, n = nrow(p))
}

# Group distance for all respondents and for "low" and "high" levels of the
# three factors in the model. Respondents between the cut-offs are left out of
# the low/high comparisons.
factor_distances <- function(people, identity_cut = c(0.25, 0.75),
                             perceived_cut = c(3, 5)) {
  pp <- people$perceived_polarization
  specs <- list(
    list("All respondents", "All", "everyone", rep(TRUE, nrow(people))),
    list("Identity strength", "Low",
         sprintf("party identity importance <= %.2f (0-1)", identity_cut[1]),
         people$identity <= identity_cut[1]),
    list("Identity strength", "High",
         sprintf("party identity importance >= %.2f (0-1)", identity_cut[2]),
         people$identity >= identity_cut[2]),
    list("Congenial media", "Low", "no congenial TV shows", people$media == 0),
    list("Congenial media", "High", "at least one congenial TV show", people$media > 0),
    list("Perceived polarization", "Low",
         sprintf("perceived party distance <= %d (0-6)", perceived_cut[1]),
         !is.na(pp) & pp <= perceived_cut[1]),
    list("Perceived polarization", "High",
         sprintf("perceived party distance >= %d (0-6)", perceived_cut[2]),
         !is.na(pp) & pp >= perceived_cut[2])
  )
  rows <- lapply(specs, function(sp) {
    g <- group_distance(people[sp[[4]], ])
    data.frame(Factor = sp[[1]], Level = sp[[2]], Definition = sp[[3]],
               People = g[["n"]], Distance = g[["estimate"]],
               Lower = g[["lower"]], Upper = g[["upper"]])
  })
  do.call(rbind, rows)
}

# Distance between the mean opinions of two groups defined by the sign of the
# STARTING opinion (the simulation does not store group labels). `current` can
# be the starting opinions themselves or the opinions at a later step.
sim_group_distance <- function(initial, current) {
  neg <- initial < 0
  pos <- initial > 0
  if (!any(neg) || !any(pos)) return(NA_real_)
  mean(current[pos]) - mean(current[neg])
}

# Correlations between the survey measures.
survey_correlations <- function(people) {
  vars <- people[, c("opinion", "identity", "media", "perceived_polarization")]
  names(vars) <- c("Opinion (con. = high)", "Party identity importance",
                   "Congenial media", "Perceived party distance")
  m <- round(cor(vars, use = "pairwise.complete.obs"), 2)
  cbind(Measure = rownames(m), as.data.frame(m, check.names = FALSE))
}

# Tiny checks (no data file needed).
run_anes_tests <- function() {
  set.seed(5); a <- with_local_seed(1, runif(3)); b <- with_local_seed(1, runif(3))
  stopifnot(identical(a, b))
  p <- data.frame(opinion = c(-1, -1, 1, 1, 0.5), side = c("D", "D", "R", "R", "R"))
  g <- group_distance(p)
  stopifnot(abs(g[["estimate"]] - (mean(c(1, 1, 0.5)) - (-1))) < 1e-12, g[["n"]] == 5)
  stopifnot(is.na(group_distance(p[1:3, ])[["estimate"]]))   # too few Republicans
  stopifnot(sim_group_distance(c(-1, -1, 1, 1), c(-0.5, -0.5, 0.5, 0.5)) == 1)
  stopifnot(is.na(sim_group_distance(c(0.2, 0.4), c(0.2, 0.4))))
  message("All ANES helper tests passed.")
  invisible(TRUE)
}
