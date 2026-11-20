#!/usr/bin/env Rscript
# ============================================================================
# Explores three extensions to "active" target selection, all implemented as
# composable parameters on choose_target() in learners.R (see that file's
# header for the full description of each):
#
#   1. active_prob  -- partial autonomy: the learner only gets their active
#                       choice some fraction p of episodes, otherwise falls
#                       back to passive. Answers Reviewer 1's question ("how
#                       much of the day must be actively sampled to get these
#                       benefits?") with an actual dose-response curve.
#   2. active_policy -- "goldilocks": prefer words with an intermediate amount
#                       of prior exposure over totally-fresh or heavily-exposed
#                       ones, operationalizing Kidd et al. (2012)'s Goldilocks
#                       effect (already cited as motivation, never previously
#                       implemented as the actual selection rule).
#   3. choice_k      -- bounded choice: active selection is restricted to a
#                       random K-sized "window" of the vocabulary per episode,
#                       not the full M. Answers Reviewer 1's "children can't
#                       just decide to go see giraffes at the zoo" critique.
#
# This is a smaller, more targeted grid than run_verification_grid.R: rather
# than crossing these three new dimensions into the FULL (model x M x C x a x
# fam_context) grid -- which would be enormous -- we fix a representative
# "headline" condition (M=1000, C=10, a=1 Zipfian, random context) matching
# what's already reported throughout the paper, and vary only the new
# dimensions, across all three models for robustness. Widen SWEEP_* below if
# you want to check whether a finding here also holds at C=100 or other
# exponents.
#
# Same wall-clock-budgeted, resumable, censoring-aware design as
# run_verification_grid.R -- see that script's header for the general pattern.
# ============================================================================

source("learners.R")

## ---------------------------- CONFIG --------------------------------------
MODELS <- c("eliminative", "guesstest", "rankedfreq")
M <- 1000
C <- 10          # the well-behaved C/M=0.01 regime; set to 100 to also probe near Vogt's boundary
A <- 1           # Zipfian exponent for all cells here (the paper's "headline" case)
FAM_CONTEXT <- FALSE  # isolate target-selection effects; set TRUE to cross with familiar-context too

SWEEP_ACTIVE_PROB <- c(0, 0.1, 0.25, 0.5, 0.75, 1)          # item 1
SWEEP_POLICY       <- c("unknown", "goldilocks")             # item 2 (only meaningful when active_prob>0)
SWEEP_CHOICE_K     <- c(5, 20, 50, 100, Inf)                  # item 3 (only meaningful when active_prob>0)
GOLDILOCKS_TARGET <- 2
GOLDILOCKS_SIGMA  <- 2

CELL_BUDGET_SECONDS <- 60
REP_MAX_SECONDS     <- 30
REPS_MAX_PER_CELL   <- 2000
NCORES <- max(1, parallel::detectCores() - 1)
RESULTS_PATH <- "active_modes_results.rds"
## ----------------------------------------------------------------------------

log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ..., "\n")

# Cell design:
#  (a) active_prob sweep at policy="unknown", choice_k=Inf  -- isolates item 1
#  (b) policy sweep at active_prob=1, choice_k=Inf           -- isolates item 2
#  (c) choice_k sweep at active_prob=1, policy="unknown"     -- isolates item 3
# Passive (active_prob=0) is included once (shared baseline, not repeated per sweep).
build_cells <- function() {
  rows <- list()
  for (model in MODELS) {
    rows[[length(rows) + 1]] <- data.frame(model = model, sweep = "baseline", active_prob = 0,
                                            active_policy = "unknown", choice_k = Inf)
    for (p in setdiff(SWEEP_ACTIVE_PROB, 0)) {
      rows[[length(rows) + 1]] <- data.frame(model = model, sweep = "active_prob", active_prob = p,
                                              active_policy = "unknown", choice_k = Inf)
    }
    for (pol in setdiff(SWEEP_POLICY, "unknown")) {
      rows[[length(rows) + 1]] <- data.frame(model = model, sweep = "policy", active_prob = 1,
                                              active_policy = pol, choice_k = Inf)
    }
    rows[[length(rows) + 1]] <- data.frame(model = model, sweep = "policy", active_prob = 1,
                                            active_policy = "unknown", choice_k = Inf)  # policy-sweep's own baseline
    for (k in setdiff(SWEEP_CHOICE_K, Inf)) {
      rows[[length(rows) + 1]] <- data.frame(model = model, sweep = "choice_k", active_prob = 1,
                                              active_policy = "unknown", choice_k = k)
    }
  }
  do.call(rbind, rows)
}

cells <- build_cells()
NCELLS <- nrow(cells)
log_msg(sprintf("Grid: %d cells (M=%d, C=%d, a=%s, fam_context=%s), NCORES=%d", NCELLS, M, C, A, FAM_CONTEXT, NCORES))
log_msg(sprintf("Per-cell budget: %ds | per-rep cap: %ds | worst-case total: %.1f min",
                 CELL_BUDGET_SECONDS, REP_MAX_SECONDS, NCELLS * CELL_BUDGET_SECONDS / 60))

cl <- makeCluster(NCORES)
registerDoParallel(cl)

prior <- if (file.exists(RESULTS_PATH)) readRDS(RESULTS_PATH) else NULL
already_done <- function(row) {
  if (is.null(prior)) return(FALSE)
  any(prior$model == row$model & prior$active_prob == row$active_prob &
        prior$active_policy == row$active_policy & prior$choice_k_label == (if (is.infinite(row$choice_k)) "Inf" else as.character(row$choice_k)))
}

results <- if (!is.null(prior)) list(prior) else list()
n_skipped <- 0

for (i in seq_len(nrow(cells))) {
  row <- cells[i, ]
  if (already_done(row)) { n_skipped <- n_skipped + 1; next }

  log_msg(sprintf("[%d/%d] start: %-11s sweep=%-11s active_prob=%.2f policy=%-11s choice_k=%s",
                   i, NCELLS, row$model, row$sweep, row$active_prob, row$active_policy,
                   if (is.infinite(row$choice_k)) "Inf" else row$choice_k))
  t0 <- Sys.time()
  d <- run_cell_budgeted(row$model, C, M, A, FALSE, FALSE, FAM_CONTEXT,
                          cell_budget_seconds = CELL_BUDGET_SECONDS, rep_max_seconds = REP_MAX_SECONDS,
                          reps_max = REPS_MAX_PER_CELL, ncores = NCORES,
                          active_prob = row$active_prob, active_policy = row$active_policy,
                          choice_k = row$choice_k, goldilocks_target = GOLDILOCKS_TARGET,
                          goldilocks_sigma = GOLDILOCKS_SIGMA)
  dt <- as.numeric(Sys.time() - t0, units = "secs")

  if (nrow(d) == 0) {
    log_msg(sprintf("  WARNING: 0 reps completed in %.0fs -- widen CELL_BUDGET_SECONDS or shorten REP_MAX_SECONDS. Recording a fully-censored placeholder.", dt))
    d <- as.data.frame(matrix(c(rep(NA_real_, 9), CELL_BUDGET_SECONDS, 1), nrow = 1))
    names(d) <- c(paste0("dec", 1:9), "episodes", "censored")
    d$model <- row$model; d$active <- "NA"; d$uniform <- "Zipfian"; d$fam_context <- ifelse(FAM_CONTEXT, "Familiar", "Random")
    d$C <- C; d$M <- M; d$a <- A
    d$cell_elapsed_secs <- dt
  } else {
    n_ok <- sum(d$censored == 0)
    mean_ok <- if (n_ok > 0) mean(d$episodes[d$censored == 0]) else NA
    log_msg(sprintf("  done in %.0fs: %d reps (%d ok, %d censored), mean episodes (ok only) = %s",
                     dt, nrow(d), n_ok, nrow(d) - n_ok, if (is.na(mean_ok)) "NA" else sprintf("%.0f", mean_ok)))
  }
  d$sweep <- row$sweep; d$active_prob <- row$active_prob; d$active_policy <- row$active_policy
  d$choice_k_label <- if (is.infinite(row$choice_k)) "Inf" else as.character(row$choice_k)

  results[[length(results) + 1]] <- d
  saveRDS(do.call(rbind, results), RESULTS_PATH)
}

log_msg(sprintf("All done. %d cells skipped, %d newly run.", n_skipped, NCELLS - n_skipped))
stopCluster(cl)
final <- do.call(rbind, results)
write.csv(final, "active_modes_results.csv", row.names = FALSE)
log_msg(sprintf("Saved %d rows.", nrow(final)))
