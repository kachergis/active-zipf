#!/usr/bin/env Rscript
# ============================================================================
# Wider, higher-precision follow-up to run_active_modes_grid.R (which used a
# quick 60s/cell budget at C=10 only). This version:
#
#   - adds C=100 as a second condition, to check whether the three findings
#     from the quick run (concave active_prob dose-response; Goldilocks
#     helping guess-test specifically; a puzzling non-monotonic choice_k
#     pattern for guess-test) hold near Vogt's C/M=0.1 boundary too, not just
#     in the "easy" C/M=0.01 regime
#   - gives every cell a much bigger wall-clock budget for tighter SEs
#   - adds finer choice_k resolution (10,15,25,30,40,60) around K=20, where
#     the quick run's guess-test result was oddly non-monotonic and the two
#     neighboring cells (K=50, K=100) had SEs too wide to trust
#
# Writes to a SEPARATE results file (active_modes_results_overnight.rds) from
# the quick run (active_modes_results.rds) rather than resuming/appending to
# it -- mixing a few-hundred-rep quick pass with a many-thousand-rep overnight
# pass in one dataset would recreate exactly the "inconsistent precision"
# problem run_verification_grid.R was built to get away from. Both datasets
# stay on disk; summarize_active_modes.R takes a --file argument (or edit the
# RESULTS_FILE constant at its top) to pick which one to summarize.
#
# Same wall-clock-budgeted, resumable, censoring-aware design as the other
# grid scripts in this directory -- see run_verification_grid.R's header for
# the general pattern (per-cell time budget, per-rep hard cap, incremental
# saving, safe to Ctrl-C and restart).
#
# SIZING THIS RUN: worst case is n_cells * CELL_BUDGET_SECONDS. With the
# defaults below (~78 cells, 600s budget) that's up to ~13 hours, but as with
# every other grid in this directory, most of that is headroom for the
# handful of genuinely slow cells (passive baselines, small choice_k, C=100)
# -- fast cells hit REPS_MAX_PER_CELL and move on long before their time
# budget is used up. Reasonable to kick off before bed and check in the
# morning; safe to Ctrl-C at any point and resume later, or just use whatever
# fraction completed (summarize_active_modes.R works fine on a partial file).
# ============================================================================

source("learners.R")

## ---------------------------- CONFIG --------------------------------------
MODELS <- c("eliminative", "guesstest", "rankedfreq")
M <- 1000
A <- 1                  # Zipfian exponent (the paper's headline case, as in the quick run)
FAM_CONTEXT <- FALSE    # isolate target-selection effects; set TRUE to also cross with familiar-context

# Per-C sweep resolution: fine at C=10 (cheap, and where the open questions
# from the quick run live), coarser at C=100 (expensive; enough points to
# check directional robustness without an enormous budget).
SWEEPS_BY_C <- list(
  `10` = list(
    active_prob = c(0, 0.1, 0.25, 0.5, 0.75, 1),
    choice_k    = c(5, 10, 15, 20, 25, 30, 40, 60, 100, Inf)
  ),
  `100` = list(
    active_prob = c(0, 0.25, 0.5, 0.75, 1),
    choice_k    = c(5, 20, 50, 100, Inf)
  )
)
SWEEP_POLICY <- c("unknown", "goldilocks")
GOLDILOCKS_TARGET <- 2
GOLDILOCKS_SIGMA  <- 2

CELL_BUDGET_SECONDS <- 600   # 10 min/cell (up from 60s in the quick run)
REP_MAX_SECONDS     <- 90    # up from 30s, so slower C=100 reps get a real chance to finish
REPS_MAX_PER_CELL   <- 3000  # up from 2000, for tighter SEs on fast cells
NCORES <- max(1, parallel::detectCores() - 1)
RESULTS_PATH <- "active_modes_results_overnight.rds"
## ----------------------------------------------------------------------------

log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ..., "\n")

# Builds the unique set of (active_prob, active_policy, choice_k) cells needed
# to cover all three sweeps for one C, then dedupes shared reference points
# (e.g. active_prob=1/policy="unknown"/choice_k=Inf is simultaneously the top
# of the active_prob sweep, the "unknown" baseline for the policy sweep, and
# the Inf endpoint of the choice_k sweep -- it should only be run once).
# Which sweep(s) a cell belongs to is derived later, in summarize_active_modes.R,
# from the parameter values themselves rather than tracked here.
build_cells_for_C <- function(C) {
  s <- SWEEPS_BY_C[[as.character(C)]]
  rows <- list()
  for (p in s$active_prob) {
    rows[[length(rows) + 1]] <- data.frame(C = C, active_prob = p, active_policy = "unknown", choice_k = Inf)
  }
  for (pol in setdiff(SWEEP_POLICY, "unknown")) {
    rows[[length(rows) + 1]] <- data.frame(C = C, active_prob = 1, active_policy = pol, choice_k = Inf)
  }
  for (k in s$choice_k) {
    rows[[length(rows) + 1]] <- data.frame(C = C, active_prob = 1, active_policy = "unknown", choice_k = k)
  }
  d <- do.call(rbind, rows)
  d[!duplicated(d), ]
}

cells <- do.call(rbind, lapply(as.numeric(names(SWEEPS_BY_C)), build_cells_for_C))
for (model in MODELS[-1]) {
  cells <- rbind(cells, do.call(rbind, lapply(as.numeric(names(SWEEPS_BY_C)), build_cells_for_C)))
}
cells$model <- rep(MODELS, each = nrow(cells) / length(MODELS))
NCELLS <- nrow(cells)

log_msg(sprintf("Grid: %d cells (M=%d, a=%s, fam_context=%s, C in {%s}), NCORES=%d",
                 NCELLS, M, A, FAM_CONTEXT, paste(names(SWEEPS_BY_C), collapse = ","), NCORES))
log_msg(sprintf("Per-cell budget: %ds | per-rep cap: %ds | worst-case total: %.1f hours",
                 CELL_BUDGET_SECONDS, REP_MAX_SECONDS, NCELLS * CELL_BUDGET_SECONDS / 3600))

cl <- makeCluster(NCORES)
registerDoParallel(cl)

prior <- if (file.exists(RESULTS_PATH)) readRDS(RESULTS_PATH) else NULL
already_done <- function(row) {
  if (is.null(prior)) return(FALSE)
  any(prior$model == row$model & prior$C == row$C & prior$active_prob == row$active_prob &
        prior$active_policy == row$active_policy &
        prior$choice_k_label == (if (is.infinite(row$choice_k)) "Inf" else as.character(row$choice_k)))
}

results <- if (!is.null(prior)) list(prior) else list()
n_skipped <- 0

for (i in seq_len(nrow(cells))) {
  row <- cells[i, ]
  if (already_done(row)) { n_skipped <- n_skipped + 1; next }

  log_msg(sprintf("[%d/%d] start: %-11s C=%-4d active_prob=%.2f policy=%-11s choice_k=%s",
                   i, NCELLS, row$model, row$C, row$active_prob, row$active_policy,
                   if (is.infinite(row$choice_k)) "Inf" else row$choice_k))
  t0 <- Sys.time()
  d <- run_cell_budgeted(row$model, row$C, M, A, FALSE, FALSE, FAM_CONTEXT,
                          cell_budget_seconds = CELL_BUDGET_SECONDS, rep_max_seconds = REP_MAX_SECONDS,
                          reps_max = REPS_MAX_PER_CELL, ncores = NCORES,
                          active_prob = row$active_prob, active_policy = row$active_policy,
                          choice_k = row$choice_k, goldilocks_target = GOLDILOCKS_TARGET,
                          goldilocks_sigma = GOLDILOCKS_SIGMA)
  dt <- as.numeric(Sys.time() - t0, units = "secs")

  if (nrow(d) == 0) {
    log_msg(sprintf("  WARNING: 0 reps completed in %.0fs. Recording a fully-censored placeholder.", dt))
    d <- as.data.frame(matrix(c(rep(NA_real_, 9), CELL_BUDGET_SECONDS, 1), nrow = 1))
    names(d) <- c(paste0("dec", 1:9), "episodes", "censored")
    d$model <- row$model; d$active <- "NA"; d$uniform <- "Zipfian"; d$fam_context <- ifelse(FAM_CONTEXT, "Familiar", "Random")
    d$C <- row$C; d$M <- M; d$a <- A
    d$cell_elapsed_secs <- dt
  } else {
    n_ok <- sum(d$censored == 0)
    mean_ok <- if (n_ok > 0) mean(d$episodes[d$censored == 0]) else NA
    log_msg(sprintf("  done in %.0fs: %d reps (%d ok, %d censored), mean episodes (ok only) = %s",
                     dt, nrow(d), n_ok, nrow(d) - n_ok, if (is.na(mean_ok)) "NA" else sprintf("%.0f", mean_ok)))
  }
  d$active_prob <- row$active_prob; d$active_policy <- row$active_policy
  d$choice_k_label <- if (is.infinite(row$choice_k)) "Inf" else as.character(row$choice_k)

  results[[length(results) + 1]] <- d
  saveRDS(do.call(rbind, results), RESULTS_PATH)
}

log_msg(sprintf("All done. %d cells skipped, %d newly run.", n_skipped, NCELLS - n_skipped))
stopCluster(cl)
final <- do.call(rbind, results)
write.csv(final, "active_modes_results_overnight.csv", row.names = FALSE)
log_msg(sprintf("Saved %d rows.", nrow(final)))
