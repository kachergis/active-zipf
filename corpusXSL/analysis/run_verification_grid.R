#!/usr/bin/env Rscript
# ============================================================================
# Full, apples-to-apples verification re-run of all three word-learning
# mechanisms (eliminative, guess-test, ranked-frequency), replacing the
# patchwork dataset behind the paper's current numbers -- which mixed rep
# counts (15-100), silently reused old, differently-parametrized data for a
# few cells that were too slow to brute-force, and left one guess-test cell
# only partially characterized (5 diagnostic reps).
#
# Every cell here gets IDENTICAL treatment:
#   - the same p_r ~ r^-a Zipf parametrization (no Mandelbrot-form fallback)
#   - a per-cell WALL-CLOCK BUDGET (not a fixed rep count): replications run
#     in parallel batches until the cell's time budget is used up, so fast
#     cells naturally accumulate many reps and slow/near-intractable cells
#     accumulate few (or zero) -- itself an honest, informative outcome
#   - a per-replication hard wall-clock cap (rep_max_seconds), so a single
#     pathological replication (we found two: an eliminative C=100 cell that
#     didn't finish in 42 minutes, and a bimodal guess-test cell where ~1-in-5
#     replications ran away) can never block a whole cell -- it just gets
#     flagged `censored=1` and excluded from the mean/median, with the
#     censoring rate itself reported (a censored cell is a real result: it
#     means "this condition is intractable at this budget", not missing data)
#
# HOW TO USE
#   1. Skim the CONFIG block below and adjust to taste (see comments on each
#      setting -- the defaults are a reasonable overnight run on a 12-16 core
#      machine, see the "Sizing this run" note below for how to estimate time).
#   2. Rscript run_verification_grid.R
#      (or `Rscript run_verification_grid.R &` to background it, or run it in
#      a screen/tmux session -- there is nothing cluster-specific here, it's
#      just a long-running local R process)
#   3. It saves incrementally after every cell to verification_results.rds, and
#      SKIPS any cell already present in that file if you stop and restart it
#      -- so it's safe to Ctrl-C and resume later, or to just let it run
#      indefinitely and check in on progress with:
#        Rscript -e 'source("learners.R"); d <- readRDS("verification_results.rds");
#                    cat(nrow(unique(d[,c("model","C","M","a","active","fam_context")])), "of", NCELLS, "cells done\n")'
#      (NCELLS is printed when this script starts)
#      NOTE: a cell that came back with 0 completed reps (fully censored -- see
#      the "WARNING: 0 reps completed" case below) is still marked "done" on
#      resume, so it won't retry itself automatically. If you want to give a
#      specific stuck cell a bigger budget, delete its rows from
#      verification_results.rds (filter on model/M/C/a/active/fam_context)
#      before restarting, or just bump CELL_BUDGET_SECONDS/REP_MAX_SECONDS
#      globally and delete the whole file to redo everything.
#   4. Once you have enough, run summarize_verification_grid.R to get a
#      mean/median/CI/censoring-rate table and figures comparable to the ones
#      already in the paper, so you can see directly whether the smaller,
#      compute-constrained runs held up.
#
# SIZING THIS RUN
#   Total worst-case time = n_cells * cell_budget_seconds. With the defaults
#   below (96 cells at M=1000, cell_budget_seconds=600) that's up to 16 hours,
#   but almost all of that budget will only be spent on the handful of cells
#   that are genuinely slow (passive+random+steep-Zipfian, especially for the
#   eliminative model at C=100) -- most cells (anything active, anything
#   uniform, most C=10 cells) finish in seconds and won't use their budget.
#   Enable INCLUDE_M10000 for a second, much slower tier at M=10,000; expect
#   most eliminative/rankedfreq passive cells there to come back fully or
#   mostly censored even at a generous budget -- that is itself the answer to
#   "is M=10,000 tractable for this mechanism at this C", which is exactly why
#   we abandoned that tier by hand last time instead of measuring it directly.
# ============================================================================

source("learners.R")

## ---------------------------- CONFIG --------------------------------------
MODELS   <- c("eliminative", "guesstest", "rankedfreq")
Ms_MAIN  <- c(1000)
Cs       <- c(10, 100)
As       <- c(0.5, 1, 1.5)          # Zipfian exponents; uniform (a=0) is always included separately
INCLUDE_M10000 <- FALSE             # set TRUE to also run the M=10,000 tier (see note above)
Ms_LARGE <- c(10000)

CELL_BUDGET_SECONDS <- 600          # wall-clock budget PER CELL (10 min default)
REP_MAX_SECONDS     <- 120          # hard cap on a SINGLE replication within a cell (2 min default)
REPS_MAX_PER_CELL   <- 2000         # extra safety valve: stop a cell after this many reps even if time remains
                                     # (only binds for very cheap cells that would otherwise accumulate
                                     # an unnecessarily huge number of reps within the time budget)

NCORES <- max(1, parallel::detectCores() - 1)
RESULTS_PATH <- "verification_results.rds"
LOG_EVERY_CELL <- TRUE
## ----------------------------------------------------------------------------

log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ..., "\n")

# Build the full cell list (one row per model x M x C x a-or-uniform x active x fam_context)
build_cells <- function(Ms) {
  rows <- list()
  for (model in MODELS) for (M in Ms) for (C in Cs) {
    if (C >= M) next
    for (active in c(FALSE, TRUE)) for (fam in c(FALSE, TRUE)) {
      rows[[length(rows) + 1]] <- data.frame(model = model, M = M, C = C, a = 0,
                                              uniform = TRUE, active = active, fam_context = fam)
      for (a in As) {
        rows[[length(rows) + 1]] <- data.frame(model = model, M = M, C = C, a = a,
                                                uniform = FALSE, active = active, fam_context = fam)
      }
    }
  }
  do.call(rbind, rows)
}

cells <- build_cells(Ms_MAIN)
if (INCLUDE_M10000) cells <- rbind(cells, build_cells(Ms_LARGE))
NCELLS <- nrow(cells)
log_msg(sprintf("Grid: %d cells (%d models x %d C x %d distributions x 2 active x 2 fam_context%s), NCORES=%d",
                 NCELLS, length(MODELS), length(Cs), 1 + length(As),
                 if (INCLUDE_M10000) sprintf(", M in {%s}", paste(c(Ms_MAIN, Ms_LARGE), collapse=",")) else sprintf(", M=%d", Ms_MAIN),
                 NCORES))
log_msg(sprintf("Per-cell budget: %ds | per-rep cap: %ds | worst-case total: %.1f hours",
                 CELL_BUDGET_SECONDS, REP_MAX_SECONDS, NCELLS * CELL_BUDGET_SECONDS / 3600))

cl <- makeCluster(NCORES)
registerDoParallel(cl)

prior <- if (file.exists(RESULTS_PATH)) readRDS(RESULTS_PATH) else NULL
already_done <- function(row) {
  if (is.null(prior)) return(FALSE)
  act_s <- ifelse(row$active, "Active", "Passive")
  uni_s <- ifelse(row$uniform, "Uniform", "Zipfian")
  fam_s <- ifelse(row$fam_context, "Familiar", "Random")
  any(prior$model == row$model & prior$C == row$C & prior$M == row$M & prior$a == row$a &
        prior$active == act_s & prior$uniform == uni_s & prior$fam_context == fam_s)
}

results <- if (!is.null(prior)) list(prior) else list()
n_skipped <- 0

for (i in seq_len(nrow(cells))) {
  row <- cells[i, ]
  if (already_done(row)) { n_skipped <- n_skipped + 1; next }

  if (LOG_EVERY_CELL) {
    log_msg(sprintf("[%d/%d] start: %-11s M=%-6d C=%-4d a=%-4s uniform=%-5s active=%-7s fam=%-7s",
                     i, NCELLS, row$model, row$M, row$C, row$a, row$uniform, row$active, row$fam_context))
  }
  t0 <- Sys.time()
  d <- run_cell_budgeted(row$model, row$C, row$M, row$a, row$uniform, row$active, row$fam_context,
                          cell_budget_seconds = CELL_BUDGET_SECONDS, rep_max_seconds = REP_MAX_SECONDS,
                          reps_max = REPS_MAX_PER_CELL, ncores = NCORES)
  dt <- as.numeric(Sys.time() - t0, units = "secs")

  if (nrow(d) == 0) {
    log_msg(sprintf("  WARNING: 0 reps completed in %.0fs (cell_budget_seconds may be smaller than one rep_max_seconds -- widen CELL_BUDGET_SECONDS or shorten REP_MAX_SECONDS). Recording a single fully-censored placeholder row.", dt))
    placeholder <- as.data.frame(matrix(c(rep(NA_real_, 9), CELL_BUDGET_SECONDS, 1), nrow = 1))
    names(placeholder) <- c(paste0("dec", 1:9), "episodes", "censored")
    placeholder$model <- row$model; placeholder$C <- row$C; placeholder$M <- row$M; placeholder$a <- row$a
    placeholder$active <- ifelse(row$active, "Active", "Passive")
    placeholder$uniform <- ifelse(row$uniform, "Uniform", "Zipfian")
    placeholder$fam_context <- ifelse(row$fam_context, "Familiar", "Random")
    placeholder$cell_elapsed_secs <- dt
    d <- placeholder
  } else {
    n_ok <- sum(d$censored == 0)
    n_cens <- sum(d$censored == 1)
    mean_ok <- if (n_ok > 0) mean(d$episodes[d$censored == 0]) else NA
    log_msg(sprintf("  done in %.0fs: %d reps (%d ok, %d censored), mean episodes (ok only) = %s",
                     dt, nrow(d), n_ok, n_cens, if (is.na(mean_ok)) "NA (all censored)" else sprintf("%.0f", mean_ok)))
  }

  results[[length(results) + 1]] <- d
  saveRDS(do.call(rbind, results), RESULTS_PATH)
}

log_msg(sprintf("All done. %d cells skipped (already present from a previous run), %d newly run.",
                 n_skipped, NCELLS - n_skipped))
stopCluster(cl)

final <- do.call(rbind, results)
write.csv(final, "verification_results.csv", row.names = FALSE)
log_msg(sprintf("Saved %d total replication rows to %s / verification_results.csv", nrow(final), RESULTS_PATH))
