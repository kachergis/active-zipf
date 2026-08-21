#!/usr/bin/env Rscript
# ============================================================================
# Tops up any cell in active_modes_results_overnight.rds that has fewer than
# TARGET_N_OK completed (non-censored) replications, rather than rerunning the
# whole 78-cell grid. Appends new replications to the SAME file (doesn't
# overwrite existing good data).
#
# WHY THIS SCRIPT EXISTS: the overnight run used a uniform 90s rep_max_seconds
# / 600s cell_budget_seconds for every cell. That was fine for C=10 and for
# most of C=100, but ALL 10 eliminative/C=100 cells came back 100% censored
# (91/91 attempts, every single one hit the 90s cap) -- 0 usable data. A
# follow-up calibration probe (uncapped up to 900s) confirmed why: a single
# eliminative/C=100/a=1 replication hadn't finished even after 900s and ~3-3.7
# million episodes, for BOTH active and passive conditions. So simply rerunning
# with the same settings would produce more of the same nothing -- these cells
# need a fundamentally larger per-replication budget, which this script gives
# them (while giving the already-mostly-complete guesstest/C=100 cells a much
# smaller, cheaper top-up).
#
# UPDATE: brute-force was ruled out entirely for eliminative/C=100, not just
# under-budgeted. analytical_elimination_bound.R derives (and validates to
# ~8-12% against real C=10 simulation data) a closed-form-ish approximation
# for this model's expected episodes; applied to C=100 it predicts T_active
# ~= 1.46 BILLION episodes and T_passive ~= 11 BILLION -- at the observed
# ~4000 episodes/sec, that's ~4 DAYS for a single active replication and far
# longer for passive-leaning ones. No practical rep_max_seconds/cell budget
# fixes that. So this script no longer attempts brute force for
# eliminative/C=100 at all: those cells are filled in analytically instead
# (see the bottom of this script), clearly labeled as estimated, not
# simulated. Only the guesstest/C=100 cells (genuinely tractable, just
# under-budgeted at 90s/rep) still go through run_cell_budgeted.
#
# The analytical formula currently only covers the two extremes cleanly:
# active_prob=0 (pure passive) and active_prob=1 with policy="unknown",
# choice_k=Inf (pure, unrestricted active). It does NOT yet cover
# intermediate active_prob mixtures, the goldilocks policy, or bounded
# choice_k -- those cells are left as genuinely open (reported at the end,
# not silently dropped).
#
# USAGE
#   Rscript fill_incomplete_active_modes_cells.R
#   (safe to Ctrl-C and rerun -- it recomputes remaining gaps from the file
#   every time, so a partial run just leaves you closer than before)
# ============================================================================

source("learners.R")
source("analytical_elimination_bound.R")
suppressMessages(library(dplyr))

## ---------------------------- CONFIG --------------------------------------
RESULTS_PATH <- "active_modes_results_overnight.rds"
ANALYTICAL_PATH <- "eliminative_C100_analytical_estimates.csv"
TARGET_N_OK <- 100
M <- 1000

# Budget presets by (model, C). Cells not matching a preset use DEFAULT.
# rep_max_seconds: hard cap on ONE replication. cell_max_seconds: ceiling on
# TOTAL time spent topping up any single cell (not "per batch" -- the whole
# top-up for that cell stops here regardless of whether TARGET_N_OK was hit).
# NOTE: eliminative/C=100 deliberately has NO preset here -- brute force is
# ruled out for it entirely (see header); those cells are skipped in the
# simulation loop below and filled in analytically at the end instead.
BUDGET_PRESETS <- list(
  list(model = "guesstest", C = 100, rep_max_seconds = 180, cell_max_seconds = 1800)  # already mostly done; cheap top-up
)
DEFAULT_BUDGET <- list(rep_max_seconds = 90, cell_max_seconds = 600)

NCORES <- max(1, parallel::detectCores() - 1)
## ----------------------------------------------------------------------------

log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ..., "\n")

budget_for <- function(model, C) {
  for (p in BUDGET_PRESETS) if (p$model == model && p$C == C) return(p)
  DEFAULT_BUDGET
}

stopifnot(file.exists(RESULTS_PATH))
d <- readRDS(RESULTS_PATH)
d$episodes <- as.numeric(d$episodes); d$censored <- as.numeric(d$censored)
d$active_prob <- as.numeric(d$active_prob); d$C <- as.numeric(d$C); d$a <- as.numeric(d$a)

gaps <- d %>%
  group_by(model, C, a, active_prob, active_policy, choice_k_label) %>%
  summarise(n_ok = sum(censored == 0), n_total = n(), .groups = "drop") %>%
  filter(n_ok < TARGET_N_OK) %>%
  arrange(desc(n_ok))  # closest-to-target first: cheap wins happen before expensive long-shots

if (nrow(gaps) == 0) {
  cat("Nothing to do -- every cell already has >=", TARGET_N_OK, "completed replications.\n")
  quit(save = "no")
}

cat("=== Cells needing top-up (", nrow(gaps), "of them ), ordered closest-to-target first ===\n")
print(as.data.frame(gaps), row.names = FALSE)
cat("\n")

sim_gaps <- gaps %>% filter(!(model == "eliminative" & C == 100))
skipped_for_analytical <- gaps %>% filter(model == "eliminative" & C == 100)
if (nrow(skipped_for_analytical) > 0) {
  cat(nrow(skipped_for_analytical), "eliminative/C=100 cell(s) skipped for brute force (infeasible -- see header); handled analytically below instead.\n\n")
}

cl <- makeCluster(NCORES)
registerDoParallel(cl)

final_status <- list()

for (i in seq_len(nrow(sim_gaps))) {
  g <- sim_gaps[i, ]
  bud <- budget_for(g$model, g$C)
  choice_k_val <- if (g$choice_k_label == "Inf") Inf else as.numeric(g$choice_k_label)
  needed <- TARGET_N_OK - g$n_ok

  log_msg(sprintf("[%d/%d] %-11s C=%-4d active_prob=%.2f policy=%-11s choice_k=%-4s -- have %d/%d, need %d more (rep_cap=%ds, cell_ceiling=%ds)",
                   i, nrow(sim_gaps), g$model, g$C, g$active_prob, g$active_policy, g$choice_k_label,
                   g$n_ok, TARGET_N_OK, needed, bud$rep_max_seconds, bud$cell_max_seconds))

  t0 <- Sys.time()
  new_d <- run_cell_budgeted(g$model, g$C, M, g$a, g$a == 0, FALSE, FALSE,
                              cell_budget_seconds = bud$cell_max_seconds,
                              rep_max_seconds = bud$rep_max_seconds,
                              reps_max = needed + 50,  # small margin over `needed` since some will be censored
                              ncores = NCORES,
                              active_prob = g$active_prob, active_policy = g$active_policy,
                              choice_k = choice_k_val)
  dt <- as.numeric(Sys.time() - t0, units = "secs")

  if (nrow(new_d) == 0) {
    log_msg(sprintf("  got 0 new replications in %.0fs (cell_max_seconds may be smaller than one rep_max_seconds for this cell)", dt))
    final_status[[length(final_status) + 1]] <- data.frame(g, n_ok_before = g$n_ok, n_ok_after = g$n_ok, secs_spent = dt)
    next
  }

  new_d$active_prob <- g$active_prob; new_d$active_policy <- g$active_policy
  new_d$choice_k_label <- g$choice_k_label
  n_new_ok <- sum(new_d$censored == 0)
  n_ok_after <- g$n_ok + n_new_ok

  log_msg(sprintf("  done in %.0fs: %d new reps (%d ok, %d censored) -- now %d/%d%s",
                   dt, nrow(new_d), n_new_ok, nrow(new_d) - n_new_ok, n_ok_after, TARGET_N_OK,
                   if (n_ok_after < TARGET_N_OK && n_new_ok > 0) {
                     rate <- n_new_ok / dt
                     remaining_secs <- (TARGET_N_OK - n_ok_after) / rate
                     sprintf(" -- still short; at this rate, ~%.1f more hours needed", remaining_secs / 3600)
                   } else if (n_ok_after < TARGET_N_OK) {
                     " -- still short, and got ZERO clean reps this round (likely needs an even bigger rep_max_seconds for this cell)"
                   } else ""))

  # append and save immediately, so interrupting the script never loses progress
  d <- bind_rows(d, new_d)
  saveRDS(d, RESULTS_PATH)

  final_status[[length(final_status) + 1]] <- data.frame(g, n_ok_before = g$n_ok, n_ok_after = n_ok_after, secs_spent = dt)
}

stopCluster(cl)
write.csv(d, "active_modes_results_overnight.csv", row.names = FALSE)

if (length(final_status) > 0) {
  cat("\n=== Simulation summary: before -> after ===\n")
  summary_df <- do.call(rbind, final_status)
  print(as.data.frame(summary_df %>% select(model, C, active_prob, active_policy, choice_k_label, n_ok_before, n_ok_after, secs_spent)),
        row.names = FALSE)

  still_short <- summary_df %>% filter(n_ok_after < TARGET_N_OK)
  if (nrow(still_short) > 0) {
    cat("\n", nrow(still_short), "cell(s) still short of", TARGET_N_OK, "after this run:\n")
    print(as.data.frame(still_short %>% select(model, C, active_prob, active_policy, choice_k_label, n_ok_after)), row.names = FALSE)
    cat("Rerun this script (raise BUDGET_PRESETS if a cell is making very slow progress) to keep topping these up.\n")
  } else {
    cat("\nAll simulated cells now at or above target.\n")
  }
} else {
  cat("\nNo cells needed brute-force top-up this run.\n")
}

## ---- Analytical fill-in for eliminative/C=100 (brute force infeasible) ----
if (nrow(skipped_for_analytical) > 0) {
  cat("\n=== Analytical estimates for eliminative/C=100 (see analytical_elimination_bound.R for derivation + validation) ===\n")
  covered <- skipped_for_analytical %>%
    filter((active_prob == 0) | (active_prob == 1 & active_policy == "unknown" & choice_k_label == "Inf"))
  uncovered <- skipped_for_analytical %>% anti_join(covered, by = c("model","C","a","active_prob","active_policy","choice_k_label"))

  if (nrow(covered) > 0) {
    Nstar <- mean_Nstar(M, 100, 1)
    est <- covered %>% rowwise() %>% mutate(
      T_predicted = if (active_prob == 0) predict_T_passive(M, 100, a) else predict_T_active(M, 100, a),
      method = if (active_prob == 0) "NegBinom(rarest word, N*, min_p) 99th pctile" else "M * mean_w(E[N_w]), no wasted draws",
      validated_error_at_C10 = if (active_prob == 0) "~12% (79008 vs actual 89454)" else "~8% (4353 vs actual 4695)"
    ) %>% ungroup()
    print(as.data.frame(est %>% select(model, C, active_prob, active_policy, choice_k_label, T_predicted, method, validated_error_at_C10)),
          row.names = FALSE)
    write.csv(est, ANALYTICAL_PATH, row.names = FALSE)
    cat("Saved to", ANALYTICAL_PATH, "-- these are ESTIMATES, not simulated replications; do not append them to active_modes_results_overnight.rds.\n")
  }
  if (nrow(uncovered) > 0) {
    cat("\n", nrow(uncovered), "eliminative/C=100 cell(s) remain genuinely open (neither simulated nor analytically covered yet):\n")
    print(as.data.frame(uncovered %>% select(model, C, active_prob, active_policy, choice_k_label)), row.names = FALSE)
    cat("These are intermediate active_prob mixtures and/or the goldilocks/bounded-choice_k variants -- the\n")
    cat("current analytical formula only derives the pure-passive and pure-active(unknown,unrestricted) cases.\n")
  }
}
