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
# HONESTY NOTE: even with a generous cap, it's possible some eliminative/C=100
# cells (especially low-active_prob / passive-leaning ones, which need far
# more total episodes than active ones) will NOT reach TARGET_N_OK within any
# practical budget -- the calibration probe suggests a single passive-leaning
# replication may need many thousands of seconds. This script does NOT pretend
# otherwise: it spends each cell's own MAX_CELL_SECONDS ceiling, keeps
# whatever completed, and prints an honest before/after table plus an
# extrapolated "time to reach target at this rate" for anything still short,
# so you can decide whether to raise the ceiling and run it again rather than
# have it silently fall short forever.
#
# USAGE
#   Rscript fill_incomplete_active_modes_cells.R
#   (safe to Ctrl-C and rerun -- it recomputes remaining gaps from the file
#   every time, so a partial run just leaves you closer than before)
# ============================================================================

source("learners.R")
suppressMessages(library(dplyr))

## ---------------------------- CONFIG --------------------------------------
RESULTS_PATH <- "active_modes_results_overnight.rds"
TARGET_N_OK <- 100
M <- 1000

# Budget presets by (model, C). Cells not matching a preset use DEFAULT.
# rep_max_seconds: hard cap on ONE replication. cell_max_seconds: ceiling on
# TOTAL time spent topping up any single cell (not "per batch" -- the whole
# top-up for that cell stops here regardless of whether TARGET_N_OK was hit).
BUDGET_PRESETS <- list(
  list(model = "eliminative", C = 100, rep_max_seconds = 3600, cell_max_seconds = 14400),  # 1hr/rep, 4hr/cell ceiling
  list(model = "guesstest",   C = 100, rep_max_seconds = 180,  cell_max_seconds = 1800)     # already mostly done; cheap top-up
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

cl <- makeCluster(NCORES)
registerDoParallel(cl)

final_status <- list()

for (i in seq_len(nrow(gaps))) {
  g <- gaps[i, ]
  bud <- budget_for(g$model, g$C)
  choice_k_val <- if (g$choice_k_label == "Inf") Inf else as.numeric(g$choice_k_label)
  needed <- TARGET_N_OK - g$n_ok

  log_msg(sprintf("[%d/%d] %-11s C=%-4d active_prob=%.2f policy=%-11s choice_k=%-4s -- have %d/%d, need %d more (rep_cap=%ds, cell_ceiling=%ds)",
                   i, nrow(gaps), g$model, g$C, g$active_prob, g$active_policy, g$choice_k_label,
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

cat("\n=== Summary: before -> after ===\n")
summary_df <- do.call(rbind, final_status)
print(as.data.frame(summary_df %>% select(model, C, active_prob, active_policy, choice_k_label, n_ok_before, n_ok_after, secs_spent)),
      row.names = FALSE)

still_short <- summary_df %>% filter(n_ok_after < TARGET_N_OK)
if (nrow(still_short) > 0) {
  cat("\n", nrow(still_short), "cell(s) still short of", TARGET_N_OK, "after this run:\n")
  print(as.data.frame(still_short %>% select(model, C, active_prob, active_policy, choice_k_label, n_ok_after)), row.names = FALSE)
  cat("Rerun this script (raise BUDGET_PRESETS if a cell is making very slow progress) to keep topping these up.\n")
} else {
  cat("\nAll cells now at or above target.\n")
}
