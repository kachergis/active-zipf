# Consolidated simulation grid: eliminative vs. guess-test learners, crossed with
# active/passive target selection, random/familiar context selection, and word
# frequency distribution (uniform + Zipf exponents), at M=1000 (primary) and
# M=10,000 (active-only subset).
#
# Tractability notes (determined empirically before/during this run):
# - The eliminative model's per-episode cost scales with M (it maintains a full
#   M-length candidate-elimination vector per word), so passive+Zipfian(a>=1)
#   cells at C=100 require both very many episodes (Vogt 2012's C/M ratio effect --
#   C=100/M=1000=0.1 is right at his reported tractability boundary) AND expensive
#   per-episode bookkeeping. A single rep of eliminative/C=100/a=1/passive/random
#   did not finish in 42 minutes. These specific cells are therefore reported via
#   the already-validated analytical bound (Eq. eq-Tbound) instead of brute force.
# - The guess-test model has no such cost (a single current guess per word, O(C)
#   per episode, not O(M)); C=100/a=1/passive/random completes a rep in ~22s. It is
#   brute-forced across the full grid without exclusions.

source("learners.R")

cl <- makeCluster(12)
registerDoParallel(cl)

log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n")

results_path <- "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/full_grid_results.rds"
prior <- if (file.exists(results_path)) readRDS(results_path) else NULL
results <- if (!is.null(prior)) list(prior) else list()

already_done <- function(learner, C, M, a, active, uniform, fam) {
  if (is.null(prior)) return(FALSE)
  act_s <- ifelse(active, "Active", "Passive")
  uni_s <- ifelse(uniform, "Uniform", "Zipfian")
  fam_s <- ifelse(fam, "Familiar", "Random")
  any(prior$model == learner & prior$C == C & prior$M == M & prior$a == a &
        prior$active == act_s & prior$uniform == uni_s & prior$fam_context == fam_s)
}

run_cell <- function(learner, C, M, a, uniform, active, fam_context, reps) {
  if (already_done(learner, C, M, a, active, uniform, fam_context)) {
    log_msg(sprintf("skip (already done): %-11s M=%-6d C=%-4d a=%-4s uniform=%-5s active=%-7s fam=%-7s",
                     learner, M, C, a, uniform, active, fam_context))
    return(NULL)
  }
  log_msg(sprintf("start: %-11s M=%-6d C=%-4d a=%-4s uniform=%-5s active=%-7s fam=%-7s reps=%d",
                   learner, M, C, a, uniform, active, fam_context, reps))
  t0 <- Sys.time()
  r <- repeat_sim(learner, C, M, a, uniform, active, fam_context, reps = reps)
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  log_msg(sprintf("  done in %.1fs, mean p99=%.0f", dt, mean(r[, 10])))
  d <- as.data.frame(r)
  names(d) <- c("dec1","dec2","dec3","dec4","dec5","dec6","dec7","dec8","dec9","p99")
  d$model <- learner; d$C <- C; d$M <- M; d$a <- a
  d$active <- ifelse(active, "Active", "Passive")
  d$uniform <- ifelse(uniform, "Uniform", "Zipfian")
  d$fam_context <- ifelse(fam_context, "Familiar", "Random")
  d
}

save_progress <- function() {
  saveRDS(do.call(rbind, results), results_path)
}

# eliminative/C=100/passive/a>=1 is excluded from brute force (see header);
# everything else uses full reps.
skip_brute_force <- function(learner, C, active, a) {
  learner == "eliminative" && C >= 100 && !active && a >= 1
}

Ms_main <- c(1000)
Cs_main <- c(10, 100)
As_full <- c(0.5, 1)
reps_main <- 30

for (learner in c("eliminative", "guesstest")) {
  for (M in Ms_main) {
    for (C in Cs_main) {
      for (active in c(FALSE, TRUE)) {
        for (fam in c(FALSE, TRUE)) {
          results[[length(results) + 1]] <- run_cell(learner, C, M, 0, TRUE, active, fam, reps_main)
        }
      }
      for (a in As_full) {
        for (active in c(FALSE, TRUE)) {
          if (!active && skip_brute_force(learner, C, active, a)) {
            log_msg(sprintf("EXCLUDED from brute force: %s C=%d a=%s passive (see header) -- using analytical T+ instead", learner, C, a))
            next
          }
          for (fam in c(FALSE, TRUE)) {
            results[[length(results) + 1]] <- run_cell(learner, C, M, a, FALSE, active, fam, reps_main)
          }
        }
      }
      # a=1.5: active only (for both models, matching original design)
      for (fam in c(FALSE, TRUE)) {
        results[[length(results) + 1]] <- run_cell(learner, C, M, 1.5, FALSE, TRUE, fam, reps_main)
      }
      save_progress()
    }
  }
}

log_msg("Main M=1000 grid complete.")

## ---- M=10,000 extension: active-target conditions only ----
Ms_big <- c(10000)
Cs_big <- c(10, 100)
reps_big <- 15

for (learner in c("eliminative", "guesstest")) {
  for (M in Ms_big) {
    for (C in Cs_big) {
      for (fam in c(FALSE, TRUE)) {
        results[[length(results) + 1]] <- run_cell(learner, C, M, 1, FALSE, TRUE, fam, reps_big)
      }
      for (fam in c(FALSE, TRUE)) {
        results[[length(results) + 1]] <- run_cell(learner, C, M, 0, TRUE, TRUE, fam, reps_big)
      }
      save_progress()
    }
  }
}

log_msg("All done.")
stopCluster(cl)

final <- do.call(rbind, results)
write.csv(final, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/full_grid_results.csv", row.names = FALSE)
log_msg("Saved", nrow(final), "rows.")
