# Guess-test model only: full grid at M=1000 (C=10, C=100, exponents 0/0.5/1/1.5)
# plus an M=10,000 active-only extension. The eliminative model's C=100 cells
# turned out to be far more expensive than anticipated even for active target
# selection (an eliminative/C=100/a=1/active cell did not finish in 28 minutes),
# so eliminative results at C=100 reuse already-existing simulation data from
# earlier work (act_pass_sim_results1000_*.RData, Zipf-Mandelbrot form, a~1)
# rather than being re-simulated here; the eliminative M=10,000 extension is
# dropped for the same reason. Guess-test's O(1)-per-word bookkeeping (vs.
# eliminative's O(M) elimination vector) makes it cheap enough to brute-force
# across the full design.

source("learners.R")

cl <- makeCluster(12)
registerDoParallel(cl)

log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n")

results_path <- "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/guesstest_results.rds"
results <- list()

run_cell <- function(learner, C, M, a, uniform, active, fam_context, reps) {
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
save_progress <- function() saveRDS(do.call(rbind, results), results_path)

Cs <- c(10, 100)
As_full <- c(0.5, 1)
reps_main <- 30

for (C in Cs) {
  for (active in c(FALSE, TRUE)) {
    for (fam in c(FALSE, TRUE)) {
      results[[length(results) + 1]] <- run_cell("guesstest", C, 1000, 0, TRUE, active, fam, reps_main)
    }
  }
  for (a in As_full) {
    for (active in c(FALSE, TRUE)) {
      for (fam in c(FALSE, TRUE)) {
        results[[length(results) + 1]] <- run_cell("guesstest", C, 1000, a, FALSE, active, fam, reps_main)
      }
    }
  }
  for (fam in c(FALSE, TRUE)) {
    results[[length(results) + 1]] <- run_cell("guesstest", C, 1000, 1.5, FALSE, TRUE, fam, reps_main)
  }
  save_progress()
}
log_msg("M=1000 guess-test grid complete.")

## M=10,000 extension, active only
reps_big <- 15
for (C in c(10, 100)) {
  for (fam in c(FALSE, TRUE)) {
    results[[length(results) + 1]] <- run_cell("guesstest", C, 10000, 1, FALSE, TRUE, fam, reps_big)
  }
  for (fam in c(FALSE, TRUE)) {
    results[[length(results) + 1]] <- run_cell("guesstest", C, 10000, 0, TRUE, TRUE, fam, reps_big)
  }
  save_progress()
}
log_msg("All done.")
stopCluster(cl)
final <- do.call(rbind, results)
write.csv(final, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/guesstest_results.csv", row.names = FALSE)
log_msg("Saved", nrow(final), "rows.")
