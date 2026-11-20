# Re-runs only the 8 conditions affected by the lazy-evaluation closure bug
# (T_NEW = {2000,3000,8000,12000}, forward and reverse) with the fixed
# make_linear_ramp/make_reverse_ramp, and merges into the existing
# active_ramp_results.rds -- the original 12 conditions were unaffected
# (built from literal T values, not a loop variable) and are left as-is.
source("learners.R")
M <- 1000; a <- 1; C <- 10; REPS <- 30

make_linear_ramp   <- function(Tt) { force(Tt); function(ep) pmin(1, ep / Tt) }
make_reverse_ramp  <- function(Tt) { force(Tt); function(ep) pmax(0, 1 - ep / Tt) }

T_NEW <- c(2000, 3000, 8000, 12000)
conditions <- list()
for (Tt in T_NEW) {
  conditions[[sprintf("ramp_fwd_T%d", Tt)]] <- list(active_prob_fn = make_linear_ramp(Tt))
  conditions[[sprintf("ramp_rev_T%d", Tt)]] <- list(active_prob_fn = make_reverse_ramp(Tt))
}

old <- readRDS("active_ramp_results.rds")
results <- list()
# offset seeds well past anything the main script used, so this is an
# independent draw, not a silent reuse of the (buggy) prior run's seeds
for (i in seq_along(conditions)) {
  nm <- names(conditions)[i]
  cc <- conditions[[nm]]
  t0 <- Sys.time()
  runs <- sapply(seq_len(REPS), function(s) {
    set.seed(50000 * i + s)
    learn_corpus_eliminative(C = C, M = M, a = a, uniform = FALSE, active_prob_fn = cc$active_prob_fn)
  })
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  m <- rowMeans(runs)
  results[[nm]] <- m
  cat(sprintf("%-16s  50%%:%9.0f  90%%:%9.0f  99%%:%9.0f   (%.1fs)\n", nm, m[5], m[9], m[10], dt))
}
new_rows <- do.call(rbind, results)
colnames(new_rows) <- colnames(old)

summ <- old
summ[rownames(new_rows), ] <- new_rows  # overwrite the buggy 8 rows
saveRDS(summ, "active_ramp_results.rds")
write.csv(summ, "active_ramp_results.csv")
cat("\nmerged and saved active_ramp_results.rds/.csv\n")
