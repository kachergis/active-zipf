# Tests whether a developmental passive-to-active TRAJECTORY (mirroring how
# infants start with little control over their own visual scenes -- carried,
# immobile -- and gain more active control over time as motor/attentional
# capacities develop) captures more of pure active selection's benefit than a
# CONSTANT blend at the same time-averaged active_prob.
#
# Prediction (see conversation): the completion-checkpoint analysis already
# showed active selection's marginal value GROWS over the course of learning
# (the active/passive gap widens from 50% to 90% to 99% completion), which
# implies a fixed "active budget" is spent more efficiently late, where it's
# worth more, than early, where passive sampling is already nearly as good
# (common words get learned fast either way). A forward ramp (passive early,
# active late) should therefore beat a constant blend with the same average
# active_prob, and a REVERSE ramp (active early, passive late) -- which
# front-loads the same budget exactly where the checkpoint data says it's
# worth least -- should do considerably worse, as a directional sanity check.
#
# Ramps are indexed by EPISODE COUNT, not fraction of vocabulary learned:
# real motor/attentional development runs on its own maturational clock, not
# contingent on how much vocabulary a particular child happens to have
# learned so far. This means the ramp's timescale is a free parameter (how
# many episodes does "gaining mobility" take), swept below rather than
# guessed at once.

source("learners.R")
suppressMessages(library(dplyr))

M <- 1000; a <- 1; C <- 10; REPS <- 30

make_linear_ramp   <- function(Tt) { force(Tt); function(ep) pmin(1, ep / Tt) }
make_reverse_ramp  <- function(Tt) { force(Tt); function(ep) pmax(0, 1 - ep / Tt) }
# force(Tt) matters here: the returned closure doesn't touch Tt until it's
# actually CALLED (deep inside the simulation loop, long after any `for`
# loop that generated several of these has finished), and without forcing,
# R's lazy evaluation would resolve Tt against whatever the calling scope's
# variable holds AT THAT LATER TIME -- e.g. inside `for (Tt in T_NEW) ...`,
# every closure built this way would silently end up using T_NEW's LAST
# value instead of its own. Confirmed empirically: before this fix, all 4
# T_NEW conditions (2000/3000/8000/12000) converged on nearly identical
# results, all consistent with actually running at T=12000 (the last value
# T_NEW takes), not their own intended T.

# T values: the original 3 (1000, 5000, 20000) plus 4 more added to resolve
# the curve's shape, concentrated around 5000-20000 where the reverse ramp's
# transition (still near-passive vs. behaving like sustained high activity;
# see the code comment on that result below) turned out to happen. New T
# conditions are APPENDED, not interleaved, so the original 12 conditions
# keep their original list positions and therefore their original seeds
# (`1000 * which(names(conditions) == nm) + s`) -- the first 3 T values'
# numbers reproduce exactly, not just approximately, when this is re-run.
T_ORIG <- c(1000, 5000, 20000)
T_NEW <- c(2000, 3000, 8000, 12000)

conditions <- list(
  passive              = list(active_prob = 0),
  const_0.10           = list(active_prob = 0.10),
  const_0.25           = list(active_prob = 0.25),
  const_0.50           = list(active_prob = 0.50),
  const_0.75           = list(active_prob = 0.75),
  active                = list(active_prob = 1),
  ramp_fwd_T1000        = list(active_prob_fn = make_linear_ramp(1000)),
  ramp_fwd_T5000        = list(active_prob_fn = make_linear_ramp(5000)),
  ramp_fwd_T20000       = list(active_prob_fn = make_linear_ramp(20000)),
  ramp_rev_T1000        = list(active_prob_fn = make_reverse_ramp(1000)),
  ramp_rev_T5000        = list(active_prob_fn = make_reverse_ramp(5000)),
  ramp_rev_T20000       = list(active_prob_fn = make_reverse_ramp(20000))
)
for (Tt in T_NEW) {
  conditions[[sprintf("ramp_fwd_T%d", Tt)]] <- list(active_prob_fn = make_linear_ramp(Tt))
  conditions[[sprintf("ramp_rev_T%d", Tt)]] <- list(active_prob_fn = make_reverse_ramp(Tt))
}

cat(sprintf("Eliminative learner, M=%d, C=%d, a=%d, %d reps/condition\n\n", M, C, a, REPS))
results <- list()
for (nm in names(conditions)) {
  cc <- conditions[[nm]]
  t0 <- Sys.time()
  runs <- sapply(seq_len(REPS), function(s) {
    set.seed(1000 * which(names(conditions) == nm) + s)
    if (!is.null(cc$active_prob_fn)) {
      learn_corpus_eliminative(C = C, M = M, a = a, uniform = FALSE, active_prob_fn = cc$active_prob_fn)
    } else {
      learn_corpus_eliminative(C = C, M = M, a = a, uniform = FALSE, active_prob = cc$active_prob)
    }
  })
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  m <- rowMeans(runs)
  results[[nm]] <- m
  cat(sprintf("%-16s  50%%:%9.0f  90%%:%9.0f  99%%:%9.0f   (%.1fs)\n",
              nm, m[5], m[9], m[10], dt))
}

summ <- do.call(rbind, results)
colnames(summ) <- c(paste0("dec", 1:9), "episodes", "censored")
saveRDS(summ, "active_ramp_results.rds")
write.csv(summ, "active_ramp_results.csv")
cat("\nsaved active_ramp_results.rds/.csv\n")
