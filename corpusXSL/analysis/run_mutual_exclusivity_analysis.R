# Separate analysis (not part of the main paper's Table 1 / Figure 6 pipeline):
# tests whether a Reisenauer-style cross-word mutual exclusivity constraint
# (Reisenauer, Smith, Smith, & Blythe, 2013, PRL) rescues the eliminative
# learner's catastrophic C=100 slowdown, as flagged as a natural extension in
# the paper's Discussion ("Fifth" simplifying assumption).
#
# learn_corpus_eliminative(..., mutual_exclusivity=TRUE) in learners.R: the
# moment a word is learned, its referent is immediately excluded as a
# candidate from every OTHER still-unlearned word's row too (not just its
# own), which can cascade into an avalanche of further learning events within
# the same episode. mutual_exclusivity=FALSE (the default, used everywhere
# else in this project) exactly reproduces the original, independent model.
#
# We only run the mutual_exclusivity=TRUE cells here -- the mutual_exclusivity
# =FALSE baseline for the same M=1000, a=1, Random-context conditions is
# already the paper's own Table 1 (89,454 / 4,695 at C=10; 11,751,913 /
# 713,244 at C=100), so re-simulating it would be redundant (and, at C=100
# passive, was already established as impractically slow to brute-force).

source("learners.R")
suppressMessages(library(dplyr))

M <- 1000; a <- 1; REPS <- 30
REP_MAX_SECONDS <- 600  # safety cap; mutual exclusivity is expected to be fast,
                         # but this guards against an unexpectedly slow replication
NCORES <- max(1, parallel::detectCores() - 1)

baseline <- data.frame(
  C = c(10, 10, 100, 100),
  active = c(FALSE, TRUE, FALSE, TRUE),
  mean_episodes_no_mutex = c(89454, 4695, 11751913, 713244)
)

cl <- parallel::makeCluster(NCORES)
doParallel::registerDoParallel(cl)

configs <- expand.grid(C = c(10, 100), active = c(FALSE, TRUE))
results <- list()
for (i in seq_len(nrow(configs))) {
  C <- configs$C[i]; active <- configs$active[i]
  cat(sprintf("[%s] C=%d, active=%s, mutual_exclusivity=TRUE, %d reps...\n",
              format(Sys.time(), "%H:%M:%S"), C, active, REPS))
  t0 <- Sys.time()
  batch <- foreach::foreach(rep = 1:REPS, .combine = rbind,
                             .export = c("zipf_probs", "TIME_CHECK_EVERY", "choose_target",
                                         "learn_corpus_eliminative")) %dopar% {
    set.seed(10000 * i + rep)
    r <- learn_corpus_eliminative(C = C, M = M, a = a, uniform = FALSE, active = active,
                                   fam_context = FALSE, max_seconds = REP_MAX_SECONDS,
                                   mutual_exclusivity = TRUE)
    data.frame(dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
  }
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  batch$C <- C; batch$active <- active
  n_censored <- sum(batch$censored)
  cat(sprintf("  done in %.0fs (%.1fs/rep) -- %d/%d censored\n", dt, dt / REPS, n_censored, REPS))
  results[[i]] <- batch
}
parallel::stopCluster(cl)

d <- bind_rows(results)
saveRDS(d, "mutual_exclusivity_results.rds")

summ <- d %>% filter(censored == 0) %>%
  group_by(C, active) %>%
  summarise(n = n(), mean_episodes_mutex = mean(episodes), median_episodes_mutex = median(episodes), .groups = "drop")

out <- baseline %>% left_join(summ, by = c("C", "active")) %>%
  mutate(speedup_from_mutex = mean_episodes_no_mutex / mean_episodes_mutex)

cat("\n=== Mutual exclusivity vs. independent elimination (M=1000, a=1, Random context) ===\n")
print(as.data.frame(out %>% select(C, active, mean_episodes_no_mutex, mean_episodes_mutex, median_episodes_mutex, n, speedup_from_mutex)),
      row.names = FALSE, digits = 5)

write.csv(out, "mutual_exclusivity_summary.csv", row.names = FALSE)
cat("\nSaved: mutual_exclusivity_results.rds, mutual_exclusivity_summary.csv\n")
