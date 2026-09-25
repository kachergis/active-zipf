# Re-runs the C=100 mutual-exclusivity cells of Appendix E (Table tab-mutex)
# with the exact C++ port (learners_fast.cpp, validated against the R learner
# with and without mutual exclusivity in validate_elim_fast.R). The original R
# runs (run_mutual_exclusivity_analysis.R) used a 600-second per-replication
# cap, which censored 1 passive and 2 active replications and biased those
# means downward; here the cap is effectively never binding.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

M <- 1000; a <- 1; C <- 100
REPS <- 200  # heavy-tailed waits (one cascade does nearly all learning); 30 reps left SEs ~15% of the mean
MAX_EPISODES <- 5e8
cells <- expand.grid(rep = seq_len(REPS), active = c(FALSE, TRUE))
cells$seed <- 400000 + seq_len(nrow(cells))

t0 <- Sys.time()
res <- mclapply(seq_len(nrow(cells)), function(j) {
  cl <- cells[j, ]
  set.seed(cl$seed)
  r <- elim_fast(C, M, zipf_probs(M, a), cl$active, FALSE, max_episodes = MAX_EPISODES,
                 mutual_exclusivity = TRUE)
  data.frame(active = ifelse(cl$active, "Active", "Passive"), rep = cl$rep,
             dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = 6, mc.preschedule = FALSE)
cat(sprintf("done in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

d <- bind_rows(res)
saveRDS(d, "c100_mutex_fast_results.rds")
summ <- d %>% group_by(active) %>%
  summarise(n = n(), n_censored = sum(censored),
            mean50 = mean(dec5), mean90 = mean(dec9), mean99 = mean(episodes[censored == 0]),
            median99 = median(episodes[censored == 0]), se99 = sd(episodes[censored == 0]) / sqrt(sum(censored == 0)),
            .groups = "drop")
print(as.data.frame(summ), row.names = FALSE, digits = 6)
write.csv(summ, "c100_mutex_fast_summary.csv", row.names = FALSE)
cat("Saved: c100_mutex_fast_results.rds, c100_mutex_fast_summary.csv\n")
