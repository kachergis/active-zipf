# Two small simulations needed by the manuscript's Discussion and checkpoint table:
#
# (1) Mutual exclusivity under a UNIFORM distribution (eliminative learner,
#     fast C++ learner), at C=10 and C=100, passive and active, so Appendix E /
#     the Hendrickson & Perfors paragraph can compare the Zipfian penalty with
#     and without mutual exclusivity against a matched uniform baseline.
# (2) Guess-test at C=100, a=1, random context, passive and active, keeping the
#     50%/90%/99% checkpoints (the earlier grid run, run_guesstest_grid.R, did
#     not save per-checkpoint data at C=100).
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})
M <- 1000; REPS <- 30

## (1) uniform + mutual exclusivity, and uniform independent for the same seeds' setting
u <- expand.grid(rep = seq_len(REPS), active = c(FALSE, TRUE), C = c(10, 100), me = c(FALSE, TRUE))
u$seed <- 1100000 + seq_len(nrow(u))
ur <- mclapply(seq_len(nrow(u)), function(j) {
  jb <- u[j, ]; set.seed(jb$seed)
  r <- elim_fast(jb$C, M, rep(1 / M, M), jb$active, FALSE, max_episodes = 5e8, mutual_exclusivity = jb$me)
  data.frame(C = jb$C, active = ifelse(jb$active, "Active", "Passive"), me = jb$me, rep = jb$rep,
             episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2)
ud <- bind_rows(ur)
us <- ud %>% group_by(C, active, me) %>%
  summarise(n = n(), n_censored = sum(censored), mean99 = mean(episodes), se99 = sd(episodes) / sqrt(n()), .groups = "drop")
cat("=== Uniform distribution, eliminative learner, with vs without mutual exclusivity ===\n")
print(as.data.frame(us), row.names = FALSE, digits = 6)
write.csv(us, "uniform_mutex_summary.csv", row.names = FALSE)

## (2) guess-test C=100 checkpoints
g <- expand.grid(rep = seq_len(REPS), active = c(FALSE, TRUE))
g$seed <- 1200000 + seq_len(nrow(g))
gr <- mclapply(seq_len(nrow(g)), function(j) {
  jb <- g[j, ]; set.seed(jb$seed)
  r <- learn_corpus_guesstest(C = 100, M = M, a = 1, uniform = FALSE, active = jb$active)
  data.frame(active = ifelse(jb$active, "Active", "Passive"), rep = jb$rep,
             dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2)
gd <- bind_rows(gr)
saveRDS(gd, "guesstest_c100_checkpoints.rds")
gs <- gd %>% group_by(active) %>%
  summarise(n = n(), n_censored = sum(censored), mean50 = mean(dec5), mean90 = mean(dec9), mean99 = mean(episodes),
            med50 = median(dec5), med90 = median(dec9), med99 = median(episodes), .groups = "drop")
cat("\n=== Guess-test, C=100, a=1, random context ===\n")
print(as.data.frame(gs), row.names = FALSE, digits = 6)
P <- gs[gs$active == "Passive", ]; A <- gs[gs$active == "Active", ]
cat(sprintf("speedup (means) 50%%: %.2f  90%%: %.2f  99%%: %.2f | (medians) %.2f %.2f %.2f\n",
            P$mean50 / A$mean50, P$mean90 / A$mean90, P$mean99 / A$mean99,
            P$med50 / A$med50, P$med90 / A$med90, P$med99 / A$med99))
write.csv(gs, "guesstest_c100_checkpoints_summary.csv", row.names = FALSE)
