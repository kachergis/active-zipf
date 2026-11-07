# Uniform choice among ALL unknown words (choice set = whole vocabulary, chosen uniformly)
source("diagnose_guesstest.R"); suppressMessages(library(parallel))
jobs <- expand.grid(C = c(10, 100), rep = 1:6)
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  set.seed(1700 + i)
  r <- gt_diag(jobs$C[i], active_prob = 1, choice_k = 1000, within_pool_uniform = TRUE, max_episodes = 3e6)
  r$cond <- sprintf("C%d uniform among unknown", jobs$C[i]); r$rep <- jobs$rep[i]; r
}, mc.cores = 12, mc.preschedule = FALSE)
saveRDS(res, "diag_gt/D_uniform_unknown.rds")
s <- do.call(rbind, lapply(res, function(r) data.frame(cond = r$cond, rep = r$rep, finished = r$finished, episodes = r$final$episodes)))
print(aggregate(episodes ~ cond, s, mean)); print(s, row.names = FALSE)
