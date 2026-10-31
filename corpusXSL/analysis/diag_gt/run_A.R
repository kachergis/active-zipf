source("diagnose_guesstest.R"); suppressMessages(library(parallel))
jobs <- expand.grid(cond = c("active", "ap0.75"), rep = 1:6, stringsAsFactors = FALSE)
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  set.seed(900 + i)
  ap <- if (jobs$cond[i] == "active") 1 else 0.75
  r <- gt_diag(100, fam_context = TRUE, active_prob = ap, max_episodes = 1e6, trace_every = 20000)
  r$cond <- jobs$cond[i]; r$rep <- jobs$rep[i]; r
}, mc.cores = 12, mc.preschedule = FALSE)
saveRDS(res, "diag_gt/A_basic.rds")
for (r in res) { cat(sprintf("%-7s rep %d finished=%s\n", r$cond, r$rep, r$finished)); print(r$final, row.names = FALSE) }
