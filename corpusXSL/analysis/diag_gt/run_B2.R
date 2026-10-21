source("diagnose_guesstest.R"); suppressMessages(library(parallel))
jobs <- expand.grid(k = c(Inf, 100, 50, 20, 5), rep = 1:3)
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  set.seed(1200 + i)
  r <- gt_diag(100, fam_context = FALSE, active_prob = 1, choice_k = jobs$k[i], max_episodes = 3e6, trace_every = 50000)
  r$cond <- paste0("k=", jobs$k[i]); r$rep <- jobs$rep[i]; r
}, mc.cores = 12, mc.preschedule = FALSE)
saveRDS(res, "diag_gt/B2_choice_sets.rds")
s <- do.call(rbind, lapply(res, function(r) cbind(cond = r$cond, rep = r$rep, finished = r$finished,
  r$final[, c("episodes", "n_learned", "wasted", "unknown_targets", "blocked_targets", "n_disconfirm")])))
s$blocked_share <- round(s$blocked_targets / s$unknown_targets, 3); s$wasted_share <- round(s$wasted / s$episodes, 3)
print(s[order(s$cond), ], row.names = FALSE)
