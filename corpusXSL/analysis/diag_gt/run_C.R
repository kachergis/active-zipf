source("diagnose_guesstest.R"); suppressMessages(library(parallel))
specs <- list(
  list(lab = "C100 k=100 uniform-within", C = 100, k = 100, wpu = TRUE,  pol = "unknown"),
  list(lab = "C100 k=20 uniform-within",  C = 100, k = 20,  wpu = TRUE,  pol = "unknown"),
  list(lab = "C100 unrestricted",         C = 100, k = Inf, wpu = FALSE, pol = "unknown"),
  list(lab = "C100 goldilocks",           C = 100, k = Inf, wpu = FALSE, pol = "goldilocks"),
  list(lab = "C10 unrestricted",          C = 10,  k = Inf, wpu = FALSE, pol = "unknown"),
  list(lab = "C10 goldilocks",            C = 10,  k = Inf, wpu = FALSE, pol = "goldilocks"))
jobs <- expand.grid(s = seq_along(specs), rep = 1:3)
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  s <- specs[[jobs$s[i]]]; set.seed(1500 + i)
  r <- gt_diag(s$C, fam_context = FALSE, active_prob = 1, choice_k = s$k, within_pool_uniform = s$wpu,
               active_policy = s$pol, max_episodes = 3e6, trace_every = 50000)
  r$cond <- s$lab; r$rep <- jobs$rep[i]; r
}, mc.cores = 12, mc.preschedule = FALSE)
saveRDS(res, "diag_gt/C_interventions.rds")
s <- do.call(rbind, lapply(res, function(r) cbind(cond = r$cond, rep = r$rep, finished = r$finished,
  r$final[, c("episodes", "unknown_targets", "blocked_targets", "repeat_targets", "n_disconfirm")])))
s$repeat_share <- round(s$repeat_targets / s$unknown_targets, 3)
print(s[order(s$cond), ], row.names = FALSE)
