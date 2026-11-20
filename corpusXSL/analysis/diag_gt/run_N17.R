# Mechanism tests for the guess-test learner at 30 replications per condition
# (the earlier runs in run_A.R, run_AB.R and run_C.R used 3-6).
#   fam_*  C = 100, familiar context, fully active: baseline, rehearsing known
#          words on a quarter of episodes, and familiarity from known words only.
#   k100_* C = 100, random context, choice sets of 100: target chosen within the
#          set by frequency (baseline) or uniformly.
# Resumable: one .rds per run in diag_gt/N17/. Run from corpusXSL/analysis.
source("diagnose_guesstest.R"); suppressMessages(library(parallel))
dir.create("diag_gt/N17", showWarnings = FALSE)
conds <- c("fam_active", "fam_rehearse0.25", "fam_knownOnly", "k100_freq", "k100_uniform")
jobs <- expand.grid(cond = conds, rep = 1:30, stringsAsFactors = FALSE)
jobs <- jobs[order(jobs$cond != "k100_freq"), ]   # longest runs first
out <- mclapply(seq_len(nrow(jobs)), function(i) {
  cond <- jobs$cond[i]; rep <- jobs$rep[i]
  f <- sprintf("diag_gt/N17/%s_rep%02d.rds", cond, rep)
  if (file.exists(f)) return(TRUE)
  set.seed(170000 + 100 * match(cond, conds) + rep)
  r <- switch(cond,
    fam_active       = gt_diag(100, fam_context = TRUE, max_episodes = 1e6, trace_every = 1e6),
    fam_rehearse0.25 = gt_diag(100, fam_context = TRUE, max_episodes = 1e6, trace_every = 1e6, rehearse_prob = 0.25),
    fam_knownOnly    = gt_diag(100, fam_context = TRUE, max_episodes = 1e6, trace_every = 1e6, fam_known_only = TRUE),
    k100_freq        = gt_diag(100, choice_k = 100, max_episodes = 5e6, trace_every = 5e6),
    k100_uniform     = gt_diag(100, choice_k = 100, within_pool_uniform = TRUE, max_episodes = 5e6, trace_every = 5e6))
  saveRDS(list(cond = cond, rep = rep, finished = r$finished, final = r$final, unknown_words = r$unknown_words), f)
  TRUE
}, mc.cores = 5, mc.preschedule = FALSE)
