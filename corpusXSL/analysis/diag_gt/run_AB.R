source("diagnose_guesstest.R"); suppressMessages(library(parallel))
jobsA <- expand.grid(cond = c("active+rehearse0.25", "active+famKnownOnly"), rep = 1:6, stringsAsFactors = FALSE)
jobsB <- expand.grid(k = c(Inf, 100, 20, 5), rep = 1:3)
jobs <- c(lapply(seq_len(nrow(jobsA)), function(i) list(type = "A", cond = jobsA$cond[i], rep = jobsA$rep[i])),
          lapply(seq_len(nrow(jobsB)), function(i) list(type = "B", k = jobsB$k[i], rep = jobsB$rep[i])))
res <- mclapply(seq_along(jobs), function(i) {
  j <- jobs[[i]]; set.seed(950 + i)
  if (j$type == "A") {
    r <- gt_diag(100, fam_context = TRUE, active_prob = 1, max_episodes = 1e6, trace_every = 20000,
                 rehearse_prob = if (j$cond == "active+rehearse0.25") 0.25 else 0,
                 fam_known_only = j$cond == "active+famKnownOnly")
    r$cond <- j$cond
  } else {
    r <- gt_diag(100, fam_context = FALSE, active_prob = 1, choice_k = j$k, max_episodes = 3e6, trace_every = 50000)
    r$cond <- paste0("k=", j$k)
  }
  r$type <- j$type; r$rep <- j$rep; r
}, mc.cores = 12, mc.preschedule = FALSE)
saveRDS(res, "diag_gt/AB_interventions.rds")
for (r in res) { cat(sprintf("\n%s %-20s rep %d finished=%s\n", r$type, r$cond, r$rep, r$finished)); print(r$final, row.names = FALSE) }
