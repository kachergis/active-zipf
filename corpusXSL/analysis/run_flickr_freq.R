# Flickr8k, eliminative learner, with frequency-weighted active targeting
# (target_policy = "freq") instead of rarest-first, so that its active policy
# matches the synthetic learners and the guess-test caption learner
# (flickr_guesstest.R). Passive is rerun alongside for within-set speedups.
# 100 replications per cell; results in flickr_freq/.
suppressMessages({library(dplyr); library(parallel)})
src <- readLines("run_flickr_incremental.R")
eval(parse(text = src[seq_len(grep("^REPS <- 30", src) - 1)]))
REPS <- 100
dir.create("flickr_freq", showWarnings = FALSE)
jobs <- rbind(data.frame(policy = "passive", mc = 0),
              expand.grid(policy = c("active_moveon", "active_linger"), mc = c(0, 1, 2, 3, 5, 10)))
jobs <- merge(jobs, data.frame(rep = seq_len(REPS)))
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  j <- jobs[i, ]
  f <- sprintf("flickr_freq/%s_mc%d_rep%03d.rds", j$policy, j$mc, j$rep)
  if (file.exists(f)) return(readRDS(f))
  v <- run_learner(as.character(j$policy), move_cost = j$mc, seed = 880000 + j$rep, target_policy = "freq")
  out <- data.frame(policy = j$policy, move_cost = j$mc, rep = j$rep, c50 = v[["0.5"]], c90 = v[["0.9"]],
                    c99 = v[["0.99"]], final_frac = v[["final_frac"]])
  saveRDS(out, f); out
}, mc.cores = max(1, detectCores() - 2), mc.preschedule = FALSE)
d <- bind_rows(res)
s <- d %>% group_by(policy, move_cost) %>%
  summarise(n = n(), mean50 = mean(c50), mean90 = mean(c90), mean99 = mean(c99), min_final = min(final_frac), .groups = "drop")
print(as.data.frame(s), digits = 6)
# linger vs. move-on at 99%, percentile bootstrap over runs
set.seed(1)
for (mc in c(0, 1, 2, 3, 5, 10)) {
  a <- d$c99[d$policy == "active_linger" & d$move_cost == mc]; b <- d$c99[d$policy == "active_moveon" & d$move_cost == mc]
  bs <- replicate(5000, mean(sample(a, replace = TRUE)) / mean(sample(b, replace = TRUE)))
  cat(sprintf("move_cost %2d: linger/moveon at 99%% = %.3f (95%% CI %.3f-%.3f)\n", mc, mean(a) / mean(b), quantile(bs, .025), quantile(bs, .975)))
}
write.csv(s, "flickr_freq/summary.csv", row.names = FALSE)
