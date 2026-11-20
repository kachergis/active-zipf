# Follow-up to diagnose_familiar_scaling.R. Under familiar context at C=100,
# the passive learner's last 1% of words need ~34 exposures at M=1,000 but only
# ~5-6 at M=10,000/60,000, which inflates the active/passive speedup at
# M=1,000. Hypothesis: the hard tail is set by C/M -- with C-1 distractors drawn
# from only M referents, a surviving competitor has a high chance (~C/M, up to
# familiarity weighting) of reappearing in each context. Test: vary C at
# M=1,000 and at M=10,000; if the tail tracks C/M, it should reappear at
# M=10,000 when C/M=0.1 (C=1,000) and vanish at M=1,000 when C/M=0.01 (C=10).
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

REPS <- 5
jobs <- rbind(expand.grid(M = 1000, C = c(10, 30, 100)),
              expand.grid(M = 10000, C = c(100, 300, 1000)))
jobs <- merge(jobs, merge(data.frame(ap = c(0, 1)), data.frame(rep = seq_len(REPS))))
jobs$seed <- 1600000 + seq_len(nrow(jobs))
jobs <- jobs[order(-jobs$C, jobs$ap), ]
CAP <- 5e7

res <- mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]; set.seed(jb$seed)
  probs <- zipf_probs(jb$M, 1)
  out <- elim_trace(jb$C, jb$M, probs, TRUE, 1e9, max_episodes = CAP, active_prob = jb$ap)
  L <- out$learned
  L$rank <- rank(-probs, ties.method = "first")[L$word]
  cbind(M = jb$M, C = jb$C, policy = ifelse(jb$ap == 1, "Active", "Passive"), rep = jb$rep,
        total = out$result[10], censored = out$result[11], L)
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
d <- bind_rows(res)
saveRDS(d, "familiar_tail_learned.rds")

tails <- d %>% filter(policy == "Passive") %>% group_by(M, C, rep) %>% arrange(episode, .by_group = TRUE) %>%
  mutate(o = row_number()) %>% filter(o > 0.98 * M) %>% ungroup() %>%
  group_by(M, C) %>% summarise(CM = first(C / M), last1pct_median_exposures = median(exposures),
                               last1pct_mean_exposures = mean(exposures), .groups = "drop")
rare <- d %>% filter(rank / M > 0.5) %>% group_by(M, C) %>%
  summarise(rare_median = median(exposures), rare_q99 = quantile(exposures, .99), .groups = "drop")
tot <- d %>% distinct(M, C, policy, rep, total, censored) %>% group_by(M, C, policy) %>%
  summarise(mean_total = mean(total), n_censored = sum(censored), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = policy, values_from = c(mean_total, n_censored)) %>%
  mutate(speedup = mean_total_Passive / mean_total_Active)
out <- tails %>% left_join(rare, by = c("M", "C")) %>% left_join(tot, by = c("M", "C")) %>% arrange(CM, M)
print(as.data.frame(out), row.names = FALSE, digits = 4)
write.csv(out, "familiar_tail_by_CM.csv", row.names = FALSE)
