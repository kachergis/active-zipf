# Why does the active advantage under familiar context at C=100 shrink with
# vocabulary size (28x at M=1,000; 7.6x at 10,000; 9.3x at 60,000; see
# run_m10k.R / run_m60k.R), while at C=10 with random context it grows
# (19x, 24x, 28x)?
#
# Records, for every learned word, how many times it had been the target
# (its exposures) when it was learned, and when. Compares passive and active
# target selection by word-frequency rank, at M = 1,000 / 10,000 / 60,000.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

REPS <- 10
settings <- data.frame(C = c(100, 10), fam = c(TRUE, FALSE),
                       label = c("C=100, familiar", "C=10, random"))
jobs <- merge(merge(settings, data.frame(M = c(1000, 10000, 60000))),
              merge(data.frame(ap = c(0, 1)), data.frame(rep = seq_len(REPS))))
jobs$seed <- 1500000 + seq_len(nrow(jobs))
jobs <- jobs[order(-jobs$M, jobs$ap), ]

res <- mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]; set.seed(jb$seed)
  probs <- zipf_probs(jb$M, 1)
  out <- elim_trace(jb$C, jb$M, probs, jb$fam, 1e9, max_episodes = 5e8, active_prob = jb$ap)
  L <- out$learned
  L$rank <- rank(-probs, ties.method = "first")[L$word]
  L$p <- probs[L$word]
  cbind(setting = jb$label, M = jb$M, policy = ifelse(jb$ap == 1, "Active", "Passive"), rep = jb$rep,
        total = out$result[10], L)
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
d <- bind_rows(res)
saveRDS(d, "familiar_scaling_learned.rds")

# frequency bands: top 1%, 1-10%, 10-50%, bottom 50% of ranks
d$band <- cut(d$rank / d$M, c(0, 0.01, 0.1, 0.5, 1), labels = c("top 1%", "1-10%", "10-50%", "bottom 50%"))
by_band <- d %>% group_by(setting, M, policy, band) %>%
  summarise(median_exposures = median(exposures), mean_exposures = mean(exposures),
            median_episode_learned = median(episode), .groups = "drop")
cat("=== Exposures needed to learn a word, by frequency band ===\n")
print(as.data.frame(by_band), row.names = FALSE, digits = 4)
write.csv(by_band, "familiar_scaling_by_band.csv", row.names = FALSE)

# who finishes last: the words learned after 90% of the vocabulary
last <- d %>% group_by(setting, M, policy, rep) %>% arrange(episode, .by_group = TRUE) %>%
  mutate(order = row_number()) %>% filter(order > 0.9 * M) %>% ungroup() %>%
  group_by(setting, M, policy) %>%
  summarise(median_rank_pct = median(rank / M), median_exposures = median(exposures),
            share_top10pct = mean(rank / M <= 0.1), .groups = "drop")
cat("\n=== The last 10% of words learned (to 99%): where they sit in the frequency ranking ===\n")
print(as.data.frame(last), row.names = FALSE, digits = 3)
write.csv(last, "familiar_scaling_last_words.csv", row.names = FALSE)

tot <- d %>% distinct(setting, M, policy, rep, total) %>% group_by(setting, M, policy) %>%
  summarise(mean_total = mean(total), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = policy, values_from = mean_total) %>%
  mutate(speedup = Passive / Active)
cat("\n=== Totals (episodes to 99%) ===\n")
print(as.data.frame(tot), row.names = FALSE, digits = 4)
