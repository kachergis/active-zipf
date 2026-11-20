# Tests the conjectured mechanism behind Study 3's C=100 familiar-context
# results (paper2/draft.tex): frequency-weighted choice sets are very costly
# and Goldilocks targeting is very beneficial there, but not elsewhere.
#
# Conjecture: familiar-context selection weights a referent r by
# 1/(colSums(hyp)[r] + 1), the number of words still holding r as a candidate.
# A word that has never been targeted has an all-ones row, so every unseen
# word adds the same +1 to every referent's count. While many words are
# unseen, this shared count dilutes the familiarity signal and familiar-context
# selection behaves almost like random selection. Policies that spread targets
# across the vocabulary (uniform choice sets, Goldilocks) reduce the number of
# unseen words quickly; policies that concentrate targets on frequent words
# (frequency-weighted choice sets) leave many words unseen for long.
#
# Test 1 (intervention): compute familiarity from SEEN words only
# (fam_seen_only; weight 1/(colcount - n_unseen + 1)), removing the dilution.
#   Prediction: the frequency-weighted choice-set cost and the Goldilocks
#   benefit (relative to unrestricted active selection) largely disappear.
# Test 2 (trajectories): under the original weighting, track n_unseen and the
#   competitor-survival rate (share of the target's remaining competitors that
#   reappear as distractors, i.e. escape elimination) over time.
#   Prediction: slow policies keep n_unseen high and survival high for longer.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr); library(ggplot2); library(tidyr)})
M <- 1000; a <- 1; C <- 100; REPS <- 30

conds <- data.frame(
  label = c("Passive", "Unrestricted active", "Goldilocks", "Choice set 100 (freq.)",
            "Choice set 20 (freq.)", "Choice set 100 (uniform)"),
  ap = c(0, 1, 1, 1, 1, 1), policy = c(0, 0, 1, 0, 0, 0),
  k = c(0, 0, 0, 100, 20, 100), pu = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))

## ---- Test 1: intervention ----
jobs <- merge(merge(conds, data.frame(seen_only = c(FALSE, TRUE))), data.frame(rep = seq_len(REPS)))
jobs$seed <- 950000 + seq_len(nrow(jobs))
res <- mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]; set.seed(jb$seed)
  r <- elim_fast(C, M, zipf_probs(M, a), FALSE, TRUE, max_episodes = 5e8, active_prob = jb$ap,
                 policy = jb$policy, choice_k = jb$k, pool_uniform = jb$pu, fam_seen_only = jb$seen_only)
  data.frame(label = jb$label, seen_only = jb$seen_only, rep = jb$rep, dec5 = r[5], episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
d1 <- bind_rows(res)
t1 <- d1 %>% group_by(seen_only, label) %>%
  summarise(n_censored = sum(censored), mean50 = mean(dec5), mean99 = mean(episodes),
            se99 = sd(episodes) / sqrt(n()), .groups = "drop") %>%
  group_by(seen_only) %>%
  mutate(ratio_to_unrestricted = mean99 / mean99[label == "Unrestricted active"]) %>% ungroup() %>%
  mutate(weighting = ifelse(seen_only, "seen words only", "original (all words)"))
cat("=== Test 1: familiarity from all words vs seen words only (C=100, a=1, familiar context) ===\n")
print(as.data.frame(t1 %>% select(weighting, label, mean50, mean99, se99, ratio_to_unrestricted, n_censored)),
      row.names = FALSE, digits = 4)
write.csv(t1, "familiar_dilution_intervention.csv", row.names = FALSE)

## ---- Test 2: trajectories under the original weighting ----
TR_REPS <- 10; EVERY <- 500
tconds <- conds %>% filter(label != "Passive", label != "Choice set 20 (freq.)")
tj <- merge(tconds, data.frame(rep = seq_len(TR_REPS)))
tj$seed <- 960000 + seq_len(nrow(tj))
tr <- mclapply(seq_len(nrow(tj)), function(j) {
  jb <- tj[j, ]; set.seed(jb$seed)
  out <- elim_trace(C, M, zipf_probs(M, a), TRUE, EVERY, max_episodes = 5e8, active_prob = jb$ap,
                    policy = jb$policy, choice_k = jb$k, pool_uniform = jb$pu)
  cbind(label = jb$label, rep = jb$rep, out$trace)
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
d2 <- bind_rows(tr)
t2 <- d2 %>% group_by(label, episode) %>%
  summarise(n_unseen = mean(n_unseen), n_learned = mean(n_learned),
            competitor_survival = mean(competitor_survival, na.rm = TRUE), n_runs = n(), .groups = "drop") %>%
  filter(n_runs >= TR_REPS / 2)  # drop the tail where most runs have finished
write.csv(t2, "familiar_dilution_trajectories.csv", row.names = FALSE)
cat("\n=== Test 2: trajectories at selected episodes (means over runs) ===\n")
print(as.data.frame(t2 %>% filter(episode %in% c(2000, 5000, 10000, 15000, 30000, 60000))),
      row.names = FALSE, digits = 3)

# competitor survival is shown only until half the vocabulary is learned: late
# in a run the few remaining targets are the hardest words and few runs are
# still going, so the rate becomes noisy and no longer describes the phase the
# dilution conjecture is about
long <- t2 %>% mutate(competitor_survival = ifelse(n_learned > M / 2, NA, competitor_survival)) %>%
  select(label, episode, n_unseen, competitor_survival, n_learned) %>%
  pivot_longer(c(n_unseen, competitor_survival, n_learned), names_to = "measure") %>%
  mutate(measure = factor(measure, levels = c("n_unseen", "competitor_survival", "n_learned"),
                          labels = c("Words never yet targeted", "Competitor survival rate", "Words learned")))
p <- ggplot(long, aes(x = episode, y = value, color = label)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~measure, scales = "free_y", nrow = 1) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c("Unrestricted active" = "#377eb8", "Goldilocks" = "#4daf4a",
                                "Choice set 100 (freq.)" = "#e41a1c", "Choice set 100 (uniform)" = "#984ea3")) +
  labs(x = "Episode (log scale)", y = NULL, color = NULL) +
  theme_bw(base_size = 11) + theme(legend.position = "top", strip.background = element_rect(fill = "grey90"))
ggsave("../../paper2/familiar_dilution.pdf", p, width = 10, height = 3.8)
cat("\nFigure saved: paper2/familiar_dilution.pdf\n")
