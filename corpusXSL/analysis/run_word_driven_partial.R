# Word-driven pursuit under partial autonomy (companion paper, Study 2).
#
# Fully word-driven learners fail in two C=100 settings (run_word_driven.R):
# the learner keeps pursuing the few words it has heard and, with no passive
# episodes, hears no new ones. Here each episode is word-driven with
# probability active_prob and passive otherwise, so new words keep arriving.
# Object-driven pursuit at the same active_prob is run alongside, from the
# same seeds, for comparison.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})
W <- 1000
NC <- detectCores() - 2
aps <- c(0.25, 0.5, 0.75)

## ---- eliminative learner (C++), C=100, a=1, familiar context ----
jobs <- merge(data.frame(ap = aps), merge(data.frame(policy = c("object-driven", "word-driven"), code = c(0L, 3L)),
                                          data.frame(rep = 1:30)))
jobs$seed <- 2100000 + jobs$rep  # same seed across policies and levels
el <- bind_rows(mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]; set.seed(jb$seed)
  r <- elim_fast(100, W, zipf_probs(W, 1), FALSE, TRUE, max_episodes = 5e8,
                 active_prob = jb$ap, policy = jb$code)
  data.frame(learner = "eliminative", setting = "C=100, a=1, familiar", policy = jb$policy, ap = jb$ap,
             rep = jb$rep, dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = NC, mc.preschedule = FALSE))

## ---- guess-test learner (R), word-driven, C=100, a=1; object-driven partial
## autonomy values come from the Study 2 guess-test grid ----
gjobs <- merge(data.frame(ap = aps), merge(data.frame(setting = c("C=100, a=1, random", "C=100, a=1, familiar"),
                                                      fam = c(FALSE, TRUE)), data.frame(rep = 1:30)))
gjobs$seed <- 2200000 + seq_len(nrow(gjobs))
gt <- bind_rows(mclapply(seq_len(nrow(gjobs)), function(j) {
  jb <- gjobs[j, ]; set.seed(jb$seed)
  r <- learn_corpus_guesstest(C = 100, M = W, a = 1, uniform = FALSE, active_prob = jb$ap,
                              active_policy = "heard", fam_context = jb$fam, max_episodes = 5e6)
  data.frame(learner = "guess-test", setting = jb$setting, policy = "word-driven", ap = jb$ap,
             rep = jb$rep, dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = NC, mc.preschedule = FALSE))

d <- bind_rows(el, gt)
saveRDS(d, "word_driven_partial_results.rds")
summ <- d %>% group_by(learner, setting, policy, ap) %>%
  summarise(n = n(), n_censored = sum(censored), mean99 = mean(episodes),
            median99 = median(episodes), se99 = sd(episodes) / sqrt(n()), .groups = "drop")
print(as.data.frame(summ), row.names = FALSE, digits = 6)
write.csv(summ, "word_driven_partial_summary.csv", row.names = FALSE)
