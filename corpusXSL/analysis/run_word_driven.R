# Word-driven vs object-driven active pursuit (companion paper, Study 2).
#
# Object-driven (the default active policy, "unknown"): the learner brings into
# view an object it cannot yet name and hears it named. It can reach words it
# has never heard. Word-driven ("heard"): the learner pursues a word it has
# heard at least once but not yet learned (e.g. by asking about it). A word
# never heard cannot be pursued, so every word's first encounter still comes
# from passive input; with no heard-but-unknown word, the episode is passive.
#
# Prediction: word-driven pursuit removes the wait for repeat exposures but
# not for the first one, so for the eliminative learner its time to 99%
# should approach the time a passive learner needs merely to HEAR 99% of the
# words once (computed below by sampling targets alone), and its advantage
# over passive should be far smaller than object-driven pursuit's.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})
W <- 1000
NC <- detectCores() - 2

## ---- eliminative learner (C++), Study 2's four settings ----
settings <- data.frame(setting = c("C=10, a=1, random", "C=10, a=1, familiar", "C=100, a=0.75, random", "C=100, a=1, familiar"),
                       C = c(10, 10, 100, 100), a = c(1, 1, 0.75, 1), fam = c(FALSE, TRUE, FALSE, TRUE),
                       reps = c(200, 200, 30, 30))
policies <- data.frame(policy = c("passive", "object-driven", "word-driven"), ap = c(0, 1, 1), code = c(0L, 0L, 3L))
jobs <- do.call(rbind, lapply(seq_len(nrow(settings)), function(i)
  merge(merge(settings[i, ], policies), data.frame(rep = seq_len(settings$reps[i])))))
jobs$seed <- 1700000 + seq_len(nrow(jobs))
jobs <- jobs[order(-jobs$C, jobs$ap), ]
el <- bind_rows(mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]; set.seed(jb$seed)
  r <- elim_fast(jb$C, W, zipf_probs(W, jb$a), FALSE, jb$fam, max_episodes = 5e8,
                 active_prob = jb$ap, policy = jb$code)
  data.frame(learner = "eliminative", setting = jb$setting, policy = jb$policy, rep = jb$rep,
             dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = NC, mc.preschedule = FALSE))

## ---- passive time to HEAR 99% of words at least once (no learning involved) ----
first_hear <- function(a, reps = 200) {
  sapply(seq_len(reps), function(s) {
    set.seed(1800000 + s + 1000 * a)
    p <- zipf_probs(W, a); heard <- logical(W); n <- 0; t <- 0
    while (n < 0.99 * W) {
      draws <- sample.int(W, 20000, replace = TRUE, prob = p)
      for (d in draws) { t <- t + 1; if (!heard[d]) { heard[d] <- TRUE; n <- n + 1; if (n >= 0.99 * W) break } }
    }
    t
  })
}
fh <- data.frame(a = c(1, 0.75), first_hear_99 = c(mean(first_hear(1)), mean(first_hear(0.75))))

## ---- guess-test learner (R): word-driven only; passive and object-driven come
## from the Study 2 guess-test grid (same settings and cap) ----
gsettings <- data.frame(setting = c("C=10, a=1, random", "C=10, a=1, familiar", "C=100, a=1, random", "C=100, a=1, familiar"),
                        C = c(10, 10, 100, 100), fam = c(FALSE, TRUE, FALSE, TRUE))
gjobs <- merge(gsettings, data.frame(rep = 1:30)); gjobs$seed <- 1900000 + seq_len(nrow(gjobs))
gt <- bind_rows(mclapply(seq_len(nrow(gjobs)), function(j) {
  jb <- gjobs[j, ]; set.seed(jb$seed)
  r <- learn_corpus_guesstest(C = jb$C, M = W, a = 1, uniform = FALSE, active_prob = 1,
                              active_policy = "heard", fam_context = jb$fam, max_episodes = 5e6)
  data.frame(learner = "guess-test", setting = jb$setting, policy = "word-driven", rep = jb$rep,
             dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = NC, mc.preschedule = FALSE))

d <- bind_rows(el, gt)
saveRDS(list(results = d, first_hear = fh), "word_driven_results.rds")
summ <- d %>% group_by(learner, setting, policy) %>%
  summarise(n = n(), n_censored = sum(censored), mean50 = mean(dec5), mean90 = mean(dec9),
            mean99 = mean(episodes), se99 = sd(episodes) / sqrt(n()), .groups = "drop")
print(as.data.frame(summ), row.names = FALSE, digits = 6)
cat("\nPassive episodes until 99% of words heard at least once:\n"); print(fh)
write.csv(summ, "word_driven_summary.csv", row.names = FALSE)
write.csv(fh, "word_driven_first_hear.csv", row.names = FALSE)
