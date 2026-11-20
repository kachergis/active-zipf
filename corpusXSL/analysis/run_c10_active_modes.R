# C=10 reference for run_c100_active_modes.R: the same 10 target-selection
# conditions (passive, active_prob mixtures, fully active, goldilocks, choice_k)
# for the eliminative learner at C=10, a=1, random and familiar context, with
# the same fast C++ learner, so that all settings in the companion paper's
# Study 3 come from one validated implementation. (The R overnight sweep,
# active_modes_results_overnight.rds, covers random context only and has
# censored replications for choice_k=5.)
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

M <- 1000; C <- 10; a <- 1; REPS <- 200
conds <- data.frame(
  label = c("passive", "ap=0.25", "ap=0.50", "ap=0.75", "active", "goldilocks", "k=100", "k=50", "k=20", "k=5"),
  ap = c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1, 1),
  policy = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  k = c(0, 0, 0, 0, 0, 0, 100, 50, 20, 5))
settings <- data.frame(setting = c("C=10, a=1, random context", "C=10, a=1, familiar context"), fam = c(FALSE, TRUE))
jobs <- merge(merge(conds, settings), data.frame(rep = seq_len(REPS)))
jobs$seed <- 800000 + seq_len(nrow(jobs))

res <- mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]
  set.seed(jb$seed)
  r <- elim_fast(C, M, zipf_probs(M, a), FALSE, jb$fam, max_episodes = 5e8,
                 active_prob = jb$ap, policy = jb$policy, choice_k = jb$k)
  data.frame(setting = jb$setting, label = jb$label, rep = jb$rep,
             dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)

d <- bind_rows(res)
d$label <- factor(d$label, levels = conds$label)
saveRDS(d, "c10_active_modes_results.rds")
summ <- d %>% group_by(setting, label) %>%
  summarise(n = n(), n_censored = sum(censored), mean99 = mean(episodes), se99 = sd(episodes) / sqrt(n()),
            .groups = "drop") %>%
  group_by(setting) %>%
  mutate(ratio_to_active = mean99 / mean99[label == "active"],
         speedup_vs_passive = mean99[label == "passive"] / mean99) %>% ungroup()
print(as.data.frame(summ), row.names = FALSE, digits = 5)
write.csv(summ, "c10_active_modes_summary.csv", row.names = FALSE)
