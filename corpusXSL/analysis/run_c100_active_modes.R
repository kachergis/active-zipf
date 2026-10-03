# Simulates the eliminative learner's C=100 "active modes" cells -- partial
# autonomy (active_prob mixtures), the goldilocks policy, and bounded choice
# sets (choice_k) -- with the fast C++ learner (learners_fast.cpp, validated
# for these modes against the R overnight sweep in validate_elim_fast.R).
#
# These cells were previously filled by analytical_mixture_extension.R, which
# ratio-transferred C=10 results onto an analytical C=100 pure-active baseline
# (1.46e9 episodes). That baseline used a with-replacement approximation to
# distractor sampling that puts the most frequent referent's absence
# probability at ~7e-7 instead of the exact ~5.4e-10
# (c100_absence_probability.R), so those estimates are superseded.
#
# At the setting they were meant for (a=1, random context) learning never
# completes even under pure active selection (run_c100_a1.R), so every variant
# would simply hit the cap. We therefore run the same conditions at the two
# C=100 settings where learning does complete:
#   - a=1,    familiar context
#   - a=0.75, random context (the steepest random-context skew at which every
#             run finished in run_c100_exponent_sweep.R)
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

M <- 1000; C <- 100; REPS <- 30
MAX_EPISODES <- 5e8
conds <- data.frame(
  label = c("passive", "ap=0.25", "ap=0.50", "ap=0.75", "active", "goldilocks", "k=100", "k=50", "k=20", "k=5"),
  ap = c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1, 1),
  policy = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  k = c(0, 0, 0, 0, 0, 0, 100, 50, 20, 5))
settings <- data.frame(setting = c("a=1, familiar context", "a=0.75, random context"),
                       a = c(1, 0.75), fam = c(TRUE, FALSE))
jobs <- merge(merge(conds, settings), data.frame(rep = seq_len(REPS)))
jobs$seed <- 700000 + seq_len(nrow(jobs))
# longest (most passive / smallest choice set) first, to avoid stragglers
jobs <- jobs[order(jobs$ap + (jobs$k > 0 & jobs$k < 20)), ]

t0 <- Sys.time()
res <- mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]
  set.seed(jb$seed)
  r <- elim_fast(C, M, zipf_probs(M, jb$a), FALSE, jb$fam, max_episodes = MAX_EPISODES,
                 active_prob = jb$ap, policy = jb$policy, choice_k = jb$k)
  data.frame(setting = jb$setting, label = jb$label, rep = jb$rep,
             dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
cat(sprintf("done in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

d <- bind_rows(res)
d$label <- factor(d$label, levels = conds$label)
saveRDS(d, "c100_active_modes_results.rds")
summ <- d %>% group_by(setting, label) %>%
  summarise(n = n(), n_censored = sum(censored), mean50 = mean(dec5), mean90 = mean(dec9),
            mean99 = mean(episodes[censored == 0]), se99 = sd(episodes[censored == 0]) / sqrt(sum(censored == 0)),
            .groups = "drop") %>%
  group_by(setting) %>%
  mutate(ratio_to_active = mean99 / mean99[label == "active"],
         speedup_vs_passive = mean99[label == "passive"] / mean99) %>% ungroup()
print(as.data.frame(summ), row.names = FALSE, digits = 5)
write.csv(summ, "c100_active_modes_summary.csv", row.names = FALSE)
cat("Saved: c100_active_modes_results.rds, c100_active_modes_summary.csv\n")
