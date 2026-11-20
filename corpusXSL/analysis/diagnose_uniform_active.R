# Why uniform-among-unknown active targeting helps the eliminative learner in
# familiar context (run_uniform_active.R). Traces n_unseen (words never yet
# targeted) and n_learned under both active policies, and compares against
# fam_seen_only (familiarity weights that ignore unseen words' all-ones rows;
# see learners_fast.cpp), to test whether the benefit comes from removing the
# dilution of the familiarity signal by unseen words.
suppressMessages({library(dplyr); library(parallel)})
source("learners.R"); Rcpp::sourceCpp("learners_fast.cpp")
jobs <- expand.grid(C = c(10, 100), policy = c(0L, 2L), seen_only = c(FALSE, TRUE), rep = 1:10)
res <- mclapply(seq_len(nrow(jobs)), function(i) {
  jb <- jobs[i, ]; set.seed(630000 + jb$rep)
  tr <- elim_trace(jb$C, 1000, zipf_probs(1000, 1), TRUE, trace_every = 250, policy = jb$policy,
                   fam_seen_only = jb$seen_only)
  t <- tr$trace; ep <- function(n) t$episode[which(t$n_learned >= n)[1]]
  at <- function(e) if (any(t$episode == e)) t$n_unseen[t$episode == e] else 0
  data.frame(C = jb$C, policy = ifelse(jb$policy == 2L, "uniform", "freq"), seen_only = jb$seen_only,
             rep = jb$rep, p99 = tr$result[10],
             unseen_at_1000 = at(1000), unseen_at_5000 = at(5000),
             ep_all_seen = t$episode[which(t$n_unseen == 0)[1]], ep50 = ep(500))
}, mc.cores = 12)
d <- bind_rows(res)
s <- d %>% group_by(C, policy, seen_only) %>%
  summarise(n = n(), mean99 = mean(p99), unseen_at_1000 = mean(unseen_at_1000),
            unseen_at_5000 = mean(unseen_at_5000), ep_all_seen = mean(ep_all_seen), ep50 = mean(ep50), .groups = "drop")
write.csv(s, "uniform_active/familiar_unseen_diagnostic.csv", row.names = FALSE)
print(as.data.frame(s), row.names = FALSE, digits = 5)
