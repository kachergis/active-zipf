# M=10,000 extension of the eliminative-learner simulations (the pre-literacy
# vocabulary target), with the fast C++ learner. Previously omitted from the
# paper as too expensive for the R implementation.
#
# Also a direct test of whether C/M governs learnability, as Vogt (2012)'s
# framing implies: C=100 at M=10,000 has the same C/M (0.01) as C=10 at
# M=1,000, which learns easily. The exact absence probability
# (c100_absence_probability.R) predicts otherwise: at M=10,000, C=100, a=1 the
# most frequent referent (p=0.10) is absent from a context with probability
# ~1.5e-6, so random-context learning needs ~6.5e10 episodes for the rarest
# word. Those cells are therefore run with fewer replications and a 1e8 cap.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

M <- 10000
cells <- rbind(
  expand.grid(a = 1, C = 10, active = c(FALSE, TRUE), fam = c(FALSE, TRUE), rep = 1:30, cap = 5e8),
  expand.grid(a = 1, C = 100, active = c(FALSE, TRUE), fam = TRUE, rep = 1:30, cap = 5e8),
  expand.grid(a = 1, C = 100, active = c(FALSE, TRUE), fam = FALSE, rep = 1:10, cap = 1e8),
  expand.grid(a = 0, C = c(10, 100), active = c(FALSE, TRUE), fam = FALSE, rep = 1:30, cap = 5e8))
cells$seed <- 1300000 + seq_len(nrow(cells))
# capped / slowest cells first
cells <- cells[order(-(cells$C == 100 & !cells$fam & cells$a == 1), cells$active, -cells$C), ]

t0 <- Sys.time()
res <- mclapply(seq_len(nrow(cells)), function(j) {
  cl <- cells[j, ]
  set.seed(cl$seed)
  probs <- if (cl$a == 0) rep(1 / M, M) else zipf_probs(M, cl$a)
  r <- elim_fast(cl$C, M, probs, cl$active, cl$fam, max_episodes = cl$cap)
  data.frame(a = cl$a, C = cl$C, active = ifelse(cl$active, "Active", "Passive"),
             context = ifelse(cl$fam, "Familiar", "Random"), rep = cl$rep, cap = cl$cap,
             dec1 = r[1], dec5 = ifelse(r[5] > 0, r[5], NA), dec9 = ifelse(r[9] > 0, r[9], NA),
             episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
cat(sprintf("done in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

d <- bind_rows(res)
saveRDS(d, "m10k_results.rds")
summ <- d %>% group_by(a, C, context, active) %>%
  summarise(n = n(), n_censored = sum(censored), reached10 = sum(dec1 > 0), reached50 = sum(!is.na(dec5)),
            mean50 = mean(dec5), mean90 = mean(dec9),
            mean99 = ifelse(any(censored == 0), mean(episodes[censored == 0]), NA),
            se99 = sd(episodes[censored == 0]) / sqrt(sum(censored == 0)), .groups = "drop")
print(as.data.frame(summ), row.names = FALSE, digits = 6)
write.csv(summ, "m10k_summary.csv", row.names = FALSE)
cat("Saved: m10k_results.rds, m10k_summary.csv\n")
