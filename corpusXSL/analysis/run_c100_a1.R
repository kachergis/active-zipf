# Re-runs the four eliminative-learner C=100 cells of Table tab-incremental
# under the paper's p_r ~ r^-a parametrization with a=1. Until now these cells
# were filled with numbers from older Zipf-Mandelbrot (p_r ~ (r+2.7)^-1) runs
# (see summarize_verification_grid.R), because the R implementation could not
# finish C=100 passive runs in reasonable time. Uses the exact C++ port
# (learners_fast.cpp), validated against the R version by validate_elim_fast.R.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

M <- 1000; a <- 1; C <- 100; REPS <- 30
MAX_EPISODES <- 5e8
cells <- expand.grid(rep = seq_len(REPS), active = c(FALSE, TRUE), fam = c(FALSE, TRUE))
cells$seed <- 300000 + seq_len(nrow(cells))

t0 <- Sys.time()
res <- mclapply(seq_len(nrow(cells)), function(j) {
  cl <- cells[j, ]
  set.seed(cl$seed)
  r <- elim_fast(C, M, zipf_probs(M, a), cl$active, cl$fam, max_episodes = MAX_EPISODES)
  data.frame(active = ifelse(cl$active, "Active", "Passive"), context = ifelse(cl$fam, "Familiar", "Random"),
             rep = cl$rep, dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
cat(sprintf("done in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

d <- bind_rows(res)
saveRDS(d, "c100_a1_results.rds")
summ <- d %>% group_by(active, context) %>%
  summarise(n = n(), n_censored = sum(censored),
            mean50 = mean(dec5), mean90 = mean(dec9), mean99 = mean(episodes[censored == 0]),
            median99 = median(episodes[censored == 0]), se99 = sd(episodes[censored == 0]) / sqrt(sum(censored == 0)),
            .groups = "drop")
base <- summ$mean99[summ$active == "Passive" & summ$context == "Random"]
summ$speedup99 <- base / summ$mean99
print(as.data.frame(summ), row.names = FALSE, digits = 6)
write.csv(summ, "c100_a1_summary.csv", row.names = FALSE)
cat("Saved: c100_a1_results.rds, c100_a1_summary.csv\n")
