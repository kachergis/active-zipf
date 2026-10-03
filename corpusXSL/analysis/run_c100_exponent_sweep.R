# Eliminative learner at C=100 (C/M=0.1) across Zipf exponents, all four
# target x context conditions, with the exact C++ port (learners_fast.cpp).
# Complements run_c100_a1.R (a=1). Motivation: under p_r ~ r^-a with a=1, the
# most frequent referent is absent from a C=100 context with probability
# ~5e-10, so random-context learning never completes; the exact absence
# probability (see c100_absence_probability.R) falls from 0.05 at a=0.6 to
# 1.6e-3 at a=0.75 and 5.4e-10 at a=1; this sweep traces the resulting slowdown.
# Uniform (a=0) cells also replace the older C=100 uniform data previously
# used in Figure fig-incremental-sim.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})

# Usage: Rscript run_c100_exponent_sweep.R [ext]
#   (no argument) a in {0, .25, .5, .6, .7, .75}, cap 5e7 -> c100_exponent_sweep_*
#   ext           a in {.8, .85, .9}, cap 1e8           -> c100_exponent_sweep_ext_*
EXT <- length(commandArgs(trailingOnly = TRUE)) >= 1 && commandArgs(trailingOnly = TRUE)[1] == "ext"
M <- 1000; C <- 100; REPS <- 30
MAX_EPISODES <- if (EXT) 1e8 else 5e7
A_VALUES <- if (EXT) c(0.8, 0.85, 0.9) else c(0, 0.25, 0.5, 0.6, 0.7, 0.75)
OUT <- if (EXT) "c100_exponent_sweep_ext" else "c100_exponent_sweep"
cells <- expand.grid(rep = seq_len(REPS), active = c(FALSE, TRUE), fam = c(FALSE, TRUE),
                     a = A_VALUES)
cells$seed <- (if (EXT) 600000 else 500000) + seq_len(nrow(cells))
# run the slow (likely capped) cells first so they don't straggle at the end
cells <- cells[order(-(cells$a * !cells$fam)), ]

t0 <- Sys.time()
res <- mclapply(seq_len(nrow(cells)), function(j) {
  cl <- cells[j, ]
  set.seed(cl$seed)
  probs <- if (cl$a == 0) rep(1 / M, M) else zipf_probs(M, cl$a)
  r <- elim_fast(C, M, probs, cl$active, cl$fam, max_episodes = MAX_EPISODES)
  data.frame(a = cl$a, active = ifelse(cl$active, "Active", "Passive"),
             context = ifelse(cl$fam, "Familiar", "Random"), rep = cl$rep,
             dec5 = ifelse(r[5] > 0, r[5], NA), dec9 = ifelse(r[9] > 0, r[9], NA),
             episodes = ifelse(r[11] == 1, NA, r[10]), censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
cat(sprintf("done in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

d <- bind_rows(res) %>% arrange(a, active, context, rep)
saveRDS(d, paste0(OUT, "_results.rds"))
summ <- d %>% group_by(a, active, context) %>%
  summarise(n = n(), n_censored = sum(censored),
            mean50 = mean(dec5), mean90 = mean(dec9), mean99 = mean(episodes),
            se99 = sd(episodes) / sqrt(n()), .groups = "drop")
print(as.data.frame(summ), row.names = FALSE, digits = 6)
write.csv(summ, paste0(OUT, "_summary.csv"), row.names = FALSE)
cat(sprintf("Saved: %s_results.rds, %s_summary.csv\n", OUT, OUT))
