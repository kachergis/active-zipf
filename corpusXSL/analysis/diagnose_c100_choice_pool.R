# Why are bounded choice sets (choice_k) so costly for the eliminative learner
# at C=100 with familiar context (run_c100_active_modes.R: k=100 is 5.5x slower
# than unrestricted active selection), when they cost little at C=10 or with
# random context?
#
# Hypothesis: double frequency weighting. The choice set is drawn ~ probs and
# the target is then chosen among its unknown words ~ probs again, so rare
# words' effective targeting weight is ~ probs^2 and they are starved of
# exposures. Test: draw the choice set UNIFORMLY instead (pool_uniform, a
# diagnostic-only option of learners_fast.cpp) and compare.
#
# Result (30 reps per cell, C=100, a=1, familiar context; mean episodes to
# 50% / 90% / 99%):
#   k=100 freq-weighted pool  82,873 / 84,450 / 87,478
#   k=100 uniform pool        12,328 / 13,583 / 15,124
#   k=20  freq-weighted pool 159,342 / 161,530 / 178,896
#   k=20  uniform pool        10,496 / 11,744 / 15,894
#   k=5   freq-weighted pool  84,000 / 90,032 / 163,910
#   k=5   uniform pool         8,031 /  9,592 / 23,211
#   unrestricted active       13,316 / 14,576 / 15,876
# -> the cost is entirely due to the frequency-weighted pool, not to
#    restriction per se.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages({library(parallel); library(dplyr)})
M <- 1000; a <- 1; C <- 100
run <- function(k, pu, seed0) {
  r <- simplify2array(mclapply(1:30, function(s) { set.seed(seed0 + s)
    elim_fast(C, M, zipf_probs(M, a), FALSE, TRUE, active_prob = 1, choice_k = k, pool_uniform = pu)[c(5, 9, 10)] },
    mc.cores = detectCores() - 2))
  data.frame(k = ifelse(k == 0, "unrestricted", as.character(k)), pool = ifelse(k == 0, "--", ifelse(pu, "uniform", "freq")),
             mean50 = mean(r[1, ]), mean90 = mean(r[2, ]), mean99 = mean(r[3, ]))
}
out <- bind_rows(lapply(c(100, 20, 5), function(k) bind_rows(run(k, FALSE, 910000 + k * 100), run(k, TRUE, 920000 + k * 100))),
                 run(0, FALSE, 930000))
print(out, row.names = FALSE, digits = 6)
write.csv(out, "c100_choice_pool_diagnostic.csv", row.names = FALSE)
