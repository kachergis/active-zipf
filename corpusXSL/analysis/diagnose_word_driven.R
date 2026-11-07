# Why fully word-driven pursuit fails at C=100 (companion paper, Study 2).
# See run_word_driven.R for the main comparison.
#
# 1. Eliminative learner, a=1, familiar context: trace one fully word-driven
#    run -- how many words it has heard and learned, and competitor survival.
# 2. Guess-test learner, a=1, random context: an instrumented copy of the
#    guess-test core (lock_k = 0, no decay, as in learners.R) under the "heard"
#    policy; after 3e5 episodes, report how many words have been heard and
#    learned, and the frequency rank of the referents held as wrong guesses.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
library(parallel)

## ---- 1. eliminative ----
set.seed(5)
tr <- elim_trace(100, 1000, zipf_probs(1000, 1), TRUE, 200000, max_episodes = 2e6, active_prob = 1, policy = 3)
cat("Eliminative, C=100, a=1, familiar, fully word-driven:\n")
print(tr$result); print(tr$trace); print(tr$learned)

## ---- 2. guess-test ----
gt_diag <- function(seed, C = 100, M = 1000, fam = FALSE, n_ep = 3e5) {
  set.seed(seed); probs <- zipf_probs(M, 1); rk <- rank(-probs)
  g <- rep(0L, M); cb <- rep(0L, M); conf <- rep(0L, M); known <- rep(FALSE, M); tt <- rep(0L, M)
  for (ep in 1:n_ep) {
    target <- choose_target(M, probs, known, tt, 1, "heard")
    tt[target] <- tt[target] + 1L
    nt <- setdiff(1:M, target)
    d <- sample(nt, C - 1, prob = probs[nt] * (if (fam) conf[nt] + 1 else 1)); cc <- c(target, d)
    gg <- g[target]
    if (gg != 0L && !(gg %in% cc)) { cb[gg] <- 0L; g[target] <- 0L; conf[target] <- 0L; gg <- 0L }
    if (gg == 0L) {
      un <- cc[cb[cc] == 0L]
      if (length(un)) { ng <- if (length(un) == 1) un else sample(un, 1); g[target] <- ng; cb[ng] <- target }
    } else conf[target] <- conf[target] + 1L
    if (!known[target] && g[target] == target) known[target] <- TRUE
  }
  hu <- which(tt > 0 & !known)
  data.frame(seed = seed, n_heard = sum(tt > 0), n_known = sum(known), n_heard_unknown = length(hu),
             median_rank_of_wrong_guesses = median(rk[g[hu][g[hu] > 0]]),
             median_rank_heard_unknown = median(rk[hu]))
}
cat("\nGuess-test, C=100, a=1, random, fully word-driven, after 3e5 episodes:\n")
print(do.call(rbind, mclapply(1:6, function(s) gt_diag(9000 + s), mc.cores = 6)))
