# A CHEAP stand-in for the full eliminative-model simulation, used to extend
# analytical coverage to active_prob mixtures, Goldilocks, and bounded choice_k
# -- conditions the closed-ish-form analytical_elimination_bound.R formulas
# don't (yet) cover directly.
#
# KEY IDEA: the expensive part of the real simulation is the O(M) hypothesis-
# matrix elimination bookkeeping repeated every episode (that's what makes
# C=100 take days). But elimination mechanics only matter for determining ONE
# thing per word: how many times it needs to be TARGETED before it's resolved
# (N*_w, from analytical_elimination_bound.R's expected_N_word(), already
# validated to ~8-12% against real simulation at C=10). The actual dynamics of
# WHICH word gets targeted each episode -- active/passive/mixture/Goldilocks/
# bounded-choice_k -- doesn't touch the M x M hypothesis matrix at all. So:
# replace "simulate full elimination" with "simulate target selection only,
# using the SAME choose_target() as the real model, and declare a word done
# once it's been targeted N*_w times" -- reusing choose_target() directly means
# this cannot silently drift from the real selection semantics.
#
# This makes simulating millions of episodes cheap (no M x M matrix touched),
# so it stays fast even where N*_w is itself in the millions (C=100).

source("learners.R")

lightweight_target_sim <- function(M, probs, Nstar_w, active_prob = 1, active_policy = "unknown",
                                    choice_k = Inf, goldilocks_target = 2, goldilocks_sigma = 2,
                                    epsilon = 0.01, max_episodes = Inf) {
  times_targeted <- rep(0L, M)
  word_known <- rep(FALSE, M)
  total <- M * (1 - epsilon)
  n_learned <- 0L
  episodes <- 0L

  while (n_learned < total && episodes < max_episodes) {
    target <- choose_target(M, probs, word_known, times_targeted, active_prob,
                             active_policy, choice_k, goldilocks_target, goldilocks_sigma)
    times_targeted[target] <- times_targeted[target] + 1L
    if (!word_known[target] && times_targeted[target] >= Nstar_w[target]) {
      n_learned <- n_learned + 1L
      word_known[target] <- TRUE
    }
    episodes <- episodes + 1L
  }
  episodes
}

# Run `reps` replications and return the mean episode count.
# NOTE: deliberately NOT using base R replicate() here -- it forwards `...`
# from an enclosing function via substitute()-based non-standard evaluation,
# which silently fails to pick up the caller's extra named args (confirmed by
# hand: replicate(reps, lightweight_target_sim(M, probs, Nstar_w, ...)) inside
# this wrapper gave IDENTICAL output for active_prob=1 and active_prob=0,
# while the equivalent plain sapply/loop -- and calling replicate() directly
# at the top level, not wrapped in a function -- both work correctly). sapply
# with an explicit closure avoids the issue.
mean_lightweight_sim <- function(M, probs, Nstar_w, reps = 30, seed = 1, ...) {
  set.seed(seed)
  out <- sapply(seq_len(reps), function(i) lightweight_target_sim(M, probs, Nstar_w, ...))
  list(mean = mean(out), median = median(out), se = sd(out) / sqrt(reps), raw = out)
}
