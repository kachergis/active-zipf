# Analytical approximation for the ELIMINATIVE model's expected episodes to
# learn a Zipfian vocabulary, as an alternative to brute-force simulation for
# conditions (like C=100) where simulation is impractically slow.
#
# DERIVATION SUMMARY (see conversation for the full working, including a first
# attempt that was wrong by 5-7x and why):
#
# For a target word w, a competing referent r survives (remains a candidate in
# w's hypothesis row) as long as r has been present among the C-1 distractors
# on every one of w's targeted exposures so far. Using a with-replacement
# approximation for the (actually without-replacement) weighted distractor draw
# -- reasonable since C << M -- referent r's per-trial survival probability is
#   p_survive(r) = 1 - (1 - probs[r])^(C-1)
# w is learned once ALL M-1 competitors have been excluded at least once. This
# gives an exact (given the approximation) equation for N, the number of w's
# targeted exposures needed:
#   P(w resolved by N targeted exposures) = prod_{r != w} (1 - p_survive(r)^N)
# solved numerically (not by substituting a single "worst-case" probability
# into the paper's f_{W,eps}(x) with exponent M-1 -- that was the original,
# WRONG approach: it treats every one of the M-1 competitors as being exactly
# as hard to exclude as the single hardest one, wildly overestimating N when
# the competitor-difficulty profile is itself Zipfian-shaped, i.e. dominated by
# one or two genuinely hard competitors with ~997 nearly-trivial ones).
#
# E[N] is then obtained by direct summation of the survival function (not a
# closed form), and:
#
#   T_active  ~= M * mean_w(E[N_w])
#     (active selection never wastes a draw on an already-learned word, so the
#     total is just each word's own requirement summed/averaged over M words)
#
#   T_passive ~= the number of episodes for the RAREST word (min word-sampling
#     probability) to be TARGETED N* times, i.e. a Negative Binomial(size=N*,
#     prob=min_p) waiting time, at a high percentile (analogous to how the
#     rest of the paper uses the rarest word to bound T+). This -- not the
#     classical uniform-probability "double Dixie cup" asymptotic, which
#     does not account for the rare word ALSO being rarely targeted in the
#     first place -- is what matches simulation.
#
# VALIDATION (against real eliminative-model simulation data, M=1000, C=10, a=1):
#   T_active:  predicted 4322,  actual simulated ~4695   (~8% off)
#   T_passive: predicted 79008, actual simulated ~89454  (~12% off)
# Both a large improvement over literature-standard asymptotics applied naively;
# good enough to be directly useful for conditions too slow to simulate.

zipf_probs_fixed <- function(M, a, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (a == 0) return(rep(1 / M, M))
  probs <- (1:M)^(-a)
  probs <- probs / sum(probs)
  sample(probs, length(probs))
}

# Expected number of targeted exposures word `w` needs before all M-1
# competitors have been excluded at least once (with-replacement approximation).
#
# IMPORTANT: at large C (e.g. C=100 at M=1000), the single hardest competitor's
# per-trial survival probability can be extremely close to 1 (essentially always
# drawn as a distractor), making its own expected exclusion wait astronomically
# large (order 10^5-10^7+) -- a naive numerical sum_{n=0}^{nmax} P(N>n) with a
# fixed, modest nmax silently UNDER-estimates E[N] by simply truncating the tail
# (this bit us: nmax=8000 gave N*~7980, nmax=100000 gave N*~96662, still rising
# ~linearly with nmax -- i.e. not converged at all, just truncated later).
# Fix: sum the head (n=0..n0) numerically, where multiple competitors still
# matter, then add the tail (n>n0) in closed form using ONLY the single hardest
# competitor (by then, every other competitor's own p_survive_r^n is already
# negligible, so P(N>n) ~= p_survive_max^n exactly there -- a geometric series).
expected_N_word <- function(w, probs, M, C, n0 = 2000) {
  competitors <- probs[-w]
  p_survive <- 1 - (1 - competitors)^(C - 1)
  p_max <- max(p_survive)

  ns <- 0:n0
  head_terms <- sapply(ns, function(n) 1 - prod(1 - p_survive^n))
  head_sum <- sum(head_terms)

  # analytic geometric tail from n0+1 to infinity, dominant-competitor-only
  tail_sum <- if (p_max < 1) (p_max^(n0 + 1)) / (1 - p_max) else Inf
  head_sum + tail_sum
}

# Average E[N_w] across a sample of words (every `by`-th rank by default, for
# speed -- the quantity is smooth in rank so this subsamples cleanly).
mean_Nstar <- function(M, C, a, by = 20, seed = 1, n0 = 2000) {
  probs <- zipf_probs_fixed(M, a, seed = seed)
  words <- seq(1, M, by = by)
  mean(sapply(words, expected_N_word, probs = probs, M = M, C = C, n0 = n0))
}

predict_T_active <- function(M, C, a, ...) {
  Nstar <- mean_Nstar(M, C, a, ...)
  M * Nstar
}

predict_T_passive <- function(M, C, a, eps_pop = 0.01, ...) {
  probs <- zipf_probs_fixed(M, a, seed = 1)
  min_p <- min(probs)
  Nstar <- mean_Nstar(M, C, a, ...)
  qnbinom(1 - eps_pop, size = Nstar, prob = min_p) + Nstar
}

if (sys.nframe() == 0) {
  # quick self-check when run directly: Rscript analytical_elimination_bound.R
  for (C in c(10, 100)) {
    cat(sprintf("M=1000, C=%d, a=1: T_active=%.0f  T_passive=%.0f\n",
                 C, predict_T_active(1000, C, 1), predict_T_passive(1000, C, 1)))
  }
}
