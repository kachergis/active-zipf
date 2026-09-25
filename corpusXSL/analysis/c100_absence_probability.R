# Exact probability that a given referent is ABSENT from one learning context,
# for the eliminative learner's context construction (C-1 distractors drawn
# sequentially without replacement, probability proportional to frequency, from
# the non-target referents -- i.e. R's sample(nontarg, C-1, prob = probs)).
#
# An eliminative learner can only rule out referent r for word w on an exposure
# where r is absent, so 1/P(absent) is the expected number of exposures of w
# needed just to eliminate r. Under Zipf a=1 at C=100 this is ~1.9e9 for the
# most frequent referent -- i.e. random-context learning never completes.
#
# Method: exponential-clock representation of weighted sampling without
# replacement. Give item j a clock E_j ~ Exp(w_j); the sample is the k = C-1
# items whose clocks ring first. r is absent iff at least k other clocks ring
# before E_r:
#   P(absent) = int_0^inf w_r exp(-w_r t) P(S_t >= k) dt,
# where S_t = sum_{j != r} 1[E_j < t] is Poisson-binomial with p_j = 1-exp(-w_j t).

#
# The integral is evaluated on a fine grid in log(t). R's adaptive integrate()
# is NOT used: for steep distributions (e.g. a >= 0.75 at C=100) the integrand
# is a narrow peak that integrate() misses, returning values too small by
# orders of magnitude (e.g. 4.5e-7 instead of 1.6e-3 at a=0.75). The grid
# result is stable to widening/refining the grid and matches simulation.
absent_prob <- function(w, r, k, lo = 1e-4, hi = 1e7, n = 12000) {
  others <- w[-r]
  lt <- seq(log(lo), log(hi), length.out = n); t <- exp(lt)
  f <- sapply(t, function(tt) {
    p <- 1 - exp(-others * tt)
    # dp[i+1] = P(S = i) for i = 0..k-1 (only the lower tail is needed)
    dp <- c(1, rep(0, k - 1))
    for (pj in p) dp <- dp * (1 - pj) + c(0, dp[-k]) * pj
    w[r] * exp(-w[r] * tt) * (1 - sum(dp))
  })
  sum(f * t) * diff(lt[1:2])  # dt = t d(log t)
}

if (sys.nframe() == 0) {
  M <- 1000
  out <- expand.grid(a = c(0.25, 0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 1), C = c(10, 100))
  out$p_absent_top <- mapply(function(a, C) {
    w <- (1:M)^(-a); w <- w / sum(w)
    absent_prob(w[-M], 1, C - 1)  # target = rarest word, so the pool is all others
  }, out$a, out$C)
  out$exposures_to_eliminate_top <- 1 / out$p_absent_top
  print(out, digits = 3)
  write.csv(out, "c100_absence_probability.csv", row.names = FALSE)

  # Check against direct simulation where the probability is large enough
  set.seed(7)
  w <- (1:M)^-1; w <- w / sum(w); wt <- w[-M]
  sim <- mean(replicate(3e5, !(1 %in% sample(seq_along(wt), 9, prob = wt))))
  cat(sprintf("Check (a=1, C=10, top referent): exact %.4f vs simulated %.4f\n",
              absent_prob(wt, 1, 9), sim))
}
