# Diagnostic (not part of the main pipeline): instruments learn_corpus_guesstest
# to explain WHY bounded choice_k's cost, relative to unrestricted active choice,
# flips direction between C=10 (restriction helps early, hurts late) and C=100
# (restriction hurts a lot early, hurts less late) -- see median_vs_mean_checkpoints.R.
#
# Two candidate mechanisms, both testable directly from what the model already
# computes internally:
#
#  (A) "Empty K-window near completion." Active choice under bounded K first
#      draws a random, frequency-weighted K-sized window, then prefers unknown
#      words WITHIN it; if the window has none, it falls back to a passive draw
#      inside that same window (choose_target()'s empty-unknown_pool branch) --
#      wasting the episode on an already-known word. As the unknown set shrinks
#      toward 99% completion, a small K is decreasingly likely to contain even
#      one of the few remaining unknown words, so this fallback rate should rise
#      sharply late in the run for small K, and stay near-zero for K=Inf (which
#      only empties out at the very last word). This mechanism doesn't depend on
#      C and should explain the SHARED late-stage restriction penalty seen in
#      all three models.
#
#  (B) "Referent-claim contention." Guess-test's mutual-exclusivity bookkeeping
#      means a word can only claim a referent that's currently UNCLAIMED by any
#      other word; a larger C means more candidate referents are visible each
#      episode, and (if many other still-unknown words are simultaneously
#      squatting on WRONG guesses) a bigger context means a higher chance the
#      needed referent is already blocked. Concentrating active attention onto
#      one high-priority word at a time (unrestricted choice) vs. spreading it
#      across many words at once (small K) could plausibly change how fast wrong
#      guesses get revisited and corrected -- and that effect could plausibly be
#      C-dependent, unlike (A).
#
# This script bins episodes by n_learned/M (0-100%, 5% bins) and tracks, per
# active episode:
#   fallback_rate  = P(K-window contained no unknown word -> wasted passive draw)   [tests A]
#   unclaimed_pool = mean size of the unclaimed-candidate pool when a NEW guess is
#                    actually attempted (bigger = easier to guess right by chance)  [tests B]
#   guess_success  = P(a newly-made guess is correct)                               [tests B]

source("learners.R")

diag_guesstest <- function(C, M, a, choice_k, active_prob = 1, reps = 15, seed = 1,
                            nbins = 20, max_episodes = 3e6) {
  set.seed(seed)
  bin_edges <- seq(0, 1, length.out = nbins + 1)

  fallback_n <- fallback_d <- numeric(nbins)
  pool_sum <- pool_n <- numeric(nbins)
  succ_n <- succ_d <- numeric(nbins)

  for (rep in seq_len(reps)) {
    probs <- zipf_probs(M, a)
    current_guess <- rep(0L, M); claimed_by <- rep(0L, M)
    word_known <- rep(FALSE, M); times_targeted <- rep(0L, M)
    episodes <- 0L; n_learned <- 0L; total <- M * 0.99

    while (n_learned < total && episodes < max_episodes) {
      frac <- n_learned / M
      b <- max(1, min(nbins, ceiling(frac * nbins)))

      do_active <- TRUE  # active_prob=1 throughout this diagnostic
      if (is.finite(choice_k) && choice_k < M) {
        pool <- sample(1:M, choice_k, prob = probs)
      } else {
        pool <- 1:M
      }
      unknown_pool <- pool[!word_known[pool]]
      fallback_d[b] <- fallback_d[b] + 1
      if (length(unknown_pool) == 0) {
        fallback_n[b] <- fallback_n[b] + 1
        target <- if (length(pool) == 1) pool else sample(pool, 1, prob = probs[pool])
      } else {
        target <- if (length(unknown_pool) == 1) unknown_pool else sample(unknown_pool, 1, prob = probs[unknown_pool])
      }
      times_targeted[target] <- times_targeted[target] + 1L

      nontarg <- setdiff(1:M, target)
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
      cc <- c(target, distractors)

      g <- current_guess[target]
      if (g != 0L && !(g %in% cc)) {
        claimed_by[g] <- 0L; current_guess[target] <- 0L; g <- 0L
      }
      if (g == 0L) {
        unclaimed <- cc[claimed_by[cc] == 0L]
        pool_d_val <- length(unclaimed)
        if (pool_d_val > 0) {
          pool_sum[b] <- pool_sum[b] + pool_d_val; pool_n[b] <- pool_n[b] + 1
          newg <- if (pool_d_val == 1) unclaimed else sample(unclaimed, 1)
          succ_d[b] <- succ_d[b] + 1
          if (newg == target) succ_n[b] <- succ_n[b] + 1
          current_guess[target] <- newg; claimed_by[newg] <- target
        }
      }
      if (!word_known[target] && current_guess[target] == target) {
        n_learned <- n_learned + 1L; word_known[target] <- TRUE
      }
      episodes <- episodes + 1L
    }
  }

  data.frame(
    bin_mid = (bin_edges[-length(bin_edges)] + bin_edges[-1]) / 2,
    fallback_rate = fallback_n / pmax(fallback_d, 1),
    mean_unclaimed_pool = pool_sum / pmax(pool_n, 1),
    guess_success_rate = succ_n / pmax(succ_d, 1),
    n_new_guesses = pool_n
  )
}

M <- 1000; a <- 1
configs <- expand.grid(C = c(10, 100), K = c(5, Inf))
cat("Running instrumented guess-test diagnostic (this simulates from scratch, ~1-3 min)...\n")

results <- list()
for (i in seq_len(nrow(configs))) {
  C <- configs$C[i]; K <- configs$K[i]
  cat(sprintf("  C=%d, choice_k=%s...\n", C, ifelse(is.finite(K), K, "Inf")))
  r <- diag_guesstest(C, M, a, choice_k = K, reps = if (C == 100) 8 else 15)
  r$C <- C; r$K <- ifelse(is.finite(K), as.character(K), "Inf")
  results[[i]] <- r
}
d <- do.call(rbind, results)

cat("\n=== Fallback rate (K-window had no unknown word -> wasted passive draw) ===\n")
print(as.data.frame(reshape(d[, c("bin_mid","C","K","fallback_rate")], idvar = c("bin_mid","C"), timevar = "K", direction = "wide")),
      row.names = FALSE, digits = 3)

cat("\n=== Mean unclaimed-pool size when a NEW guess is attempted (bigger = less contention) ===\n")
print(as.data.frame(reshape(d[, c("bin_mid","C","K","mean_unclaimed_pool")], idvar = c("bin_mid","C"), timevar = "K", direction = "wide")),
      row.names = FALSE, digits = 4)

cat("\n=== New-guess success rate (should be ~1/mean_unclaimed_pool if guesses are ~uniform) ===\n")
print(as.data.frame(reshape(d[, c("bin_mid","C","K","guess_success_rate")], idvar = c("bin_mid","C"), timevar = "K", direction = "wide")),
      row.names = FALSE, digits = 4)

saveRDS(d, "guesstest_choice_k_diagnostic.rds")
cat("\nSaved raw binned data to guesstest_choice_k_diagnostic.rds\n")
