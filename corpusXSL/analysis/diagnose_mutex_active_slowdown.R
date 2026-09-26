# Diagnostic (not part of the main pipeline): why does mutual exclusivity
# help the eliminative learner enormously under PASSIVE selection at C=100
# (20x speedup) but not help -- or even hurt -- under ACTIVE selection?
# See run_mutual_exclusivity_analysis.R for the headline numbers.
#
# Reisenauer et al. (2013)'s own theory gives a candidate mechanism: mutual
# exclusivity's cascade only propagates when a word is DIRECTLY learned via
# ordinary elimination (absence from a distractor draw); a small "candidate
# loop" of mutually-confounding words -- where each word's only remaining
# rival is another word IN the same small set -- can persist indefinitely,
# because excluding an OUTSIDE referent never touches it. Their theory
# predicts such loops are most likely to form among the highest-frequency
# words specifically, since two high-frequency words are almost always
# present as each other's distractors at large C, making it rare for either
# row to narrow down by ordinary absence-based elimination.
#
# This script instruments a handful of replications (both active and passive,
# C=100, mutex=TRUE) to record the full (episode, n_learned) trajectory, and
# at the end, inspects which words remain un-learned and whether they show
# loop-like structure (small remaining candidate sets pointing at each other).

source("learners.R")

diag_run <- function(C, M, a, active, max_episodes = 3e6, seed = 1) {
  set.seed(seed)
  probs <- zipf_probs(M, a)
  hyp <- matrix(1, nrow = M, ncol = M)
  word_known <- rep(FALSE, M)
  times_targeted <- rep(0L, M)
  episodes <- 0L; n_learned <- 0L
  total <- M * 0.99
  trajectory <- list()

  mark_learned <- function(w) {
    n_learned <<- n_learned + 1L
    word_known[w] <<- TRUE
  }

  while (n_learned < total && episodes < max_episodes) {
    target <- choose_target(M, probs, word_known, times_targeted,
                             active_prob = as.numeric(active), active_policy = "unknown", choice_k = Inf)
    times_targeted[target] <- times_targeted[target] + 1L
    nontarg <- setdiff(1:M, target)
    distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
    cc <- c(target, distractors)
    hyp[cc[1], which(!is.element(1:M, cc))] <- 0
    if (sum(hyp[cc[1], ]) == 1 && !word_known[cc[1]]) {
      queue <- cc[1]
      while (length(queue) > 0) {
        w <- queue[1]; queue <- queue[-1]
        if (word_known[w]) next
        mark_learned(w)
        trajectory[[length(trajectory) + 1]] <- c(episode = episodes, n_learned = n_learned)
        others <- which(!word_known)
        if (length(others) > 0) {
          hyp[others, w] <- 0
          newly_single <- others[rowSums(hyp[others, , drop = FALSE]) == 1]
          queue <- c(queue, newly_single)
        }
      }
    }
    episodes <- episodes + 1L
  }

  traj_df <- do.call(rbind, trajectory)
  remaining <- which(!word_known)
  list(episodes = episodes, n_learned = n_learned, completed = n_learned >= total,
       trajectory = traj_df, remaining = remaining, probs = probs, hyp = hyp,
       times_targeted = times_targeted)
}

M <- 1000; a <- 1; C <- 100
cat("Running instrumented diagnostic reps (this simulates from scratch)...\n")

for (active in c(FALSE, TRUE)) {
  cat(sprintf("\n=== active=%s ===\n", active))
  r <- diag_run(C, M, a, active, max_episodes = 3e6, seed = 42)
  cat(sprintf("Finished=%s, episodes=%d, n_learned=%d/%d\n", r$completed, r$episodes, r$n_learned, round(M * 0.99)))

  # Trajectory shape: episodes elapsed to reach each decile of n_learned
  td <- r$trajectory
  deciles <- round(M * seq(.1, .99, .1))
  cat("Episode at which n_learned first reaches each decile:\n")
  for (dec in deciles) {
    idx <- which(td[, "n_learned"] >= dec)[1]
    if (!is.na(idx)) cat(sprintf("  n_learned=%d: episode %d\n", dec, td[idx, "episode"]))
  }

  # Remaining un-learned words: rank by frequency, and check candidate-set sizes
  if (length(r$remaining) > 0) {
    ranks <- rank(-r$probs)[r$remaining]  # frequency rank (1 = most frequent)
    cat(sprintf("\n%d words remain unlearned. Frequency ranks: %s\n",
                length(r$remaining), paste(sort(ranks), collapse = ", ")))
    cat("Candidate-set size (should be small if stuck in a loop) and times targeted, for remaining words:\n")
    for (w in r$remaining) {
      cset <- which(r$hyp[w, ] == 1)
      cat(sprintf("  word (freq rank %d): candidate set size=%d, candidates' freq ranks={%s}, times_targeted=%d\n",
                  rank(-r$probs)[w], length(cset), paste(sort(rank(-r$probs)[cset]), collapse = ","), r$times_targeted[w]))
    }
  }
}
