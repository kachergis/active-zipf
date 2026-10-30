# Why fully active guess-test learners stall under the observable lock-in
# criterion (lock_k = 2; companion paper, Study 2). See run_guesstest_lock.R.
#
# An instrumented copy of the guess-test core (no decay, as in learners.R) under
# fully active "unknown" targeting, which pursues words the learner has not yet
# locked. After n_ep episodes, report how many words are learned (locked and
# correct), how many hold a locked WRONG guess, how many unlearned words have
# their own referent held by some other word, and the frequency rank of the
# referents that locked wrong guesses hold.
source("learners.R")
library(parallel)

lock_diag <- function(seed, C, fam = FALSE, n_ep = 5e5, M = 1000, lock_k = 2) {
  set.seed(seed); probs <- zipf_probs(M, 1); rk <- rank(-probs)
  g <- rep(0L, M); cb <- rep(0L, M); conf <- rep(0L, M); tt <- rep(0L, M)
  for (ep in 1:n_ep) {
    target <- choose_target(M, probs, conf >= lock_k, tt, 1, "unknown")
    if (is.na(target) || length(target) == 0) break
    tt[target] <- tt[target] + 1L
    nt <- setdiff(1:M, target)
    d <- sample(nt, C - 1, prob = probs[nt] * (if (fam) conf[nt] + 1 else 1)); cc <- c(target, d)
    gg <- g[target]
    if (gg != 0L && !(gg %in% cc)) { cb[gg] <- 0L; g[target] <- 0L; conf[target] <- 0L; gg <- 0L }
    if (gg == 0L) {
      un <- cc[cb[cc] == 0L]
      if (length(un)) { ng <- if (length(un) == 1) un else sample(un, 1); g[target] <- ng; cb[ng] <- target }
    } else conf[target] <- conf[target] + 1L
  }
  locked <- conf >= lock_k
  learned <- locked & g == seq_len(M)
  lw <- which(locked & g != seq_len(M) & g > 0)
  blocked <- which(!learned & cb[seq_len(M)] != 0L & cb[seq_len(M)] != seq_len(M))
  data.frame(C = C, fam = fam, seed = seed, episodes = n_ep, learned = sum(learned),
             locked_wrong = length(lw), unlocked = sum(!locked),
             referent_held_by_other = length(blocked),
             blocked_by_locked_wrong = sum(cb[blocked] %in% lw),
             median_rank_locked_wrong_word = median(rk[lw]),
             median_rank_held_referent = median(rk[g[lw]]))
}

res <- do.call(rbind, mclapply(c(
  lapply(1:6, function(s) list(seed = 9100 + s, C = 100, fam = FALSE)),
  lapply(1:6, function(s) list(seed = 9200 + s, C = 100, fam = TRUE))),
  function(a) lock_diag(a$seed, a$C, a$fam), mc.cores = 12))
print(res, row.names = FALSE)
