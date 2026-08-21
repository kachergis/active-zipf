# Three cross-situational word-learning mechanisms, each crossed with:
#   - target selection: passive (random) vs. active (choose an unknown word),
#     now generalized into three composable dimensions -- see "ACTIVE TARGET
#     SELECTION" below.
#   - context selection: random distractors vs. familiar (already-partly-known) distractors
#   - word frequency distribution: uniform, or Zipfian with a general exponent a
#     (p_k propto k^-a, matching the parametrization used in the paper's analytical
#     fast-mapping section)
#
# Model 1: "Eliminative" (as in corpusXSL_eliminative_active_parallel.R) -- tracks,
#   for each word, the full set of referents consistent with every past exposure;
#   learned once that set narrows to a single referent. Cost per episode scales
#   with M (a full M-length vector is touched every episode), so this is the most
#   expensive model at large M/C.
#
# Model 2: "Guess-test" -- a distinct, propose-and-revise mutual-exclusivity learner.
#   Each word holds at most one current guessed referent. A guess is only made when
#   the word has none, by picking an "unclaimed" referent (not currently guessed by
#   any other word) from the current situation. A held guess is discarded (and must
#   be re-proposed) if a later exposure to that word does not include it -- since the
#   true referent is always present whenever its word is spoken (the standard
#   assumption in this modeling tradition), a guess that goes missing must be wrong.
#   Cheap (O(C) per episode, not O(M)), but can genuinely err and needs to recover.
#   NOTE: the original corpusXSL/Rsimulations/corpusXSL_guesstest.R never checked
#   guesses against the true word-referent mapping -- this version fixes that.
#
# Model 3: "Ranked-frequency" (as in corpusXSL_rankedFreq_active_parallel.R) --
#   tracks a full M-length co-occurrence *count* matrix; a word is learned once its
#   true referent's count is (weakly) the maximum among all candidates for that
#   word. CAUTION: because ties count as success and a word's first exposure as an
#   active target always produces a tie, active-selection episodes-to-99% for this
#   model is deterministically M*(1-epsilon) regardless of distribution -- this is
#   a known, verified artifact of the original criterion, not a bug introduced here.
#   See the paper's "Robustness across Zipf exponent and learning mechanism" section.
#
# All three learners accept `max_seconds` (wall-clock cap on a single replication,
# checked every TIME_CHECK_EVERY episodes) and `max_episodes` (hard episode-count
# cap), whichever binds first. A replication that hits either cap returns with
# `censored=1` and whatever deciles/episode count it had reached; it is NOT a
# valid draw of "episodes to reach 99%" and should be excluded from mean/median
# calculations (but its censoring should be reported -- see
# summarize_verification_grid.R).
#
# ACTIVE TARGET SELECTION
# ------------------------
# The original binary `active` (TRUE/FALSE) is generalized into three composable
# dimensions, all handled by the shared choose_target() below so the three
# learners don't duplicate this logic three times:
#
#   active_prob (p, default: NULL -> falls back to as.numeric(active), i.e. 0 or 1,
#                exactly reproducing old behavior when unspecified)
#       Probability that THIS episode's target is chosen actively rather than
#       passively/randomly. p=1 is "fully active" (the original `active=TRUE`),
#       p=0 is "fully passive" (the original `active=FALSE`), and 0<p<1 is a
#       learner who only sometimes gets to exercise choice -- e.g. a caregiver
#       who isn't always following the child's lead. Answers "how much of the
#       day must be actively sampled to get the benefits of active learning?"
#
#   active_policy (default: "unknown", the original policy)
#       "unknown"    -- prefer any not-yet-known word, weighted by base frequency
#                       (this is the ONLY policy used anywhere in the paper so far)
#       "goldilocks" -- among not-yet-known words, prefer ones with an
#                       INTERMEDIATE amount of prior exposure (tracked via
#                       times_targeted, a per-word counter incremented every time
#                       a word is picked as target, active or passive), rather
#                       than either completely-fresh (never encountered) or
#                       heavily-exposed-but-still-unresolved words. Operationalizes
#                       the "Goldilocks effect" (Kidd, Piantadosi, & Aslin, 2012)
#                       already cited in the paper as motivation for the active
#                       learner, but never previously implemented as the actual
#                       selection rule.
#
#   choice_k (K, default: Inf, i.e. unrestricted -- the original behavior)
#       When acting actively, first restrict the candidate pool to a random
#       K-sized subset of the vocabulary (redrawn every episode, frequency-
#       weighted) before applying active_policy within it. Models a learner who
#       can only exercise choice over what happens to be available right now
#       (a bounded set of toys in the room), not the entire vocabulary. If the
#       K-window happens to contain no unknown words, falls back to a passive
#       draw WITHIN that same window (the learner still can't reach outside it).
#
# These three compose freely and are each backward-compatible individually and
# jointly: leaving all three at their defaults exactly reproduces every result
# already in the paper.

require(foreach)
require(doParallel)

# p_k propto k^-a over frequency rank k=1..M; a=0 reduces to uniform.
zipf_probs <- function(M, a) {
  if (a == 0) return(rep(1 / M, M))
  probs <- (1:M)^(-a)
  probs <- probs / sum(probs)
  sample(probs, length(probs))  # randomize which referent index gets which rank
}

# Shared active/passive/bounded/Goldilocks target-selection rule used by all
# three learners. See "ACTIVE TARGET SELECTION" in the file header.
choose_target <- function(M, probs, word_known, times_targeted, active_prob,
                           active_policy = "unknown", choice_k = Inf,
                           goldilocks_target = 2, goldilocks_sigma = 2) {
  do_active <- if (active_prob >= 1) TRUE else if (active_prob <= 0) FALSE else (runif(1) < active_prob)

  if (!do_active) {
    return(sample(1:M, 1, prob = probs))
  }

  if (is.finite(choice_k) && choice_k < M) {
    pool <- sample(1:M, choice_k, prob = probs)
  } else {
    pool <- 1:M
  }

  unknown_pool <- pool[!word_known[pool]]
  if (length(unknown_pool) == 0) {
    # Nothing unknown is available in this window -- the learner still can't
    # reach outside it, so fall back to a passive draw within the window.
    if (length(pool) == 1) return(pool)
    return(sample(pool, 1, prob = probs[pool]))
  }

  if (active_policy == "goldilocks") {
    dist <- abs(times_targeted[unknown_pool] - goldilocks_target)
    w <- exp(-(dist^2) / (2 * goldilocks_sigma^2)) * probs[unknown_pool]
    if (sum(w) <= 0) w <- probs[unknown_pool]  # degenerate fallback (shouldn't normally trigger)
    if (length(unknown_pool) == 1) return(unknown_pool)
    return(sample(unknown_pool, 1, prob = w))
  }

  # default policy: "unknown" -- prefer any unknown, weighted by base frequency
  if (length(unknown_pool) == 1) return(unknown_pool)
  sample(unknown_pool, 1, prob = probs[unknown_pool])
}

# How often (in episodes) each learner checks the wall-clock. Checking every
# iteration would make Sys.time() overhead dominate for cheap models; checking
# too rarely makes max_seconds imprecise. 2000 is a reasonable middle ground
# across all three models at M in [1000, 10000].
TIME_CHECK_EVERY <- 2000L

learn_corpus_eliminative <- function(C, M, a = 1, uniform = TRUE, active = FALSE,
                                      fam_context = FALSE, epsilon = .01,
                                      max_episodes = Inf, max_seconds = Inf,
                                      active_prob = NULL, active_policy = "unknown",
                                      choice_k = Inf, goldilocks_target = 2, goldilocks_sigma = 2) {
  if (is.null(active_prob)) active_prob <- as.numeric(active)
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  hyp <- matrix(1, nrow = M, ncol = M)
  word_known <- rep(FALSE, M)
  times_targeted <- rep(0L, M)
  episodes <- 0
  n_learned <- 0
  total <- M * (1 - epsilon)
  eps_to_learn_decile <- rep(FALSE, 9)
  deciles <- M * seq(.1, .9, .1)
  t0 <- Sys.time()
  censored <- FALSE

  while (n_learned < total) {
    if (episodes %% TIME_CHECK_EVERY == 0 && episodes > 0) {
      if (episodes >= max_episodes || as.numeric(Sys.time() - t0, units = "secs") >= max_seconds) {
        censored <- TRUE
        break
      }
    }
    target <- choose_target(M, probs, word_known, times_targeted, active_prob,
                             active_policy, choice_k, goldilocks_target, goldilocks_sigma)
    times_targeted[target] <- times_targeted[target] + 1L
    nontarg <- setdiff(1:M, target)
    if (fam_context) {
      familiar <- 1 / (colSums(hyp) + 1)
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg] * familiar[nontarg])
    } else {
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
    }
    cc <- c(target, distractors)
    hyp[cc[1], which(!is.element(1:M, cc))] <- 0
    if (sum(hyp[cc[1], ]) == 1 && !word_known[cc[1]]) {
      n_learned <- n_learned + 1
      word_known[cc[1]] <- TRUE
      for (dd in 1:9) if (n_learned >= deciles[dd] && !eps_to_learn_decile[dd]) eps_to_learn_decile[dd] <- episodes
    }
    episodes <- episodes + 1
  }
  c(eps_to_learn_decile, episodes, censored = as.numeric(censored))
}

learn_corpus_guesstest <- function(C, M, a = 1, uniform = TRUE, active = FALSE,
                                    fam_context = FALSE, epsilon = .01,
                                    max_episodes = Inf, max_seconds = Inf,
                                    active_prob = NULL, active_policy = "unknown",
                                    choice_k = Inf, goldilocks_target = 2, goldilocks_sigma = 2,
                                    p_decay = 0) {
  # p_decay: per-episode forgetting hazard for an UNCONFIRMED (not-yet-learned)
  # guess. Checked lazily, only when a word holding a guess is re-exposed --
  # not swept over all M words every episode -- so this stays O(1) per episode
  # and doesn't undermine guess-test's tractability advantage over the
  # eliminative learner. A guess surviving `elapsed` idle episodes (since it
  # was last (re)proposed) has decayed with probability 1-(1-p_decay)^elapsed,
  # i.e. a constant per-episode hazard applied retroactively over the gap.
  # Decay is scoped to unconfirmed guesses only: once current_guess[w]==w
  # (word_known[w] set), it is never revisited -- forgetting an established
  # word is a different, bigger claim than forgetting a tentative guess.
  # p_decay=0 (the default) reproduces the original model exactly.
  if (is.null(active_prob)) active_prob <- as.numeric(active)
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  current_guess <- rep(0L, M)  # current_guess[w]: referent word w currently claims (0 = none)
  claimed_by <- rep(0L, M)     # claimed_by[r]: word currently claiming referent r (0 = unclaimed)
  confirm_count <- rep(0L, M)  # streak of exposures in which word w's guess survived
  guess_since <- rep(0L, M)    # episode at which current_guess[w] was last (re)proposed
  word_known <- rep(FALSE, M)
  times_targeted <- rep(0L, M)
  episodes <- 0
  n_learned <- 0
  total <- M * (1 - epsilon)
  eps_to_learn_decile <- rep(FALSE, 9)
  deciles <- M * seq(.1, .9, .1)
  t0 <- Sys.time()
  censored <- FALSE

  while (n_learned < total) {
    if (episodes %% TIME_CHECK_EVERY == 0 && episodes > 0) {
      if (episodes >= max_episodes || as.numeric(Sys.time() - t0, units = "secs") >= max_seconds) {
        censored <- TRUE
        break
      }
    }
    target <- choose_target(M, probs, word_known, times_targeted, active_prob,
                             active_policy, choice_k, goldilocks_target, goldilocks_sigma)
    times_targeted[target] <- times_targeted[target] + 1L
    nontarg <- setdiff(1:M, target)
    if (fam_context) {
      familiar <- confirm_count[nontarg] + 1
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg] * familiar)
    } else {
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
    }
    cc <- c(target, distractors)  # target's true referent is "target" itself (1-1 convention)

    g <- current_guess[target]
    if (p_decay > 0 && g != 0L) {
      elapsed <- episodes - guess_since[target]
      if (elapsed > 0 && runif(1) < 1 - (1 - p_decay)^elapsed) {
        # forgotten during the gap since this guess was last checked in on
        claimed_by[g] <- 0L
        current_guess[target] <- 0L
        confirm_count[target] <- 0L
        g <- 0L
      }
    }
    if (g != 0L && !(g %in% cc)) {
      # disconfirmed: the true referent is always present, so a missing guess is wrong
      claimed_by[g] <- 0L
      current_guess[target] <- 0L
      confirm_count[target] <- 0L
      g <- 0L
    }
    if (g == 0L) {
      unclaimed <- cc[claimed_by[cc] == 0L]
      if (length(unclaimed) > 0) {
        newg <- if (length(unclaimed) == 1) unclaimed else sample(unclaimed, 1)
        current_guess[target] <- newg
        claimed_by[newg] <- target
        guess_since[target] <- episodes
      }
    } else {
      confirm_count[target] <- confirm_count[target] + 1L
    }

    if (!word_known[target] && current_guess[target] == target) {
      n_learned <- n_learned + 1
      word_known[target] <- TRUE
      for (dd in 1:9) if (n_learned >= deciles[dd] && !eps_to_learn_decile[dd]) eps_to_learn_decile[dd] <- episodes
    }
    episodes <- episodes + 1
  }
  c(eps_to_learn_decile, episodes, censored = as.numeric(censored))
}

learn_corpus_rankedfreq <- function(C, M, a = 1, uniform = TRUE, active = FALSE,
                                     fam_context = FALSE, epsilon = .01,
                                     max_episodes = Inf, max_seconds = Inf,
                                     active_prob = NULL, active_policy = "unknown",
                                     choice_k = Inf, goldilocks_target = 2, goldilocks_sigma = 2) {
  if (is.null(active_prob)) active_prob <- as.numeric(active)
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  hyp <- matrix(1, nrow = M, ncol = M)
  word_known <- rep(FALSE, M)
  times_targeted <- rep(0L, M)
  episodes <- 0
  n_learned <- 0
  total <- M * (1 - epsilon)
  eps_to_learn_decile <- rep(FALSE, 9)
  deciles <- M * seq(.1, .9, .1)
  t0 <- Sys.time()
  censored <- FALSE

  while (n_learned < total) {
    if (episodes %% TIME_CHECK_EVERY == 0 && episodes > 0) {
      if (episodes >= max_episodes || as.numeric(Sys.time() - t0, units = "secs") >= max_seconds) {
        censored <- TRUE
        break
      }
    }
    target <- choose_target(M, probs, word_known, times_targeted, active_prob,
                             active_policy, choice_k, goldilocks_target, goldilocks_sigma)
    times_targeted[target] <- times_targeted[target] + 1L
    nontarg <- setdiff(1:M, target)
    if (fam_context) {
      familiar <- 1 / (colSums(hyp) + 1)
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg] * familiar[nontarg])
    } else {
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
    }
    cc <- c(target, distractors)
    hyp[cc[1], cc] <- hyp[cc[1], cc] + 1
    max_cooc <- max(hyp[cc[1], ])
    if (hyp[cc[1], cc[1]] == max_cooc && !word_known[cc[1]]) {
      n_learned <- n_learned + 1
      word_known[cc[1]] <- TRUE
      for (dd in 1:9) if (n_learned >= deciles[dd] && !eps_to_learn_decile[dd]) eps_to_learn_decile[dd] <- episodes
    }
    episodes <- episodes + 1
  }
  c(eps_to_learn_decile, episodes, censored = as.numeric(censored))
}

# Backward-compatible with the earlier (pre-verification-script) fixed-reps API
# used by run_full_grid.R / run_guesstest_grid.R / run_rankedfreq_grid.R /
# make_final_figures.R. New code should prefer run_cell_budgeted() below, which
# adds the wall-clock stopping rule and per-replication censoring.
repeat_sim <- function(learner, C, M, a, uniform, active, fam_context, reps, seed = 982709,
                        active_prob = NULL, active_policy = "unknown", choice_k = Inf,
                        goldilocks_target = 2, goldilocks_sigma = 2) {
  set.seed(seed)
  fn <- LEARNERS[[learner]]
  out <- foreach(i = 1:reps, .combine = rbind,
                 .export = c("zipf_probs", "TIME_CHECK_EVERY", "choose_target", "learn_corpus_eliminative",
                             "learn_corpus_guesstest", "learn_corpus_rankedfreq")) %dopar%
    fn(C, M, a, uniform, active, fam_context, active_prob = active_prob, active_policy = active_policy,
       choice_k = choice_k, goldilocks_target = goldilocks_target, goldilocks_sigma = goldilocks_sigma)
  out[, 1:10, drop = FALSE]  # drop the new `censored` column for old callers expecting 10 cols
}

LEARNERS <- list(
  eliminative = learn_corpus_eliminative,
  guesstest = learn_corpus_guesstest,
  rankedfreq = learn_corpus_rankedfreq
)

# Run `reps` replications of one cell IN PARALLEL BATCHES of `ncores`, checking
# elapsed wall-clock time between batches and stopping once `cell_budget_seconds`
# is exceeded (keeping whatever reps have completed so far). This is the
# "per-cell wall-clock budget" stopping rule: fast cells naturally accumulate
# many reps, slow/near-intractable cells accumulate few (or zero) within budget,
# and no single pathological replication can hang indefinitely because each
# replication also carries its own `rep_max_seconds` hard cap (checked inside
# the learner itself every TIME_CHECK_EVERY episodes).
#
# `active_prob`/`active_policy`/`choice_k`/`goldilocks_*` are passed straight
# through to the learner (see "ACTIVE TARGET SELECTION" in the file header);
# leave them at their defaults to reproduce the original active/passive design.
#
# Returns a data.frame with one row per completed replication (both normal and
# individually-censored ones are included; censored replications are flagged in
# the `censored` column and should be excluded from mean/median summaries).
run_cell_budgeted <- function(learner, C, M, a, uniform, active, fam_context,
                               cell_budget_seconds, rep_max_seconds, reps_max = 10000,
                               ncores = max(1, parallel::detectCores() - 1),
                               seed = 982709, active_prob = NULL, active_policy = "unknown",
                               choice_k = Inf, goldilocks_target = 2, goldilocks_sigma = 2) {
  fn <- LEARNERS[[learner]]
  set.seed(seed)
  t_cell_start <- Sys.time()
  all_reps <- list()
  n_done <- 0

  repeat {
    elapsed <- as.numeric(Sys.time() - t_cell_start, units = "secs")
    if (elapsed >= cell_budget_seconds) break
    if (n_done >= reps_max) break
    batch_size <- min(ncores, reps_max - n_done)
    batch <- foreach(i = 1:batch_size, .combine = rbind,
                      .export = c("zipf_probs", "TIME_CHECK_EVERY", "choose_target", "learn_corpus_eliminative",
                                  "learn_corpus_guesstest", "learn_corpus_rankedfreq")) %dopar%
      fn(C, M, a, uniform, active, fam_context, max_seconds = rep_max_seconds,
         active_prob = active_prob, active_policy = active_policy, choice_k = choice_k,
         goldilocks_target = goldilocks_target, goldilocks_sigma = goldilocks_sigma)
    all_reps[[length(all_reps) + 1]] <- batch
    n_done <- n_done + batch_size
  }

  if (length(all_reps) == 0) {
    return(data.frame())  # budget too small even for one batch; caller should widen it
  }
  m <- do.call(rbind, all_reps)
  d <- as.data.frame(m)
  names(d) <- c(paste0("dec", 1:9), "episodes", "censored")
  d$model <- learner; d$C <- C; d$M <- M; d$a <- a
  d$active <- ifelse(active, "Active", "Passive")
  d$uniform <- ifelse(uniform, "Uniform", "Zipfian")
  d$fam_context <- ifelse(fam_context, "Familiar", "Random")
  d$cell_elapsed_secs <- as.numeric(Sys.time() - t_cell_start, units = "secs")
  d
}
