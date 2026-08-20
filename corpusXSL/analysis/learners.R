# Three cross-situational word-learning mechanisms, each crossed with:
#   - target selection: passive (random) vs. active (choose an unknown word)
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
# All three learners accept `max_seconds` (wall-clock cap on a single replication)
# and `max_episodes` (hard episode-count cap), whichever binds first. A replication
# that hits either cap returns with `censored=1` and whatever deciles/episode count
# it had reached; it is NOT a valid draw of "episodes to reach 99%" and should be
# excluded from mean/median calculations (but its censoring should be reported --
# see summarize_verification_grid.R).

require(foreach)
require(doParallel)

# p_k propto k^-a over frequency rank k=1..M; a=0 reduces to uniform.
zipf_probs <- function(M, a) {
  if (a == 0) return(rep(1 / M, M))
  probs <- (1:M)^(-a)
  probs <- probs / sum(probs)
  sample(probs, length(probs))  # randomize which referent index gets which rank
}

# How often (in episodes) each learner checks the wall-clock. Checking every
# iteration would make Sys.time() overhead dominate for cheap models; checking
# too rarely makes max_seconds imprecise. 2000 is a reasonable middle ground
# across all three models at M in [1000, 10000].
TIME_CHECK_EVERY <- 2000L

learn_corpus_eliminative <- function(C, M, a = 1, uniform = TRUE, active = FALSE,
                                      fam_context = FALSE, epsilon = .01,
                                      max_episodes = Inf, max_seconds = Inf) {
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  hyp <- matrix(1, nrow = M, ncol = M)
  word_known <- rep(FALSE, M)
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
    if (active) {
      unknown <- which(!word_known)
      target <- if (length(unknown) > 1) sample(unknown, 1, prob = probs[unknown]) else unknown
    } else {
      target <- sample(1:M, 1, prob = probs)
    }
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
                                    max_episodes = Inf, max_seconds = Inf) {
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  current_guess <- rep(0L, M)  # current_guess[w]: referent word w currently claims (0 = none)
  claimed_by <- rep(0L, M)     # claimed_by[r]: word currently claiming referent r (0 = unclaimed)
  confirm_count <- rep(0L, M)  # streak of exposures in which word w's guess survived
  word_known <- rep(FALSE, M)
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
    if (active) {
      unknown <- which(!word_known)
      target <- if (length(unknown) > 1) sample(unknown, 1, prob = probs[unknown]) else unknown
    } else {
      target <- sample(1:M, 1, prob = probs)
    }
    nontarg <- setdiff(1:M, target)
    if (fam_context) {
      familiar <- confirm_count[nontarg] + 1
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg] * familiar)
    } else {
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
    }
    cc <- c(target, distractors)  # target's true referent is "target" itself (1-1 convention)

    g <- current_guess[target]
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
                                     max_episodes = Inf, max_seconds = Inf) {
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  hyp <- matrix(1, nrow = M, ncol = M)
  word_known <- rep(FALSE, M)
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
    if (active) {
      unknown <- which(!word_known)
      target <- if (length(unknown) > 1) sample(unknown, 1, prob = probs[unknown]) else unknown
    } else {
      target <- sample(1:M, 1, prob = probs)
    }
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
repeat_sim <- function(learner, C, M, a, uniform, active, fam_context, reps, seed = 982709) {
  set.seed(seed)
  fn <- LEARNERS[[learner]]
  out <- foreach(i = 1:reps, .combine = rbind,
                 .export = c("zipf_probs", "TIME_CHECK_EVERY", "learn_corpus_eliminative",
                             "learn_corpus_guesstest", "learn_corpus_rankedfreq")) %dopar%
    fn(C, M, a, uniform, active, fam_context)
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
# Returns a data.frame with one row per completed replication (both normal and
# individually-censored ones are included; censored replications are flagged in
# the `censored` column and should be excluded from mean/median summaries).
run_cell_budgeted <- function(learner, C, M, a, uniform, active, fam_context,
                               cell_budget_seconds, rep_max_seconds, reps_max = 10000,
                               ncores = max(1, parallel::detectCores() - 1),
                               seed = 982709) {
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
                      .export = c("zipf_probs", "TIME_CHECK_EVERY", "learn_corpus_eliminative",
                                  "learn_corpus_guesstest", "learn_corpus_rankedfreq")) %dopar%
      fn(C, M, a, uniform, active, fam_context, max_seconds = rep_max_seconds)
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
