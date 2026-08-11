# Two cross-situational word-learning mechanisms, both crossed with:
#   - target selection: passive (random) vs. active (choose an unknown word)
#   - context selection: random distractors vs. familiar (already-partly-known) distractors
#   - word frequency distribution: uniform, or Zipfian with a general exponent a
#     (p_k propto k^-a, matching the parametrization used in the paper's analytical
#     fast-mapping section, rather than the earlier fixed Zipf-Mandelbrot form)
#
# Model 1: "Eliminative" (as in corpusXSL_eliminative_active_parallel.R) -- tracks,
#   for each word, the full set of referents consistent with every past exposure;
#   learned once that set narrows to a single referent.
#
# Model 2: "Guess-test" -- a distinct, propose-and-revise mutual-exclusivity learner.
#   Each word holds at most one current guessed referent. A guess is only made when
#   the word has none, by picking an "unclaimed" referent (not currently guessed by
#   any other word) from the current situation. A held guess is discarded (and must
#   be re-proposed) if a later exposure to that word does not include it -- since the
#   true referent is always present whenever its word is spoken (the standard
#   assumption in this modeling tradition), a guess that goes missing must be wrong.
#   This is a cheaper, more error-prone mechanism than full elimination (it tracks one
#   hypothesis instead of the whole candidate set), closer to the "propose and verify"
#   family of models cited in the Introduction. NOTE: the original
#   corpusXSL/Rsimulations/corpusXSL_guesstest.R never checked guesses against the
#   true word-referent mapping (mutual exclusivity alone was used to decide when a
#   word counted as "learned"), so it was not actually measuring correct learning.
#   This version fixes that: a word only counts as learned once its held guess matches
#   its true referent.

require(foreach)
require(doParallel)

# p_k propto k^-a over frequency rank k=1..M; a=0 reduces to uniform.
zipf_probs <- function(M, a) {
  if (a == 0) return(rep(1 / M, M))
  probs <- (1:M)^(-a)
  probs <- probs / sum(probs)
  sample(probs, length(probs))  # randomize which referent index gets which rank
}

learn_corpus_eliminative <- function(C, M, a = 1, uniform = TRUE, active = FALSE,
                                      fam_context = FALSE, epsilon = .01) {
  probs <- if (uniform) rep(1 / M, M) else zipf_probs(M, a)

  hyp <- matrix(1, nrow = M, ncol = M)
  word_known <- rep(FALSE, M)
  episodes <- 0
  n_learned <- 0
  total <- M * (1 - epsilon)
  eps_to_learn_decile <- rep(FALSE, 9)
  deciles <- M * seq(.1, .9, .1)

  while (n_learned < total) {
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
  c(eps_to_learn_decile, episodes)
}

learn_corpus_guesstest <- function(C, M, a = 1, uniform = TRUE, active = FALSE,
                                    fam_context = FALSE, epsilon = .01, max_episodes = Inf) {
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

  while (n_learned < total && episodes < max_episodes) {
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
  c(eps_to_learn_decile, episodes)
}

repeat_sim <- function(learner, C, M, a, uniform, active, fam_context, reps, seed = 982709) {
  set.seed(seed)
  fn <- if (learner == "eliminative") learn_corpus_eliminative else learn_corpus_guesstest
  foreach(i = 1:reps, .combine = rbind,
          .export = c("zipf_probs", "learn_corpus_eliminative", "learn_corpus_guesstest")) %dopar%
    fn(C, M, a, uniform, active, fam_context)
}

run_sim_grid <- function(learner, Cs, Ms, As, uniform, active, fam_context, reps = 50) {
  d <- data.frame(sim = NA, model = NA, C = NA, M = NA, a = NA, active = NA, uniform = NA,
                   fam_context = NA, dec1 = NA, dec2 = NA, dec3 = NA, dec4 = NA, dec5 = NA,
                   dec6 = NA, dec7 = NA, dec8 = NA, dec9 = NA, p99 = NA)
  a_list <- if (uniform) c(0) else As
  for (m in Ms) {
    for (c in Cs) {
      if (c < m) {
        for (a in a_list) {
          results <- repeat_sim(learner, c, m, a, uniform, active, fam_context, reps = reps)
          for (row in 1:nrow(results)) {
            d <- rbind(d, c(sim = row, model = learner, C = c, M = m, a = a, active = active,
                             uniform = uniform, fam_context = fam_context, results[row, ]))
          }
        }
      }
    }
  }
  d <- na.omit(d)
  d$C <- as.numeric(d$C); d$M <- as.numeric(d$M); d$a <- as.numeric(d$a)
  for (col in c("dec1", "dec2", "dec3", "dec4", "dec5", "dec6", "dec7", "dec8", "dec9", "p99")) {
    d[[col]] <- as.numeric(d[[col]])
  }
  d$active <- ifelse(d$active == 1, "Active", "Passive")
  d$uniform <- ifelse(d$uniform == 1, "Uniform", "Zipfian")
  d$fam_context <- ifelse(d$fam_context == 1, "Familiar", "Random")
  d
}
