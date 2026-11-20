# Diagnostics for two guess-test results in the companion paper (Study 2):
#   (A) with familiar context at C = 100, fully active target selection
#       usually never finishes, while partially active learners do;
#   (B) frequency-weighted choice sets are very costly at C = 100 with random
#       context, and non-monotonically so (sets of 20-100 cost more than 5).
#
# gt_diag() reproduces learn_corpus_guesstest() (lock_k = 0, no decay) and
# records the learner's state every `trace_every` episodes, plus a final
# snapshot. It also supports two interventions:
#   rehearse_prob  on this proportion of episodes, target a known word
#                  (drawn by frequency) instead of choosing as usual;
#   fam_known_only familiar-context weights count confirmations of known
#                  words only (an oracle version of the familiarity signal).

source("learners.R")

gt_diag <- function(C, M = 1000, a = 1, fam_context = FALSE, active_prob = 1, choice_k = Inf,
                    active_policy = "unknown", within_pool_uniform = FALSE,
                    max_episodes = 1e6, trace_every = 5000, rehearse_prob = 0, fam_known_only = FALSE,
                    epsilon = 0.01) {
  probs <- zipf_probs(M, a)          # NB: indices are randomly permuted ranks
  freq_rank <- rank(-probs, ties.method = "first")
  blocked_targets <- 0; active_unknown_targets <- 0; repeat_targets <- 0
  current_guess <- integer(M); claimed_by <- integer(M); confirm_count <- integer(M)
  word_known <- logical(M); times_targeted <- integer(M)
  n_learned <- 0; total <- M * (1 - epsilon); episodes <- 0
  wasted <- 0; n_disconfirm <- 0
  trace <- list()

  snapshot <- function() {
    unk <- which(!word_known)
    g <- current_guess[unk]
    wrong <- g != 0L & g != unk
    # a wrong guess "blocks" another unknown word if it claims that word's own referent
    blocks_unknown <- wrong & !word_known[pmax(g, 1L)]
    own_claimed <- claimed_by[unk] != 0L & claimed_by[unk] != unk
    w <- probs * (confirm_count + 1)
    data.frame(episodes = episodes, n_learned = n_learned, n_unknown = length(unk),
               unk_no_guess = sum(g == 0L), unk_wrong = sum(wrong),
               unk_wrong_on_unknown_ref = sum(blocks_unknown),
               unk_own_ref_claimed = sum(own_claimed),
               conf_known = sum(confirm_count[word_known]), conf_unknown = sum(confirm_count[!word_known]),
               fam_weight_share_unknown_refs = if (fam_context) sum(w[!word_known]) / sum(w) else NA,
               wasted = wasted, n_disconfirm = n_disconfirm,
               median_rank_unknown = if (length(unk)) median(freq_rank[unk]) else NA,
               min_rank_unknown = if (length(unk)) min(freq_rank[unk]) else NA,
               unknown_targets = active_unknown_targets, blocked_targets = blocked_targets,
               repeat_targets = repeat_targets)
  }

  while (n_learned < total && episodes < max_episodes) {
    if (episodes %% trace_every == 0) trace[[length(trace) + 1]] <- snapshot()
    if (rehearse_prob > 0 && n_learned > 0 && runif(1) < rehearse_prob) {
      kn <- which(word_known)
      target <- if (length(kn) == 1) kn else sample(kn, 1, prob = probs[kn])
    } else if (within_pool_uniform && is.finite(choice_k)) {
      # pool drawn by frequency (what is at hand), but the unknown word is chosen uniformly within it
      pool <- sample(1:M, choice_k, prob = probs); up <- pool[!word_known[pool]]
      target <- if (length(up) == 0) { if (length(pool) == 1) pool else sample(pool, 1, prob = probs[pool]) }
                else if (length(up) == 1) up else sample(up, 1)
    } else {
      target <- choose_target(M, probs, word_known, times_targeted, active_prob, active_policy, choice_k)
    }
    if (word_known[target]) wasted <- wasted + 1 else {
      active_unknown_targets <- active_unknown_targets + 1
      # blocked: the word's own referent is held by another word, so it cannot be learned this episode
      if (claimed_by[target] != 0L && claimed_by[target] != target) blocked_targets <- blocked_targets + 1
      if (times_targeted[target] >= 10L) repeat_targets <- repeat_targets + 1
    }
    times_targeted[target] <- times_targeted[target] + 1L
    nontarg <- setdiff(1:M, target)
    if (fam_context) {
      cc_w <- if (fam_known_only) ifelse(word_known, confirm_count, 0L) else confirm_count
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg] * (cc_w[nontarg] + 1))
    } else {
      distractors <- sample(nontarg, C - 1, prob = probs[nontarg])
    }
    cc <- c(target, distractors)
    g <- current_guess[target]
    if (g != 0L && !(g %in% cc)) {
      claimed_by[g] <- 0L; current_guess[target] <- 0L; confirm_count[target] <- 0L; g <- 0L
      n_disconfirm <- n_disconfirm + 1
    }
    if (g == 0L) {
      unclaimed <- cc[claimed_by[cc] == 0L]
      if (length(unclaimed) > 0) {
        newg <- if (length(unclaimed) == 1) unclaimed else sample(unclaimed, 1)
        current_guess[target] <- newg; claimed_by[newg] <- target
      }
    } else {
      confirm_count[target] <- confirm_count[target] + 1L
    }
    if (!word_known[target] && current_guess[target] == target) {
      n_learned <- n_learned + 1; word_known[target] <- TRUE
    }
    episodes <- episodes + 1
  }
  final <- snapshot()
  unk <- which(!word_known)
  list(trace = do.call(rbind, trace), final = final, finished = n_learned >= total,
       unknown_words = data.frame(word = unk, freq_rank = freq_rank[unk],
                                  guess_rank = ifelse(current_guess[unk] > 0, freq_rank[pmax(current_guess[unk], 1L)], NA),
                                  claimer_rank = ifelse(claimed_by[unk] > 0, freq_rank[pmax(claimed_by[unk], 1L)], NA),
                                  claimer_known = ifelse(claimed_by[unk] > 0, word_known[pmax(claimed_by[unk], 1L)], NA),
                                  times_targeted = times_targeted[unk],
                                  claimer_targeted = ifelse(claimed_by[unk] > 0, times_targeted[pmax(claimed_by[unk], 1L)], NA)))
}
