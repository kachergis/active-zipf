# Guess-test learner on Flickr8k captions: the counterpart of the eliminative
# learner in run_flickr_incremental.R, with the same policies (passive, active
# move-on, active linger), the same move_cost accounting, and the same scored
# vocabulary (the eliminative-learnable content words).
#
# Each caption is a situation, and updates every content word it contains,
# in random order: a word holding a guess that is absent from the caption
# discards it (its true referent, the word itself, is always present); a word
# without a guess proposes a referent from the caption that no other word
# currently claims. A word is learned once its guess is correct, and then keeps
# its claim. "Lingering" continues on the current photo for as long as the
# last caption changed some unlearned word's guess.
#
# Active targeting differs from the eliminative version, which always targets
# the rarest unlearned word. For guess-test that rule deadlocks: a rare word
# whose wrong guess appears in every caption containing it is never
# disconfirmed, and a rarest-first learner keeps returning to it forever. The
# active learner here instead samples its target among unlearned learnable
# words in proportion to their frequency (number of captions), as the
# synthetic learners in learners.R do.
#
# lock_k (default 0, the model above): with lock_k > 0 the learner treats a
# word as known only once its guess has survived lock_k consecutive captions
# containing the word -- something it can observe -- as in learners.R. Every
# word is then processed whenever it appears (a locked wrong guess can still
# be disconfirmed, dropping its lock); active targeting draws among learnable
# words not yet locked; and a word is scored as learned once its guess is
# locked and correct. Lingering continues while the last caption changed some
# unlocked word's guess or advanced its confirmations toward the lock.
#
# Source this file; it evaluates the data setup of run_flickr_incremental.R
# (everything before its run loop) and defines run_learner_gt().

local({
  src <- readLines("run_flickr_incremental.R")
  eval(parse(text = src[seq_len(grep("^REPS <- 30", src) - 1)]), envir = globalenv())
})

run_learner_gt <- function(policy, move_cost = 0, max_cost = 5e6, seed = 1,
                           checkpoints = c(.5, .9, .99), lock_k = 0) {
  set.seed(seed)
  guess <- integer(N)        # guess[w]: word (referent) w currently claims, 0 = none
  claimed_by <- integer(N)   # claimed_by[r]: word claiming referent r, 0 = unclaimed
  known <- logical(N)        # scored as learned
  conf <- integer(N)         # consecutive confirmations of the current guess (lock_k > 0)
  believed <- function(w) if (lock_k > 0) conf[w] >= lock_k else known[w]
  is_learnable <- logical(N); is_learnable[learnable] <- TRUE
  n_known <- 0L
  cost <- 0
  ckpt_cost <- setNames(rep(NA_real_, length(checkpoints)), checkpoints)
  cur_photo <- NA_integer_; cur_used <- integer(0)
  tgt <- NA_integer_

  process_caption <- function(cw) {
    changed <- FALSE
    for (w in if (length(cw) > 1) sample(cw) else cw) {
      if (known[w]) next
      g <- guess[w]
      if (g != 0L && !(g %in% cw)) {
        claimed_by[g] <<- 0L; guess[w] <<- 0L; g <- 0L; conf[w] <<- 0L
        if (!believed(w)) changed <- TRUE
      }
      if (g == 0L) {
        unclaimed <- cw[claimed_by[cw] == 0L]
        if (length(unclaimed) > 0) {
          ng <- if (length(unclaimed) == 1) unclaimed else sample(unclaimed, 1)
          guess[w] <<- ng; claimed_by[ng] <<- w; changed <- TRUE
        }
      } else if (lock_k > 0) {
        if (conf[w] < lock_k) changed <- TRUE
        conf[w] <<- conf[w] + 1L
      }
      if (guess[w] == w && (lock_k == 0 || conf[w] >= lock_k)) {
        known[w] <<- TRUE
        if (is_learnable[w]) n_known <<- n_known + 1L
      }
    }
    changed
  }

  advance_target <- function() {
    pool <- learnable[!(if (lock_k > 0) conf[learnable] >= lock_k else known[learnable])]
    if (length(pool) == 0) return(NA_integer_)
    if (length(pool) == 1) pool else sample(pool, 1, prob = appears_in_caps[pool])
  }

  repeat {
    if (n_known >= n_learn || cost >= max_cost) break

    if (policy == "passive") {
      p <- sample.int(M, 1)
      caps <- caps_by_photo[[p]]
      cw <- caps[[sample.int(length(caps), 1)]]
      cost <- cost + 1
      if (length(cw) > 0) process_caption(cw)
    } else {
      # keep the current target while lingering on its photo; otherwise draw a new one
      if (is.na(tgt) || believed(tgt) || is.na(cur_photo)) tgt <- advance_target()
      if (is.na(tgt)) break
      need_new_photo <- is.na(cur_photo) || length(cur_used) >= length(caps_by_photo[[cur_photo]]) ||
        !(tgt %in% unlist(caps_by_photo[[cur_photo]]))
      if (need_new_photo) {
        pw <- photos_with_word[[tgt]]
        cur_photo <- if (length(pw) == 1) pw else pw[sample.int(length(pw), 1)]
        cur_used <- integer(0)
        cost <- cost + move_cost
      }
      avail <- setdiff(seq_along(caps_by_photo[[cur_photo]]), cur_used)
      with_tgt <- intersect(avail, caps_with_word(cur_photo, tgt))
      pick <- if (length(with_tgt) > 0) {
        if (length(with_tgt) == 1) with_tgt else sample(with_tgt, 1)
      } else {
        if (length(avail) == 1) avail else sample(avail, 1)
      }
      cur_used <- c(cur_used, pick)
      cw <- caps_by_photo[[cur_photo]][[pick]]
      cost <- cost + 1
      changed <- if (length(cw) > 0) process_caption(cw) else FALSE
      if (policy == "active_moveon" || !changed || length(cur_used) >= length(caps_by_photo[[cur_photo]])) {
        cur_photo <- NA_integer_
      }
    }

    frac <- n_known / n_learn
    for (k in seq_along(checkpoints)) {
      if (is.na(ckpt_cost[k]) && frac >= checkpoints[k]) ckpt_cost[k] <- cost
    }
  }
  c(ckpt_cost, final_frac = n_known / n_learn, total_cost = cost)
}
