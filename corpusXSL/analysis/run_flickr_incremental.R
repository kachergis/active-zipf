# Incremental cross-situational word learning on real Flickr8k captions,
# comparing passive sampling to an active learner that chooses BOTH which
# word to work on AND whether to "linger" on the current photo (elicit
# another of its 5 independent captions) or "move on" to a new photo.
#
# This is the naturalistic-data counterpart to the paper's synthetic
# incremental-learner simulations: each caption is a situation (a target word
# plus ~5 co-occurring content words as distractors), and a word is learned,
# eliminative-style, once the intersection of every caption it has appeared
# in narrows to the word itself.
#
# Mechanism: eliminative. cand[[w]] = intersection of the content-word sets
# of every processed caption containing w; w is learned when |cand[[w]]| == 1.
#
# Policies:
#   passive         -- uniform-random photo, one random caption, always move on
#   active_moveon   -- target the rarest not-yet-learned learnable word,
#                      sample a photo containing it, one caption, move on
#   active_linger   -- as active_moveon, but stay on the current photo and
#                      reveal another of its captions as long as the last
#                      caption shrank some unknown word's candidate set;
#                      re-target + new photo only when the photo stops paying off
#
# Cost: `move_cost` episodes are charged when switching to a new photo (on
# top of 1 per caption revealed), modeling the idea that eliciting another
# description of the scene in front of you is cheaper than getting a new
# scene built around a specific word. move_cost = 0 makes lingering vs moving
# on purely about information, not economy.

set.seed(1)
d <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/flickr_incremental_data.rds")
caps_by_photo <- d$caps_by_photo          # list[M] of list[5] of int vectors (content-word ids)
learnable <- d$learnable                   # int ids of the words we score learning of
appears_in_caps <- d$appears_in_caps
N <- length(d$content_words)
M <- length(caps_by_photo)
n_learn <- length(learnable)

# photos containing each word (for active target-directed photo sampling)
photos_with_word <- vector("list", N)
for (p in seq_len(M)) {
  wp <- unique(unlist(caps_by_photo[[p]]))
  for (w in wp) photos_with_word[[w]] <- c(photos_with_word[[w]], p)
}
# captions of photo p that contain word w
caps_with_word <- function(p, w) which(vapply(caps_by_photo[[p]], function(c) w %in% c, logical(1)))

run_learner <- function(policy, move_cost = 0, max_cost = 5e6, seed = 1,
                         checkpoints = c(.5, .9, .99)) {
  set.seed(seed)
  cand <- vector("list", N)
  known <- logical(N)
  n_known <- 0L
  cost <- 0
  ckpt_cost <- setNames(rep(NA_real_, length(checkpoints)), checkpoints)
  targets <- if (policy == "passive") NULL else learnable[order(appears_in_caps[learnable])]  # rarest first
  ti <- 1L
  cur_photo <- NA_integer_; cur_used <- integer(0)

  process_caption <- function(cw) {
    shrank <- FALSE
    for (w in cw) {
      if (known[w]) next
      if (is.null(cand[[w]])) { cand[[w]] <<- cw }
      else {
        new <- intersect(cand[[w]], cw)
        if (length(new) < length(cand[[w]])) { cand[[w]] <<- new; shrank <- TRUE }
      }
      if (!is.null(cand[[w]]) && length(cand[[w]]) == 1L && !known[w]) {
        known[w] <<- TRUE; n_known <<- n_known + 1L
      }
    }
    shrank
  }

  advance_target <- function() {
    while (ti <= length(targets) && known[targets[ti]]) ti <<- ti + 1L
    if (ti > length(targets)) NA_integer_ else targets[ti]
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
      tgt <- advance_target()
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
      shrank <- if (length(cw) > 0) process_caption(cw) else FALSE

      # linger decision
      if (policy == "active_moveon" || !shrank || length(cur_used) >= length(caps_by_photo[[cur_photo]])) {
        cur_photo <- NA_integer_  # force new photo next iter
      }
    }

    frac <- n_known / n_learn
    for (k in seq_along(checkpoints)) {
      if (is.na(ckpt_cost[k]) && frac >= checkpoints[k]) ckpt_cost[k] <- cost
    }
  }
  c(ckpt_cost, final_frac = n_known / n_learn, total_cost = cost)
}

REPS <- 30
policies <- list(
  passive            = list(policy = "passive",      move_cost = 0),
  active_moveon_mc0  = list(policy = "active_moveon", move_cost = 0),
  active_linger_mc0  = list(policy = "active_linger", move_cost = 0),
  active_moveon_mc1  = list(policy = "active_moveon", move_cost = 1),
  active_linger_mc1  = list(policy = "active_linger", move_cost = 1),
  active_moveon_mc3  = list(policy = "active_moveon", move_cost = 3),
  active_linger_mc3  = list(policy = "active_linger", move_cost = 3),
  active_moveon_mc10 = list(policy = "active_moveon", move_cost = 10),
  active_linger_mc10 = list(policy = "active_linger", move_cost = 10)
)

cat(sprintf("Flickr8k incremental learner: %d learnable content words, %d photos, %d reps/policy\n\n",
            n_learn, M, REPS))
res <- list()
for (nm in names(policies)) {
  pc <- policies[[nm]]
  runs <- sapply(seq_len(REPS), function(s)
    run_learner(pc$policy, move_cost = pc$move_cost, seed = s))
  m <- rowMeans(runs)
  res[[nm]] <- m
  cat(sprintf("%-18s  50%%: %10.0f   90%%: %10.0f   99%%: %10.0f   (final %.3f)\n",
              nm, m["0.5"], m["0.9"], m["0.99"], m["final_frac"]))
}

summ <- do.call(rbind, res)
saveRDS(summ, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/flickr_incremental_results.rds")
write.csv(summ, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/flickr_incremental_results.csv")
cat("\nsaved flickr_incremental_results.rds/.csv\n")
