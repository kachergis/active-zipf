# Developmental mixture model for cross-situational word learning.
#
# Real word learning involves at least three types of episodes:
#
#   PASSIVE   -- overheard speech with no particular referential grounding.
#                Target drawn from Zipfian distribution; C-1 random distractors.
#
#   JOINT-ATTENTION (JT) -- caregiver directs child's attention to a named
#                referent (pointing, showing, naming a held object). The
#                referent is unambiguous: implemented here as a C=1 scene
#                containing only the target's true referent, so the existing
#                update logic correctly sets (or confirms) the target's guess
#                without special-casing the learner. The caregiver selects the
#                target word by one of three policies (see jt_policy below).
#
#   CHILD-INITIATED (CI) -- the child selects a target by their own active
#                policy (e.g., picks up an object, points, asks "what's that?").
#                Identical to the existing "active" episode in learners.R;
#                passes through to choose_target().
#
# The mixing proportions can vary over developmental time via pi_p_fn and
# pi_jt_fn (functions of episode count). The CI proportion is the remainder:
#   pi_ci(t) = 1 - pi_p_fn(t) - pi_jt_fn(t) (must be >= 0 for all t).
#
# Key questions addressed in explore_mixture.Rmd:
#   1. Does even a small fraction of JT episodes protect the guess-test learner
#      against self-confirming stalls that plague pure active learning?
#   2. Which caregiver policy (frequency-weighted, gap-filling, heard-first)
#      provides the most leverage?
#   3. Does the developmental ramp (early passive+JT -> later CI+JT) outperform
#      a constant blend or a simple passive-to-active ramp (Study 4 of paper 2)?

source("learners.R")
# Compile the Rcpp version on first load (fast path for M >= 5000).
if (!exists("mixture_fast")) Rcpp::sourceCpp("mixture_learner_fast.cpp")

# learn_corpus_mixture_fast(): same model as learn_corpus_mixture() but
# implemented in C++ via mixture_fast(). Accepts the same ramp-function
# arguments and translates them to the linear-ramp parameters that
# mixture_fast() expects. For non-linear schedules (make_sigmoid_ramp etc.)
# the ramp is evaluated at t=0 and t=T_ramp and a linear approximation is
# used -- good enough for exploratory runs; use the R version for exact
# sigmoid schedules.
#
# Returns the same 11-element vector as learn_corpus_mixture().
learn_corpus_mixture_fast <- function(
    C, M, a = 1, epsilon = 0.01,
    pi_p_fn  = function(t) 0.5,
    pi_jt_fn = function(t) 0.3,
    jt_policy  = c("frequency", "gap", "heard"),
    lock_k = 2,
    T_ramp = 1e5,          # reference T for linear approximation
    max_episodes = 5e6) {

  jt_policy <- match.arg(jt_policy)
  jt_code   <- c(frequency = 0L, gap = 1L, heard = 2L)[jt_policy]

  probs <- zipf_probs(M, a)

  mixture_fast(
    C          = C,
    M          = M,
    probs      = probs,
    pi_p_start  = pi_p_fn(0),
    pi_p_end    = pi_p_fn(T_ramp),
    pi_jt_start = pi_jt_fn(0),
    pi_jt_end   = pi_jt_fn(T_ramp),
    T_ramp     = T_ramp,
    jt_policy  = jt_code,
    lock_k     = lock_k,
    epsilon    = epsilon,
    max_episodes = max_episodes
  )
}

# Returns a function(t) -> value that ramps linearly from `start` to `end`
# over `T_ramp` episodes, then holds at `end`.
make_ramp <- function(start, end, T_ramp) {
  function(t) start + (end - start) * min(1.0, t / T_ramp)
}

# Returns a function(t) -> value that ramps via a logistic curve, reaching
# midpoint at t = T_ramp/2.
make_sigmoid_ramp <- function(start, end, T_ramp, steepness = 10) {
  function(t) {
    r <- 1 / (1 + exp(-steepness * (t / T_ramp - 0.5)))
    start + (end - start) * r
  }
}

# Guess-test learner with a three-way episode mixture and an optional
# developmental ramp. Internally reuses the same guess/claim bookkeeping as
# learn_corpus_guesstest(); JT episodes are plain C=1 trials (the caregiver
# presents only the target's referent, leaving no ambiguity).
#
# Parameters:
#   C             context size for PASSIVE and CI episodes
#   M             vocabulary size
#   a             Zipf exponent
#   pi_p_fn       function(t) -> proportion of passive episodes at time t
#   pi_jt_fn      function(t) -> proportion of JT episodes at time t
#   jt_policy     how the caregiver selects the JT target:
#                   "frequency" -- frequency-weighted over all words (reflects
#                                  that caregivers talk about common things)
#                   "gap"       -- frequency-weighted among unknown words only
#                                  (responsive teacher model)
#                   "heard"     -- frequency-weighted among words the child has
#                                  heard at least once but not yet learned
#                                  (expansion / responsive-naming)
#   ci_policy     active-selection policy for CI episodes: passed through to
#                 choose_target() as active_policy; default "unknown" (frequency-
#                 weighted among unknowns, the standard active learner)
#   lock_k        confirmation threshold (0 = original criterion; 2 = observable
#                 lock-in, as in paper 2's primary guess-test results)
#   epsilon, max_episodes, max_seconds  as in learn_corpus_guesstest
learn_corpus_mixture <- function(
    C, M, a = 1, epsilon = 0.01,
    pi_p_fn  = function(t) 0.5,
    pi_jt_fn = function(t) 0.3,
    jt_policy  = c("frequency", "gap", "heard"),
    ci_policy  = c("unknown", "goldilocks", "uniform"),
    lock_k = 2,
    max_episodes = 5e6, max_seconds = Inf) {

  jt_policy <- match.arg(jt_policy)
  ci_policy <- match.arg(ci_policy)

  probs <- zipf_probs(M, a)

  current_guess <- rep(0L, M)
  claimed_by    <- rep(0L, M)
  confirm_count <- rep(0L, M)
  word_known    <- rep(FALSE, M)
  times_targeted <- rep(0L, M)

  n_learned <- 0L
  total <- as.integer(M * (1 - epsilon))
  episodes <- 0L
  eps_to_learn_decile <- rep(NA_real_, 9)
  deciles <- M * seq(0.1, 0.9, 0.1)
  t0 <- Sys.time()
  censored <- FALSE

  believed_known <- function() if (lock_k > 0) confirm_count >= lock_k else word_known

  # Apply a single guess-test update: target's referent is always index `tgt`;
  # `scene` is an integer vector of referent indices present in the scene.
  do_update <- function(tgt, scene) {
    times_targeted[tgt] <<- times_targeted[tgt] + 1L
    g <- current_guess[tgt]
    bk <- believed_known()
    # Disconfirm a wrong guess (true referent is always present in scene)
    if (g != 0L && !(g %in% scene)) {
      claimed_by[g] <<- 0L
      current_guess[tgt] <<- 0L
      confirm_count[tgt] <<- 0L
      g <- 0L
    }
    if (g == 0L) {
      unclaimed <- scene[claimed_by[scene] == 0L]
      if (length(unclaimed) > 0) {
        ng <- if (length(unclaimed) == 1L) unclaimed else sample(unclaimed, 1L)
        current_guess[tgt] <<- ng
        claimed_by[ng] <<- tgt
      }
    } else {
      confirm_count[tgt] <<- confirm_count[tgt] + 1L
    }
    # Score as learned
    if (!word_known[tgt] && current_guess[tgt] == tgt &&
        (lock_k == 0L || confirm_count[tgt] >= lock_k)) {
      n_learned  <<- n_learned + 1L
      word_known[tgt] <<- TRUE
      for (d in seq_len(9L))
        if (is.na(eps_to_learn_decile[d]) && n_learned >= deciles[d])
          eps_to_learn_decile[d] <<- episodes
    }
  }

  while (n_learned < total) {
    if (episodes %% TIME_CHECK_EVERY == 0L && episodes > 0L) {
      if (episodes >= max_episodes ||
          as.numeric(Sys.time() - t0, units = "secs") >= max_seconds) {
        censored <- TRUE
        break
      }
    }

    pi_p  <- pi_p_fn(episodes)
    pi_jt <- pi_jt_fn(episodes)
    pi_ci <- max(0, 1 - pi_p - pi_jt)
    type  <- sample.int(3L, 1L, prob = c(pi_p, pi_jt, pi_ci))

    if (type == 1L) {
      # -- PASSIVE: target from Zipfian, random distractors --
      tgt <- sample.int(M, 1L, prob = probs)
      distractors <- sample(setdiff(seq_len(M), tgt), C - 1L, prob = probs[setdiff(seq_len(M), tgt)])
      do_update(tgt, c(tgt, distractors))

    } else if (type == 2L) {
      # -- JOINT ATTENTION: caregiver selects target; scene = {tgt} only --
      bk <- believed_known()
      pool <- switch(jt_policy,
        "frequency" = seq_len(M),
        "gap"       = which(!bk),
        "heard"     = which(!bk & times_targeted > 0L)
      )
      if (length(pool) == 0L) {
        # Fall back to passive if the caregiver pool is empty
        tgt <- sample.int(M, 1L, prob = probs)
        distractors <- sample(setdiff(seq_len(M), tgt), C - 1L, prob = probs[setdiff(seq_len(M), tgt)])
        do_update(tgt, c(tgt, distractors))
      } else {
        tgt <- if (length(pool) == 1L) pool else sample(pool, 1L, prob = probs[pool])
        # C=1 scene: only the true referent, so wrong guesses are always disconfirmed
        do_update(tgt, tgt)
      }

    } else {
      # -- CHILD-INITIATED: active target selection, random context --
      bk <- believed_known()
      tgt <- choose_target(M, probs, bk, times_targeted, active_prob = 1,
                           active_policy = ci_policy)
      distractors <- sample(setdiff(seq_len(M), tgt), C - 1L, prob = probs[setdiff(seq_len(M), tgt)])
      do_update(tgt, c(tgt, distractors))
    }

    episodes <- episodes + 1L
  }

  c(eps_to_learn_decile, episodes = episodes, censored = as.numeric(censored))
}

# Convenience wrapper: run `reps` replications of learn_corpus_mixture in
# parallel (mclapply), saving per-run .rds to `out_dir` for resumability.
run_mixture_grid <- function(conditions, reps, out_dir, ncores = max(1L, parallel::detectCores() - 1L)) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  jobs <- do.call(rbind, lapply(seq_len(nrow(conditions)), function(i)
    data.frame(conditions[i, , drop = FALSE], rep = seq_len(reps), stringsAsFactors = FALSE)))
  jobs$seed <- seq_len(nrow(jobs)) * 100L + 42L

  parallel::mclapply(seq_len(nrow(jobs)), function(j) {
    jb  <- jobs[j, ]
    rds <- file.path(out_dir, sprintf("%s_rep%03d.rds", jb$label, jb$rep))
    if (file.exists(rds)) return(invisible(NULL))
    set.seed(jb$seed)
    res <- learn_corpus_mixture(
      C = jb$C, M = jb$M, a = jb$a, epsilon = 0.01,
      pi_p_fn  = eval(parse(text = jb$pi_p_fn_expr)),
      pi_jt_fn = eval(parse(text = jb$pi_jt_fn_expr)),
      jt_policy = jb$jt_policy, ci_policy = "unknown",
      lock_k = jb$lock_k, max_episodes = jb$max_episodes)
    saveRDS(data.frame(label = jb$label, rep = jb$rep, as.list(res)), rds)
  }, mc.cores = ncores, mc.preschedule = FALSE)

  rds_files <- list.files(out_dir, "\\.rds$", full.names = TRUE)
  do.call(rbind, lapply(rds_files, readRDS))
}
