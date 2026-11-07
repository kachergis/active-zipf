# More replications for the companion paper's noisiest comparisons
# (paper2/draft.tex), so that they can be reported with confidence intervals.
# Guess-test learning times at C = 10 are bimodal, so 30 runs per cell gave
# unstable medians and imprecise ramp-vs-blend ratios.
#
#   E3  eliminative learner, Study 3 ramps and constant blends (M = 1000,
#       C = 10, a = 1), 200 replications, all with the C++ learner
#       (elim_fast with ramp_T), in elim_ramp_cpp/. Supersedes the 30 R runs
#       in overnight_fill/G3 for the paper.
#   G2  guess-test learner, Study 2 conditions at C = 10 (both kinds of
#       context; all conditions except fully word-driven), replications
#       31-200, original criterion (lock_k = 0, into guesstest_paper2/S2) and
#       observable criterion (lock_k = 2, into guesstest_lock2/S2). Uniform
#       targeting with the original criterion was not in
#       run_guesstest_paper2.R, so its replications 1-30 (C = 10 and C = 100)
#       are added here, with the seeds run_guesstest_lock.R used for it.
#   G3  guess-test learner, Study 3 ramps and blends, replications 31-200,
#       both criteria (into guesstest_paper2/S3 and guesstest_lock2/S3).
#
# Replications 31+ use new seeds (the original formulas collide beyond 100
# replications); within a cell, the two criteria share seeds, as before.
# Resumable: finished jobs are skipped on restart.
#
# Usage:  Rscript run_more_reps.R   (then summarize_ci.R)

suppressMessages({library(parallel)})
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")

REPS <- 200; CAP <- 5e6
LOG <- "more_reps_progress.log"
logmsg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n",
                            sep = "", file = LOG, append = TRUE)
dec_or_na <- function(x) ifelse(x > 0, x, NA)
dir.create("elim_ramp_cpp", showWarnings = FALSE)

jobs <- list()
add_job <- function(dir, name, rep, seed, cost, fn) {
  jobs[[length(jobs) + 1]] <<- list(file = file.path(dir, sprintf("%s_rep%02d.rds", name, rep)),
                                    seed = seed, cost = cost, fn = fn)
}
gt <- function(lock_k, ...) learn_corpus_guesstest(M = 1000, a = 1, uniform = FALSE, max_episodes = CAP,
                                                    lock_k = lock_k, ...)
crit_dir <- c(`0` = "guesstest_paper2", `2` = "guesstest_lock2")

ramp_T <- c(1000, 2000, 3000, 5000, 8000, 12000, 20000)
blend_grid <- c(0, 0.02, 0.05, 0.075, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.90, 1)
ramp_fn <- function(dir, Tt) if (dir == "fwd") function(ep) pmin(1, ep / Tt) else function(ep) pmax(0, 1 - ep / Tt)
s3_row <- function(cond, Tt, ap, r, v) data.frame(condition = cond, T = Tt, active_prob = ap, rep = r,
                                                  dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]),
                                                  episodes = v[10], censored = v[11])

# ---- E3: eliminative ramps and blends, C++
probs <- zipf_probs(1000, 1)
for (r in seq_len(REPS)) {
  for (dir in c("fwd", "rev")) for (Tt in ramp_T) local({
    dir <- dir; Tt <- Tt; r <- r
    add_job("elim_ramp_cpp", sprintf("ramp_%s_T%d", dir, Tt), r, 6e6 + 1e5 * (dir == "rev") + Tt + r, 1,
            function() s3_row(sprintf("ramp_%s", dir), Tt, NA, r,
                              elim_fast(10, 1000, probs, FALSE, FALSE, ramp_T = Tt,
                                        ramp_dir = if (dir == "fwd") 1L else -1L)))
  })
  for (ap in blend_grid) local({
    ap <- ap; r <- r
    add_job("elim_ramp_cpp", sprintf("blend_%.3f", ap), r, 6.5e6 + round(1000 * ap) * 1000 + r, 1,
            function() s3_row("blend", NA, ap, r, elim_fast(10, 1000, probs, FALSE, FALSE, active_prob = ap)))
  })
}

# ---- G2: guess-test Study 2, C = 10 (and uniform, original criterion, C = 100)
# condition indices as in run_guesstest_lock.R (heard = 12 omitted)
settings <- data.frame(C = c(10, 10, 100, 100), fam = c(FALSE, TRUE, FALSE, TRUE))
conds <- list(
  passive    = list(active_prob = 0),
  `ap=0.25`  = list(active_prob = 0.25),
  `ap=0.50`  = list(active_prob = 0.50),
  `ap=0.75`  = list(active_prob = 0.75),
  active     = list(active_prob = 1),
  goldilocks = list(active_prob = 1, active_policy = "goldilocks"),
  `k=100`    = list(active_prob = 1, choice_k = 100),
  `k=50`     = list(active_prob = 1, choice_k = 50),
  `k=20`     = list(active_prob = 1, choice_k = 20),
  `k=5`      = list(active_prob = 1, choice_k = 5),
  uniform    = list(active_prob = 1, active_policy = "uniform"))
s2_job <- function(lock_k, s, ci, r, seed) local({
  C <- settings$C[s]; fam <- settings$fam[s]; cc <- conds[[ci]]; label <- names(conds)[ci]
  setting <- sprintf("C=%d, %s context", C, if (fam) "familiar" else "random")
  add_job(file.path(crit_dir[[as.character(lock_k)]], "S2"),
          sprintf("C%d_%s_%s", C, if (fam) "fam" else "rand", gsub("[=.]", "", label)), r, seed,
          if (C == 100) 20 else if (label %in% c("uniform", "goldilocks", "active")) 5 else 2,
          function() {
            v <- do.call(gt, c(list(lock_k = lock_k, C = C, fam_context = fam), cc))
            data.frame(setting = setting, label = label, rep = r, dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]),
                       episodes = v[10], censored = v[11])
          })
})
for (lock_k in c(0, 2)) for (s in 1:2) for (ci in seq_along(conds)) for (r in 31:REPS)
  s2_job(lock_k, s, ci, r, 3e6 + 1e5 * s + 1e3 * ci + r)
for (s in 1:4) for (r in 1:30) s2_job(0, s, 11, r, 700000 + 10000 * s + 100 * 11 + r)

# ---- G3: guess-test Study 3
for (lock_k in c(0, 2)) for (r in 31:REPS) {
  d <- file.path(crit_dir[[as.character(lock_k)]], "S3")
  for (dir in c("fwd", "rev")) for (Tt in ramp_T) local({
    dir <- dir; Tt <- Tt; r <- r; lock_k <- lock_k
    add_job(d, sprintf("ramp_%s_T%d", dir, Tt), r, 4e6 + 1e5 * (dir == "rev") + Tt + r, 3,
            function() s3_row(sprintf("ramp_%s", dir), Tt, NA, r,
                              gt(lock_k, C = 10, active_prob_fn = ramp_fn(dir, Tt))))
  })
  for (ap in blend_grid) local({
    ap <- ap; r <- r; lock_k <- lock_k
    add_job(d, sprintf("blend_%.3f", ap), r, 4.5e6 + round(1000 * ap) * 1000 + r, if (ap == 1) 5 else 3,
            function() s3_row("blend", NA, ap, r, gt(lock_k, C = 10, active_prob = ap)))
  })
}

# ---- run
todo <- Filter(function(j) !file.exists(j$file), jobs)
todo <- todo[order(-vapply(todo, `[[`, numeric(1), "cost"))]
ncores <- max(1, detectCores() - 2)
logmsg("start: %d jobs total, %d remaining, %d cores", length(jobs), length(todo), ncores)
t0 <- Sys.time()
res <- mclapply(todo, function(j) {
  t1 <- Sys.time()
  set.seed(j$seed)
  out <- tryCatch(j$fn(), error = function(e) e)
  if (inherits(out, "error")) {
    logmsg("ERROR %s: %s", j$file, conditionMessage(out))
    return(FALSE)
  }
  tmp <- paste0(j$file, ".tmp")
  saveRDS(out, tmp); file.rename(tmp, j$file)
  logmsg("done %s (%.0fs)", j$file, as.numeric(Sys.time() - t1, units = "secs"))
  TRUE
}, mc.cores = ncores, mc.preschedule = FALSE)
ok <- vapply(res, isTRUE, logical(1))
logmsg("finished: %d ok, %d failed, %.1f min", sum(ok), sum(!ok), as.numeric(Sys.time() - t0, units = "mins"))
