# Extends Studies 2-4 of the companion paper (paper2/draft.tex), which used
# only the eliminative learner, to the guess-test learner.
#
#   S2  partial autonomy, Goldilocks targeting, bounded choice sets: four
#       settings, M = 1000, a = 1 -- C = 10 and C = 100, each with random and
#       familiar context. (The eliminative study used a = 0.75 for C = 100 with
#       random context, because eliminative learning never finishes there at
#       a = 1; guess-test learning does.)
#   S3  developmental ramps (forward and reverse, seven timescales) and
#       constant blends at 15 levels of autonomy, C = 10, a = 1, for the exact
#       matched-autonomy comparison.
#   S4  Flickr8k captions: passive, and active move-on vs. linger at
#       move_cost 0, 1, 2, 3, 5, 10 (flickr_guesstest.R).
#
# Resumable, like run_overnight_fill.R: every replication is saved to
# guesstest_paper2/<study>/<job>.rds and finished jobs are skipped on restart.
#
# Usage:  Rscript run_guesstest_paper2.R            # run (or resume) + summarize
#         Rscript run_guesstest_paper2.R smoke      # tiny caps/reps, separate dir
#         Rscript run_guesstest_paper2.R summarize  # summaries only

suppressMessages({library(dplyr); library(parallel)})
source("learners.R")
source("flickr_guesstest.R")

args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) >= 1) args[1] else "run"
SMOKE <- MODE == "smoke"
OUT <- if (SMOKE) "guesstest_paper2_smoke" else "guesstest_paper2"
LOG <- file.path(OUT, "progress.log")
dir.create(OUT, showWarnings = FALSE)
for (g in c("S2", "S3", "S4")) dir.create(file.path(OUT, g), showWarnings = FALSE)

REPS       <- if (SMOKE) 2 else 30
CAP        <- if (SMOKE) 2e4 else 5e6   # episodes per synthetic run
CAP_FLICKR <- if (SMOKE) 2e4 else 1e6   # active guess-test plateaus near 98% (see flickr_guesstest.R)

logmsg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n",
                            sep = "", file = LOG, append = TRUE)
dec_or_na <- function(x) ifelse(x > 0, x, NA)

jobs <- list()
add_job <- function(group, name, rep, seed, cost, fn) {
  jobs[[length(jobs) + 1]] <<- list(group = group, name = name, rep = rep, seed = seed, cost = cost, fn = fn)
}

# ---- S2: autonomy, Goldilocks, choice sets
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
  `k=5`      = list(active_prob = 1, choice_k = 5))
for (s in seq_len(nrow(settings))) for (ci in seq_along(conds)) for (r in seq_len(REPS)) local({
  C <- settings$C[s]; fam <- settings$fam[s]; cc <- conds[[ci]]; label <- names(conds)[ci]; r <- r
  setting <- sprintf("C=%d, %s context", C, if (fam) "familiar" else "random")
  add_job("S2", sprintf("C%d_%s_%s", C, if (fam) "fam" else "rand", gsub("[=.]", "", label)), r,
          700000 + 10000 * s + 100 * ci + r, if (C == 100) 20 else 2,
          function() {
            v <- do.call(learn_corpus_guesstest,
                         c(list(C = C, M = 1000, a = 1, uniform = FALSE, fam_context = fam, max_episodes = CAP), cc))
            data.frame(setting = setting, label = label, rep = r, dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]),
                       episodes = v[10], censored = v[11])
          })
})

# ---- S3: ramps and constant blends
ramp_T <- c(1000, 2000, 3000, 5000, 8000, 12000, 20000)
blend_grid <- c(0, 0.02, 0.05, 0.075, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.90, 1)
ramp_fn <- function(dir, Tt) if (dir == "fwd") function(ep) pmin(1, ep / Tt) else function(ep) pmax(0, 1 - ep / Tt)
for (r in seq_len(REPS)) {
  for (dir in c("fwd", "rev")) for (Tt in ramp_T) local({
    dir <- dir; Tt <- Tt; r <- r
    add_job("S3", sprintf("ramp_%s_T%d", dir, Tt), r, 740000 + 100000 * (dir == "rev") + Tt + r, 3,
            function() {
              v <- learn_corpus_guesstest(C = 10, M = 1000, a = 1, uniform = FALSE,
                                          active_prob_fn = ramp_fn(dir, Tt), max_episodes = CAP)
              data.frame(condition = sprintf("ramp_%s", dir), T = Tt, active_prob = NA, rep = r,
                         dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]), episodes = v[10], censored = v[11])
            })
  })
  for (ap in blend_grid) local({
    ap <- ap; r <- r
    add_job("S3", sprintf("blend_%.3f", ap), r, 760000 + round(1000 * ap) * 100 + r, 3,
            function() {
              v <- learn_corpus_guesstest(C = 10, M = 1000, a = 1, uniform = FALSE, active_prob = ap,
                                          max_episodes = CAP)
              data.frame(condition = "blend", T = NA, active_prob = ap, rep = r,
                         dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]), episodes = v[10], censored = v[11])
            })
  })
}

# ---- S4: Flickr8k captions
flickr_pols <- c(list(list(policy = "passive", move_cost = 0)),
                 unlist(lapply(c(0, 1, 2, 3, 5, 10), function(mc)
                   list(list(policy = "active_moveon", move_cost = mc),
                        list(policy = "active_linger", move_cost = mc))), recursive = FALSE))
for (pc in flickr_pols) for (r in seq_len(REPS)) local({
  pol <- pc$policy; mc <- pc$move_cost; r <- r
  add_job("S4", sprintf("%s_mc%d", pol, mc), r, 780000 + r, if (pol == "passive") 30 else 10,
          function() {
            v <- run_learner_gt(pol, move_cost = mc, max_cost = CAP_FLICKR, seed = 780000 + r,
                                checkpoints = c(.5, .9, .95, .99))
            data.frame(policy = pol, move_cost = mc, rep = r, c50 = v[["0.5"]], c90 = v[["0.9"]],
                       c95 = v[["0.95"]], c99 = v[["0.99"]], final_frac = v[["final_frac"]],
                       total_cost = v[["total_cost"]])
          })
})

job_file <- function(j) file.path(OUT, j$group, sprintf("%s_rep%02d.rds", j$name, j$rep))

# ---- run
if (MODE != "summarize") {
  todo <- Filter(function(j) !file.exists(job_file(j)), jobs)
  todo <- todo[order(-vapply(todo, `[[`, numeric(1), "cost"))]
  ncores <- max(1, detectCores() - 2)
  logmsg("start: %d jobs total, %d remaining, %d cores (mode=%s)", length(jobs), length(todo), ncores, MODE)
  t0 <- Sys.time()
  res <- mclapply(todo, function(j) {
    t1 <- Sys.time()
    set.seed(j$seed)
    out <- tryCatch(j$fn(), error = function(e) e)
    if (inherits(out, "error")) {
      logmsg("ERROR %s/%s rep %d: %s", j$group, j$name, j$rep, conditionMessage(out))
      return(FALSE)
    }
    tmp <- paste0(job_file(j), ".tmp")
    saveRDS(out, tmp); file.rename(tmp, job_file(j))
    logmsg("done %s/%s rep %d (%.0fs)", j$group, j$name, j$rep, as.numeric(Sys.time() - t1, units = "secs"))
    TRUE
  }, mc.cores = ncores, mc.preschedule = FALSE)
  ok <- vapply(res, isTRUE, logical(1))
  logmsg("finished run: %d ok, %d failed, %.1f min", sum(ok), sum(!ok), as.numeric(Sys.time() - t0, units = "mins"))
}

# ---- summarize
load_group <- function(g) {
  fs <- list.files(file.path(OUT, g), pattern = "\\.rds$", full.names = TRUE)
  if (length(fs) == 0) return(NULL)
  bind_rows(lapply(fs, readRDS))
}

s2 <- load_group("S2")
if (!is.null(s2)) {
  lv <- names(conds)
  t2 <- s2 %>% group_by(setting, label) %>%
    summarise(n = n(), n_censored = sum(censored), mean99 = mean(episodes), median99 = median(episodes),
              mean50 = mean(dec5, na.rm = TRUE), .groups = "drop") %>%
    group_by(setting) %>%
    mutate(ratio_to_active = mean99 / mean99[label == "active"],
           share_of_log_speedup = ifelse(label %in% c("ap=0.25", "ap=0.50", "ap=0.75"),
                                         log(mean99[label == "passive"] / mean99) /
                                           log(mean99[label == "passive"] / mean99[label == "active"]), NA)) %>%
    ungroup() %>% mutate(label = factor(label, levels = lv)) %>% arrange(setting, label)
  write.csv(t2, file.path(OUT, "S2_modes_summary.csv"), row.names = FALSE)
  cat("\n=== S2: guess-test, autonomy / Goldilocks / choice sets (censored runs counted at the cap) ===\n")
  print(as.data.frame(t2), row.names = FALSE, digits = 4)
}

s3 <- load_group("S3")
if (!is.null(s3)) {
  realized <- function(cond, Tt, Fn) {
    if (cond == "blend") return(NA_real_)
    t <- seq_len(Fn); mean(if (cond == "ramp_fwd") pmin(1, t / Tt) else pmax(0, 1 - t / Tt))
  }
  s3$autonomy <- ifelse(s3$condition == "blend", s3$active_prob,
                        mapply(realized, s3$condition, s3$T, s3$episodes))
  blends <- s3 %>% filter(condition == "blend") %>% group_by(active_prob) %>%
    summarise(n = n(), n_censored = sum(censored), mean_ep = mean(episodes), .groups = "drop") %>% arrange(active_prob)
  blend_at <- function(x) exp(approx(blends$active_prob, log(blends$mean_ep), xout = x)$y)
  ramps <- s3 %>% filter(condition != "blend") %>% group_by(condition, T) %>%
    summarise(n = n(), n_censored = sum(censored), mean_ep = mean(episodes), mean_autonomy = mean(autonomy),
              .groups = "drop") %>%
    mutate(matched_blend_ep = blend_at(mean_autonomy), ramp_over_blend = mean_ep / matched_blend_ep)
  write.csv(blends, file.path(OUT, "S3_blends_summary.csv"), row.names = FALSE)
  write.csv(ramps, file.path(OUT, "S3_ramps_matched_summary.csv"), row.names = FALSE)
  cat("\n=== S3: guess-test constant blends ===\n"); print(as.data.frame(blends), row.names = FALSE, digits = 6)
  cat("\n=== S3: guess-test ramps vs. matched constant blend (ratio < 1: ramp faster) ===\n")
  print(as.data.frame(ramps), row.names = FALSE, digits = 4)
}

s4 <- load_group("S4")
if (!is.null(s4)) {
  t4 <- s4 %>% group_by(policy, move_cost) %>%
    summarise(n = n(), mean50 = mean(c50), mean90 = mean(c90), mean95 = mean(c95),
              n_reached99 = sum(!is.na(c99)), mean99 = mean(c99), mean_final = mean(final_frac),
              min_final = min(final_frac), .groups = "drop") %>% arrange(move_cost, policy)
  write.csv(t4, file.path(OUT, "S4_flickr_summary.csv"), row.names = FALSE)
  cat("\n=== S4: guess-test on Flickr8k captions ===\n"); print(as.data.frame(t4), row.names = FALSE, digits = 6)
}
logmsg("summaries written")
