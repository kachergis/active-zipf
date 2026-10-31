# Overnight batch filling the empty or capped cells in both papers.
#
#   G1  passive target selection at Zipf a = 1.5, C = 10, M = 1000
#       (eliminative and guess-test, random and familiar context). Only the
#       active a = 1.5 cells existed; fills the gap in the exponent figures
#       (paper/exponent_sensitivity_c10.pdf, paper/exponent_sensitivity.pdf).
#   G2  forgetting cells that hit the 2M-episode cap (main paper Table
#       tab-decay and the lock_k = 3 robustness check), rerun with a 2e7 cap:
#       lock 2 / t1/2 = 200 passive, lock 3 / t1/2 = 1000 and 200 passive.
#   G3  developmental ramps (companion paper Study 3), rerun saving every
#       replication, plus constant blends on a fine autonomy grid, so each
#       ramp can be compared with a constant blend of exactly matched
#       realized autonomy (replaces the approximation from mean times).
#   G4  Flickr8k linger vs. move on at move_cost 2 and 5 (companion paper
#       Study 4), to locate the crossover between move_cost 1 and 3.
#
# Resumable: every replication is saved to overnight_fill/<group>/<job>.rds as
# soon as it finishes, and finished jobs are skipped on restart. Long jobs are
# scheduled first. Summaries are written at the end (and can be rebuilt any
# time with:  Rscript run_overnight_fill.R summarize).
#
# Usage:  Rscript run_overnight_fill.R            # run (or resume) + summarize
#         Rscript run_overnight_fill.R smoke      # tiny caps/reps, separate dir
#         Rscript run_overnight_fill.R summarize  # summaries only

suppressMessages({library(dplyr); library(parallel)})
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")

args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) >= 1) args[1] else "run"
SMOKE <- MODE == "smoke"
OUT <- if (SMOKE) "overnight_fill_smoke" else "overnight_fill"
LOG <- file.path(OUT, "progress.log")
dir.create(OUT, showWarnings = FALSE)
for (g in c("G1", "G2", "G3", "G4")) dir.create(file.path(OUT, g), showWarnings = FALSE)

REPS       <- if (SMOKE) 2 else 30
CAP_ELIM   <- if (SMOKE) 2e6 else 2e9   # C++ eliminative, ~2.5M episodes/s
CAP_DECAY  <- if (SMOKE) 2e4 else 2e7   # R guess-test, ~14k episodes/s
CAP_FLICKR <- if (SMOKE) 2e4 else 5e6

logmsg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n",
                            sep = "", file = LOG, append = TRUE)
dec_or_na <- function(x) ifelse(x > 0, x, NA)
to_p_decay <- function(halflife) 1 - 0.5^(1 / halflife)

# ---------------------------------------------------------------- job list
jobs <- list()
add_job <- function(group, name, rep, seed, cost, fn) {
  jobs[[length(jobs) + 1]] <<- list(group = group, name = name, rep = rep, seed = seed,
                                    cost = cost, fn = fn)
}

# G1: passive at a = 1.5
for (fam in c(FALSE, TRUE)) for (r in seq_len(REPS)) {
  ctx <- if (fam) "Familiar" else "Random"
  local({
    fam <- fam; ctx <- ctx; r <- r
    add_job("G1", sprintf("elim_%s", ctx), r, 510000 + 100 * fam + r, if (fam) 1 else 100,
            function() {
              v <- elim_fast(10, 1000, zipf_probs(1000, 1.5), FALSE, fam, max_episodes = CAP_ELIM)
              data.frame(model = "Eliminative", context = ctx, rep = r, dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]),
                         p99 = v[10], censored = v[11])
            })
    add_job("G1", sprintf("guess_%s", ctx), r, 520000 + 100 * fam + r, 2,
            function() {
              v <- learn_corpus_guesstest(C = 10, M = 1000, a = 1.5, uniform = FALSE, active = FALSE,
                                          fam_context = fam, max_episodes = CAP_DECAY)
              data.frame(model = "Guess-test", context = ctx, rep = r, dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]),
                         p99 = v[10], censored = v[11])
            })
  })
}

# G2: forgetting cells that were capped at 2M
decay_cells <- data.frame(lock_k = c(2, 3, 3), halflife = c(200, 1000, 200))
for (i in seq_len(nrow(decay_cells))) for (r in seq_len(REPS)) local({
  k <- decay_cells$lock_k[i]; h <- decay_cells$halflife[i]; r <- r
  add_job("G2", sprintf("lock%d_t%d_passive", k, h), r, 530000 + 1000 * k + h + r, 50,
          function() {
            v <- learn_corpus_guesstest(C = 10, M = 1000, a = 1, uniform = FALSE, active = FALSE,
                                        p_decay = to_p_decay(h), lock_k = k, max_episodes = CAP_DECAY)
            data.frame(lock_k = k, halflife = h, active = "Passive", rep = r,
                       dec5 = dec_or_na(v[5]), dec9 = dec_or_na(v[9]),
                       episodes = ifelse(v[11] == 1, NA, v[10]), censored = v[11])
          })
})

# G3: ramps and a fine grid of constant blends (eliminative, M = 1000, C = 10, a = 1)
ramp_T <- c(1000, 2000, 3000, 5000, 8000, 12000, 20000)
blend_grid <- c(0, 0.02, 0.05, 0.075, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.90, 1)
ramp_fn <- function(dir, Tt) if (dir == "fwd") function(ep) pmin(1, ep / Tt) else function(ep) pmax(0, 1 - ep / Tt)
for (r in seq_len(REPS)) {
  for (dir in c("fwd", "rev")) for (Tt in ramp_T) local({
    dir <- dir; Tt <- Tt; r <- r
    add_job("G3", sprintf("ramp_%s_T%d", dir, Tt), r, 540000 + 100000 * (dir == "rev") + Tt + r, 3,
            function() {
              v <- learn_corpus_eliminative(C = 10, M = 1000, a = 1, uniform = FALSE,
                                            active_prob_fn = ramp_fn(dir, Tt))
              data.frame(condition = sprintf("ramp_%s", dir), T = Tt, active_prob = NA, rep = r,
                         dec5 = v[5], dec9 = v[9], episodes = v[10], censored = v[11])
            })
  })
  for (ap in blend_grid) local({
    ap <- ap; r <- r
    add_job("G3", sprintf("blend_%.3f", ap), r, 560000 + round(1000 * ap) * 100 + r, 3,
            function() {
              v <- learn_corpus_eliminative(C = 10, M = 1000, a = 1, uniform = FALSE, active_prob = ap)
              data.frame(condition = "blend", T = NA, active_prob = ap, rep = r,
                         dec5 = v[5], dec9 = v[9], episodes = v[10], censored = v[11])
            })
  })
}

# G4: Flickr8k linger vs. move on at move_cost 2 and 5. Evaluates the setup
# part of run_flickr_incremental.R (everything before its run loop), which
# defines run_learner() and the caption data.
flickr_src <- readLines("run_flickr_incremental.R")
eval(parse(text = flickr_src[seq_len(grep("^REPS <- 30", flickr_src) - 1)]))
for (mc in c(2, 5)) for (pol in c("active_moveon", "active_linger")) for (r in seq_len(REPS)) local({
  mc <- mc; pol <- pol; r <- r
  add_job("G4", sprintf("%s_mc%d", pol, mc), r, r, 2,   # seed = rep, as in the original runs
          function() {
            v <- run_learner(pol, move_cost = mc, max_cost = CAP_FLICKR, seed = r)
            data.frame(policy = pol, move_cost = mc, rep = r, c50 = v[["0.5"]], c90 = v[["0.9"]],
                       c99 = v[["0.99"]], final_frac = v[["final_frac"]], total_cost = v[["total_cost"]])
          })
})

job_file <- function(j) file.path(OUT, j$group, sprintf("%s_rep%02d.rds", j$name, j$rep))

# ---------------------------------------------------------------- run
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
  logmsg("finished run: %d ok, %d failed, %.1f min", sum(ok), sum(!ok),
         as.numeric(Sys.time() - t0, units = "mins"))
}

# ---------------------------------------------------------------- summarize
load_group <- function(g) {
  fs <- list.files(file.path(OUT, g), pattern = "\\.rds$", full.names = TRUE)
  if (length(fs) == 0) return(NULL)
  bind_rows(lapply(fs, readRDS))
}
cmed <- function(x) { y <- ifelse(is.na(x), Inf, x); m <- median(y); if (is.infinite(m)) NA else m }

g1 <- load_group("G1")
if (!is.null(g1)) {
  s1 <- g1 %>% group_by(model, context) %>%
    summarise(n = n(), n_censored = sum(censored), mean_p99 = mean(p99), median_p99 = median(p99),
              reached90 = sum(!is.na(dec9)), mean50 = mean(dec5, na.rm = TRUE),
              mean90 = mean(dec9, na.rm = TRUE), .groups = "drop")
  write.csv(s1, file.path(OUT, "G1_a1.5_passive_summary.csv"), row.names = FALSE)
  cat("\n=== G1: passive, a = 1.5, C = 10 (censored runs counted at the cap) ===\n")
  print(as.data.frame(s1), row.names = FALSE, digits = 6)
}

g2 <- load_group("G2")
if (!is.null(g2)) {
  s2 <- g2 %>% group_by(lock_k, halflife, active) %>%
    summarise(n = n(), reached50 = sum(!is.na(dec5)), reached90 = sum(!is.na(dec9)),
              reached99 = sum(!is.na(episodes)), med50 = cmed(dec5), med90 = cmed(dec9),
              med99 = cmed(episodes), .groups = "drop")
  write.csv(s2, file.path(OUT, "G2_decay_summary.csv"), row.names = FALSE)
  cat(sprintf("\n=== G2: forgetting, passive, cap %.0e (medians; NA = median run did not reach it) ===\n", CAP_DECAY))
  print(as.data.frame(s2), row.names = FALSE, digits = 6)
}

g3 <- load_group("G3")
if (!is.null(g3)) {
  # Realized autonomy of a ramp run = mean of its active_prob over the
  # episodes it actually ran (exact, since active_prob depends only on episode).
  realized <- function(cond, Tt, Fn) {
    t <- seq_len(Fn)
    mean(if (cond == "ramp_fwd") pmin(1, t / Tt) else pmax(0, 1 - t / Tt))
  }
  g3$autonomy <- ifelse(g3$condition == "blend", g3$active_prob,
                        mapply(realized, g3$condition, g3$T, g3$episodes))
  blends <- g3 %>% filter(condition == "blend") %>% group_by(active_prob) %>%
    summarise(n = n(), mean_ep = mean(episodes), .groups = "drop") %>% arrange(active_prob)
  blend_at <- function(x) exp(approx(blends$active_prob, log(blends$mean_ep), xout = x)$y)
  ramps <- g3 %>% filter(condition != "blend") %>% group_by(condition, T) %>%
    summarise(n = n(), mean_ep = mean(episodes), mean_autonomy = mean(autonomy), .groups = "drop") %>%
    mutate(matched_blend_ep = blend_at(mean_autonomy),
           ramp_over_blend = mean_ep / matched_blend_ep)
  write.csv(blends, file.path(OUT, "G3_blends_summary.csv"), row.names = FALSE)
  write.csv(ramps, file.path(OUT, "G3_ramps_matched_summary.csv"), row.names = FALSE)
  cat("\n=== G3: constant blends ===\n"); print(as.data.frame(blends), row.names = FALSE, digits = 6)
  cat("\n=== G3: ramps vs. constant blend of matched realized autonomy (ratio < 1: ramp faster) ===\n")
  print(as.data.frame(ramps), row.names = FALSE, digits = 4)
}

g4 <- load_group("G4")
if (!is.null(g4)) {
  s4 <- g4 %>% group_by(policy, move_cost) %>%
    summarise(n = n(), mean50 = mean(c50), mean90 = mean(c90), mean99 = mean(c99),
              min_final = min(final_frac), .groups = "drop")
  write.csv(s4, file.path(OUT, "G4_flickr_summary.csv"), row.names = FALSE)
  cat("\n=== G4: Flickr8k, move_cost 2 and 5 ===\n"); print(as.data.frame(s4), row.names = FALSE, digits = 6)
}
logmsg("summaries written")
