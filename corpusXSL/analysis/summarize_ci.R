# Bootstrap confidence intervals for the companion paper's Study 2 (guess-test,
# C = 10) and Study 3 (both learners) comparisons, after run_more_reps.R.
# Writes ci_summary/*.csv.
#
#   S2_cells.csv   per cell: n, censored, mean and median episodes to 99%
#                  (censored runs counted at the cap), mean episodes to 90%,
#                  each with a 95% percentile-bootstrap CI.
#   S2_shares.csv  share of the passive-to-fully-active speedup (log scale)
#                  gained by partial autonomy, by means and by medians.
#   S3_ramps.csv   each ramp vs. the constant-blend curve at the ramp's
#                  realized autonomy (log-linear interpolation), by means and
#                  by medians, with CIs from resampling runs within every
#                  ramp and blend condition.
#   S3_blends.csv  blend means and medians; share of the speedup at 25%.
suppressMessages(library(dplyr))
B <- 2000
set.seed(20261029)
dir.create("ci_summary", showWarnings = FALSE)
load_dir <- function(d) bind_rows(lapply(list.files(d, pattern = "\\.rds$", full.names = TRUE), readRDS))
ci <- function(x, f) { b <- replicate(B, f(sample(x, replace = TRUE))); quantile(b, c(.025, .975), names = FALSE) }

## ---- Study 2, guess-test
s2 <- bind_rows(load_dir("guesstest_paper2/S2") %>% mutate(criterion = "original"),
                load_dir("guesstest_lock2/S2") %>% mutate(criterion = "lock2"))
cells <- s2 %>% group_by(criterion, setting, label) %>% group_modify(function(d, k) {
  m <- ci(d$episodes, mean); md <- ci(d$episodes, median)
  d9 <- d$dec9[!is.na(d$dec9)]
  data.frame(n = nrow(d), n_censored = sum(d$censored), mean99 = mean(d$episodes), mean_lo = m[1], mean_hi = m[2],
             median99 = median(d$episodes), median_lo = md[1], median_hi = md[2],
             q25 = quantile(d$episodes, .25, names = FALSE), q75 = quantile(d$episodes, .75, names = FALSE),
             n_reached90 = length(d9), mean90 = if (length(d9)) mean(d9) else NA)
}) %>% ungroup()
write.csv(cells, "ci_summary/S2_cells.csv", row.names = FALSE)

share_ci <- function(d, stat) {
  get <- function(l) d$episodes[d$label == l]
  p <- get("passive"); a <- get("active")
  sapply(c("ap=0.25", "ap=0.50", "ap=0.75"), function(l) {
    x <- get(l)
    f <- function(pp, xx, aa) log(stat(pp) / stat(xx)) / log(stat(pp) / stat(aa))
    b <- replicate(B, f(sample(p, replace = TRUE), sample(x, replace = TRUE), sample(a, replace = TRUE)))
    c(est = f(p, x, a), quantile(b, c(.025, .975), names = FALSE))
  })
}
shares <- s2 %>% filter(grepl("C=10", setting)) %>% group_by(criterion, setting) %>% group_modify(function(d, k) {
  bind_rows(lapply(c(mean = mean, median = median), function(st) {
    s <- share_ci(d, st); data.frame(level = colnames(s), est = s[1, ], lo = s[2, ], hi = s[3, ])
  }), .id = "statistic")
}) %>% ungroup()
write.csv(shares, "ci_summary/S2_shares.csv", row.names = FALSE)

## ---- Study 3
realized <- function(cond, Tt, Fn) {
  t <- seq_len(Fn); mean(if (cond == "ramp_fwd") pmin(1, t / Tt) else pmax(0, 1 - t / Tt))
}
s3 <- bind_rows(load_dir("elim_ramp_cpp") %>% mutate(learner = "eliminative"),
                load_dir("guesstest_paper2/S3") %>% mutate(learner = "guess-test, original"),
                load_dir("guesstest_lock2/S3") %>% mutate(learner = "guess-test, lock2"))
s3$autonomy <- ifelse(s3$condition == "blend", s3$active_prob,
                      mapply(realized, s3$condition, s3$T, s3$episodes))

ramp_rows <- list(); blend_rows <- list()
for (ln in unique(s3$learner)) {
  d <- s3 %>% filter(learner == ln)
  bl <- d %>% filter(condition == "blend"); lv <- sort(unique(bl$active_prob))
  bl_eps <- lapply(lv, function(a) bl$episodes[bl$active_prob == a])
  ramps <- d %>% filter(condition != "blend") %>% distinct(condition, T) %>% arrange(condition, T)
  r_eps <- lapply(seq_len(nrow(ramps)), function(i) d %>% filter(condition == ramps$condition[i], T == ramps$T[i]))
  ratio <- function(stat, bl_eps, r_eps) {
    curve <- log(sapply(bl_eps, stat))
    sapply(r_eps, function(r) stat(r$episodes) / exp(approx(lv, curve, xout = stat(r$autonomy))$y))
  }
  share25 <- function(stat, bl_eps) {
    s <- sapply(bl_eps, stat); log(s[1] / s[lv == 0.25]) / log(s[1] / s[length(s)])
  }
  for (st in c("mean", "median")) {
    f <- get(st)
    est <- ratio(f, bl_eps, r_eps); sh <- share25(f, bl_eps)
    boot <- replicate(B, {
      bb <- lapply(bl_eps, sample, replace = TRUE)
      rr <- lapply(r_eps, function(r) r[sample(nrow(r), replace = TRUE), ])
      c(ratio(f, bb, rr), share25(f, bb))
    })
    q <- apply(boot, 1, quantile, c(.025, .975))
    ramp_rows[[length(ramp_rows) + 1]] <- data.frame(
      learner = ln, statistic = st, condition = ramps$condition, T = ramps$T,
      n = sapply(r_eps, nrow), n_censored = sapply(r_eps, function(r) sum(r$censored)),
      episodes = sapply(r_eps, function(r) f(r$episodes)), autonomy = sapply(r_eps, function(r) f(r$autonomy)),
      ratio = est, lo = q[1, seq_along(est)], hi = q[2, seq_along(est)])
    blend_rows[[length(blend_rows) + 1]] <- data.frame(
      learner = ln, statistic = st, active_prob = c(lv, NA),
      n = c(sapply(bl_eps, length), NA),
      n_censored = c(sapply(lv, function(a) sum(bl$censored[bl$active_prob == a])), NA),
      episodes = c(sapply(bl_eps, f), NA),
      share25 = c(rep(NA, length(lv)), sh), share25_lo = c(rep(NA, length(lv)), q[1, nrow(boot)]),
      share25_hi = c(rep(NA, length(lv)), q[2, nrow(boot)]))
  }
}
write.csv(bind_rows(ramp_rows), "ci_summary/S3_ramps.csv", row.names = FALSE)
write.csv(bind_rows(blend_rows), "ci_summary/S3_blends.csv", row.names = FALSE)
cat("written: ci_summary/{S2_cells,S2_shares,S3_ramps,S3_blends}.csv\n")
