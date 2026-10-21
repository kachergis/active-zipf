# Does choosing active targets UNIFORMLY among unknown words, rather than in
# proportion to frequency (choose_target()'s default "unknown" policy, the
# only active policy used in paper 1), speed up the eliminative learner?
# For the guess-test learner it does, a lot (diag_gt/run_D.R: 3,833 vs 7,814
# episodes to 99% at C = 10; 43,904 vs 179,983 at C = 100).
#
#   U1  C++ elim_fast, active "unknown" (policy 0) vs. "uniform" (policy 2),
#       plus passive for reference, in five settings:
#         W = 1000,   C = 10,  a = 1,    random and familiar context
#         W = 1000,   C = 100, a = 1,    familiar context (random never finishes)
#         W = 1000,   C = 100, a = 0.75, random context
#         W = 10000,  C = 10,  a = 1,    random context
#   U2  R learn_corpus_eliminative, active "unknown" vs. "uniform", at
#       W = 1000, C = 10, a = 1, random and familiar context: validates the
#       C++ policy 2 against the reference R implementation.
#
# Each replication uses the same seed under both policies (so the same
# frequency-to-index permutation of the vocabulary). Checkpoints: 50% and 90%
# (deciles 5 and 9) and 99% (episodes).
#
# Resumable: every replication is saved to uniform_active/<group>/<job>.rds
# as soon as it finishes, and finished jobs are skipped on restart.
#
# Usage:  Rscript run_uniform_active.R            # run (or resume) + summarize
#         Rscript run_uniform_active.R smoke      # tiny caps/reps, separate dir
#         Rscript run_uniform_active.R summarize  # summaries only

suppressMessages({library(dplyr); library(parallel)})
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")

args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) >= 1) args[1] else "run"
SMOKE <- MODE == "smoke"
OUT <- if (SMOKE) "uniform_active_smoke" else "uniform_active"
LOG <- file.path(OUT, "progress.log")
dir.create(OUT, showWarnings = FALSE)
for (g in c("U1", "U2")) dir.create(file.path(OUT, g), showWarnings = FALSE)

REPS     <- if (SMOKE) 2 else 30
CAP_ELIM <- if (SMOKE) 2e5 else 2e9   # C++ eliminative, ~2.5M episodes/s
CAP_R    <- if (SMOKE) 2e3 else 1e6

logmsg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n",
                            sep = "", file = LOG, append = TRUE)
dec_or_na <- function(x) ifelse(x > 0, x, NA)

# ---------------------------------------------------------------- job list
jobs <- list()
add_job <- function(group, name, rep, seed, cost, fn) {
  jobs[[length(jobs) + 1]] <<- list(group = group, name = name, rep = rep, seed = seed,
                                    cost = cost, fn = fn)
}
row_out <- function(impl, s, cond, r, v) {
  data.frame(impl = impl, setting = s$setting, W = s$W, C = s$C, a = s$a, context = s$context,
             condition = cond, rep = r, p50 = dec_or_na(v[5]), p90 = dec_or_na(v[9]),
             p99 = ifelse(v[11] == 1, NA, v[10]), censored = v[11])
}

settings <- data.frame(
  setting = c("W1000_C10_a1_random", "W1000_C10_a1_familiar", "W1000_C100_a1_familiar",
              "W1000_C100_a0.75_random", "W10000_C10_a1_random"),
  W = c(1000, 1000, 1000, 1000, 10000), C = c(10, 10, 100, 100, 10), a = c(1, 1, 1, 0.75, 1),
  context = c("Random", "Familiar", "Familiar", "Random", "Random"),
  passive_cost = c(2, 1, 5, 50, 20), stringsAsFactors = FALSE)
conditions <- data.frame(condition = c("active_freq", "active_uniform", "passive"),
                         active = c(TRUE, TRUE, FALSE), policy = c(0L, 2L, 0L),
                         r_policy = c("unknown", "uniform", NA), stringsAsFactors = FALSE)

for (i in seq_len(nrow(settings))) for (j in seq_len(nrow(conditions))) for (r in seq_len(REPS)) local({
  s <- settings[i, ]; cd <- conditions[j, ]; r <- r
  add_job("U1", sprintf("%s_%s", s$setting, cd$condition), r, 610000 + 1000 * i + r,
          if (cd$active) 1 else s$passive_cost,
          function() {
            v <- elim_fast(s$C, s$W, zipf_probs(s$W, s$a), cd$active, s$context == "Familiar",
                           max_episodes = CAP_ELIM, policy = cd$policy)
            row_out("C++", s, cd$condition, r, v)
          })
  if (s$W == 1000 && s$C == 10 && cd$active) {
    add_job("U2", sprintf("%s_%s", s$setting, cd$condition), r, 620000 + 1000 * i + r, 3,
            function() {
              v <- learn_corpus_eliminative(C = s$C, M = s$W, a = s$a, uniform = FALSE, active = TRUE,
                                            fam_context = s$context == "Familiar", max_episodes = CAP_R,
                                            active_policy = cd$r_policy)
              row_out("R", s, cd$condition, r, v)
            })
  }
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
d <- bind_rows(load_group("U1"), load_group("U2"))
if (nrow(d) > 0) {
  saveRDS(d, file.path(OUT, "uniform_active_results.rds"))
  s <- d %>% group_by(impl, setting, W, C, a, context, condition) %>%
    summarise(n = n(), n_censored = sum(censored),
              mean50 = mean(p50), mean90 = mean(p90), mean99 = mean(p99),
              se99 = sd(p99) / sqrt(n()), median99 = median(p99), .groups = "drop")
  # ratios of frequency-weighted to uniform active targeting (> 1: uniform faster),
  # and each active policy's speedup over passive at 99%
  ratios <- s %>% filter(condition != "passive") %>%
    select(impl, setting, condition, mean50, mean90, mean99) %>%
    tidyr::pivot_wider(names_from = condition, values_from = c(mean50, mean90, mean99)) %>%
    mutate(freq_over_unif50 = mean50_active_freq / mean50_active_uniform,
           freq_over_unif90 = mean90_active_freq / mean90_active_uniform,
           freq_over_unif99 = mean99_active_freq / mean99_active_uniform)
  passive <- s %>% filter(condition == "passive") %>% select(setting, passive99 = mean99)
  ratios <- ratios %>% left_join(passive, by = "setting") %>%
    mutate(speedup99_freq = passive99 / mean99_active_freq,
           speedup99_unif = passive99 / mean99_active_uniform)
  write.csv(s, file.path(OUT, "uniform_active_summary.csv"), row.names = FALSE)
  write.csv(ratios, file.path(OUT, "uniform_active_ratios.csv"), row.names = FALSE)
  cat("\n=== Episodes to 50/90/99% (means; censored runs give NA) ===\n")
  print(as.data.frame(s), row.names = FALSE, digits = 6)
  cat("\n=== Frequency-weighted / uniform (> 1: uniform faster); speedups over passive at 99% ===\n")
  print(as.data.frame(ratios %>% select(impl, setting, starts_with("freq_over"), starts_with("speedup"))),
        row.names = FALSE, digits = 4)
  # validation: C++ vs. R at C = 10 (different seeds streams; compare distributions)
  v <- d %>% filter(C == 10, W == 1000, condition != "passive") %>% group_by(setting, condition) %>%
    summarise(R = mean(p99[impl == "R"]), Cpp = mean(p99[impl == "C++"]),
              z = (Cpp - R) / sqrt(var(p99[impl == "C++"]) / sum(impl == "C++") +
                                   var(p99[impl == "R"]) / sum(impl == "R")), .groups = "drop")
  cat("\n=== Validation: C++ vs. R, episodes to 99% ===\n"); print(as.data.frame(v), row.names = FALSE, digits = 6)
}
logmsg("summaries written")
