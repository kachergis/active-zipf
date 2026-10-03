# Forgetting sweep, redesigned so forgetting CAN matter (cf. run_decay_sweep.R).
#
# In the original guess-test learner (lock_k = 0) a word counts as learned the
# instant its guess is correct, so decay can only erase wrong guesses -- at the
# same moment disconfirmation would have erased them anyway -- and the
# (bug-fixed) run_decay_sweep.R accordingly found no effect of forgetting at
# all. Here a guess must survive lock_k consecutive re-exposures before the
# learner locks it in, and until then any guess, correct or not, can decay;
# each confirmation counts as rehearsal and resets the decay clock. Active
# target selection uses the learner's own lock state rather than the
# simulator's knowledge of correctness (see learners.R, lock_k).
#
# Usage: Rscript run_decay_lock_sweep.R <lock_k>   (default 2)

source("learners.R")
suppressMessages({library(dplyr); library(parallel)})

args <- commandArgs(trailingOnly = TRUE)
LOCK_K <- if (length(args) >= 1) as.integer(args[1]) else 2L
M <- 1000; a <- 1; C <- 10; REPS <- 30
MAX_EPISODES <- 2e6  # fixed cap (machine-load independent); capped runs are censored
half_lives <- c(Inf, 20000, 5000, 1000, 200)
to_p_decay <- function(halflife) if (is.infinite(halflife)) 0 else 1 - 0.5^(1 / halflife)

jobs <- expand.grid(rep = seq_len(REPS), active = c(TRUE, FALSE), halflife = half_lives)
jobs$seed <- 20000 + LOCK_K * 1e5 + seq_len(nrow(jobs))

cat(sprintf("lock_k=%d: running %d replications on %d cores...\n", LOCK_K, nrow(jobs), detectCores() - 2))
t0 <- Sys.time()
res <- mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j, ]
  set.seed(jb$seed)
  r <- learn_corpus_guesstest(C = C, M = M, a = a, uniform = FALSE, active = jb$active,
                               p_decay = to_p_decay(jb$halflife), lock_k = LOCK_K,
                               max_episodes = MAX_EPISODES)
  # unreached deciles are recorded as 0 (FALSE) by the learner -> NA here
  data.frame(lock_k = LOCK_K, active = jb$active, halflife = jb$halflife, rep = jb$rep,
             dec5 = ifelse(r[5] > 0, r[5], NA), dec9 = ifelse(r[9] > 0, r[9], NA),
             episodes = ifelse(r[11] == 1, NA, r[10]), censored = r[11])
}, mc.cores = detectCores() - 2, mc.preschedule = FALSE)
cat(sprintf("done in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

d <- bind_rows(res)
d$active <- ifelse(d$active, "Active", "Passive")
d$halflife_label <- factor(ifelse(is.infinite(d$halflife), "No decay", paste0("t1/2=", d$halflife)),
                           levels = c("No decay", "t1/2=20000", "t1/2=5000", "t1/2=1000", "t1/2=200"))

# Censoring-aware summary: median treating unreached as > cap (valid while
# fewer than half the runs are censored), plus the count reaching each checkpoint.
cmed <- function(x) { y <- ifelse(is.na(x), Inf, x); m <- median(y); if (is.infinite(m)) NA else m }
summ <- d %>% group_by(halflife_label, active) %>%
  summarise(n = n(),
            reached50 = sum(!is.na(dec5)), reached90 = sum(!is.na(dec9)), reached99 = sum(!is.na(episodes)),
            med50 = cmed(dec5), med90 = cmed(dec9), med99 = cmed(episodes),
            mean50 = mean(dec5, na.rm = TRUE), mean90 = mean(dec9, na.rm = TRUE), mean99 = mean(episodes, na.rm = TRUE),
            .groups = "drop")
cat(sprintf("\n=== Guess-test with lock_k=%d, C=10, a=1: episodes by forgetting half-life ===\n", LOCK_K))
print(as.data.frame(summ), row.names = FALSE, digits = 5)

saveRDS(d, sprintf("decay_lock%d_results.rds", LOCK_K))
write.csv(summ, sprintf("decay_lock%d_summary.csv", LOCK_K), row.names = FALSE)
cat(sprintf("\nSaved: decay_lock%d_results.rds, decay_lock%d_summary.csv\n", LOCK_K, LOCK_K))
