# Tests whether guess-test's active/passive advantage survives a more
# cognitively realistic memory assumption: unconfirmed guesses decay back to
# "unknown" if not rehearsed (learners.R's new p_decay parameter). Prediction
# (see conversation): decay should widen the active/passive gap, not shrink
# it, because active target selection incidentally functions as a rehearsal
# scheduler -- it keeps revisit gaps short for whatever it's currently
# prioritizing, protecting against decay in a way passive sampling can't.
#
# p_decay values are parametrized as forgetting half-lives (in episodes)
# rather than raw hazard rates, since half-life is the interpretable
# quantity: p_decay = 1 - 0.5^(1/halflife).
#
# CALIBRATION NOTE: the first version of this sweep used half-lives of
# 200/50/10 episodes, chosen without checking them against the actual
# distribution of passive revisit gaps. Under passive sampling at C=10, a=1,
# M=1000, the RAREST word's expected revisit gap is ~7,485 episodes, and 97%
# of words have a gap exceeding 200 -- meaning all three original half-lives
# were deep in the "guess almost always decays completely between visits"
# regime for nearly every word, so the three decay conditions were
# indistinguishable (all near-saturated) rather than showing a genuine
# dose-response gradient. Widened here to span below and above the typical
# revisit-gap range (median ~3,746, rarest ~7,485 episodes).

source("learners.R")
suppressMessages(library(dplyr))

M <- 1000; a <- 1; C <- 10; REPS <- 30
half_lives <- c(Inf, 20000, 5000, 1000, 200)  # Inf = no decay (p_decay=0), the baseline

to_p_decay <- function(halflife) if (is.infinite(halflife)) 0 else 1 - 0.5^(1 / halflife)

run_one <- function(active, halflife, rep_seed) {
  set.seed(rep_seed)
  r <- learn_corpus_guesstest(C = C, M = M, a = a, uniform = FALSE, active = active,
                               p_decay = to_p_decay(halflife))
  data.frame(active = active, halflife = halflife, dec5 = r[5], dec9 = r[9], episodes = r[10], censored = r[11])
}

cat("Running", length(half_lives) * 2 * REPS, "replications...\n")
results <- list()
i <- 1
for (hl in half_lives) {
  for (active in c(TRUE, FALSE)) {
    for (rep in seq_len(REPS)) {
      results[[i]] <- run_one(active, hl, rep_seed = 10000 * i + rep)
      i <- i + 1
    }
  }
  cat(sprintf("  half-life=%s done\n", ifelse(is.infinite(hl), "Inf (baseline)", hl)))
}
d <- bind_rows(results)
d$active <- ifelse(d$active, "Active", "Passive")
d$halflife_label <- factor(ifelse(is.infinite(d$halflife), "No decay", paste0("t1/2=", d$halflife)),
                            levels = c("No decay", "t1/2=20000", "t1/2=5000", "t1/2=1000", "t1/2=200"))

summ <- d %>% filter(censored == 0) %>%
  group_by(halflife_label, active) %>%
  summarise(n = n(), mean50 = mean(dec5), mean90 = mean(dec9), mean99 = mean(episodes), .groups = "drop")

cat("\n=== Mean episodes by decay half-life, active vs passive (guess-test, C=10, a=1) ===\n")
print(as.data.frame(summ), row.names = FALSE, digits = 5)

ratios <- summ %>% select(halflife_label, active, mean50, mean90, mean99) %>%
  tidyr::pivot_wider(names_from = active, values_from = c(mean50, mean90, mean99)) %>%
  mutate(speedup50 = mean50_Passive / mean50_Active,
         speedup90 = mean90_Passive / mean90_Active,
         speedup99 = mean99_Passive / mean99_Active)

cat("\n=== Active/passive speedup by decay half-life and checkpoint ===\n")
print(as.data.frame(ratios %>% select(halflife_label, speedup50, speedup90, speedup99)), row.names = FALSE, digits = 4)

saveRDS(d, "decay_sweep_results.rds")
write.csv(summ, "decay_sweep_summary.csv", row.names = FALSE)
cat("\nSaved: decay_sweep_results.rds, decay_sweep_summary.csv\n")
