# Extends analytical_elimination_bound.R's C=100 coverage from the two pure
# extremes (active_prob=0, active_prob=1/unknown/unrestricted) to the 8
# genuinely-open eliminative/C=100 cells: intermediate active_prob mixtures,
# the goldilocks policy, and bounded choice_k.
#
# WHY NOT SIMULATE DIRECTLY (even with the cheap lightweight_target_sim.R)?
# At C=100, N* (mean per-word targeted-exposures-to-exclude-all-competitors)
# is ~1.46 million (see analytical_elimination_bound.R). Even the lightweight
# simulator -- which skips the O(M) elimination bookkeeping entirely and only
# simulates target selection -- runs at ~50,000 episodes/sec (measured at
# C=10, M=1000, reps=200). A single active-leaning replication at C=100 would
# need ~1.46e9 episodes -> ~8 hours; passive-leaning conditions are worse.
# Getting even minimal statistics (3 reps) across all 8 open cells would be a
# multi-day background job. Not attempted here.
#
# APPROACH INSTEAD: ratio transfer. We have REAL simulated ground truth at
# C=10 for all 8 conditions (from active_modes_results_overnight.rds) relative
# to real C=10 pure-active (4678 episodes). We anchor those empirical ratios
# to the ANALYTICALLY-derived C=100 pure-active baseline (T_active=1.4638e9,
# itself validated to ~8% against real simulation at C=10) instead of the
# lightweight simulator's C=100 estimate (which would need days to obtain and
# adds a second layer of approximation on top).
#
#   T_100(cell) ~= T_active_100 * [ T_10(cell) / T_active_10 ]
#
# KEY UNTESTED ASSUMPTION: the relative slowdown from mixing/restricting
# active choice is roughly INVARIANT to C. This is plausible (the mechanism
# generating the slowdown -- episodes "wasted" on already-known or
# poorly-timed words -- is a population-dynamics property, not obviously tied
# to per-trial exclusion difficulty) but NOT validated the way the pure-
# extreme formulas are. Direction of any bias is unknown a priori: it is
# equally plausible that active selection matters relatively MORE at C=100
# (each wasted passive episode is "more expensive" when exclusion itself is
# slow) as that it matters relatively LESS. Report these as order-of-magnitude
# estimates, not validated point predictions.

source("analytical_elimination_bound.R")
suppressMessages(library(dplyr))

M <- 1000
C100 <- 100
a <- 1

RESULTS_PATH <- "active_modes_results_overnight.rds"
OUT_PATH <- "eliminative_C100_mixture_estimates.csv"

stopifnot(file.exists(RESULTS_PATH))
d <- readRDS(RESULTS_PATH)
d$episodes <- as.numeric(d$episodes); d$censored <- as.numeric(d$censored)
d$active_prob <- as.numeric(d$active_prob); d$C <- as.numeric(d$C)

real10 <- d %>%
  filter(model == "eliminative", C == 10, censored == 0) %>%
  group_by(active_prob, active_policy, choice_k_label) %>%
  summarise(mean_ep_C10 = mean(episodes), n = n(), .groups = "drop")

anchor <- real10 %>% filter(active_prob == 1, active_policy == "unknown", choice_k_label == "Inf")
stopifnot(nrow(anchor) == 1)
T_active_C10_real <- anchor$mean_ep_C10

# the 8 genuinely-open cells (mirrors fill_incomplete_active_modes_cells.R's
# `uncovered` set)
open_cells <- real10 %>%
  filter(!(active_prob %in% c(0, 1) & active_policy == "unknown" & choice_k_label == "Inf")) %>%
  filter((active_prob %in% c(0.25, 0.5, 0.75) & active_policy == "unknown" & choice_k_label == "Inf") |
         (active_prob == 1 & active_policy == "goldilocks" & choice_k_label == "Inf") |
         (active_prob == 1 & active_policy == "unknown" & choice_k_label %in% c("5", "20", "100")))

# C=100's sweep used choice_k=50, but C=10's finer grid skipped straight from
# 40 to 60 -- no exact real-C=10 match. Log-linearly interpolate mean_ep_C10
# between k=40 and k=60 (episodes-vs-log(k) is smooth/near-linear over this
# range in the real data: 5563 at 40, 5126 at 60) rather than leaving this
# cell out entirely.
k40 <- real10 %>% filter(active_prob == 1, active_policy == "unknown", choice_k_label == "40")
k60 <- real10 %>% filter(active_prob == 1, active_policy == "unknown", choice_k_label == "60")
stopifnot(nrow(k40) == 1, nrow(k60) == 1)
w <- (log(50) - log(40)) / (log(60) - log(40))
ep50 <- exp((1 - w) * log(k40$mean_ep_C10) + w * log(k60$mean_ep_C10))
open_cells <- bind_rows(open_cells, data.frame(
  active_prob = 1, active_policy = "unknown", choice_k_label = "50",
  mean_ep_C10 = ep50, n = NA
))

T_active_C100 <- predict_T_active(M, C100, a)

est <- open_cells %>%
  mutate(
    ratio_to_active_C10 = mean_ep_C10 / T_active_C10_real,
    T_predicted_C100 = T_active_C100 * ratio_to_active_C10,
    method = "ratio transfer: real C=10 ratio x analytical C=100 active baseline"
  ) %>%
  arrange(active_prob, active_policy, choice_k_label)

cat(sprintf("Analytical C=100 active baseline: %.0f episodes (validated ~8%% at C=10)\n", T_active_C100))
cat(sprintf("Real C=10 active baseline (anchor): %.0f episodes\n\n", T_active_C10_real))
cat("=== Ratio-transfer estimates for the 8 open eliminative/C=100 cells ===\n")
print(as.data.frame(est %>% select(active_prob, active_policy, choice_k_label, mean_ep_C10, ratio_to_active_C10, T_predicted_C100)),
      row.names = FALSE, digits = 4)

write.csv(est, OUT_PATH, row.names = FALSE)
cat("\nSaved to", OUT_PATH, "-- UNVALIDATED beyond the ratio-invariance assumption above; do not present with the same confidence as the pure-extreme formulas.\n")
