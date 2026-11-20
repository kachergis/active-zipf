#!/usr/bin/env Rscript
# Summarizes verification_results.rds (produced by run_verification_grid.R):
# per-cell mean/median/SE/censoring rate, plus a direct comparison against the
# point estimates currently reported in the paper, so you can see at a glance
# whether the longer/cleaner run confirms them.
suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

d <- readRDS("verification_results.rds")
d$episodes <- as.numeric(d$episodes)
d$censored <- as.numeric(d$censored)
d$a <- as.numeric(d$a); d$C <- as.numeric(d$C); d$M <- as.numeric(d$M)

## ---- Per-cell summary ----
summ <- d %>%
  group_by(model, M, C, a, uniform, active, fam_context) %>%
  summarise(
    n_total = n(),
    n_censored = sum(censored, na.rm = TRUE),
    censoring_rate = mean(censored, na.rm = TRUE),
    n_ok = sum(censored == 0, na.rm = TRUE),
    mean_episodes = ifelse(n_ok > 0, mean(episodes[censored == 0]), NA_real_),
    median_episodes = ifelse(n_ok > 0, median(episodes[censored == 0]), NA_real_),
    se = ifelse(n_ok > 1, sd(episodes[censored == 0]) / sqrt(n_ok), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(model, M, C, uniform, a, active, fam_context)

write.csv(summ, "verification_summary.csv", row.names = FALSE)
cat("=== Per-cell summary (verification_summary.csv) ===\n")
print(as.data.frame(summ), row.names = FALSE)

cat("\n=== Cells that are fully or mostly censored (>=50% of reps hit the cap) ===\n")
print(as.data.frame(summ %>% filter(censoring_rate >= 0.5) %>%
                       select(model, M, C, a, uniform, active, fam_context, n_total, censoring_rate)),
      row.names = FALSE)

## ---- Comparison against the point estimates currently in the paper ----
## (Table tab-incremental and the exponent-sensitivity prose, eliminative model,
##  M=1000; from the compute-constrained run this script supersedes)
paper_reference <- tribble(
  ~model,        ~M,   ~C,  ~a,  ~uniform,   ~active,   ~fam_context, ~paper_mean,
  "eliminative", 1000, 10,  0,   "Uniform",  "Passive", "Random",     6676,
  "eliminative", 1000, 10,  0,   "Uniform",  "Active",  "Random",     2068,
  "eliminative", 1000, 10,  0.5, "Zipfian",  "Passive", "Random",     10795,
  "eliminative", 1000, 10,  1,   "Zipfian",  "Passive", "Random",     89454,
  "eliminative", 1000, 10,  1,   "Zipfian",  "Active",  "Random",     4695,
  "eliminative", 1000, 10,  1,   "Zipfian",  "Passive", "Familiar",   44607,
  "eliminative", 1000, 10,  1,   "Zipfian",  "Active",  "Familiar",   3127,
  "eliminative", 1000, 10,  1.5, "Zipfian",  "Active",  "Random",     296637,
  "eliminative", 1000, 100, 1,   "Zipfian",  "Passive", "Random",     11751913,  # from OLD Mandelbrot-form data, not this script's parametrization -- expect this one to differ
  "eliminative", 1000, 100, 1,   "Zipfian",  "Active",  "Random",     713244,    # same caveat
  "eliminative", 1000, 100, 1,   "Zipfian",  "Passive", "Familiar",   81269,     # same caveat
  "eliminative", 1000, 100, 1,   "Zipfian",  "Active",  "Familiar",   9093,      # same caveat
  "guesstest",   1000, 10,  1,   "Zipfian",  "Passive", "Random",     38037,
  "guesstest",   1000, 10,  1,   "Zipfian",  "Active",  "Random",     7747,
  "guesstest",   1000, 10,  1.5, "Zipfian",  "Active",  "Random",     37570,
  "rankedfreq",  1000, 10,  0,   "Uniform",  "Passive", "Random",     4573,
  "rankedfreq",  1000, 10,  0.5, "Zipfian",  "Passive", "Random",     6754,
  "rankedfreq",  1000, 10,  1,   "Zipfian",  "Passive", "Random",     22777,
  "rankedfreq",  1000, 10,  1,   "Zipfian",  "Active",  "Random",     990   # expected to reproduce EXACTLY (deterministic, see learners.R header)
)

cmp <- paper_reference %>%
  left_join(summ, by = c("model", "M", "C", "a", "uniform", "active", "fam_context")) %>%
  mutate(ratio_new_over_paper = mean_episodes / paper_mean)

cat("\n=== Comparison: paper's point estimate vs. this verification run ===\n")
cat("(ratio far from 1.0 for the C=100 eliminative rows is EXPECTED -- those paper\n")
cat(" numbers came from old Zipf-Mandelbrot-form data as a stand-in, never from\n")
cat(" this script's p_r~r^-a parametrization; this run replaces them for real.)\n\n")
print(as.data.frame(cmp %>% select(model, M, C, a, uniform, active, fam_context,
                                    paper_mean, mean_episodes, median_episodes,
                                    n_ok, censoring_rate, ratio_new_over_paper)),
      row.names = FALSE)

## ---- Figures (same style as the ones already in the paper) ----
d10 <- d %>% filter(C == 10, censored == 0)
d10$active <- factor(d10$active, levels = c("Passive", "Active"))
d10$fam_context <- factor(d10$fam_context, levels = c("Random", "Familiar"))
d10$model <- factor(d10$model, levels = c("eliminative", "guesstest", "rankedfreq"),
                     labels = c("Eliminative", "Guess-test", "Ranked-frequency"))

summ10 <- d10 %>% group_by(model, a, active, fam_context) %>%
  summarise(mean_episodes = mean(episodes), n = n(), .groups = "drop")

p1 <- ggplot(summ10, aes(x = a, y = mean_episodes, color = active, linetype = fam_context,
                          group = interaction(active, fam_context))) +
  geom_point(size = 2) + geom_line() +
  facet_wrap(~model, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Zipf exponent (a); a=0 is uniform", y = "Mean episodes to learn 99% (log scale)",
       color = "Target\nselection", linetype = "Context\nselection",
       title = "Verification run: C=10, all three models") +
  theme_bw(base_size = 12) + theme(legend.position = "right", strip.background = element_rect(fill = "grey90"))
ggsave("exponent_sensitivity_VERIFIED.pdf", p1, width = 10, height = 4.2)

cat("\nFigure saved: exponent_sensitivity_VERIFIED.pdf\n")
cat("Compare against paper/exponent_sensitivity.pdf.\n")
