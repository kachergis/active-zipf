# Builds the completion-checkpoint (50/90/99%) table and figure for the paper,
# from the EXACT datasets already backing Table~tab-incremental and
# Figure~fig-incremental-sim (full_grid_results.rds, old_eliminative_C100.csv,
# guesstest_results.rds) -- not a new simulation, and not the separate
# active-selection-extensions sweep used elsewhere in corpusXSL/analysis.
# Every learner records dec5 (episodes to 50% known), dec9 (90%), and p99
# (99%, the paper's existing headline metric) per replication, so this is a
# pure re-summary at finer completion resolution.

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

## ---- Eliminative, C=10 and C=100 (Zipfian, a=1, Random context) ----
elim10 <- readRDS("full_grid_results.rds") %>%
  filter(M == 1000, a == 1, uniform == "Zipfian", C == 10, fam_context == "Random")
elim100 <- read.csv("old_eliminative_C100.csv") %>%
  filter(uniform == "Zipfian", fam_context == "Random") %>%
  mutate(C = 100)
elim <- bind_rows(elim10 %>% select(active, C, dec5, dec9, p99),
                   elim100 %>% select(active, C, dec5, dec9, p99)) %>%
  mutate(model = "Eliminative")

## ---- Guess-test, C=10 only (C=100 not saved at per-replication/decile
## granularity -- see README's note on run_guesstest_grid.R) ----
gt10 <- readRDS("guesstest_results.rds") %>%
  filter(M == 1000, a == 1, uniform == "Zipfian", C == 10, fam_context == "Random") %>%
  select(active, C, dec5, dec9, p99) %>%
  mutate(model = "Guess-test")

d <- bind_rows(elim, gt10)
d$model <- factor(d$model, levels = c("Eliminative", "Guess-test"))
d$Clabel <- factor(paste0("C=", d$C), levels = c("C=10", "C=100"))

long <- d %>%
  pivot_longer(c(dec5, dec9, p99), names_to = "checkpoint", values_to = "episodes") %>%
  mutate(checkpoint = factor(checkpoint, levels = c("dec5", "dec9", "p99"),
                              labels = c("50%", "90%", "99%")))

summ <- long %>%
  group_by(model, Clabel, active, checkpoint) %>%
  summarise(mean_ep = mean(episodes), median_ep = median(episodes),
            p25 = quantile(episodes, .25), p75 = quantile(episodes, .75),
            skew_ratio = mean_ep / median_ep, n = n(), .groups = "drop")

cat("=== Checkpoint summary (paper's own datasets) ===\n")
print(as.data.frame(summ %>% arrange(model, Clabel, checkpoint, active)), row.names = FALSE, digits = 5)

ratios <- summ %>% select(model, Clabel, checkpoint, active, mean_ep, median_ep) %>%
  pivot_wider(names_from = active, values_from = c(mean_ep, median_ep)) %>%
  mutate(speedup_mean = mean_ep_Passive / mean_ep_Active,
         speedup_median = median_ep_Passive / median_ep_Active)
cat("\n=== Active/Passive speedup at each checkpoint ===\n")
print(as.data.frame(ratios %>% select(model, Clabel, checkpoint, speedup_mean, speedup_median)),
      row.names = FALSE, digits = 4)

write.csv(summ, "checkpoint_divergence_paper_data.csv", row.names = FALSE)

## ---- Figure ----
p <- ggplot(summ, aes(x = checkpoint, y = median_ep, color = active, group = active)) +
  geom_line() + geom_point(size = 2) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = active, group = active), alpha = 0.15, color = NA) +
  facet_grid(Clabel ~ model, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Active = "#377eb8", Passive = "#D55E00")) +
  scale_fill_manual(values = c(Active = "#377eb8", Passive = "#D55E00")) +
  labs(x = "Vocabulary completion checkpoint", y = "Median episodes (log scale; band = IQR)",
       color = NULL, fill = NULL) +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/checkpoint_divergence.pdf", p, width = 8, height = 5.5)
cat("\nFigure saved: paper/checkpoint_divergence.pdf\n")
