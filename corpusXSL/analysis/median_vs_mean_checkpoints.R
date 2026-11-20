# Re-analyzes EXISTING simulation data (no new simulation) at three
# completion checkpoints -- 50%, 90%, 99% of the vocabulary known -- using
# both mean and median, to see whether active-vs-passive divergence is a
# tail phenomenon or shows up even in a "typical" (median) run.
#
# Every learner already records dec1..dec9 (episodes to reach each decile of
# M words known) alongside `episodes` (episodes to reach the final target,
# M*(1-epsilon) with epsilon=0.01 by default -- i.e. 99%). So:
#   dec5     = episodes to 50% known
#   dec9     = episodes to 90% known
#   episodes = episodes to 99% known
# No new simulation needed -- this is a pure re-summary of
# active_modes_results_overnight.rds.

suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

d <- readRDS("active_modes_results_overnight.rds")
d$episodes <- as.numeric(d$episodes); d$censored <- as.numeric(d$censored)
d$dec5 <- as.numeric(d$dec5); d$dec9 <- as.numeric(d$dec9)
d$active_prob <- as.numeric(d$active_prob); d$C <- as.numeric(d$C)
d <- d %>% filter(censored == 0)
d$model <- factor(d$model, levels = c("eliminative", "guesstest", "rankedfreq"),
                   labels = c("Eliminative", "Guess-test", "Ranked-frequency"))
d$Clabel <- factor(paste0("C=", d$C), levels = c("C=10", "C=100"))

long <- d %>%
  filter(active_policy == "unknown", choice_k_label == "Inf") %>%
  select(model, Clabel, active_prob, dec5, dec9, episodes) %>%
  pivot_longer(c(dec5, dec9, episodes), names_to = "checkpoint", values_to = "n_episodes") %>%
  mutate(checkpoint = factor(checkpoint, levels = c("dec5", "dec9", "episodes"),
                              labels = c("50%", "90%", "99%")))

summ <- long %>%
  group_by(model, Clabel, active_prob, checkpoint) %>%
  summarise(mean_ep = mean(n_episodes), median_ep = median(n_episodes),
            p10 = quantile(n_episodes, .1), p90 = quantile(n_episodes, .9),
            skew_ratio = mean_ep / median_ep, n = n(), .groups = "drop")

cat("=== active_prob sweep: mean vs median at 50/90/99%, by model x C ===\n")
print(as.data.frame(summ %>% arrange(model, Clabel, checkpoint, active_prob) %>%
                       select(model, Clabel, checkpoint, active_prob, mean_ep, median_ep, skew_ratio, n)),
      row.names = FALSE, digits = 4)

## ---- Divergence: active/passive RATIO at each checkpoint, mean vs median ----
endpoints <- summ %>% filter(active_prob %in% c(0, 1)) %>%
  select(model, Clabel, checkpoint, active_prob, mean_ep, median_ep) %>%
  pivot_wider(names_from = active_prob, values_from = c(mean_ep, median_ep), names_sep = "_p")

endpoints <- endpoints %>% mutate(
  ratio_mean   = mean_ep_p0 / mean_ep_p1,
  ratio_median = median_ep_p0 / median_ep_p1
)

cat("\n=== Passive/Active ratio at each checkpoint (mean-based vs median-based) ===\n")
print(as.data.frame(endpoints %>% select(model, Clabel, checkpoint, ratio_mean, ratio_median) %>%
                       arrange(model, Clabel, checkpoint)),
      row.names = FALSE, digits = 4)

## ---- Figure: median trajectory across checkpoints, active vs passive ----
plot_d <- summ %>% filter(active_prob %in% c(0, 1)) %>%
  mutate(condition = ifelse(active_prob == 1, "Active", "Passive"))

p <- ggplot(plot_d, aes(x = checkpoint, y = median_ep, color = condition, group = condition)) +
  geom_line() + geom_point(size = 2) +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = condition, group = condition), alpha = 0.15, color = NA) +
  facet_grid(Clabel ~ model, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Active = "#377eb8", Passive = "#D55E00")) +
  scale_fill_manual(values = c(Active = "#377eb8", Passive = "#D55E00")) +
  labs(x = "Vocabulary completion checkpoint", y = "Median episodes (log scale; ribbon = 10th-90th pctile)",
       color = NULL, fill = NULL,
       title = "Active vs. passive divergence grows with the completion target",
       subtitle = "Median across replications, not mean -- shaded band shows within-condition spread") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/median_checkpoint_divergence.pdf", p, width = 10, height = 5.5)
cat("\nFigure saved: paper/median_checkpoint_divergence.pdf\n")

## ---- Mean-vs-median skew check: how right-skewed is each checkpoint? ----
cat("\n=== Skew ratio (mean/median) at each checkpoint, active_prob=0 vs 1 only ===\n")
print(as.data.frame(summ %>% filter(active_prob %in% c(0,1)) %>%
                       mutate(condition = ifelse(active_prob==1, "Active", "Passive")) %>%
                       select(model, Clabel, condition, checkpoint, skew_ratio) %>%
                       arrange(model, Clabel, checkpoint, condition)),
      row.names = FALSE, digits = 3)

## ===========================================================================
## Same checkpoint re-analysis (mean vs median at 50/90/99%) for the other two
## active-selection dimensions: Goldilocks policy, and bounded choice_k.
## ===========================================================================

## ---- Goldilocks vs. unknown policy, at active_prob=1, choice_k=Inf ----
long_g <- d %>%
  filter(active_prob == 1, choice_k_label == "Inf") %>%
  select(model, Clabel, active_policy, dec5, dec9, episodes) %>%
  pivot_longer(c(dec5, dec9, episodes), names_to = "checkpoint", values_to = "n_episodes") %>%
  mutate(checkpoint = factor(checkpoint, levels = c("dec5", "dec9", "episodes"),
                              labels = c("50%", "90%", "99%")))

summ_g <- long_g %>%
  group_by(model, Clabel, active_policy, checkpoint) %>%
  summarise(mean_ep = mean(n_episodes), median_ep = median(n_episodes),
            p10 = quantile(n_episodes, .1), p90 = quantile(n_episodes, .9),
            skew_ratio = mean_ep / median_ep, n = n(), .groups = "drop")

cat("\n\n=== Goldilocks vs unknown-policy: mean vs median at 50/90/99%, by model x C ===\n")
print(as.data.frame(summ_g %>% arrange(model, Clabel, checkpoint, active_policy) %>%
                       select(model, Clabel, checkpoint, active_policy, mean_ep, median_ep, skew_ratio, n)),
      row.names = FALSE, digits = 4)

gold_ratio <- summ_g %>% select(model, Clabel, checkpoint, active_policy, mean_ep, median_ep) %>%
  pivot_wider(names_from = active_policy, values_from = c(mean_ep, median_ep))
gold_ratio <- gold_ratio %>% mutate(
  ratio_mean   = mean_ep_unknown / mean_ep_goldilocks,
  ratio_median = median_ep_unknown / median_ep_goldilocks
)
cat("\n=== Unknown/Goldilocks ratio at each checkpoint (>1 means goldilocks is faster) ===\n")
print(as.data.frame(gold_ratio %>% select(model, Clabel, checkpoint, ratio_mean, ratio_median) %>%
                       arrange(model, Clabel, checkpoint)),
      row.names = FALSE, digits = 4)

## ---- Bounded choice_k, at active_prob=1, policy="unknown" ----
d$choice_k_num <- ifelse(d$choice_k_label == "Inf", d$M, as.numeric(d$choice_k_label))
long_k <- d %>%
  filter(active_prob == 1, active_policy == "unknown") %>%
  select(model, Clabel, choice_k_num, choice_k_label, dec5, dec9, episodes) %>%
  pivot_longer(c(dec5, dec9, episodes), names_to = "checkpoint", values_to = "n_episodes") %>%
  mutate(checkpoint = factor(checkpoint, levels = c("dec5", "dec9", "episodes"),
                              labels = c("50%", "90%", "99%")))

summ_k <- long_k %>%
  group_by(model, Clabel, choice_k_num, choice_k_label, checkpoint) %>%
  summarise(mean_ep = mean(n_episodes), median_ep = median(n_episodes),
            skew_ratio = mean_ep / median_ep, n = n(), .groups = "drop") %>%
  arrange(model, Clabel, checkpoint, choice_k_num)

cat("\n\n=== Bounded choice_k: mean vs median at 50/90/99%, by model x C ===\n")
print(as.data.frame(summ_k %>% select(model, Clabel, checkpoint, choice_k_label, mean_ep, median_ep, skew_ratio, n)),
      row.names = FALSE, digits = 4)

## smallest-K / unbounded ratio at each checkpoint, per model x C
k_bounds <- summ_k %>% group_by(model, Clabel, checkpoint) %>%
  filter(choice_k_num == min(choice_k_num) | choice_k_label == "Inf") %>%
  mutate(which = ifelse(choice_k_label == "Inf", "unbounded", "smallest_k")) %>%
  select(model, Clabel, checkpoint, which, choice_k_label, mean_ep, median_ep) %>%
  pivot_wider(names_from = which, values_from = c(choice_k_label, mean_ep, median_ep))
k_bounds <- k_bounds %>% mutate(
  ratio_mean   = mean_ep_smallest_k / mean_ep_unbounded,
  ratio_median = median_ep_smallest_k / median_ep_unbounded
)
cat("\n=== Smallest-K / Unbounded ratio at each checkpoint ===\n")
print(as.data.frame(k_bounds %>% select(model, Clabel, checkpoint, choice_k_label_smallest_k, ratio_mean, ratio_median) %>%
                       arrange(model, Clabel, checkpoint)),
      row.names = FALSE, digits = 4)

## ---- Figures ----
plot_g <- summ_g %>% mutate(condition = ifelse(active_policy == "goldilocks", "Goldilocks", "Unknown"))
pg <- ggplot(plot_g, aes(x = checkpoint, y = median_ep, color = condition, group = condition)) +
  geom_line() + geom_point(size = 2) +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = condition, group = condition), alpha = 0.15, color = NA) +
  facet_grid(Clabel ~ model, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Goldilocks = "#D55E00", Unknown = "#888888")) +
  scale_fill_manual(values = c(Goldilocks = "#D55E00", Unknown = "#888888")) +
  labs(x = "Vocabulary completion checkpoint", y = "Median episodes (log scale; ribbon = 10th-90th pctile)",
       color = NULL, fill = NULL,
       title = "Goldilocks vs. unknown-policy divergence across completion checkpoints") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/median_checkpoint_divergence_goldilocks.pdf", pg, width = 10, height = 5.5)

pk <- ggplot(summ_k, aes(x = choice_k_num, y = median_ep, color = checkpoint, group = checkpoint)) +
  geom_line() + geom_point(size = 1.5) +
  facet_grid(Clabel ~ model, scales = "free_y") +
  scale_x_log10() + scale_y_log10(labels = scales::comma) +
  labs(x = "K (log scale; M=1000 stands in for Inf)", y = "Median episodes (log scale)",
       color = "Checkpoint",
       title = "Cost of restricting the active choice set, across completion checkpoints") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/median_checkpoint_divergence_choice_k.pdf", pk, width = 10, height = 5.5)

cat("\nFigures saved: paper/median_checkpoint_divergence_goldilocks.pdf, paper/median_checkpoint_divergence_choice_k.pdf\n")
