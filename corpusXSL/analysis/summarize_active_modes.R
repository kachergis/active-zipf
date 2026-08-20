#!/usr/bin/env Rscript
# Summarizes active_modes_results.rds (produced by run_active_modes_grid.R):
# three figures, one per new dimension (partial autonomy, Goldilocks policy,
# bounded choice), each faceted by model.
suppressMessages({library(dplyr); library(ggplot2)})

d <- readRDS("active_modes_results.rds")
d$episodes <- as.numeric(d$episodes)
d$censored <- as.numeric(d$censored)
d$active_prob <- as.numeric(d$active_prob)
d <- d %>% filter(censored == 0)
d$model <- factor(d$model, levels = c("eliminative", "guesstest", "rankedfreq"),
                   labels = c("Eliminative", "Guess-test", "Ranked-frequency"))

## ---- Item 1: active_prob (partial autonomy) ----
s1 <- d %>% filter(sweep %in% c("baseline", "active_prob")) %>%
  group_by(model, active_prob) %>%
  summarise(mean_episodes = mean(episodes), se = sd(episodes) / sqrt(n()), n = n(), .groups = "drop")

cat("=== Item 1: partial autonomy (active_prob) ===\n")
print(as.data.frame(s1), row.names = FALSE)

p1 <- ggplot(s1, aes(x = active_prob, y = mean_episodes)) +
  geom_line(color = "#D55E00") + geom_point(size = 2, color = "#D55E00") +
  geom_errorbar(aes(ymin = mean_episodes - se, ymax = mean_episodes + se), width = 0.02) +
  facet_wrap(~model, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  labs(x = "P(active choice granted this episode)", y = "Mean episodes to learn 99% (log scale)",
       title = "How much of the time must a learner get their choice?") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/active_prob_sweep.pdf", p1, width = 9, height = 4)

## ---- Item 2: Goldilocks vs. unknown policy ----
s2 <- d %>% filter(sweep == "policy") %>%
  group_by(model, active_policy) %>%
  summarise(mean_episodes = mean(episodes), se = sd(episodes) / sqrt(n()), n = n(), .groups = "drop")

cat("\n=== Item 2: target-selection policy (unknown vs. goldilocks) ===\n")
print(as.data.frame(s2), row.names = FALSE)

p2 <- ggplot(s2, aes(x = active_policy, y = mean_episodes, fill = active_policy)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_episodes - se, ymax = mean_episodes + se), width = 0.2) +
  facet_wrap(~model, scales = "free_y") +
  scale_fill_manual(values = c(unknown = "#888888", goldilocks = "#D55E00")) +
  labs(x = NULL, y = "Mean episodes to learn 99%", fill = NULL,
       title = "Does preferring intermediate-exposure words help over preferring any unknown word?") +
  theme_bw(base_size = 12) + theme(legend.position = "none", strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/goldilocks_policy.pdf", p2, width = 9, height = 4)

## ---- Item 3: bounded choice_k ----
d$choice_k_num <- ifelse(d$choice_k_label == "Inf", 1000, as.numeric(d$choice_k_label))  # M=1000 stand-in for Inf on the x-axis
s3 <- d %>% filter(sweep %in% c("choice_k", "policy") & active_policy == "unknown" & active_prob == 1) %>%
  group_by(model, choice_k_num, choice_k_label) %>%
  summarise(mean_episodes = mean(episodes), se = sd(episodes) / sqrt(n()), n = n(), .groups = "drop") %>%
  arrange(model, choice_k_num)

cat("\n=== Item 3: bounded choice set (choice_k) ===\n")
print(as.data.frame(s3), row.names = FALSE)

p3 <- ggplot(s3, aes(x = choice_k_num, y = mean_episodes)) +
  geom_line(color = "#D55E00") + geom_point(size = 2, color = "#D55E00") +
  geom_errorbar(aes(ymin = mean_episodes - se, ymax = mean_episodes + se), width = 0.02) +
  facet_wrap(~model, scales = "free_y") +
  scale_x_log10(breaks = c(5, 20, 50, 100, 1000), labels = c("5", "20", "50", "100", "Inf (M)")) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "K: size of the randomly-available choice window (log scale)",
       y = "Mean episodes to learn 99% (log scale)",
       title = "How much does restricting the active choice set cost?") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/choice_k_sweep.pdf", p3, width = 9, height = 4)

cat("\nFigures saved: paper/active_prob_sweep.pdf, paper/goldilocks_policy.pdf, paper/choice_k_sweep.pdf\n")
