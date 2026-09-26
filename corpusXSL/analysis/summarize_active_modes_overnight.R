#!/usr/bin/env Rscript
# Summarizes active_modes_results_overnight.rds (produced by
# run_active_modes_grid_overnight.R). Unlike the quick-run summarizer, sweep
# membership isn't stored -- it's derived from the parameter values, since a
# cell like (active_prob=1, policy="unknown", choice_k=Inf) is simultaneously
# the top of the active_prob sweep, the "unknown" reference for the policy
# sweep, and the Inf endpoint of the choice_k sweep.
suppressMessages({library(dplyr); library(ggplot2)})

d <- readRDS("active_modes_results_overnight.rds")
d$episodes <- as.numeric(d$episodes)
d$censored <- as.numeric(d$censored)
d$active_prob <- as.numeric(d$active_prob)
d$C <- as.numeric(d$C)
d <- d %>% filter(censored == 0)
d$model <- factor(d$model, levels = c("eliminative", "guesstest", "rankedfreq"),
                   labels = c("Eliminative", "Guess-test", "Ranked-frequency"))
d$Clabel <- factor(paste0("C=", d$C), levels = c("C=10", "C=100"))

## ---- Item 1: active_prob (partial autonomy), at policy="unknown", choice_k=Inf ----
s1 <- d %>% filter(active_policy == "unknown", choice_k_label == "Inf") %>%
  group_by(model, Clabel, active_prob) %>%
  summarise(mean_episodes = mean(episodes), se = sd(episodes) / sqrt(n()), n = n(), .groups = "drop")

cat("=== Item 1: partial autonomy (active_prob), by C ===\n")
print(as.data.frame(s1), row.names = FALSE)

p1 <- ggplot(s1, aes(x = active_prob, y = mean_episodes, color = Clabel, group = Clabel)) +
  geom_line() + geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_episodes - se, ymax = mean_episodes + se), width = 0.02) +
  facet_wrap(~model, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("C=10" = "#377eb8", "C=100" = "#D55E00")) +
  labs(x = "P(active choice granted this episode)", y = "Mean episodes to learn 99% (log scale)",
       color = "Context size", title = "How much of the time must a learner get their choice? (C=10 vs C=100)") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/active_prob_sweep_overnight.pdf", p1, width = 10, height = 4.2)

## ---- Item 2: Goldilocks vs. unknown policy, at active_prob=1, choice_k=Inf ----
s2 <- d %>% filter(active_prob == 1, choice_k_label == "Inf") %>%
  group_by(model, Clabel, active_policy) %>%
  summarise(mean_episodes = mean(episodes), se = sd(episodes) / sqrt(n()), n = n(), .groups = "drop")

cat("\n=== Item 2: target-selection policy (unknown vs. goldilocks), by C ===\n")
print(as.data.frame(s2), row.names = FALSE)

p2 <- ggplot(s2, aes(x = active_policy, y = mean_episodes, fill = active_policy)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_episodes - se, ymax = mean_episodes + se), width = 0.2) +
  facet_grid(Clabel ~ model, scales = "free_y") +
  scale_fill_manual(values = c(unknown = "#888888", goldilocks = "#D55E00")) +
  labs(x = NULL, y = "Mean episodes to learn 99%", fill = NULL,
       title = "Goldilocks vs. any-unknown targeting, C=10 vs C=100") +
  theme_bw(base_size = 12) + theme(legend.position = "none", strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/goldilocks_policy_overnight.pdf", p2, width = 9, height = 6)

## ---- Item 3: bounded choice_k, at active_prob=1, policy="unknown" ----
d$choice_k_num <- ifelse(d$choice_k_label == "Inf", d$M, as.numeric(d$choice_k_label))
s3 <- d %>% filter(active_prob == 1, active_policy == "unknown") %>%
  group_by(model, Clabel, choice_k_num, choice_k_label) %>%
  summarise(mean_episodes = mean(episodes), se = sd(episodes) / sqrt(n()), n = n(), .groups = "drop") %>%
  arrange(model, Clabel, choice_k_num)

cat("\n=== Item 3: bounded choice set (choice_k), by C ===\n")
print(as.data.frame(s3), row.names = FALSE)

p3 <- ggplot(s3, aes(x = choice_k_num, y = mean_episodes, color = Clabel, group = Clabel)) +
  geom_line() + geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_episodes - se, ymax = mean_episodes + se), width = 0.02) +
  facet_wrap(~model) +
  scale_x_log10() +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("C=10" = "#377eb8", "C=100" = "#D55E00")) +
  labs(x = "K: size of the randomly-available choice window (log scale; M=1000 stands in for Inf)",
       y = "Mean episodes to learn 99% (log scale)", color = "Context size",
       title = "Cost of restricting the active choice set, C=10 vs C=100") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/choice_k_sweep_overnight.pdf", p3, width = 10, height = 4.2)

## ---- Flag the specific open question from the quick run: is guess-test's
## choice_k non-monotonicity (K=20 beating even unbounded choice) real now
## that SEs are tighter? ----
gt3 <- s3 %>% filter(model == "Guess-test")
cat("\n=== Guess-test choice_k curve specifically (was non-monotonic in the quick run) ===\n")
print(as.data.frame(gt3), row.names = FALSE)
if (nrow(gt3) > 0) {
  min_row <- gt3[which.min(gt3$mean_episodes), ]
  inf_row <- gt3[gt3$choice_k_label == "Inf" & gt3$Clabel == min_row$Clabel, ]
  if (nrow(inf_row) > 0 && min_row$choice_k_label != "Inf") {
    cat(sprintf("NOTE: minimum at K=%s (%.0f +/- %.0f) is still below unbounded choice (%.0f +/- %.0f) at %s -- ",
                min_row$choice_k_label, min_row$mean_episodes, min_row$se,
                inf_row$mean_episodes, inf_row$se, min_row$Clabel))
    gap_in_se <- abs(min_row$mean_episodes - inf_row$mean_episodes) / sqrt(min_row$se^2 + inf_row$se^2)
    cat(sprintf("gap is %.1f combined SEs, %s\n", gap_in_se,
                if (gap_in_se > 2) "looks like a real effect now." else "still within noise -- treat as unresolved."))
  }
}

cat("\nFigures saved: paper/active_prob_sweep_overnight.pdf, paper/goldilocks_policy_overnight.pdf, paper/choice_k_sweep_overnight.pdf\n")
