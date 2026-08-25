# Figure for the guess-test forgetting/decay robustness check (run_decay_sweep.R).
suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

d <- readRDS("decay_sweep_results.rds") %>% filter(censored == 0)
d$halflife_label <- factor(d$halflife_label, levels = c("No decay", "t1/2=20000", "t1/2=5000", "t1/2=1000", "t1/2=200"))

long <- d %>% pivot_longer(c(dec5, dec9, episodes), names_to = "checkpoint", values_to = "n_episodes") %>%
  mutate(checkpoint = factor(checkpoint, levels = c("dec5", "dec9", "episodes"), labels = c("50%", "90%", "99%")))

summ <- long %>% group_by(halflife_label, active, checkpoint) %>%
  summarise(mean_ep = mean(n_episodes), se = sd(n_episodes) / sqrt(n()), .groups = "drop")

p <- ggplot(summ, aes(x = checkpoint, y = mean_ep, color = active, group = active)) +
  geom_line() + geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_ep - se, ymax = mean_ep + se), width = 0.05) +
  facet_wrap(~halflife_label, nrow = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Active = "#377eb8", Passive = "#D55E00")) +
  labs(x = "Vocabulary completion checkpoint", y = "Mean episodes (log scale)", color = NULL,
       title = "Forgetting widens, rather than narrows, guess-test's active/passive gap",
       subtitle = "Unconfirmed guesses decay if not rehearsed (t1/2 = forgetting half-life in episodes); C=10, a=1") +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/decay_sweep.pdf", p, width = 11, height = 4.5)
cat("Figure saved: paper/decay_sweep.pdf\n")
