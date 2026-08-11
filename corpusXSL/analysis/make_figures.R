library(dplyr)
library(ggplot2)
library(tidyr)

d <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/full_grid_results.rds")
d$active <- factor(d$active, levels = c("Passive", "Active"))
d$fam_context <- factor(d$fam_context, levels = c("Random", "Familiar"))
d$model <- factor(d$model, levels = c("eliminative", "guesstest"),
                   labels = c("Eliminative", "Guess-test"))

# ---- Summary table: mean/median/SE by cell ----
summ <- d %>%
  mutate(dist = ifelse(uniform == "Uniform", "Uniform", paste0("Zipfian (a=", a, ")"))) %>%
  group_by(model, C, M, dist, a, uniform, active, fam_context) %>%
  summarise(mean_p99 = mean(p99), median_p99 = median(p99), se = sd(p99) / sqrt(n()), n = n(), .groups = "drop") %>%
  arrange(model, M, C, uniform, a, active, fam_context)
write.csv(summ, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/full_grid_summary.csv", row.names = FALSE)
print(as.data.frame(summ), row.names = FALSE)

# ---- Figure: speedup vs. Zipf exponent (M=1000, C=10, eliminative, matching Fig fig-estimate style) ----
zf <- summ %>% filter(M == 1000, uniform == "Zipfian", model == "Eliminative")
wide <- zf %>% select(C, a, active, fam_context, mean_p99) %>%
  pivot_wider(names_from = c(active, fam_context), values_from = mean_p99, names_sep = "_")

p1 <- ggplot(zf, aes(x = a, y = mean_p99, color = active, linetype = fam_context, group = interaction(active, fam_context))) +
  geom_point(size = 2) + geom_line() +
  facet_wrap(~C, labeller = label_both, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Zipf exponent (a)", y = "Mean episodes to learn 99% of vocabulary (log scale)",
       color = "Target\nselection", linetype = "Context\nselection",
       title = "Eliminative learner: episodes vs. Zipf skew") +
  theme_bw(base_size = 12) + theme(legend.position = "right")
ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/exponent_sensitivity.pdf", p1, width = 8, height = 4.5)

# ---- Figure: model comparison (eliminative vs guess-test), M=1000, a=1, matched C ----
mc <- summ %>% filter(M == 1000, C %in% c(10, 100), (uniform == "Uniform") | (a == 1))
mc$dist2 <- factor(ifelse(mc$uniform == "Uniform", "Uniform", "Zipfian (a=1)"), levels = c("Uniform", "Zipfian (a=1)"))

p2 <- ggplot(mc, aes(x = fam_context, y = mean_p99, fill = active)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_p99 - se, ymax = mean_p99 + se), position = position_dodge(width = 0.7), width = 0.2) +
  facet_grid(model ~ dist2 + C, scales = "free_y", labeller = label_both) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Distractor/Context Selection", y = "Mean episodes to learn 99% (log scale)", fill = "Target\nSelection") +
  theme_bw(base_size = 11) + theme(legend.position = "top", strip.background = element_rect(fill = "grey90"))
ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/model_comparison.pdf", p2, width = 9, height = 6)

cat("Figures saved.\n")
