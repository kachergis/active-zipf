suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

## ---- Figure 1: exponent sensitivity, C=10, both models ----
elim <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/full_grid_results.rds")
elim$model <- "Eliminative"
guess <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/guesstest_results.rds")
guess$model <- "Guess-test"
d10 <- bind_rows(elim, guess) %>% filter(C == 10)

d10$active <- factor(d10$active, levels = c("Passive", "Active"))
d10$fam_context <- factor(d10$fam_context, levels = c("Random", "Familiar"))
d10$model <- factor(d10$model, levels = c("Eliminative", "Guess-test"))

summ10 <- d10 %>% group_by(model, a, active, fam_context) %>%
  summarise(mean_p99 = mean(p99), se = sd(p99) / sqrt(n()), n = n(), .groups = "drop")

p1 <- ggplot(summ10, aes(x = a, y = mean_p99, color = active, linetype = fam_context,
                          group = interaction(active, fam_context))) +
  geom_point(size = 2) + geom_line() +
  facet_wrap(~model) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Zipf exponent (a); a=0 is uniform", y = "Mean episodes to learn 99% (log scale)",
       color = "Target\nselection", linetype = "Context\nselection") +
  theme_bw(base_size = 12) + theme(legend.position = "right", strip.background = element_rect(fill = "grey90"))
ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/exponent_sensitivity.pdf", p1, width = 8.5, height = 4.2)
cat("exponent_sensitivity.pdf saved\n")

## ---- Figure 2: incremental_sim_summary.pdf, eliminative, C=10 vs C=100, a=1 vs uniform, NOT averaged ----
new_c10 <- elim %>% filter(C == 10, (uniform == "Uniform" & a == 0) | (uniform == "Zipfian" & a == 1))
old_c100 <- read.csv("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/old_eliminative_C100.csv", stringsAsFactors = FALSE)
old_c100$model <- "Eliminative"
old_c100 <- old_c100 %>% select(model, C, M, uniform, active, fam_context, p99)
new_c10s <- new_c10 %>% select(model, C, M, uniform, active, fam_context, p99)
combined <- bind_rows(new_c10s, old_c100)
combined$active <- factor(combined$active, levels = c("Passive", "Active"))
combined$fam_context <- factor(combined$fam_context, levels = c("Random", "Familiar"))
combined$uniform <- factor(combined$uniform, levels = c("Uniform", "Zipfian"))
combined$Clabel <- factor(paste0("C=", combined$C), levels = c("C=10", "C=100"))

summ2 <- combined %>% group_by(Clabel, uniform, active, fam_context) %>%
  summarise(mean_p99 = mean(p99), se = sd(p99) / sqrt(n()), n = n(), .groups = "drop")

p2 <- ggplot(summ2, aes(x = fam_context, y = mean_p99, fill = active)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_p99 - se, ymax = mean_p99 + se), position = position_dodge(width = 0.7), width = 0.2) +
  facet_grid(uniform ~ Clabel, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Distractor/Context Selection", y = "Mean episodes to learn 99% (log scale)", fill = "Target\nSelection") +
  theme_bw(base_size = 12) + theme(legend.position = "top", strip.background = element_rect(fill = "grey90"))
ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/incremental_sim_summary.pdf", p2, width = 7.5, height = 6)
cat("incremental_sim_summary.pdf (regenerated, C disaggregated) saved\n")
