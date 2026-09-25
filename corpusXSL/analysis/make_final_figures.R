suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

## ---- Figure 1: exponent sensitivity: C=10 (both models) and C=100 (eliminative) ----
## C=100 cells come from the fast C++ learner: run_c100_exponent_sweep.R
## (a <= 0.75, cap 5e7; "ext" run a = .8-.9, cap 1e8) and run_c100_a1.R (a = 1,
## cap 5e8). A cell in which some runs hit the cap is plotted at its mean with
## capped runs counted AT the cap (a lower bound), marked with an open symbol.
elim <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/full_grid_results.rds")
elim$model <- "Eliminative"
guess <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/guesstest_results.rds")
guess$model <- "Guess-test"
d10 <- bind_rows(elim, guess) %>% filter(C == 10) %>%
  transmute(panel = paste0(model, ", C=10"), a, active, fam_context, p99, censored = 0)

sw <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/c100_exponent_sweep_results.rds") %>%
  mutate(cap = 5e7)
ext_f <- "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/c100_exponent_sweep_ext_results.rds"
if (file.exists(ext_f)) sw <- bind_rows(sw, readRDS(ext_f) %>% mutate(cap = 1e8))
a1 <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/c100_a1_results.rds") %>%
  mutate(a = 1, cap = 5e8)
d100 <- bind_rows(sw, a1) %>%
  transmute(panel = "Eliminative, C=100", a, active, fam_context = context,
            p99 = ifelse(censored == 1, cap, episodes), censored)

dd <- bind_rows(d10, d100)
dd$active <- factor(dd$active, levels = c("Passive", "Active"))
dd$fam_context <- factor(dd$fam_context, levels = c("Random", "Familiar"))
dd$panel <- factor(dd$panel, levels = c("Eliminative, C=10", "Guess-test, C=10", "Eliminative, C=100"))

summ10 <- dd %>% group_by(panel, a, active, fam_context) %>%
  summarise(mean_p99 = mean(p99), any_capped = any(censored == 1), .groups = "drop")

p1 <- ggplot(summ10, aes(x = a, y = mean_p99, color = active, linetype = fam_context,
                          group = interaction(active, fam_context))) +
  geom_line() + geom_point(aes(shape = any_capped), size = 2) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 2),
                     labels = c("All runs finished", "Some/all runs hit cap (lower bound)")) +
  facet_wrap(~panel, nrow = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Zipf exponent (a); a=0 is uniform", y = "Mean episodes to learn 99% (log scale)",
       color = "Target\nselection", linetype = "Context\nselection", shape = NULL) +
  theme_bw(base_size = 12) + theme(legend.position = "right", strip.background = element_rect(fill = "grey90"))
ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/exponent_sensitivity.pdf", p1, width = 11, height = 4.2)
cat("exponent_sensitivity.pdf saved\n")
print(as.data.frame(summ10 %>% filter(panel == "Eliminative, C=100")), row.names = FALSE, digits = 5)

## ---- Figure 2: incremental_sim_summary.pdf, eliminative, C=10 vs C=100, a=1 vs uniform, NOT averaged ----
## C=100 cells come from the fast C++ learner (run_c100_a1.R for Zipfian a=1;
## the a=0 cells of run_c100_exponent_sweep.R for uniform). They replace the
## older Zipf-Mandelbrot-form data (old_eliminative_C100.csv) used previously.
## Zipfian C=100 random-context runs never finish (all capped at 5e8 episodes);
## they are drawn at the cap as faded, dashed bars, i.e. as lower bounds.
CAP100 <- 5e8
new_c10 <- elim %>% filter(C == 10, (uniform == "Uniform" & a == 0) | (uniform == "Zipfian" & a == 1)) %>%
  transmute(C, uniform, active, fam_context, p99, censored = 0)
z100 <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/c100_a1_results.rds") %>%
  transmute(C = 100, uniform = "Zipfian", active, fam_context = context,
            p99 = ifelse(censored == 1, CAP100, episodes), censored)
u100 <- readRDS("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/c100_exponent_sweep_results.rds") %>%
  filter(a == 0) %>%
  transmute(C = 100, uniform = "Uniform", active, fam_context = context, p99 = episodes, censored)
combined <- bind_rows(new_c10, z100, u100)
combined$active <- factor(combined$active, levels = c("Passive", "Active"))
combined$fam_context <- factor(combined$fam_context, levels = c("Random", "Familiar"))
combined$uniform <- factor(combined$uniform, levels = c("Uniform", "Zipfian"))
combined$Clabel <- factor(paste0("C=", combined$C), levels = c("C=10", "C=100"))

summ2 <- combined %>% group_by(Clabel, uniform, active, fam_context) %>%
  summarise(mean_p99 = mean(p99), se = ifelse(all(censored == 1), NA, sd(p99) / sqrt(n())),
            capped = all(censored == 1), n = n(), .groups = "drop")

p2 <- ggplot(summ2, aes(x = fam_context, y = mean_p99, fill = active)) +
  geom_col(aes(alpha = capped, linetype = capped), color = "grey20",
           position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_p99 - se, ymax = mean_p99 + se), position = position_dodge(width = 0.7), width = 0.2,
                na.rm = TRUE) +
  geom_text(data = filter(summ2, capped), aes(label = "never\nfinished"), position = position_dodge(width = 0.7),
            vjust = -0.3, size = 2.8, lineheight = 0.85) +
  facet_grid(uniform ~ Clabel) +
  scale_y_log10(labels = scales::comma, expand = expansion(mult = c(0.02, 0.15))) +
  scale_fill_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.3), guide = "none") +
  scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
  labs(x = "Distractor/Context Selection", y = "Mean episodes to learn 99% (log scale)", fill = "Target\nSelection") +
  theme_bw(base_size = 12) + theme(legend.position = "top", strip.background = element_rect(fill = "grey90"))
ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/incremental_sim_summary.pdf", p2, width = 7.5, height = 6)
cat("incremental_sim_summary.pdf (regenerated, C disaggregated) saved\n")
