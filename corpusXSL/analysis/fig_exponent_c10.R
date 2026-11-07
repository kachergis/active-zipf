# Exponent-sensitivity figure for the main paper (main_revised.tex), which
# studies target selection only, at C=10 with random context. The full
# three-panel version with familiar context and C=100 (make_final_figures.R,
# exponent_sensitivity.pdf) is used by the companion paper (paper2/draft.tex).
suppressMessages({library(dplyr); library(ggplot2)})

elim <- readRDS("full_grid_results.rds") %>% mutate(model = "Eliminative")
guess <- readRDS("guesstest_results.rds") %>% mutate(model = "Guess-test")

# Passive a = 1.5 cells come from run_overnight_fill.R (group G1).
g1 <- bind_rows(lapply(list.files("overnight_fill/G1", pattern = "rds$", full.names = TRUE), readRDS)) %>%
  transmute(model, C = 10, a = 1.5, active = "Passive", fam_context = context, p99)

summ <- bind_rows(elim, guess, g1) %>%
  filter(C == 10, fam_context == "Random") %>%
  group_by(model, a, active) %>%
  summarise(mean_p99 = mean(p99), n = n(), .groups = "drop") %>%
  mutate(active = factor(active, levels = c("Passive", "Active")),
         panel = paste0(model, ", C=10"))

print(as.data.frame(summ), row.names = FALSE, digits = 5)

p <- ggplot(summ, aes(x = a, y = mean_p99, color = active, group = active)) +
  geom_line() + geom_point(size = 2) +
  facet_wrap(~panel, nrow = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Passive = "#888888", Active = "#D55E00")) +
  labs(x = "Zipf exponent (a); a=0 is uniform", y = "Mean episodes to learn 99% (log scale)",
       color = "Target\nselection") +
  theme_bw(base_size = 12) +
  theme(legend.position = "right", strip.background = element_rect(fill = "grey90"))
ggsave("../../paper/exponent_sensitivity_c10.pdf", p, width = 8, height = 3.8)
cat("exponent_sensitivity_c10.pdf saved\n")
