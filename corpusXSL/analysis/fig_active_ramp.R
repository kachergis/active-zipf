# Figure for the passive-to-active developmental ramp result: episodes to
# 99% vs. ramp timescale T, forward (passive->active) vs. reverse
# (active->passive), with pure passive/active and constant-blend reference
# lines.
suppressMessages({library(ggplot2); library(dplyr)})

# Uses the 200-replication C++ runs from run_more_reps.R (group E3), which
# supersede the 30 R runs in overnight_fill/G3 and active_ramp_results.rds.
g3 <- bind_rows(lapply(list.files("elim_ramp_cpp", pattern = "rds$", full.names = TRUE), readRDS))
ramps <- g3 %>% filter(condition != "blend") %>% group_by(condition, T) %>%
  summarise(episodes = mean(episodes), .groups = "drop") %>%
  mutate(direction = ifelse(condition == "ramp_fwd", "Forward (passive -> active)", "Reverse (active -> passive)"))
refs <- g3 %>% filter(condition == "blend", active_prob %in% c(0, 1)) %>% group_by(active_prob) %>%
  summarise(episodes = mean(episodes), .groups = "drop") %>%
  mutate(condition = ifelse(active_prob == 0, "passive", "active"))

p <- ggplot(ramps, aes(x = T, y = episodes, color = direction, group = direction)) +
  geom_hline(yintercept = refs$episodes[refs$condition == "passive"], linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = refs$episodes[refs$condition == "active"], linetype = "dashed", color = "grey50") +
  annotate("text", x = 1000, y = refs$episodes[refs$condition == "passive"] * 1.15,
           label = "Passive", size = 3.2, color = "grey40", hjust = 0) +
  annotate("text", x = 1000, y = refs$episodes[refs$condition == "active"] * 0.72,
           label = "Active", size = 3.2, color = "grey40", hjust = 0) +
  geom_line() + geom_point(size = 2.3) +
  scale_x_log10(breaks = c(1000, 5000, 20000)) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("Forward (passive -> active)" = "#377eb8",
                                 "Reverse (active -> passive)" = "#D55E00")) +
  labs(x = "Ramp timescale T (episodes; log scale)", y = "Mean episodes to learn 99% (log scale)",
       color = NULL) +
  theme_bw(base_size = 12) + theme(legend.position = "top")
ggsave("../../paper2/active_ramp.pdf", p, width = 7, height = 5)
cat("Figure saved: paper2/active_ramp.pdf\n")
