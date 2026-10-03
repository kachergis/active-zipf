# Figure for the passive-to-active developmental ramp result: episodes to
# 99% vs. ramp timescale T, forward (passive->active) vs. reverse
# (active->passive), with pure passive/active and constant-blend reference
# lines.
suppressMessages({library(ggplot2); library(dplyr)})

r <- as.data.frame(readRDS("active_ramp_results.rds"))
r$condition <- rownames(r)

ramps <- r %>% filter(grepl("^ramp_", condition)) %>%
  mutate(direction = ifelse(grepl("fwd", condition), "Forward (passive -> active)", "Reverse (active -> passive)"),
         T = as.numeric(sub(".*_T", "", condition)))

refs <- r %>% filter(condition %in% c("passive", "active")) %>% select(condition, episodes)
const <- r %>% filter(grepl("^const_", condition)) %>%
  mutate(p = as.numeric(sub("const_", "", condition)))

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
ggsave("../../paper/active_ramp.pdf", p, width = 7, height = 5)
cat("Figure saved: paper/active_ramp.pdf\n")
