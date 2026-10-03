# Figure for the companion paper's Study 3 (paper2/draft.tex): cost of partial
# autonomy, goldilocks targeting, and bounded choice sets relative to fully
# active selection, eliminative learner, across four settings. Data:
# run_c10_active_modes.R (C=10) and run_c100_active_modes.R (C=100).
suppressMessages({library(dplyr); library(ggplot2)})

d10 <- read.csv("c10_active_modes_summary.csv")
d100 <- read.csv("c100_active_modes_summary.csv") %>%
  mutate(setting = recode(setting, "a=0.75, random context" = "C=100, a=0.75, random context",
                          "a=1, familiar context" = "C=100, a=1, familiar context"))
d <- bind_rows(d10, d100) %>% filter(label != "passive")
d$label <- factor(d$label, levels = c("ap=0.25", "ap=0.50", "ap=0.75", "active", "goldilocks",
                                      "k=100", "k=50", "k=20", "k=5"),
                  labels = c("25% active", "50% active", "75% active", "Fully active", "Goldilocks",
                             "Choice set 100", "Choice set 50", "Choice set 20", "Choice set 5"))
d$setting <- factor(d$setting, levels = c("C=10, a=1, random context", "C=10, a=1, familiar context",
                                          "C=100, a=0.75, random context", "C=100, a=1, familiar context"))

p <- ggplot(d, aes(x = label, y = ratio_to_active, color = setting, group = setting)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.4, position = position_dodge(width = 0.55)) +
  scale_y_log10(breaks = c(0.5, 1, 2, 5, 10), labels = c("0.5x", "1x", "2x", "5x", "10x")) +
  scale_color_manual(values = c("#9ecae1", "#3182bd", "#fdae6b", "#e6550d")) +
  labs(x = NULL, y = "Episodes to 99%, relative to fully active (log)", color = NULL) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top") +
  guides(color = guide_legend(nrow = 2))
ggsave("../../paper2/active_modes.pdf", p, width = 8.5, height = 5)
cat("Figure saved: paper2/active_modes.pdf\n")
