# Figure for the Flickr8k linger-vs-move-on appendix: episodes to 99% vs.
# move_cost, for both active policies, with the passive baseline as a
# reference line, showing the move-on/linger crossover.
suppressMessages({library(ggplot2)})

# Frequency-weighted active targeting, 100 replications (run_flickr_freq.R).
d <- do.call(rbind, lapply(list.files("flickr_freq", pattern = "rep.*rds$", full.names = TRUE), readRDS))
agg <- aggregate(c99 ~ policy + move_cost, data = d, FUN = mean)
passive_99 <- agg$c99[agg$policy == "passive"]
summ <- agg[agg$policy != "passive", ]
summ$policy <- ifelse(summ$policy == "active_moveon", "Move on", "Linger")
names(summ)[names(summ) == "c99"] <- "0.99"

p <- ggplot(summ, aes(x = move_cost, y = .data[["0.99"]], color = policy, group = policy)) +
  geom_line() + geom_point(size = 2.2) +
  geom_hline(yintercept = passive_99, linetype = "dashed", color = "grey40") +
  annotate("text", x = max(summ$move_cost) * 0.7, y = passive_99 * 1.08,
           label = sprintf("Passive baseline (%s)", format(round(passive_99), big.mark = ",")), size = 3.3, color = "grey30", hjust = 0) +
  scale_color_manual(values = c("Move on" = "#377eb8", "Linger" = "#D55E00")) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "move_cost (extra episodes charged per photo switch)",
       y = "Mean episodes to learn 99% (log scale)", color = NULL) +
  theme_bw(base_size = 12)
ggsave("../../paper2/flickr_linger_crossover.pdf", p, width = 7, height = 4.5)
cat("Figure saved: paper2/flickr_linger_crossover.pdf\n")
