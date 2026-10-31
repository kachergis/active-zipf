# Figure for the Flickr8k linger-vs-move-on appendix: episodes to 99% vs.
# move_cost, for both active policies, with the passive baseline as a
# reference line, showing the move-on/linger crossover.
suppressMessages({library(ggplot2)})

raw <- readRDS("flickr_incremental_results.rds")
summ <- as.data.frame(raw, check.names = FALSE)
summ$condition <- rownames(summ)
summ <- summ[grepl("^active_", summ$condition), ]
summ$policy <- ifelse(grepl("moveon", summ$condition), "Move on", "Linger")
summ$move_cost <- as.numeric(sub(".*_mc", "", summ$condition))
summ <- summ[, c("policy", "move_cost", "0.99")]

# move_cost 2 and 5 come from run_overnight_fill.R (group G4).
g4 <- do.call(rbind, lapply(list.files("overnight_fill/G4", pattern = "rds$", full.names = TRUE), readRDS))
g4s <- aggregate(c99 ~ policy + move_cost, data = g4, FUN = mean)
summ <- rbind(summ, data.frame(policy = ifelse(g4s$policy == "active_moveon", "Move on", "Linger"),
                               move_cost = g4s$move_cost, `0.99` = g4s$c99, check.names = FALSE))

passive_99 <- raw["passive", "0.99"]

p <- ggplot(summ, aes(x = move_cost, y = .data[["0.99"]], color = policy, group = policy)) +
  geom_line() + geom_point(size = 2.2) +
  geom_hline(yintercept = passive_99, linetype = "dashed", color = "grey40") +
  annotate("text", x = max(summ$move_cost) * 0.7, y = passive_99 * 1.08,
           label = "Passive baseline (151,671)", size = 3.3, color = "grey30", hjust = 0) +
  scale_color_manual(values = c("Move on" = "#377eb8", "Linger" = "#D55E00")) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "move_cost (extra episodes charged per photo switch)",
       y = "Mean episodes to learn 99% (log scale)", color = NULL) +
  theme_bw(base_size = 12)
ggsave("../../paper2/flickr_linger_crossover.pdf", p, width = 7, height = 4.5)
cat("Figure saved: paper2/flickr_linger_crossover.pdf\n")
