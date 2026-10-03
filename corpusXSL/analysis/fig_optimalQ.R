# Recreation of OptimalQDistribution3.pdf (Figure fig-optimalQ): marginal
# probability of referents under the optimal active q-hat (line) vs. the
# uniform-q passive baseline (dots), rank-ordered, log-log. Uses the exact
# marginals already computed when the real SUN P matrix was reconstructed
# (corpusXSL/analysis/build_sun_matrix.R) -- no new computation needed.
suppressMessages({library(ggplot2); library(dplyr)})

load("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/sun_active_results.RData")

mkdf <- function(marg, label) {
  s <- sort(marg, decreasing = TRUE)
  data.frame(rank = seq_along(s), prob = s, which = label)
}
d <- rbind(mkdf(marg_pass, "Baseline (uniform q)"), mkdf(marg_act, "Optimal (q-hat)"))
d$which <- factor(d$which, levels = c("Baseline (uniform q)", "Optimal (q-hat)"))

p <- ggplot(d, aes(x = rank, y = prob, color = which, shape = which)) +
  geom_point(data = subset(d, which == "Baseline (uniform q)"), size = 1, alpha = 0.6) +
  geom_line(data = subset(d, which == "Optimal (q-hat)"), linewidth = 0.8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::scientific) +
  scale_color_manual(values = c("Baseline (uniform q)" = "grey40", "Optimal (q-hat)" = "#D55E00")) +
  labs(x = "Rank (objects, sorted by marginal probability)", y = "Marginal probability",
       color = NULL, shape = NULL) +
  theme_bw(base_size = 12) + theme(legend.position = "top")

ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/fig_optimalQ_R.pdf", p, width = 6.5, height = 5)
cat("fig_optimalQ_R.pdf saved\n")
cat("min(marg_pass):", min(marg_pass), " min(marg_act):", min(marg_act), " R:", min(marg_act)/min(marg_pass), "\n")
