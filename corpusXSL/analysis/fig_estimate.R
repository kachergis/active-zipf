# Recreation of Summary2.eps (Figure fig-estimate): required number of samples
# T+ from a generalized Zipf distribution (p_k = k^-a / sum(k^-a)) needed for
# 1%, 50%, and 99% of learners to reach vocabulary size W=10000 or W=60000, as
# a function of the exponent a. Pure formula application (Equation eq-Tbound /
# eqA-M in the paper) -- no simulation needed.
suppressMessages({library(dplyr); library(ggplot2)})

f <- function(W, eps, x) log(1 - (1 - eps)^(1 / W)) / log(1 - x)

min_zipf_p <- function(W, a) {
  if (a == 0) return(1 / W)
  probs <- (1:W)^(-a)
  min(probs / sum(probs))
}

grid <- expand.grid(W = c(10000, 60000), a = seq(0, 1.5, by = 0.05), eps = c(0.01, 0.5, 0.99))
grid$minp <- mapply(min_zipf_p, grid$W, grid$a)
grid$Tplus <- mapply(f, grid$W, grid$eps, grid$minp)
grid$pct_learned <- factor(1 - grid$eps, levels = c(0.01, 0.5, 0.99),
                            labels = c("1% of learners", "50% of learners (median)", "99% of learners"))
grid$linetype <- ifelse(grid$eps == 0.5, "solid", "dotted")
grid$Wlabel <- factor(paste0("W = ", format(grid$W, big.mark = ",")),
                       levels = c("W = 10,000", "W = 60,000"))

p <- ggplot(grid, aes(x = a, y = Tplus, group = pct_learned, linetype = linetype, color = Wlabel)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Wlabel, scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  scale_linetype_identity() +
  scale_color_manual(values = c("W = 10,000" = "#377eb8", "W = 60,000" = "#e41a1c"), guide = "none") +
  labs(x = "Zipf exponent (a); a = 0 is uniform",
       y = expression(T[symbol("+")] ~ "(number of samples, log scale)")) +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90"))

ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/fig_estimate_R.pdf", p, width = 8, height = 4.2)
cat("fig_estimate_R.pdf saved\n")

# sanity check against numbers already reported in the paper text
cat("\nSanity checks (should match Section sec-fastmap prose):\n")
cat("W=60000, a=1, eps=.01: T+ =", f(60000, 0.01, min_zipf_p(60000, 1)), "(paper reports 1.08e7)\n")
cat("W=51446-equivalent not applicable here; this is the idealized-distribution figure only.\n")
