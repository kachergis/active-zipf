# Recreation of FreqCHILDES.eps (Figure fig-freq), using the new CHILDES
# word-frequency export (corpusXSL/childes_english_word_freq_cleaned_noHapaxes.csv)
# in place of the original (unavailable) 2007 CHILDES pull. Rank-frequency,
# log-log, matching the style of the original figure.
suppressMessages({library(ggplot2); library(dplyr)})

f <- function(W, eps, x) log(1 - (1 - eps)^(1 / W)) / log(1 - x)

d <- read.csv("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/childes_english_word_freq_cleaned_noHapaxes.csv")
d <- d[d$word_count > 0, ]
d$prob <- d$word_count / sum(d$word_count)
d <- d[order(-d$prob), ]
d$rank <- seq_len(nrow(d))

W <- nrow(d)
minp <- min(d$prob)
cat(sprintf("W=%d  min_p=%.4e  T+(eps=.01)=%.4e  T+(eps=.5)=%.4e\n",
            W, minp, f(W, 0.01, minp), f(W, 0.5, minp)))

p <- ggplot(d, aes(x = rank, y = prob)) +
  geom_point(size = 0.6, alpha = 0.5, color = "#377eb8") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::scientific) +
  labs(x = "Word frequency rank", y = "Probability") +
  theme_bw(base_size = 12)

ggsave("/Users/gkacherg/Documents/GitHub/active-zipf/paper/fig_freq_R.pdf", p, width = 6, height = 4.5)
cat("fig_freq_R.pdf saved\n")
