# Effective targeting exponent b in P(target = w) ~ p_w^b among unknown words,
# for choice sets of size k, as a function of how many words are still unknown.
source("learners.R"); set.seed(11)
p <- zipf_probs(1000, 1)
out <- expand.grid(k = c(Inf, 100, 50, 20, 5), n_unknown = c(500, 100, 20))
out$b <- NA; out$waste <- NA
for (i in seq_len(nrow(out))) {
  U <- sample(1000, out$n_unknown[i]); known <- rep(TRUE, 1000); known[U] <- FALSE
  tg <- replicate(60000, choose_target(1000, p, known, integer(1000), 1, "unknown", out$k[i]))
  cnt <- tabulate(tg, 1000)[U]; ok <- cnt > 0
  out$b[i] <- unname(coef(lm(log(cnt[ok]) ~ log(p[U][ok])))[2])
  out$waste[i] <- mean(known[tg])
}
print(out, digits = 3, row.names = FALSE)
write.csv(out, "diag_gt/choice_exponent.csv", row.names = FALSE)
