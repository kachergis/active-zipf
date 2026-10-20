# How tight is the upper bound T+ = f_{W,eps}(min p) (main paper, Eq. 5)
# relative to the exact root T of prod_i (1 - (1 - p_i)^T) = 1 - eps (Eq. 3),
# across the Zipf exponents and completion criteria of Figure fig-estimate?
# (Reviewer 2, point 2.)

f_bound <- function(W, eps, x) log(1 - (1 - eps)^(1 / W)) / log1p(-x)

exact_root <- function(p, eps) {
  g <- function(logT) sum(log1p(-exp(exp(logT) * log1p(-p)))) - log1p(-eps)
  lo <- log(f_bound(length(p), eps, max(p))); hi <- log(f_bound(length(p), eps, min(p)))
  if (abs(hi - lo) < 1e-12) return(exp(hi))
  exp(uniroot(g, c(lo - 1e-6, hi + 1e-6), tol = 1e-10)$root)
}

grid <- expand.grid(W = c(10000, 60000), a = seq(0, 1.5, by = 0.25), eps = c(0.99, 0.5, 0.01))
grid$T_exact <- NA; grid$T_plus <- NA
for (i in seq_len(nrow(grid))) {
  W <- grid$W[i]; a <- grid$a[i]; eps <- grid$eps[i]
  p <- (seq_len(W))^-a; p <- p / sum(p)
  grid$T_exact[i] <- exact_root(p, eps)
  grid$T_plus[i] <- f_bound(W, eps, min(p))
}
grid$ratio <- grid$T_plus / grid$T_exact
print(grid, digits = 4, row.names = FALSE)
cat(sprintf("\nT+/T over a in (0, 1.5]: %.2f to %.2f\n", min(grid$ratio[grid$a > 0]), max(grid$ratio[grid$a > 0])))
write.csv(grid, "bound_tightness.csv", row.names = FALSE)
