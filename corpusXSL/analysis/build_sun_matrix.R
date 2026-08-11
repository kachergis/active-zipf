# Build the N (objects) x M (scenes) conditional probability matrix P from the
# real SUN database export (corpusXSL/SUNdb/sundb_fulldata.txt), and compute
# the passive (uniform q) vs. optimal active (q-hat) min-probability comparison
# that the paper reports (previously only from the original, unavailable P
# matrix; this reconstructs it from the raw data the authors located).

library(Matrix)
library(lpSolve)

setwd("/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/SUNdb")

lines <- readLines("sundb_fulldata.txt")
lines <- lines[nchar(lines) > 0]
fields <- strsplit(lines, "[|]")

scenes <- sapply(fields, function(x) x[1])
scene_levels <- sort(unique(scenes))
M <- length(scene_levels)
cat("M (scenes):", M, "\n")

# Build long-format (scene, object, freq) table
triples <- lapply(fields, function(f) {
  rest <- f[-(1:3)]
  if (length(rest) < 2) return(NULL)
  idx <- seq(1, length(rest) - 1, by = 2)
  data.frame(scene = f[1], object = rest[idx], freq = as.numeric(rest[idx + 1]),
             stringsAsFactors = FALSE)
})
triples <- do.call(rbind, triples)
cat("n object-image mentions:", nrow(triples), "\n")

obj_levels <- sort(unique(triples$object))
N <- length(obj_levels)
cat("N (objects):", N, "\n")

# Aggregate: sum frequency of each object within each scene category
agg <- aggregate(freq ~ scene + object, data = triples, sum)

i <- match(agg$object, obj_levels)
j <- match(agg$scene, scene_levels)
P <- sparseMatrix(i = i, j = j, x = agg$freq, dims = c(N, M),
                   dimnames = list(obj_levels, scene_levels))

# Normalize each column (scene) to a probability distribution over objects
col_sums <- Matrix::colSums(P)
stopifnot(all(col_sums > 0))
P <- P %*% Diagonal(x = 1 / col_sums)
P <- as.matrix(P)

saveRDS(P, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/sun_P_matrix.rds")

cat("\n=== Passive baseline (uniform q) ===\n")
q_unif <- rep(1 / M, M)
marg_pass <- as.vector(P %*% q_unif)
min_p_pass <- min(marg_pass)
cat("min(P q_unif):", min_p_pass, "\n")
cat("argmin object:", obj_levels[which.min(marg_pass)], "\n")

cat("\n=== Optimal active learner (q-hat) via LP ===\n")
# maximize t  s.t.  P q >= t*1,  sum(q)=1,  q>=0,  t free
# variables: q_1..q_M, t
nvar <- M + 1
obj <- c(rep(0, M), 1)  # maximize t
# constraints: P q - t >= 0  =>  for each row i: sum_j P[i,j] q_j - t >= 0
con_mat <- cbind(P, -1)
con_dir <- rep(">=", N)
con_rhs <- rep(0, N)
# equality: sum q = 1
con_mat <- rbind(con_mat, c(rep(1, M), 0))
con_dir <- c(con_dir, "=")
con_rhs <- c(con_rhs, 1)

t0 <- Sys.time()
sol <- lp("max", obj, con_mat, con_dir, con_rhs)
cat("LP status:", sol$status, " (0 = optimal)\n")
cat("LP solve time:", as.numeric(Sys.time() - t0, units = "secs"), "s\n")

q_hat <- sol$solution[1:M]
t_opt <- sol$solution[M + 1]
marg_act <- as.vector(P %*% q_hat)
min_p_act <- min(marg_act[marg_act > 1e-12], na.rm = TRUE)  # avoid float noise near zero
cat("t (LP optimum, = min(P q_hat)):", t_opt, "\n")
cat("min(P q_hat) recomputed:", min(marg_act), "\n")

R <- t_opt / min_p_pass
cat("\n=== Speedup ratio R = min(P q_hat) / min(P q_unif) ===\n")
cat("R =", R, "\n")

save(P, q_unif, q_hat, marg_pass, marg_act, min_p_pass, t_opt, R, obj_levels, scene_levels,
     file = "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/sun_active_results.RData")

cat("\nDone.\n")
