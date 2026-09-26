# Validates learners_fast.cpp's elim_fast() against the reference R
# implementation (learn_corpus_eliminative in learners.R) on cells the R
# version can finish: all four C=10 cells, plus C=100 Active/Familiar.
# Different seeds for the two implementations -- we check that the
# distributions agree (means within sampling error), not that draws match.
source("learners.R")
Rcpp::sourceCpp("learners_fast.cpp")
suppressMessages(library(parallel))
M <- 1000; a <- 1; REPS <- 30
cells <- expand.grid(active = c(FALSE, TRUE), fam = c(FALSE, TRUE), C = 10)
cells <- rbind(cells, data.frame(active = TRUE, fam = TRUE, C = 100))
for (i in seq_len(nrow(cells))) {
  cl <- cells[i, ]
  r_R <- unlist(mclapply(1:REPS, function(s) { set.seed(s)
    learn_corpus_eliminative(C = cl$C, M = M, a = a, uniform = FALSE, active = cl$active, fam_context = cl$fam)[10] }, mc.cores = 12))
  r_C <- unlist(mclapply(1:REPS, function(s) { set.seed(90000 + s)
    elim_fast(cl$C, M, zipf_probs(M, a), cl$active, cl$fam)[10] }, mc.cores = 12))
  z <- (mean(r_C) - mean(r_R)) / sqrt(var(r_C) / REPS + var(r_R) / REPS)
  cat(sprintf("C=%3d %-7s %-8s  R: %9.0f (SE %6.0f)   C++: %9.0f (SE %6.0f)   z=%5.2f\n",
              cl$C, ifelse(cl$active, "Active", "Passive"), ifelse(cl$fam, "Familiar", "Random"),
              mean(r_R), sd(r_R) / sqrt(REPS), mean(r_C), sd(r_C) / sqrt(REPS), z))
}

## Mutual exclusivity: compare against the R runs saved by
## run_mutual_exclusivity_analysis.R (C=10; all 30 reps uncensored there).
if (file.exists("mutual_exclusivity_results.rds")) {
  me <- readRDS("mutual_exclusivity_results.rds")
  for (act in c(FALSE, TRUE)) {
    rR <- me$episodes[me$C == 10 & me$active == act & me$censored == 0]
    rC <- unlist(mclapply(1:100, function(s) { set.seed(80000 + s)
      elim_fast(10, M, zipf_probs(M, a), act, FALSE, mutual_exclusivity = TRUE)[10] }, mc.cores = 12))
    z <- (mean(rC) - mean(rR)) / sqrt(var(rC) / length(rC) + var(rR) / length(rR))
    cat(sprintf("ME C= 10 %-7s  R: %9.0f (SE %6.0f, n=%d)   C++: %9.0f (SE %6.0f, n=%d)   z=%5.2f\n",
                ifelse(act, "Active", "Passive"), mean(rR), sd(rR) / sqrt(length(rR)), length(rR),
                mean(rC), sd(rC) / sqrt(length(rC)), length(rC), z))
  }
}

## Target-selection variants (active_prob mixtures, goldilocks, choice_k):
## compare against the R learner's overnight sweep at C=10 (eliminative,
## a=1, random context; up to 3000 reps per cell).
if (file.exists("active_modes_results_overnight.rds")) {
  suppressMessages(library(dplyr))
  ov <- readRDS("active_modes_results_overnight.rds") %>%
    filter(model == "eliminative", C == 10, censored == 0)
  variants <- data.frame(ap = c(0.25, 0.5, 0.75, 1, 1, 1, 1, 1),
                         policy = c("unknown", "unknown", "unknown", "goldilocks", "unknown", "unknown", "unknown", "unknown"),
                         k = c("Inf", "Inf", "Inf", "Inf", "5", "20", "50", "100"))
  for (i in seq_len(nrow(variants))) {
    v <- variants[i, ]
    rR <- ov$episodes[ov$active_prob == v$ap & ov$active_policy == v$policy & ov$choice_k_label == v$k]
    kk <- if (v$k == "Inf") 0L else as.integer(v$k)
    rC <- unlist(mclapply(1:200, function(s) { set.seed(60000 + 1000 * i + s)
      elim_fast(10, M, zipf_probs(M, a), TRUE, FALSE, active_prob = v$ap,
                policy = ifelse(v$policy == "goldilocks", 1L, 0L), choice_k = kk)[10] }, mc.cores = 12))
    z <- (mean(rC) - mean(rR)) / sqrt(var(rC) / length(rC) + var(rR) / length(rR))
    cat(sprintf("C=10 ap=%.2f %-10s k=%-3s  R: %8.0f (n=%4d)   C++: %8.0f (n=%d)   z=%5.2f\n",
                v$ap, v$policy, v$k, mean(rR), length(rR), mean(rC), length(rC), z))
  }
}

## Familiar context combined with choice_k / goldilocks (not covered by the
## overnight sweep, which used random context only). Checked in development:
##   C=10  familiar, k=20:          R 4,482 (n=60)  vs C++ 4,499 (n=200), z= 0.83
##   C=10  familiar, goldilocks:    R 3,235 (n=60)  vs C++ 3,246 (n=200), z= 1.98
##   C=100 familiar, goldilocks:    R 8,958 (n=24)  vs C++ 8,981 (n=60),  z= 0.34
##   C=100 familiar, k=100:         R 87,506 (n=72) vs C++ 87,578 (n=400), z= 0.14
## (the R runs at C=100 take ~10 min for 72 reps on 12 cores, so this block is
##  off by default; set RUN_SLOW <- TRUE to repeat it)
RUN_SLOW <- FALSE
if (RUN_SLOW) {
  famk <- function(C, k, policy, nR, nC) {
    rR <- unlist(mclapply(1:nR, function(s) { set.seed(s)
      learn_corpus_eliminative(C = C, M = M, a = a, uniform = FALSE, active_prob = 1, fam_context = TRUE,
                               choice_k = ifelse(k == 0, Inf, k), active_policy = policy)[10] }, mc.cores = 12))
    rC <- unlist(mclapply(1:nC, function(s) { set.seed(50000 + s)
      elim_fast(C, M, zipf_probs(M, a), FALSE, TRUE, active_prob = 1, choice_k = k,
                policy = ifelse(policy == "goldilocks", 1L, 0L))[10] }, mc.cores = 12))
    cat(sprintf("C=%d familiar k=%s %-10s R: %.0f (n=%d)  C++: %.0f (n=%d)  z=%.2f\n", C, ifelse(k == 0, "Inf", k), policy,
                mean(rR), nR, mean(rC), nC, (mean(rC) - mean(rR)) / sqrt(var(rC) / nC + var(rR) / nR)))
  }
  famk(10, 20, "unknown", 60, 200); famk(10, 0, "goldilocks", 60, 200)
  famk(100, 100, "unknown", 72, 400); famk(100, 0, "goldilocks", 24, 60)
}
