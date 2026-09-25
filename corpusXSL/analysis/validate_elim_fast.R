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
