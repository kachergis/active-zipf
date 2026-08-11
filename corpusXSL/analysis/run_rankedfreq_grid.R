source("learners.R")
cl <- makeCluster(12)
registerDoParallel(cl)
log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n")

results <- list()
run_cell <- function(C, M, a, uniform, active, fam_context, reps) {
  log_msg(sprintf("start: rankedfreq M=%-6d C=%-4d a=%-4s uniform=%-5s active=%-7s fam=%-7s reps=%d",
                   M, C, a, uniform, active, fam_context, reps))
  t0 <- Sys.time()
  r <- repeat_sim("rankedfreq", C, M, a, uniform, active, fam_context, reps = reps)
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  log_msg(sprintf("  done in %.1fs, mean p99=%.0f", dt, mean(r[, 10])))
  d <- as.data.frame(r)
  names(d) <- c("dec1","dec2","dec3","dec4","dec5","dec6","dec7","dec8","dec9","p99")
  d$model <- "rankedfreq"; d$C <- C; d$M <- M; d$a <- a
  d$active <- ifelse(active, "Active", "Passive")
  d$uniform <- ifelse(uniform, "Uniform", "Zipfian")
  d$fam_context <- ifelse(fam_context, "Familiar", "Random")
  d
}

C <- 10; M <- 1000; reps <- 30
for (active in c(FALSE, TRUE)) for (fam in c(FALSE, TRUE))
  results[[length(results)+1]] <- run_cell(C, M, 0, TRUE, active, fam, reps)
for (a in c(0.5, 1)) for (active in c(FALSE, TRUE)) for (fam in c(FALSE, TRUE))
  results[[length(results)+1]] <- run_cell(C, M, a, FALSE, active, fam, reps)
for (fam in c(FALSE, TRUE))
  results[[length(results)+1]] <- run_cell(C, M, 1.5, FALSE, TRUE, fam, reps)

final <- do.call(rbind, results)
saveRDS(final, "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/rankedfreq_results.rds")
log_msg("Saved", nrow(final), "rows.")
stopCluster(cl)
