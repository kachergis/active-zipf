# Summaries for the 100-replication guess-test Flickr8k runs (guesstest_lock2/S4)
# and the 30-replication mechanism tests (diag_gt/N17). Run from corpusXSL/analysis.
suppressMessages(library(dplyr)); set.seed(1)
s4 <- bind_rows(lapply(list.files("guesstest_lock2/S4", full.names = TRUE), readRDS))
boot <- function(x, y, f = mean, B = 5000) quantile(replicate(B, f(sample(x, replace = TRUE)) / f(sample(y, replace = TRUE))), c(.025, .975))
cat("=== S4 (lock-in), means ===\n")
s4 %>% group_by(move_cost, policy) %>% summarise(n = n(), m50 = mean(c50), m90 = mean(c90), n90 = sum(!is.na(c90)),
  n99 = sum(!is.na(c99)), m99 = mean(c99, na.rm = TRUE), final = mean(final_frac), min_final = min(final_frac), max_final = max(final_frac), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 6)
pas <- s4 %>% filter(policy == "passive")
cat("\n=== ratios with 95% bootstrap CIs ===\n")
for (mc in c(0, 1, 2, 3, 5, 10)) {
  mo <- s4 %>% filter(policy == "active_moveon", move_cost == mc); li <- s4 %>% filter(policy == "active_linger", move_cost == mc)
  for (ck in c("c50", "c90")) {
    r1 <- boot(li[[ck]], mo[[ck]]); r2 <- boot(pas[[ck]], mo[[ck]]); r3 <- boot(pas[[ck]], li[[ck]])
    cat(sprintf("mc=%2d %s linger/moveon %.3f [%.3f, %.3f] | passive/moveon %.2f [%.2f, %.2f] | passive/linger %.2f [%.2f, %.2f]\n",
                mc, ck, mean(li[[ck]]) / mean(mo[[ck]]), r1[1], r1[2], mean(pas[[ck]]) / mean(mo[[ck]]), r2[1], r2[2],
                mean(pas[[ck]]) / mean(li[[ck]]), r3[1], r3[2]))
  }
  d <- quantile(replicate(5000, mean(sample(li$final_frac, replace = TRUE)) - mean(sample(mo$final_frac, replace = TRUE))), c(.025, .975))
  cat(sprintf("      final linger - moveon %.4f [%.4f, %.4f]\n", mean(li$final_frac) - mean(mo$final_frac), d[1], d[2]))
}
cat("\n=== N17 mechanism tests ===\n")
n17 <- lapply(list.files("diag_gt/N17", full.names = TRUE), readRDS)
d <- bind_rows(lapply(n17, function(r) cbind(cond = r$cond, rep = r$rep, finished = r$finished, r$final,
  all_wrong_on_unknown = r$final$n_unknown > 0 && r$final$unk_wrong_on_unknown_ref == r$final$n_unknown)))
d %>% group_by(cond) %>% summarise(n = n(), n_fin = sum(finished), med_ep = median(episodes), min_ep_fin = suppressWarnings(min(episodes[finished])),
  max_ep_fin = suppressWarnings(max(episodes[finished])), med_ep_fin = median(episodes[finished]),
  min_unk = suppressWarnings(min(n_unknown[!finished])), max_unk = suppressWarnings(max(n_unknown[!finished])),
  all_wrong = sum(all_wrong_on_unknown & !finished), min_fam_share = suppressWarnings(min(fam_weight_share_unknown_refs[!finished])),
  med_fam_share = median(fam_weight_share_unknown_refs[!finished]), blocked_share = mean(blocked_targets / unknown_targets), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 6)
f <- function(c) d$episodes[d$cond == c]
r <- boot(f("k100_freq"), f("k100_uniform"), median); cat(sprintf("k100 freq/uniform (medians) %.1f [%.1f, %.1f]\n", median(f("k100_freq")) / median(f("k100_uniform")), r[1], r[2]))
