# Figure for the redesigned forgetting sweep (run_decay_lock_sweep.R, lock_k=2).
# Plots censoring-aware MEDIANS: runs that never reached a checkpoint within the
# 2M-episode cap count as > cap. Where more than half the runs in a cell were
# censored, the median itself is only known to exceed the cap and is drawn as an
# open triangle at the cap.
suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})

CAP <- 2e6
d <- readRDS("decay_lock2_results.rds")
d$halflife_label <- factor(d$halflife_label, levels = c("No decay", "t1/2=20000", "t1/2=5000", "t1/2=1000", "t1/2=200"))

long <- d %>% pivot_longer(c(dec5, dec9, episodes), names_to = "checkpoint", values_to = "n_episodes") %>%
  mutate(checkpoint = factor(checkpoint, levels = c("dec5", "dec9", "episodes"), labels = c("50%", "90%", "99%")),
         n_episodes = ifelse(is.na(n_episodes), Inf, n_episodes))

summ <- long %>% group_by(halflife_label, active, checkpoint) %>%
  summarise(med = median(n_episodes),
            lo = quantile(n_episodes, .25), hi = quantile(n_episodes, .75), .groups = "drop") %>%
  mutate(censored = is.infinite(med),
         med = ifelse(censored, CAP, med),
         lo = pmin(lo, CAP), hi = pmin(hi, CAP))

p <- ggplot(summ, aes(x = checkpoint, y = med, color = active, group = active)) +
  geom_line() +
  geom_linerange(aes(ymin = lo, ymax = hi), data = filter(summ, !censored)) +
  geom_point(aes(shape = censored), size = 2.2) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 2), labels = c("Median", "Median > 2M-episode cap")) +
  facet_wrap(~halflife_label, nrow = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c(Active = "#377eb8", Passive = "#D55E00")) +
  labs(x = "Vocabulary completion checkpoint", y = "Median episodes (log scale; bar = IQR)",
       color = NULL, shape = NULL,
       title = "When tentative guesses must be rehearsed to stick, forgetting hits passive learning far harder",
       subtitle = "Guess-test learner; a guess locks after 2 consecutive confirmations and until then decays if unrehearsed (t1/2 in episodes); C=10, a=1") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90"), legend.position = "bottom")
ggsave("../../paper/decay_lock_sweep.pdf", p, width = 11, height = 4.8)
cat("Figure saved: paper/decay_lock_sweep.pdf\n")

sp <- summ %>% select(halflife_label, active, checkpoint, med, censored) %>%
  pivot_wider(names_from = active, values_from = c(med, censored)) %>%
  mutate(speedup = med_Passive / med_Active,
         speedup_label = ifelse(censored_Passive, paste0(">", round(speedup, 1)), as.character(round(speedup, 1))))
print(as.data.frame(sp %>% select(halflife_label, checkpoint, speedup_label)), row.names = FALSE)
