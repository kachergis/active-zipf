require("ggplot2")
require("tidyverse")
#sm = subset(sim, is.element(C, c(4, 10, 50, 100)) & is.element(M, c(1000)))
require("ggpubr")
require("gganimate")

#load("active_passive_sim_results.RData")

1/ sort(act_sim$p99 / pass_sim$p99) 
# choosing an unknown target 2.2-2.6 times faster for uniform freq

1/ sort(act_sim_nonu$p99 / pass_sim_nonu$p99) 
# 48.8 - 10.3 times faster for nonuniform frequency

1/ sort(act_sim_unif_famcon$p99 / pass_sim_unif_famcon$p99) 
# 3.3 - 1.6 times faster for 

1/ sort(act_sim_unif_famcon$p99 / act_sim$p99) 
# familiar context speed up between 5.5 and .24 (!!) -- average: 1.43 (must look at relationship to C and M)

# should try M = c(20000, 40000, 60000)

add_cols <- function(sim) {
  sim$logM = log(sim$M)
  sim$logeps = log(sim$p99)
  sim$CtoM = sim$C / sim$M
  sim$active = factor(sim$active)
  sim$uniform = factor(sim$uniform)
  sim$fam_context = factor(sim$fam_context, labels=c("Random Context","Familiar Context"))
  return(sim)
}


#anim <- ggplot(sim, aes(x = C, y = p99)) + 
#  geom_point(aes(colour = active), size = 2) + 
#  transition_states(active, transition_length = 2, state_length = 1)
#anim + enter_fade() + exit_shrink()

sim2 <- add_cols(sim)



###### new
load("unfam_con_M2000.RData")
sim2k = sim
load("unfam_con_M1000.RData")
sim = rbind(sim, sim2k)
with(sim, table(C, M, active, uniform, fam_context)) # no familiar context yet

siml <- sim %>% gather("decile", "episodes", 7:16)
pd <- siml %>% group_by(C, M, active, uniform, fam_context, decile) %>%
  summarise(mean = mean(episodes))
  # tidyboot::tidyboot_mean(episodes) # takes ~5 mins

dodge = position_dodge(width = 2)
pd$dec = rep(seq(10, 100, 10), nrow(pd) / 10) # 100 should be 99
pd$Mk = ifelse(pd$M==1000, "M=1000", "M=2000")
p_u <- pd %>% filter(uniform=="Uniform") %>% ggplot(aes(x=dec, y=mean, group=C, color=C)) + geom_point(position = dodge) + 
  geom_line(position = dodge) + 
  facet_grid(active ~ Mk ) + theme_bw() + xlab("Percent of Vocabulary Learned") +
  #geom_linerange(aes(ymin = ci_lower, ymax = ci_upper, color=C), position = dodge) + 
  ylab("Mean Number of Episodes") + ggtitle("Uniform Distribution")

p_z <- pd %>% filter(uniform=="Zipfian") %>% ggplot(aes(x=dec, y=mean, group=C, color=C)) + geom_point(position = dodge) + 
  geom_line(position = dodge) + 
  facet_grid(active ~ Mk ) + theme_bw() + xlab("Percent of Vocabulary Learned") +
  #geom_linerange(aes(ymin = ci_lower, ymax = ci_upper, color=C), position = dodge) + 
  ylab("Mean Number of Episodes") + ggtitle("Zipfian Distribution")

ggarrange(p_u, p_z, ncol=2)
ggsave("M1k-2k_C5-80.pdf", width=11, height=5.5)
######


sim_ag <- sim %>% group_by(C, M, active, uniform) %>% # fam_context
  summarise(d3=mean(dec3), d5=mean(dec5), d7=mean(dec7), eps=mean(p99), sd=sd(p99))



ggplot(sim_ag, aes)

ggplot(sim_ag, aes(x=C/M, y=log(eps), shape=active, color=C)) + geom_point() + ylim(0, 18) + 
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Ratio (Context Size)/(Lexicon Size)") + 
  facet_grid(rows=vars(fam_context), cols=vars(uniform)) +
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey")
ggsave("logeps_to_acquire_by_context_and_CtoM_eliminative_eps01.pdf", width=7.5, height=7)

ggplot(sim_ag, aes(x=log(M), y=log(eps), shape=active, color=C)) + geom_point() + xlim(0, 9) + ylim(0, 14) +
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  facet_grid(rows=vars(fam_context), cols=vars(uniform)) +
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey") 
ggsave("logeps_to_acquire_by_lexicon_and_context_size_eliminative_eps01.pdf", width=7.5, height=7)

ggplot(sim_ag, aes(x=M, y=log(eps), shape=active, color=C)) + geom_point() +
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Number of meanings") + 
  facet_grid(rows=vars(fam_context), cols=vars(uniform)) +
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey")
ggsave("logeps_to_acquire_by_lexicon_and_context_size_eliminative_eps01.pdf", width=7.5, height=7)

require("GGally")
cols = c("C","M","eps") # ,"sd" "CtoM",
logcols = c("C","logM","logeps") # "logsd"
ggpairs(sim_ag, columns = cols, title = "", mapping = aes(color=active, shape=active, alpha=.8),
        axisLabels = "show")
ggsave("corplot_eps_C_M1000.pdf", width=6, height=6)

ggpairs(sim_ag, columns = c(C,log(M),log(eps)), title = "", mapping = aes(color=active, shape=active, alpha=.8),
        axisLabels = "show")
ggsave("corplot_logeps_C_logM1000.pdf", width=6, height=6)


summary(lm(log(eps) ~ C*log(M)*active*uniform*fam_context, data=sim_ag))
# C +.07  logM +.94  passive +.83  C:logM -.009 ...

summary(lm(log(eps) ~ C*M*active*uniform*fam_context, data=sim_ag))
# C+, M+, C:M-, Passive+, RandomContext+, 
# C:RandomContext-, M:RandomContext-
# C:M:fam_contextRandom+
# C:uniformZipfian:fam_contextRand+
# C:M:uniformZipfian:fam_contextRandom-

require(tidyverse)
gd = sim %>% filter(M==1000) %>%
  group_by(uniform, fam_context, active) %>% 
  summarise(mean=mean(episodes), se=sd(episodes)/sqrt(100*length(sd)))
# Zipf RandCon Pass 349x slower than Zipf FamCon Act
# Zipf RandCon Pass 13x slower than Zipf FamCon Pass
# Zipf RandCon Pass 45x slower than Zipf RandCon Act

sim %>% group_by(uniform) %>% summarise(mean=mean(episodes)) # 520378 / 10742 = uniform 48.4x faster

sim %>% group_by(active) %>% summarise(mean=mean(episodes)) # 495546 / 18270 = active 27.1x faster

sim %>% group_by(fam_context) %>% summarise(mean=mean(episodes)) # 451531 / 39595 = fam_context 11.4x faster

summary(lm(logeps ~ active*uniform*fam_context, data=sim))
# Passive+.9, Passive:Zipfian+2.5

summary(lm(episodes ~ active*uniform*fam_context * CtoM, data=sim))
# CtoM not a significant predictor of logeps
summary(lm(logeps ~ active*uniform*fam_context * CtoM, data=sim))

summary(lm(logsd ~ active*uniform*fam_context * CtoM, data=sim))
# Passive+ CtoM+ Passive:Zipfian+ Passive:CtoM-

cor(sim$logeps, log(sim$C)) # .49
cor(sim$episodes, sim$C) # .22
cor(sim$logeps, sim$C) # .35
cor(sim$episodes, sim$M) # .07
cor(sim$logeps, sim$M) # .40
cor(sim$logeps, sim$logM) # .54
cor(sim$logeps, sim$CtoM) # .1

probs = 1 / (1:M + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
probs = probs/sum(probs)
pdf("Zipf_word_freq.pdf", width=4.5, height=4)
plot(1:1000, probs, type="b", pch=20, xlab="Frequency Rank of Word", ylab="Probability of Occurrence")
dev.off()
pdf("Zipf_ref_freq.pdf", width=4.5, height=4)
plot(1:1000, probs, type="b", pch=20, xlab="Frequency Rank of Referent", ylab="Probability of Occurrence")
dev.off()

1/probs[1000]
# expected 5759 samples before rarest word is drawn
sum(1/probs) # 2,890,278 samples for each word to be drawn once

probs = 1 / (1:10000 + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
probs = probs/sum(probs)
1/probs[10000] # expected 80457 samples before rarest word is drawn (8x vocab size)

sum(1/probs) # 402,432,731 samples for each word to be drawn once

sum(1/(1/1000)) # if uniform WFD, 1000 samples..