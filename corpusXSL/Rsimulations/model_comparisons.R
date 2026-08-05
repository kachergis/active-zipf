# setwd("~/Dropbox/corpusXSL/Rsimulations")

load("siskind_sims.RData")
sim$Model = "Eliminative"
sim$Frequency = "Uniform"
sim$eps_per_word = sim$episodes/sim$M
sim$CtoM = sim$C/sim$M
agg = sim

# load("guess_n_test_sims_unif_noep.RData")
load("guess_n_test_unif_sims.RData") # with epsilon=.01
sim$Model = "Guess-Test Strong ME"
sim$Frequency = "Uniform"
agg = rbind(agg, sim)

load("guess_n_test_weakME_sims.RData") # no epsilon..re-do?
sim$Model = "Guess-Test Weak ME"
sim$Frequency = "Uniform"
sim$eps_per_word = sim$episodes/sim$M
sim$CtoM = sim$C/sim$M
agg = rbind(agg, sim)

load("guess_n_test_nonunif_sims.RData") # has epsilon=.01
sim$Model = "Guess-Test Weak ME Nonuniform"
sim$Frequency = "Nonuniform (1/N)"
sim$eps_per_word = sim$episodes/sim$M
sim$CtoM = sim$C/sim$M
agg = rbind(agg, sim)

subset(agg, C==10 & M==1000)

require("ggplot2")
dodge <- position_dodge(width=.1)
ggplot(agg, aes(x=CtoM, y=eps_per_word, group=Model, color=Model)) + geom_point(aes(color=Model), position=dodge) +
  ylab("Mean Episodes per Word to Acquire Lexicon)") + xlab("Context Size / Vocabulary Size") + theme_bw()  
ggsave("episodes_per_word_to_acquire_by_lexicon_size_and_model.pdf", width=5.5, height=5)

dodge <- position_dodge(width=.2)
limits <- with(sim, aes(ymax=logeps+logsd, ymin=logeps-logsd))
ggplot(subset(agg, C==10), aes(x=logM, y=logeps, group=Model, color=Model)) + geom_point(aes(color=Model), position=dodge) +
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  theme_bw() + geom_errorbar(limits, position=dodge) 
ggsave("episodes_to_acquire_by_lexicon_size_and_model.pdf", width=5.5, height=5)