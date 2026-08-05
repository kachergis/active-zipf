# Monte Carlo simulations for acquiring a large-scale lexicon via
# cross-situational word learning (a la Vogt 2008 and Smith, Smith, and Blythe 2010)
# by: George Kachergis  April 20, 2016


C = 4 # context size: meanings per situation
M = 18 # number of meanings in the world
# in each episode e, a target meaning is selected and it's proper word is presented
# alongside C-1 distractor meanings

learn_corpus = function(C, M) {
  hyp = matrix(0, nrow=M, ncol=M) 
  word_known = rep(F, M)
  episodes = 0
  n_learned = 0
  while(n_learned < M) {
    # sample target and C-1 distractor meanings (target is 1st one)
    cc = sample(1:M, C) 
    # eliminate any meanings from H(w_t) that are not present in C
    hyp[cc[1], which(!is.element(1:M, cc))] = 0
    # if you don't have a hypothesis, pick a referent with no other association 
    if(sum(hyp[cc[1],])==0) {
      unclaimed = which(colSums(hyp[cc,cc])==0) # weak ME--just check meanings on trial: hyp[cc,cc]) 
      if(length(unclaimed)==0) {
        print("no unclaimed referents") # does this happen? could i guess..
      } else {
        hyp[cc[1],sample(unclaimed,1)] = 1
      }
    }
    
    if(sum(hyp[cc[1], ])==1 & !word_known[cc[1]]) {
      n_learned = n_learned + 1
      word_known[cc[1]] = T
    }
    episodes = episodes + 1
  }
  return(episodes)
}

repeat_sim = function(C, M, reps) {
  set.seed(982709)
  results = rep(0,reps)
  for(i in 1:reps) {
    results[i] = learn_corpus(C, M)
  }
  return(c(round(mean(results)), round(sd(results))))
}


repeat_sim(4, 18, 100) #weak ME: 62 strong ME: m=62 sd=20
repeat_sim(5, 50, 100) # weak ME: 232 sd=66  strong ME: m=226 sd=65  
repeat_sim(10, 100, 100) # weak ME: 521 sd=139  strong ME: 500 131
repeat_sim(10, 200, 100) # weak ME: 1266 sd=280  strong ME: 1145 231
repeat_sim(10, 400, 100) # weak ME: 2720 sd=519 strong ME: 2661 503
repeat_sim(10, 1000, 100) # weak ME: 7583 1427  strong ME: m=7527 sd=1343

repeat_sim(20, 400, 100) # weak ME: 2653 508
repeat_sim(20, 1000, 100) # weak ME: 7401 1112
repeat_sim(40, 1000, 100) # weak ME: 7669 1294

levelsM = c(100,200,400,1000,2000,4000,8000)
C = c(rep(10,length(levelsM)), rep(20,length(levelsM)), rep(30,length(levelsM)), rep(40,length(levelsM)))
M = rep(levelsM, 4) 
new = c()
for(i in 1:length(C)) {
  new = rbind(new, c(C[i], M[i], repeat_sim(C[i], M[i], 100)))
  print(new)
}
new = data.frame(new)
names(new) = c("C","M","episodes","sd")

sim = new #rbind(sim, new)

sim$logM = log(sim$M)
sim$logeps = log(sim$episodes)
sim$logsd = log(sim$sd)
save(sim, file="guess_n_test_weakME_sims.RData")

sim$SE = sim$sd / sqrt(100-9)
require("ggplot2")
dodge <- position_dodge(width=.1)
limits <- with(sim, aes(ymax=logeps+logsd, ymin=logeps-logsd))
ggplot(sim, aes(x=logM, y=logeps, group=C, color=C)) + geom_point(aes(color=C), position=dodge) +
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  theme_bw() + geom_errorbar(limits, position=dodge) + geom_hline(aes(yintercept=.5), linetype=2, col="grey")
ggsave("episodes_to_acquire_by_lexicon_and_context_size_guess_n_test_weakME.pdf", width=5.5, height=5)