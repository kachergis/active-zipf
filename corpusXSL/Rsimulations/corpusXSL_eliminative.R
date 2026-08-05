# Monte Carlo simulations for acquiring a large-scale lexicon via
# cross-situational word learning (a la Vogt 2008 and Smith, Smith, and Blythe 2010)
# by: George Kachergis  April 20, 2016

# want to learn 60,000 words by 18 years (Anglin, 1993)

# kids receive between 219 to 1260 utterances per hour
# with mean utterance length of 4 (see Siskind 1996)

# assume 8 hours per day for 18 years: 52,560 hours of speech (Vogt 2008)
# between 11.5 and 66.2 million words in the first 18 years

# Vogt's simulation s4 estimated 14.3 million episodes to learn 60,000 words,
# finding that Zipfian-distributed frequencies are harder than uniform, but as long
# as the context size is a small proportion of the lexicon size, it's reasonable

# remaining assumptions in Vogt: lexicon has only 1-1 mappings and 
# every time a word was heard, the object was present

# Q: words follow a Zipfian distribution, but do meanings?

# Siskind (and Vogt's) model: possible meanings for a word = all it has appeared with so far

C = 4 # context size: meanings per situation
M = 18 # number of meanings in the world
# in each episode e, a target meaning is selected and it's proper word is presented
# alongside C-1 distractor meanings

learn_corpus = function(C, M) {
  hyp = matrix(1, nrow=M, ncol=M) 
  word_known = rep(F, M)
  episodes = 0
  n_learned = 0
  while(n_learned < M) {
    # sample target and C-1 distractor meanings (target is 1st one)
    cc = sample(1:M, C) 
    # eliminate any meanings from H(w_t) that are not present in C
    hyp[cc[1], which(!is.element(1:M, cc))] = 0
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


repeat_sim(4, 18, 100) # m=118 sd=29
repeat_sim(5, 50, 100) # m=367 sd=83
repeat_sim(10, 100, 100) # 902 161
repeat_sim(10, 200, 100) # 1864 321
repeat_sim(10, 400, 100) # 3784 592
repeat_sim(10, 1000, 100) # m=10203 sd=1398

repeat_sim(15, 40, 100)  # 471 93
repeat_sim(15, 50, 100)  # 544 98
repeat_sim(15, 100, 100) # 998 154
repeat_sim(15, 200, 100) # 2006 275
repeat_sim(15, 400, 100) # 4059 576
repeat_sim(15, 600, 100) # 6059 877
repeat_sim(15, 800, 100) # 8419 989
repeat_sim(15, 1000, 100) # 10590 1265

repeat_sim(25, 200, 100) # 2184 295
repeat_sim(25, 400, 100) # 4384 585
repeat_sim(25, 600, 100) # 6569 871
repeat_sim(25, 800, 100) 
repeat_sim(25, 1000, 100) 
repeat_sim(25, 2000, 100) 
repeat_sim(25, 4000, 100) 

repeat_sim(10, 4000, 100) # 
repeat_sim(10, 8000, 100)
repeat_sim(15, 4000, 100)
repeat_sim(15, 8000, 100)
repeat_sim(20, 4000, 100)
repeat_sim(20, 8000, 100)

C = c(4,5,10,10,10,10, 
      15,15,15,15,15,15,15,15, 
      25,25,25,25,25,25,25,
      10,10,15,15,20,20)
M = c(18,50,100,200,400,1000, 
      40,50,100,200,400,600,800,1000, 
      200,400,600,800,1000,2000,4000,
      4000,8000,4000,8000,4000,8000)
episodes = c(118,367,902,1864,3784,10203, 
             471,544,998,2006,4059,6059,8419,10590, 
             2184,4384,6569,8959,11268,22880,47801,
             46357,97521,46186,97817,47930,97616)
sd = c(29,83,161,321,592,1398, 
       93,98,154,275,576,877,989,1265, 
       295,585,871,1190,1628,2522,5965,
       6123,10656,5434,9372,4993,10276)


sim = data.frame(cbind(C,M,episodes,sd))

C = c(10,10,25,   30,30,30,30,30,30,30,30,  40,40,40,40,40,40,40,40)
M = c(800,2000,8000, 100,200,400,800,1000,2000,4000,8000, 100,200,400,800,1000,2000,4000,8000)
new = c()
for(i in 1:length(C)) {
  new = rbind(new, c(C[i], M[i], repeat_sim(C[i], M[i], 100)))
}
new = data.frame(new)
names(new) = c("C","M","episodes","sd")

sim = rbind(sim, new)

sim$logM = log(sim$M)
sim$logeps = log(sim$episodes)
sim$logsd = log(sim$sd)
save(sim, file="siskind_sims.RData")

load("siskind_sims.RData")

sim$SE = sim$sd / sqrt(100-9)
require("ggplot2")
dodge <- position_dodge(width=.2)
limits <- with(sim, aes(ymax=logeps+logsd, ymin=logeps-logsd))
ggplot(sim, aes(x=logM, y=logeps, group=C, color=C)) + geom_point(aes(color=C), position=dodge) +
   ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  theme_bw() + geom_errorbar(limits, position=dodge) #+ geom_hline(aes(yintercept=.5), linetype=2, col="grey")
ggsave("episodes_to_acquire_by_lexicon_and_context_size_eliminative.pdf", width=5.5, height=5)