# Monte Carlo simulations for acquiring a large-scale lexicon via
# cross-situational word learning (a la Vogt 2008 and Smith, Smith, and Blythe 2010)
# by: George Kachergis  April 20, 2016 - updated August 22, 2019 to investigate ACTIVE strategies

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
# alongside C-1 selected* distractor meanings

# *selection strategies: 
# 1) random (according to freq distribution)
# 2) from high frequency to low frequency
# 3) 

learn_corpus = function(C, M, active=F, epsilon=.01) {
  hyp = matrix(1, nrow=M, ncol=M) 
  word_known = rep(F, M)
  episodes = 0
  n_learned = 0
  total = M*(1-epsilon) # enough to learn almost all (e.g. 99%) of the words
  while(n_learned < total) {
    # sample target and C-1 distractor meanings (target is 1st one)
    if(active) { # choose target meaning that is unknown
      unknown = which(word_known==F)
      target = sample(unknown, 1)
    } else {
      target = sample(1:M, 1)
    }
    distractors = sample(setdiff(1:M, target), C-1) 
    cc = c(target, distractors)
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

repeat_sim = function(C, M, active, reps) {
  set.seed(982709)
  results = rep(0,reps)
  for(i in 1:reps) {
    results[i] = learn_corpus(C, M, active)
  }
  return(c(mean(results), sd(results)))
}

run_sim_grid = function(C, M, active, reps=100) {
  d = data.frame(C=NA, M=NA, episodes=NA, sd=NA, active=NA, reps=NA)
  for(c in C) {
    for(m in M) {
      if(c<m) {
        row = repeat_sim(c, m, active, reps)
        tmp = c(C=c, M=m, episodes=row[1], sd=row[2], active=active, reps=reps)
        print(tmp)
        d = rbind(d, tmp)
      }
    }
  }
  d$active = ifelse(d$active==1, "Active", "Passive")
  return(na.omit(d))
}


C = c(4, 10, 50, 100, 500, 1000)
M = c(18, 100, 500, 1000, 10000, 20000)

act_sim = run_sim_grid(C, M, active=T, reps=100)
pass_sim = run_sim_grid(C, M, active=F, reps=100)


1/ sort(act_sim$episodes / pass_sim$episodes) 
# choosing an unknown target 1.6 - 3.5 times faster

#save(sim, file="sm_sims.RData")

# should try M = c(20000, 40000, 60000)


sim = rbind(pass_sim, act_sim)
sim$logM = log(sim$M)
sim$logeps = log(sim$episodes)
sim$logsd = log(sim$sd)
sim$CtoM = sim$C / sim$M

save(sim, "unknown_vs_random_target_selection.RData")

require("ggplot2")

ggplot(sim, aes(x=CtoM, y=logeps, shape=active, color=C)) + geom_point() + ylim(0, 20) + 
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Ratio (Context Size)/(Lexicon Size)") + 
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey")

ggplot(sim, aes(x=logM, y=logeps, shape=active, color=C)) + geom_point() + ylim(0, 20) + xlim(0, 20) + 
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey")
ggsave("eps_to_acquire_by_lexicon_and_context_size_eliminative_eps01.pdf", width=5.5, height=5)

