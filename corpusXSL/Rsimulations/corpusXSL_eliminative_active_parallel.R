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

require(foreach)
require(doParallel)

#setup parallel backend to use many processors

# for sherlock:
#registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

cores = detectCores()
cl = makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

C = 4 # context size: meanings per situation
M = 18 # number of meanings in the world
# in each episode e, a target meaning is selected and it's proper word is presented
# alongside C-1 selected* distractor meanings

# *selection strategies: 
# 1) random (according to freq distribution)
# 2) from high frequency to low frequency
# 3) 

learn_corpus = function(C, M, uniform=T, active=F, fam_context=F, epsilon=.01, verbose=T) {
  if(!uniform) {
    probs = 1:M / sum(1:M)
    probs = sample(probs, length(probs)) # not Zipfian!
  } else {
    probs = rep(1/M, M)
  }
  
  hyp = matrix(1, nrow=M, ncol=M) 
  word_known = rep(F, M)
  episodes = 0
  n_learned = 0
  total = M*(1-epsilon) # enough to learn almost all (e.g. 99%) of the words
  while(n_learned < total) {
    # sample target and C-1 distractor meanings (target is 1st one)
    if(active) { # choose target meaning that is unknown
      unknown = which(word_known==F)
      if(length(unknown)>1) {
        target = sample(unknown, 1, prob=probs[unknown])
      } else {
        target = unknown
      }
    } else {
      target = sample(1:M, 1, prob=probs)
    }
    
    # fam_context: select distractors with preference for known ones
    nontarg = setdiff(1:M, target)
    if(fam_context) {
      familiar = 1/(colSums(hyp)+1)
      distractors = sample(nontarg, C-1, prob=probs[nontarg]*familiar[nontarg]) 
    } else {
      distractors = sample(nontarg, C-1, prob=probs[nontarg]) 
    }
    cc = c(target, distractors)
    # eliminate any meanings from H(w_t) that are not present in C
    hyp[cc[1], which(!is.element(1:M, cc))] = 0
    if(sum(hyp[cc[1], ])==1 & !word_known[cc[1]]) {
      n_learned = n_learned + 1
      word_known[cc[1]] = T
    }
    episodes = episodes + 1
  }
  if(verbose) print(paste(C, M, episodes, active, uniform, fam_context))
  return(episodes)
}




# parallelized
repeat_sim <- function(C, M, uniform, active, fam_context, reps) {
  set.seed(982709)
  #results = rep(0,reps)
  #for(i in 1:reps) {
  #  results[i] = learn_corpus(C, M, active)
  #}
  results <- foreach(i=1:reps, .export="learn_corpus", .combine=rbind) %dopar% 
    learn_corpus(C, M, uniform, active, fam_context)
  return(c(mean(results), sd(results)))
}

run_sim_grid <- function(Cs, Ms, uniform, active, fam_context, reps=100) {
  d = data.frame(C=NA, M=NA, episodes=NA, sd=NA, active=NA, uniform=NA, fam_context=NA, reps=NA)
  for(m in Ms) {
    for(c in Cs) {
      if(c<m) {
        row <- repeat_sim(c, m, uniform, active, fam_context, reps)
        tmp = c(C=c, M=m, episodes=row[1], sd=row[2], active=active, uniform=uniform, fam_context=fam_context, reps=reps)
        print(tmp)
        d = rbind(d, tmp)
      }
    }
  }
  d$active = ifelse(d$active==1, "Active", "Passive")
  d$uniform = ifelse(d$uniform==1, "Uniform", "Zipfian")
  d$fam_context = ifelse(d$fam_context==1, "Familiar", "Random")
  return(na.omit(d))
}


#Cs = c(4, 10, 20, 50, 75, 100, 150, 200, 500) #1000  need some Cs that are >50% of the M (e.g., 75, 150, 450, 750..)
#Ms = c(18, 100, 200, 500, 1000) # 2000, 10000

Cs = c(10, 100, 1000, 1500) # c(300,700,800) 9000 8000
Ms = c(2000) #c(2000, 10000)

act_sim = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=F, reps=100) # warnings..
act_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=T, reps=100)
act_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=F, reps=100)
act_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=T, reps=100)

pass_sim = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=F, reps=100)
pass_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=T, reps=100) # warnings..
pass_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=F, reps=100)
pass_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=T, reps=100)
# pass_sim_nonu_famcon not yet completely run with C=500 M=1000
#pass_sim_nonu_famcon = read.table("pass_sim_nonu_famcon.tsv", sep="", header=T)

sim = rbind(act_sim, pass_sim, act_sim_unif_famcon, 
            pass_sim_unif_famcon, 
            act_sim_nonu,
            pass_sim_nonu,
            act_sim_nonu_famcon, 
            pass_sim_nonu_famcon) # missing C=500 M=1000

#save(sim, file="active_passive_sim_results1000.RData")
load("active_passive_sim_results.RData")
#stop cluster
stopCluster(cl)

1/ sort(act_sim$episodes / pass_sim$episodes) 
# choosing an unknown target 1.9 - 3.3 times faster for uniform freq

1/ sort(act_sim_nonu$episodes / pass_sim_nonu$episodes) 
# 48.8 - 10.3 times faster for nonuniform frequency

1/ sort(act_sim_unif_famcon$episodes / pass_sim_unif_famcon$episodes) 
# 3.3 - 1.6 times faster for 

1/ sort(act_sim_unif_famcon$episodes / subset(act_sim, C!=1000)$episodes) 
# familiar context speed up between 5.5 and .24 (!!) -- average: 1.43 (must look at relationship to C and M)

# should try M = c(20000, 40000, 60000)

add_cols <- function(sim) {
  sim$logM = log(sim$M)
  sim$logeps = log(sim$episodes)
  sim$logsd = log(sim$sd)
  sim$CtoM = sim$C / sim$M
  sim$active = factor(sim$active, labels=c("Passive","Active"))
  sim$uniform = factor(sim$uniform, labels=c("Zipfian","Uniform"))
  sim$fam_context = factor(sim$fam_context, labels=c("Random Context","Familiar Context"))
  return(sim)
}

{
  require(tidyverse)
  ag = read.table("M2000soloruns.txt")
  names(ag) = c("C","M","episodes","active","uniform","fam_context")
  # aggregate and make SD and reps
  ag %>% group_by(C, M, active, uniform, fam_context) %>%
    summarise(episodes = mean(episodes), sd=sd(episodes), reps=n())
  
  #sim = read.table("active_passive_sim_results.tsv", sep="", header=T)
  #sim2 = read.table("active_passive_sim_results2.tsv", sep="", header=T)
  sim2 = read.table("cluster_sims_1000_2000.tsv", header=T)
  sim2 = add_cols(sim2)
  sim = rbind(sim, sim2)

  save(sim, file="active_passive_sim_results.RData")
  
  table(sim$active, sim$fam_context, sim$uniform)
}

require("ggplot2")

ggplot(sim, aes(x=CtoM, y=logeps, shape=active, color=C)) + geom_point() + ylim(0, 20) + 
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Ratio (Context Size)/(Lexicon Size)") + 
  facet_grid(rows=vars(fam_context), cols=vars(uniform)) +
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey")
ggsave("logeps_to_acquire_by_context_and_CtoM_eliminative_eps01.pdf", width=7.5, height=7)

ggplot(sim, aes(x=logM, y=logeps, shape=active, color=C)) + geom_point() + xlim(0, 10) + ylim(0, 20) +
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  facet_grid(rows=vars(fam_context), cols=vars(uniform)) +
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey") 
ggsave("logeps_to_acquire_by_lexicon_and_context_size_eliminative_eps01.pdf", width=7.5, height=7)

ggplot(sim, aes(x=M, y=episodes, shape=active, color=C)) + geom_point() +
  ylab("Mean Number of Episodes to Acquire Lexicon") + xlab("Number of meanings") + 
  facet_grid(rows=vars(fam_context), cols=vars(uniform)) +
  theme_bw() + geom_abline(slope=1, intercept=0, linetype=2, col="grey")
ggsave("eps_to_acquire_by_lexicon_and_context_size_eliminative_eps01.pdf", width=7.5, height=7)

require("GGally")
cols = c("C","M","CtoM","episodes") # ,"sd"
logcols = c("C","logM","logeps") # "logsd"
ggpairs(sim, columns = cols, title = "", mapping = aes(color=active, shape=active, alpha=.8),
        axisLabels = "show")
ggsave("corplot_eps_C_M1000.pdf", width=6, height=6)

ggpairs(sim, columns = logcols, title = "", mapping = aes(color=active, shape=active, alpha=.8),
        axisLabels = "show")
ggsave("corplot_logeps_C_logM1000.pdf", width=6, height=6)


summary(lm(logeps ~ C*logM*active*uniform*fam_context, data=sim))
# C +.02  logM +.86  passive +.81  C:logM -.003 C:Zipf +.04

summary(lm(logeps ~ C*M*active*uniform*fam_context, data=sim))
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