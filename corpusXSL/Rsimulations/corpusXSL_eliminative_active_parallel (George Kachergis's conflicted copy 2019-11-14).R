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
    #probs = 1:M / sum(1:M)
    #probs = sample(probs, length(probs))
    probs = 1 / (1:M + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
    probs = probs/sum(probs)
    probs = sample(probs, length(probs)) 
  } else {
    probs = rep(1/M, M)
  }
  
  hyp = matrix(1, nrow=M, ncol=M) 
  word_known = rep(F, M)
  episodes = 0
  n_learned = 0
  total = M*(1-epsilon) # enough to learn almost all (e.g. 99%) of the words
  
  eps_to_learn_decile = rep(F, 9)
  deciles = M*seq(.1,.9,.1)
  
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
      for(dd in 1:9) {
        if(n_learned>=deciles[dd] & eps_to_learn_decile[dd]==F) eps_to_learn_decile[dd] = episodes
      }
    }
    episodes = episodes + 1
  }
  if(verbose) print(paste(C, M, episodes, active, uniform, fam_context))
  return(c(eps_to_learn_decile, episodes)) # now return episodes to learn 10%, 20%, ... 99% of the M words
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
  #return(c(mean(results), sd(results)))
  return(results)
}

#rr <- repeat_sim(4, 1000, uniform=T, active=T, fam_context=T, reps=20) # reps=10 -> 5, reps=15 -> 10, reps=20 -> 15..

run_sim_grid <- function(Cs, Ms, uniform, active, fam_context, reps=100) {
  d = data.frame(sim=NA, C=NA, M=NA, active=NA, uniform=NA, fam_context=NA, 
                 dec1=NA, dec2=NA, dec3=NA, dec4=NA, dec5=NA, dec6=NA, dec7=NA, dec8=NA, dec9=NA, p99=NA) # , reps=NA
  for(m in Ms) {
    for(c in Cs) {
      if(c<m) {
        results <- repeat_sim(c, m, uniform, active, fam_context, reps=reps)
        #tmp = c(C=c, M=m, episodes=row[1], sd=row[2], active=active, uniform=uniform, fam_context=fam_context, reps=reps)
        for(row in 1:nrow(results)) {
          d= rbind(d, c(sim=row, C=c, M=m, active=active, uniform=uniform, fam_context=fam_context, results[row,]))
        }
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

Cs = c(4, 10, 50, 100) # try 200, 500
Ms = c(18, 100, 1000)

Cs = c(200) # try 200, 500
Ms = c(1000)


act_sim = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=F, reps=100) # warnings..
act_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=T, reps=100)

pass_sim = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=F, reps=100)
pass_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=T, reps=100) # warnings..


# BELOW NOT FINISHED WITH C=200 M=1000
act_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=F, reps=100)
act_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=T, reps=100)

pass_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=F, reps=100)
pass_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=T, reps=100)
# pass_sim_nonu_famcon not yet completely run with C=500 M=1000
#pass_sim_nonu_famcon = read.table("pass_sim_nonu_famcon.tsv", sep="", header=T)

sim2 = rbind(act_sim, pass_sim, act_sim_unif_famcon, 
            pass_sim_unif_famcon, 
            act_sim_nonu,
            pass_sim_nonu,
            act_sim_nonu_famcon, 
            pass_sim_nonu_famcon) # missing C=500 M=1000

save(sim, file="active_passive_sim_results1000.RData")

save(sim, file="active_passive_sim_results1000.RData")
load("active_passive_sim_results.RData")
#stop cluster
stopCluster(cl)

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

{
  require(tidyverse)
  ag = read.table("M2000soloruns.txt")
  names(ag) = c("C","M","episodes","active","uniform","fam_context")
  # aggregate and make SD and reps
  ag %>% group_by(C, M, active, uniform, fam_context) %>%
    summarise(sd=sd(episodes), episodes = mean(episodes), reps=n())
  
  
  #sim = read.table("active_passive_sim_results.tsv", sep="", header=T)
  #sim2 = read.table("active_passive_sim_results2.tsv", sep="", header=T)
  sim2 = read.table("cluster_sims_1000_2000.tsv", header=T)
  sim2 = add_cols(sim2)
  sim = rbind(sim, sim2)

  save(sim, file="active_passive_sim_results.RData")
  
  table(sim$active, sim$fam_context, sim$uniform)
}

require("ggplot2")

#sm = subset(sim, is.element(C, c(4, 10, 50, 100)) & is.element(M, c(1000)))

require("gganimate")
anim <- ggplot(sim, aes(x = C, y = p99)) + 
  geom_point(aes(colour = active), size = 2) + 
  transition_states(active, transition_length = 2, state_length = 1)
anim + enter_fade() + exit_shrink()

sim2 <- add_cols(sim)

sim_ag <- sim2 %>% group_by(C, M, active, uniform, fam_context) %>%
  summarise(d3=mean(dec3), d5=mean(dec5), d7=mean(dec7), eps=mean(p99), sd=sd(p99))

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
