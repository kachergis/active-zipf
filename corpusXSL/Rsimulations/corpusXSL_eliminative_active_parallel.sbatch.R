#!/bin/bash

#SBATCH --nodes=1
#SBATCH --mem=10G
#SBATCH --ntasks-per-node=16
#SBATCH --output=corpuslearn.log

# load the module
ml R

# run R code
R --no-save << EOF

require(foreach)
require(doParallel)


# for sherlock:
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

# in each episode e, a target meaning is selected and it's proper word is presented
# alongside C-1 selected* distractor meanings

# *selection strategies: 
# 1) random (according to freq distribution)
# 2) from high frequency to low frequency
# 3) 

learn_corpus = function(C, M, uniform=T, active=F, fam_context=F, epsilon=.01) {
  if(!uniform) {
    probs = 1:M / sum(1:M)
    probs = sample(probs, length(probs))
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
  for(c in Cs) {
    for(m in Ms) {
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


#Cs = c(4, 10, 50, 100, 500, 1000)
#Ms = c(18, 100, 500, 1000, 10000)
Cs = c(700,800) #c(10, 100, 1000, 1500, 8000) # 9000
Ms = c(1000) #c(2000, 10000)

#act_sim = run_sim_grid(C, M, uniform=T, active=T, fam_context=F, reps=100)
#pass_sim = run_sim_grid(C, M, uniform=T, active=F, fam_context=F, reps=100)

#act_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=T, reps=100)
#pass_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=T, reps=100)

#act_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=F, reps=100)
#pass_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=F, reps=100)

#act_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=T, reps=100)
pass_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=T, reps=100)
sim = pass_sim_nonu_famcon

#sim = rbind(act_sim_unif_famcon, pass_sim_unif_famcon, act_sim_nonu, pass_sim_nonu, act_sim_nonu_famcon, pass_sim_nonu_famcon)


# choosing an unknown target 1.7 - 3.3 times faster


# should try M = c(20000, 40000, 60000)

sim$logM = log(sim$M)
sim$logeps = log(sim$episodes)
sim$logsd = log(sim$sd)
sim$CtoM = sim$C / sim$M


save(sim, file="pass_sim.RData")
print(sim)

EOF