#!/bin/bash

#SBATCH --nodes=1
#SBATCH --mem=3G
#SBATCH --ntasks-per-node=16
#SBATCH --output=corpuslearn3.log
#SBATCH --time=48:00:00

# load the module
ml R

# run R code
R --no-save << EOF

require(foreach)
require(doParallel)

#setup parallel backend to use many processors

# for sherlock:
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

#cores = detectCores()
#cl = makeCluster(cores[1]-1) #not to overload your computer
#registerDoParallel(cl)

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



Cs = c(4, 10, 50, 100, 200)
Ms = c(18, 100, 1000)

act_sim = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=F, reps=100) # warnings..
act_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=T, reps=100)

pass_sim = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=F, reps=100)
pass_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=T, reps=100) # warnings..

act_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=F, reps=100)
act_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=T, reps=100)

pass_sim_nonu = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=F, reps=100)
pass_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=T, reps=100)

sim = rbind(act_sim, pass_sim, act_sim_unif_famcon, 
            pass_sim_unif_famcon, 
            act_sim_nonu,
            pass_sim_nonu,
            act_sim_nonu_famcon, 
            pass_sim_nonu_famcon) # missing C=500 M=1000

print(sim)
save(sim, file="/scratch/users/gkacherg/active_passive1000.RData")

#stop cluster
#stopCluster(cl)


EOF