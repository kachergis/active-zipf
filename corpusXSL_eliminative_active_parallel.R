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

#Cs = c(10, 50, 100, 150)
#Ms = c(200)
#act_sim = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=F, reps=100) 

#accel = diff(diff(as.numeric(act_sim[400,7:16])))
