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

# now track samples to learn quartiles
# first sample a scene (row from scene matrix), then sample objects (and words) from that
learn_corpus = function(C, M, scene_matrix, uniform=T, max_samples=100000) {
  if(!uniform) {
    probs = 1:M / sum(1:M)
    probs = sample(probs, length(probs))
  } else {
    probs = rep(1/M, M)
  }
  quartiles = rep(0,4)
  hyp = matrix(1, nrow=M, ncol=M) 
  word_known = rep(F, M)
  n_learned = 0
  scene = sample(1:nrow(scene_matrix), max_samples, replace=T) # scenes all equiprobable for now
  for(ep in 1:max_samples) {
    # sample target and C-1 distractor meanings (target is 1st one)
    cc = sample(1:M, C, prob=scene_matrix[scene[ep],] * probs) 
    # eliminate any meanings from H(w_t) that are not present in C
    hyp[cc[1], which(!is.element(1:M, cc))] = 0
    if(sum(hyp[cc[1], ])==1 & !word_known[cc[1]]) {
      n_learned = n_learned + 1
      word_known[cc[1]] = T
    }
    
    # log quartiles
    if(n_learned > M/4) {
      if(quartiles[1]==0) quartiles[1] = ep
      if(n_learned > M/2) {
        if(quartiles[2]==0) quartiles[2] = ep
        if(n_learned > (3*M)/4) {
          if(quartiles[3]==0) quartiles[3] = ep
          if(n_learned==M) { # maybe add a little tolerance (don't need final 1%?)
            quartiles[4] = ep
            return(quartiles)
          }
        }
      }
    }
  }
  # if after max_samples we don't finish 100%, let's log the proportion learned in the fourth quartile
  quartiles[4] = n_learned/M
  return(quartiles)
}

repeat_sim = function(C, M, reps, scene_matrix, uniform=T) {
  set.seed(982709)
  results = matrix(0, ncol=4, nrow=reps)
  for(i in 1:reps) {
    results[i,] = learn_corpus(C, M, scene_matrix, uniform)
  }
  df = data.frame(C=C, M=M, uniform=uniform, reps=reps, quartile=1:4,
                  episodes=round(colMeans(results), digits=2), sd=round(apply(results, 2, sd), digits=2))
  return(df)
}

# construct a scene structure matrix that dictates correlation between different words and objects
# right now it's additive...could make a multiplicative version. and need to write the complicated loops
# to iterate over chunks to create the nclusts
get_hierarchical_matrix <- function(size=20, incr=1, base_val=0) {
  m = matrix(base_val, nrow=size, ncol=size)
  #chunk = size/nclusts
  #diag(m) = incr # this would mean each object has a most probable situation...
  m[1:(size/4),1:(size/4)] = m[1:(size/4),1:(size/4)] + incr # 
  m[(size/4+1):(size/2),(size/4+1):(size/2)] = m[(size/4+1):(size/2),(size/4+1):(size/2)] + incr
  m[(size/2+1):(3*size/4),(size/2+1):(3*size/4)] = m[(size/2+1):(3*size/4),(size/2+1):(3*size/4)] + incr
  m[(3*size/4+1):size,(3*size/4+1):size] = m[(3*size/4+1):size,(3*size/4+1):size] + incr
  m[1:(size/2),1:(size/2)] = m[1:(size/2),1:(size/2)] + incr # upper left quadrant
  m[(size/2+1):size,(size/2+1):size] = m[(size/2+1):size,(size/2+1):size] + incr # lower right quadrant
  return(m)
}


# recursively create a hierarchical matrix
hier_matrix <- function(m, factor=1) {
  if(ncol(m)<3) {
    return(m)
  } else {
    h = nrow(m)/2
    m[1:h,1:h] = hier_matrix( m[1:h,1:h]*factor, factor) 
    m[(h+1):nrow(m),(h+1):ncol(m)] = hier_matrix( m[(h+1):nrow(m),(h+1):ncol(m)]*factor, factor)
    return(m)
  }
}

scn_m0 = hier_matrix(matrix(.01, nrow=64, ncol=64), factor=2)


# no cross-cluster appearances
scn_m0 = get_hierarchical_matrix(size=400, incr=1)
# with some cross-cluster 
scn_m_xclust = get_hierarchical_matrix(size=400, incr=1, base_val=.1)
scn_m0 = get_hierarchical_matrix(size=100, incr=1, base_val=.1)

repeat_sim(C=10, M=400, reps=100, scn_m_xclust) # 450  779 1243 3938  sd = 20   24   41  628

require("gplots")
#par(mar=c(2,2,2,2))
heatmap.2(exp(scn_m0), trace="none", scale="none") # dendrogram='none', key=F, Colv=F, Rowv=F


scn = get_hierarchical_matrix(size=1024, incr=2, base_val=0)
dat = repeat_sim(C=10, M=1024, reps=100, scn) # 1062.72 1852.56 2969.88 10741.90
dat$hier_scene = "add2"
dat$base_val = 0

scn = get_hierarchical_matrix(size=1024, incr=2, base_val=.01)
dat_bv01 = repeat_sim(C=10, M=1024, reps=100, scn) # 1067.02 1857.41 2967.59 10633.01
dat_bv01$hier_scene = "add2"
dat_bv01$base_val = .01

scn = get_hierarchical_matrix(size=1024, incr=2, base_val=.1)
dat_bv1 = repeat_sim(C=10, M=1024, reps=100, scn) # 1056.91 1837.13 2951.02 10592.40
dat_bv1$hier_scene = "add2"
dat_bv1$base_val = .1

scn = get_hierarchical_matrix(size=1024, incr=1, base_val=.1)
dat_i1 = repeat_sim(C=10, M=1024, reps=100, scn) # 1049.10 1832.75 2925.90 10622.09
dat_i1$hier_scene = "add1"
dat_i1$base_val = .1

dat = rbind(dat, dat_bv01, dat_bv1, dat_i1)

# dat_nonuniform_freq = dat

# uniform

scn = hier_matrix(matrix(.01, nrow=1024, ncol=1024), factor=1.5)
dat_f1.5 = repeat_sim(C=10, M=1024, reps=100, scn) # 1032 1796 2876 10573
dat_f1.5$base_val = .01
dat_f1.5$hier_scene = "mult1.5"

scn = hier_matrix(matrix(.01, nrow=1024, ncol=1024), factor=2)
dat_f2 = repeat_sim(C=10, M=1024, reps=100, scn) # 1134 1973 3153 11678
dat_f2$base_val = .01
dat_f2$hier_scene = "mult2"

scn = hier_matrix(matrix(.01, nrow=1024, ncol=1024), factor=3)
dat_f3 = repeat_sim(C=10, M=1024, reps=100, scn) # 2255 4009 6629 28219
dat_f3$hier_scene = "mult3"
dat_f3$base_val = .01

unif_scn = matrix(1, ncol=1024, nrow=1024)        
datu = repeat_sim(C=10, M=1024, reps=100, unif_scn) # 1030 1790 2871 10143
datu$hier_scene = "uniform"
datu$base_val = "1/1024"

mult_uni = rbind(datu, dat_f3, dat_f2, dat_f1.5)

# nonuniform

scn = get_hierarchical_matrix(size=1024, incr=1, base_val=0)
dat_i1 = repeat_sim(C=10, M=1024, reps=100, scn, uniform=F) # 1062 2105 4491    .99
dat_i1$hier_scene = "add1"
dat_i1$base_val = 0

scn = hier_matrix(matrix(.01, nrow=1024, ncol=1024), factor=1.5)
dat_f1.5 = repeat_sim(C=10, M=1024, reps=100, scn, uniform=F) # 1015 2013 4313    .99
dat_f1.5$hier_scene = "mult1.5"
dat_f1.5$base_val = .01

scn = hier_matrix(matrix(.01, nrow=1024, ncol=1024), factor=2)
dat_f2 = repeat_sim(C=10, M=1024, reps=100, scn, uniform=F) # 1129 2223 4730    .99
dat_f2$hier_scene = "mult2"
dat_f2$base_val = .01

scn = hier_matrix(matrix(.01, nrow=1024, ncol=1024), factor=3)
dat_f3 = repeat_sim(C=10, M=1024, reps=100, scn, uniform=F) # 2262 4600 9914    .98
dat_f3$hier_scene = "mult3"
dat_f3$base_val = .01

unif_scn = matrix(1, ncol=1024, nrow=1024)        
dat_uniS_nonuniF = repeat_sim(C=10, M=1024, reps=100, unif_scn, uniform=F) # 1009 2008 4299    .99
dat_uniS_nonuniF$base_val = "1/1024"
dat_uniS_nonuniF$hier_scene = "uniform"

dat_nonuni_freq = rbind(dat_i1, dat_f1.5, dat_f2, dat_f3, dat_uniS_nonuniF)


save(dat_nonuni_freq, dat, mult_uni, file="structured_scene_and_varying_freq_sims.RData")
all = rbind(dat_nonuni_freq, dat, mult_uni)
# compare dat_nonuniform_freq and dat_nonuni_freq

graph_structured_scene_results <- function(sim, fname) {
  sim$SE = sim$sd / sqrt(sim$reps-1)
  require("ggplot2")
  dodge <- position_dodge(width=.1)
  limits <- with(sim, aes(ymax=episodes+sd, ymin=episodes-sd))
  #ggplot(sim, aes(x=quartile, y=episodes, group=base_val, color=base_val)) + geom_point(aes(color=base_val), position=dodge) +
  #  ylab("Mean # of Episodes to Reach Quartile") + xlab("Quartile") + facet_wrap(hier_scene ~ uniform) + 
  #  theme_bw() + geom_errorbar(limits, position=dodge) 
  
  #ggplot(sim, aes(x=quartile, y=episodes, group=hier_scene, color=hier_scene)) + geom_point(aes(color=hier_scene), position=dodge) +
  #  ylab("Mean # of Episodes to Reach Quartile") + xlab("Quartile") + facet_wrap(base_val ~ uniform, ncol=2) + 
  #  theme_bw() + geom_errorbar(limits, position=dodge) 
  
  ggplot(sim, aes(x=hier_scene, y=episodes, group=quartile, color=quartile)) + geom_point(aes(color=quartile), position=dodge) +
    ylab("Mean # of Episodes to Reach Quartile") + xlab("Hier Scene") + facet_wrap(base_val ~ uniform, ncol=2, labeller = "label_both") + 
    theme_bw() + geom_errorbar(limits, position=dodge) 
  #ggplot(sim, aes(x=quartile, y=episodes, group=hier_scene, color=hier_scene)) + geom_point(aes(color=hier_scene), position=dodge) +
  #   ylab("Mean # of Episodes to Reach Quartile") + xlab("Quartile") + 
  #   theme_bw() + geom_errorbar(limits, position=dodge) #+ geom_hline(aes(yintercept=.5), linetype=2, col="grey")
  ggsave(paste("episodes_to_acquire_",fname,".pdf", sep=''), width=7.5, height=9)
}

graph_structured_scene_results(all, "by_lexicon_and_context_size_eliminative")