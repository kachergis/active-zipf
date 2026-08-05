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


C = 4 # context size: meanings per situation
M = 18 # number of meanings in the world
# in each episode e, a target meaning is selected and it's proper word is presented
# alongside C-1 distractor meanings

learn_corpus = function(C, M, epsilon=.01, nonuniform=F, record_growth=0) {
  if(nonuniform) {
    probs = 1:M / sum(1:M)
  } else {
    probs = rep(1/M, M)
  }
  hyp = matrix(0, nrow=M, ncol=M) 
  word_known = rep(F, M)
  episodes = 0
  n_learned = 0
  growth_curve = c() # store every record_growth episodes
  total = M*(1-epsilon)
  while(n_learned < total) {
    # sample target and C-1 distractor meanings (target is 1st one)
    cc = sample(1:M, C, replace=F, prob=probs) 
    # eliminate any meanings from H(w_t) that are not present in C
    hyp[cc[1], which(!is.element(1:M, cc))] = 0
    # if you don't have a hypothesis, pick a referent with no other association 
    if(sum(hyp[cc[1],])==0) {
      unclaimed = which(colSums(hyp[,cc])==0) # strong version: check all hypoths (not just on trial: hyp[cc,cc]) 
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
    if(record_growth>0) {
      if(episodes%%record_growth==0) {
        growth_curve = c(growth_curve, n_learned)
      }
    }
  }
  return(list(episodes=episodes, growth_curve=growth_curve))
}

repeat_sim = function(C, M, reps, nonuniform=F) {
  set.seed(982709)
  results = rep(0,reps)
  for(i in 1:reps) {
    results[i] = learn_corpus(C, M, epsilon=.01, nonuniform)$episodes
  }
  return(c(round(mean(results)), round(sd(results))))
}

# nonuniform word *and* meaning frequency is much slower (and more variable)
repeat_sim(4, 18, 100, nonuniform=F) # m=62 sd 20
repeat_sim(4, 18, 100, nonuniform=T) # m=215 sd 156
repeat_sim(5, 50, 100, nonuniform=T) # m=1798 sd=1525

# look at how quickly vocabulary grows over time (should slow because of random sampling -- especially for low frequency items!)
growth_curves <- function() {
  u = learn_corpus(10, 1000, epsilon=.01, nonuniform=F, record_growth=10)
  nu = learn_corpus(10, 1000, epsilon=.01, nonuniform=T, record_growth=10)
  #plot(seq(10,length(u$growth_curve)*10, 10), u$growth_curve)
  #plot(seq(10,length(nu$growth_curve)*10, 10), nu$growth_curve)
  unif = data.frame(cbind(seq(10,length(u$growth_curve)*10, 10), u$growth_curve))
  nonunif = data.frame(cbind(seq(10,length(nu$growth_curve)*10, 10), nu$growth_curve))
  colnames(unif) = c("Episode", "KnownWords")
  colnames(nonunif) = c("Episode", "KnownWords")
  unif$Frequency = "Uniform"
  nonunif$Frequency = "Nonuniform"
  gc = rbind(unif, nonunif) # subset(gc, Episode<10000)
  ggplot(gc, aes(x=Episode, y=KnownWords, group=Frequency, color=Frequency)) + geom_line(aes(color=Frequency), position=dodge) +
    ylab("Number of Words Acquired") + xlab("Episodes") + theme_bw()
  ggsave("growth_curve_guessntest_unif_vs_nonunif.pdf", width=7.5, height=4.5)
}

growth_curves()

# without epsilon:
#   10  100   5932   3406
#   10  200  24751  17557
#   10  400 106680  65963
#   10 1000 670454 496285

Ms_per_C = c(100,200,400,1000,2000,4000,8000)
C = c(rep(10, length(Ms_per_C)), rep(20, length(Ms_per_C)),  rep(30, length(Ms_per_C)), rep(40, length(Ms_per_C)))
M = rep(Ms_per_C, 4) 
new = c()
for(i in 1:length(C)) {
  new = rbind(new, c(C[i], M[i], repeat_sim(C[i], M[i], 100, nonuniform=F)))
  print(new)
}
new = data.frame(new)
names(new) = c("C","M","episodes","sd")

sim = new #rbind(sim, new)

sim$logM = log(sim$M)
sim$logeps = log(sim$episodes)
sim$logsd = log(sim$sd)
sim$eps_per_word = sim$episodes / sim$M
sim$CtoM = sim$C / sim$M
save(sim, file="guess_n_test_unif_sims.RData")

plot(sim$CtoM, sim$eps_per_word)

sim$SE = sim$sd / sqrt(100-9)

log_axis = list(breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=scales::trans_format("log10", scales::math_format(10^.x)))

ggplot(sim, aes(x=M, y=episodes, group=C, color=C)) + geom_point(aes(color=C)) +
  #ylab("Mean # of Episodes to Acquire Lexicon)") + xlab("Number of Meanings)") + 
  theme_bw() +# geom_errorbar(limits, position=dodge) +
  scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=scales::trans_format("log10", scales::math_format(10^.x))) +
                annotation_logticks()

require("ggplot2")
dodge <- position_dodge(width=.2)
limits <- with(sim, aes(ymax=logeps+logsd, ymin=logeps-logsd))
ggplot(sim, aes(x=logM, y=logeps, group=C, color=C)) + geom_point(aes(color=C), position=dodge) +
  ylab("Log(Mean # of Episodes to Acquire Lexicon)") + xlab("Log(# of meanings)") + 
  theme_bw() + geom_errorbar(limits, position=dodge) #+ geom_hline(aes(yintercept=.5), linetype=2, col="grey")
ggsave("episodes_to_acquire_by_lexicon_and_context_size_guess_n_test_unif.pdf", width=5.5, height=5)