load("act_pass_sim_results1000_famcon.RData")
table(sim$C, sim$fam_context)
sim_fam = sim
load("act_pass_sim_results1000_unfamcon.RData")
table(sim$C, sim$fam_context)

all = rbind(sim, sim_fam)

table(all$C, all$M, all$uniform, all$fam_context, all$active)

# missing c=200 Zipfian Random Passive

# ranked frequency model (word is learned as soon as a referent has appeared more frequently with it than any other ref)
load("passive_rankedFreq_1000.RData")
table(aggl$C, aggl$M, aggl$uniform)