#!/bin/bash

#SBATCH --job-name=corpuslearn
#SBATCH -p hns
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=2G
#SBATCH --ntasks-per-node=16
#SBATCH --output=corpuslearn.log
#SBATCH --time=48:00:00

# load the module
ml R

R --no-save << EOF


source("corpusXSL_eliminative_active_parallel.R")

# for sherlock:
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

Cs = c(10, 100, 200, 500) #1000  later try a C that is >50% of the M (e.g., 700)
Ms = c(1000) # 2000

set.seed(1234)

start_time <- Sys.time()

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

save(sim, file="active_passive_sim_results1000.RData")

#stop cluster
stopCluster(cl)

end_time <- Sys.time()
print(end_time - start_time)

EOF