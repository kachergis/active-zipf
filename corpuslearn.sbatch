#!/bin/bash

#SBATCH --job-name=corpuslearn
#SBATCH -p hns
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=2G
#SBATCH --ntasks-per-node=16
#SBATCH --output=elim1k_famcon.log
#SBATCH --time=48:00:00

# load the module
ml R

R --no-save << EOF


source("corpusXSL_eliminative_active_parallel.R")

# for sherlock:
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

#setup parallel backend to use many processors
cores = detectCores()
cl = makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

Cs = c(10, 100, 200) # 500 later try a C that is >50% of the M (e.g., 700)
Ms = c(1000) # 2000

REPS = 15 # repetitions per setting
set.seed(1234)

start_time <- Sys.time()

act_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=T, fam_context=T, reps=REPS)

pass_sim_unif_famcon = run_sim_grid(Cs, Ms, uniform=T, active=F, fam_context=T, reps=REPS) # warnings..

act_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=T, fam_context=T, reps=REPS)

pass_sim_nonu_famcon = run_sim_grid(Cs, Ms, uniform=F, active=F, fam_context=T, reps=REPS)

sim = rbind(act_sim_unif_famcon, 
            pass_sim_unif_famcon, 
            act_sim_nonu_famcon, 
            pass_sim_nonu_famcon) 

save(sim, file="act_pass_sim_results1000_famcon.RData")

#stop cluster
stopCluster(cl)

end_time <- Sys.time()
print(end_time - start_time)

EOF