
source("corpusXSL_eliminative_active_parallel.R")

# for sherlock:
#registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

#setup parallel backend to use many processors
cores = detectCores()
cl = makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

Cs = c(5, 10, 20, 40, 80) # 500 later try a C that is >50% of the M (e.g., 700)
Ms = c(1000, 2000) # 2000

REPS = 100 # repetitions per setting
set.seed(1234)

# nonuniform is super-slow unless C << M, e.g.:
# C=200, M=1000 >2.75 days...
# C=200, M=2000 >3.25 days...

run_batch <- function(Cs, Ms, REPS) {
  tmp = data.frame()
  for(unif in c(TRUE, FALSE)) 
    for(act in c(TRUE, FALSE)) 
      for(famc in c(TRUE)) { # , FALSE
        tmp = rbind(tmp, run_sim_grid(Cs, Ms, uniform=unif, active=act, fam_context=famc, reps=REPS)) 
        save(tmp, file="temp.RData") # save intermediate / final result
      }
  return(tmp)
}


start_time <- Sys.time()

sim_famcon = run_batch(Cs, Ms, 100)

end_time <- Sys.time()
print(end_time - start_time)

# 4 repetition time tests:
# 10, 20, 40  M=1000 -> 1.2 mins
# 10, 20, 40  M=2000 -> 4.5 mins
# 10, 20, 40, 80  M=1000 -> 15.9 mins so 100 reps = x25 = 400 mins (6.7 hrs)
# 10, 20, 40, 80  M=2000 -> 32.8 mins so 100 reps = x25 = 820 mins (13.7 hrs)

#stop cluster
stopCluster(cl)


save(sim, file="unfam_con_M2000.RData")