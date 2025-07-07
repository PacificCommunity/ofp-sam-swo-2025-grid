library(condor)
source("utilities.R")
options(width=160)

session <- ssh_connect("nouofpsubmit")

(jobs <- condor_dir())

# Initial test

full_submit("N_05_76_54Amax",     "ss_3.30.23.1")
full_submit("N_05_76_54Amax_opt", "ss_3.30.23.1")
full_submit("simple_small",       "ss_3.30.23.1")
