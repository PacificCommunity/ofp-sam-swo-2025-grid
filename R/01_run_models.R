library(condor)
source("utilities.R")
options(width=160)

session <- ssh_connect("nouofpsubmit")

(jobs <- condor_dir())

# Natural mortality levels
# Different percentiles from Hamel-Cope distribution

full_submit("02_one_offs/M_05th", "ss_3.30.23.1")
full_submit("02_one_offs/M_10th", "ss_3.30.23.1")
full_submit("02_one_offs/M_15th", "ss_3.30.23.1")
full_submit("02_one_offs/M_20th", "ss_3.30.23.1")
full_submit("02_one_offs/M_50th", "ss_3.30.23.1")
