library(condor)
source("utilities.R")

options(width=160)

session <- ssh_connect("nouofpsubmit")

(jobs <- condor_dir())


#####################################################################
## run full_submit for all folders except those in exclude_folders ##
#####################################################################

base_folder <- "02_one_offs"
exclude_folders <- c("base","base_O_07_85_EstMPr2_inputs","base_O_09_98_FBallPh_inputs")  # Add folders to exclude here
ss_version <- "ss_3.30.23.1"

# Generate and print commands
setwd("..")
commands <- generate_full_submit_commands(base_folder, exclude_folders, ss_version)
cat(commands, sep = "\n")


### Execute the generated commands
setwd("R")
eval(parse(text = commands))
