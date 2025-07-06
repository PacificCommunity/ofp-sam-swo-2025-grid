library(condor)
source("utilities.R")
options(width=160)

session <- ssh_connect("nouofpsubmit")

# Select Condor models that are finished
(jobs <- condor_dir(sort="dir"))
models <- jobs$dir[jobs$status=="finished"]

# Destination folders, will be created by full_download() below
folders <- file.path("//penguin/assessments/swo/2025/model_runs/ensemble",
                     models)
folders <- models


# Download results
for(i in seq_along(folders))
  try(full_download(folders[i]))
