# Convert Report.sso files r4ss objects saved as rds

library(r4ss)

# Create directory
dir.create("../rds", showWarnings=FALSE)

# Read model names
folder_1 <- "../Q_10_127_Diag"
folder_2 <- "../Q_10_128_PICTCPUE"
model_runs <- dir(c(folder_1, folder_2), full=TRUE)

# Select models of interest, prepare unique rds filenames
model_runs <- grepv("Growth-External", model_runs, invert=TRUE)
rds_files <- sub("\\.\\./", "../rds/", model_runs)       # inside rds folder
rds_files <- sub("/Steepness", "_Steepness", rds_files)  # _ separators
rds_files <- paste0(rds_files, ".rds")                   # .rds extension

# Select models to process
id <- 1:360

# Read model results into r4ss object
for(i in id)
{
  cat("Processing model", i, "... ")
  out <- SS_output(model_runs[i])
  saveRDS(out, rds_files[i])
  cat("done\n")
}
