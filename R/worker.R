arg <- as.integer(commandArgs(trailingOnly=TRUE))

library(r4ss)

r4ss <- function(i)
{
  # Create directory
  dir.create("../rds", showWarnings=FALSE)

  # Read model names
  folder_1 <- "../Q_10_127_Diag"
  folder_2 <- "../Q_10_128_PICTCPUE"
  model_runs <- dir(c(folder_1, folder_2), full=TRUE)

  # Select models of interest, prepare unique rds filenames
  model_runs <- grepv("Growth-External", model_runs, invert=TRUE)
  rds_files <- file.path("../rds", paste0(basename(dirname(model_runs)), "_",
                                          basename(model_runs), ".rds"))

  # Read model results into r4ss object
  cat("Processing model", i, "... ")
  out <- SS_output(model_runs[i])
  saveRDS(out, rds_files[i])
  cat("done\n")
}

r4ss(arg)
