# Examine rds files

rds_files <- dir("../rds", full=TRUE)

system.time(x <- readRDS(rds_files[1]))
