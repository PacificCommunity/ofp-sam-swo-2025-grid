# Extract results of interest, write CSV output tables

# Before: grid_results.rds, refpts_est.rds (model)
# After:  grid_refpts.csv, grid_tseries.csv, refpts_table.csv (output)

library(TAF)
source("boot/software/utilities_r4ss.R")

mkdir("output")

# Load grid results and estimation uncertainty
grid_results <- readRDS("model/grid_results.rds")
refpts_est <- readRDS("model/refpts_est.rds")

# Extract refpts and tseries
grid_refpts <- grid_results$refpts
grid_tseries <- grid_results$tseries

# Reference point table
refpts_names <- c("Clatest", "SBlatest", "SBrecent", "TBlatest", "TBrecent",
                 "Flatest", "Frecent", "SBmsy", "MSY", "Fmsy", "Frecent_Fmsy",
                 "Flatest_Fmsy", "SBrecent_SBmsy", "SBlatest_SBmsy",
                 "SBrecent_SBF0", "SBlatest_SBF0")
refpts_table <- sapply(grid_results$refpts[refpts_names], mean_and_quantiles)
refpts_table <- round(t(refpts_table), 2)
refpts_table[refpts_table > 100] <- round(refpts_table[refpts_table > 100])
refpts_table <- xtab2taf(refpts_table, "Metric")

# Including estimation uncertainty
refpts_est <- round(t(sapply(refpts_est, mean_and_quantiles)), 2)
refpts_est <- xtab2taf(refpts_est, "Metric")
refpts_est$Metric[refpts_est$Metric=="BBmsy"] <- "SBrecent/SBmsy (est)"
refpts_est$Metric[refpts_est$Metric=="FFmsy"] <- "Frecent/Fmsy (est)"

# Combine reference point tables
refpts_table <- rbind(refpts_table, refpts_est)

# Write table
write.taf(refpts_table, dir="output")
write.taf(grid_refpts, dir="output")
write.taf(grid_tseries, dir="output")
