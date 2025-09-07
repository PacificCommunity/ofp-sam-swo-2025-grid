# Extract results of interest, write CSV output tables

# Before: ensemble.rds, estimation_refpts.rds (model)
# After:  refpts.csv (output)

library(TAF)
source("boot/software/utilities_r4ss.R")

mkdir("output")

# Load estimation uncertainty results
ensemble <- readRDS("model/ensemble.rds")
estimation_refpts <- readRDS("model/estimation_refpts.rds")

# Reference points
refpts_names <- c("Clatest", "SBlatest", "SBrecent", "TBlatest", "TBrecent",
                 "Flatest", "Frecent", "SBmsy", "MSY", "Fmsy", "Frecent_Fmsy",
                 "Flatest_Fmsy", "SBrecent_SBmsy", "SBlatest_SBmsy",
                 "SBrecent_SBF0", "SBlatest_SBF0")
refpts <- round(t(sapply(ensemble$refpts[refpts_names], mean_and_quantiles)), 2)
refpts[refpts > 100] <- round(refpts[refpts > 100])
refpts <- xtab2taf(refpts, "Metric")

# Reference points with estimation uncertainty
refpts_est <- round(t(sapply(estimation_refpts, mean_and_quantiles)), 2)
refpts_est <- xtab2taf(refpts_est, "Metric")
refpts_est$Metric[refpts_est$Metric=="BBmsy"] <- "SBrecent/SBmsy (est)"
refpts_est$Metric[refpts_est$Metric=="FFmsy"] <- "Frecent/Fmsy (est)"

# Combine tables
refpts <- rbind(refpts, refpts_est)

# Write table
write.taf(refpts, dir="output")
