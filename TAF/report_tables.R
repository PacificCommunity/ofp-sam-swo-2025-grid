# Produce tables for report

# Before: refpts_table (output)
# After:  refpts_table.csv (report)

library(TAF)
source("boot/software/utilities_r4ss.R")

mkdir("report")

# Load reference points and time series
refpts_table <- read.taf("output/refpts_table.csv")

# Round reference point table
refpts_table <- taf2xtab(refpts_table)
refpts_table <- round(refpts_table, 2)
refpts_table[refpts_table > 100] <- round(refpts_table[refpts_table > 100])
refpts_table <- xtab2taf(refpts_table, "Metric")

# Write table
write.taf(refpts_table, dir="report")
