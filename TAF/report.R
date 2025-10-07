# Produce plots and tables for report

# Before: tseries_est.rds (model), refpts.csv, tseries.csv (output)
# After:  refpts_table.csv (report)

library(TAF)
library(rmarkdown)
source("boot/software/utilities_r4ss.R")

mkdir("report")

# Load reference points and time series
refpts <- read.taf("output/refpts.csv")
refpts_table <- read.taf("output/refpts_table.csv")
tseries <- read.taf("output/tseries.csv")
tseries_est <- readRDS("model/tseries_est.rds")

# Generate plots
render("report_tseries.Rmd", output_dir = "report")
render("report_kobe.Rmd", output_dir = "report")

# Round reference point table
refpts_table <- taf2xtab(refpts_table)
refpts_table <- round(refpts_table, 2)
refpts_table[refpts_table > 100] <- round(refpts_table[refpts_table > 100])
refpts_table <- xtab2taf(refpts_table, "Metric")

# Write table
write.taf(refpts_table, dir="report")
