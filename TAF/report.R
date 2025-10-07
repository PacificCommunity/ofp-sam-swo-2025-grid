# Produce plots and tables for report

# Before: tseries_est.rds (model), refpts.csv, tseries.csv (output)
# After:  refpts_table.csv (report)

library(TAF)
source("boot/software/utilities_r4ss.R")

mkdir("report")

# Load reference points and time series
refpts <- read.taf("output/refpts.csv")
refpts_table <- read.taf("output/refpts_table.csv")
tseries <- read.taf("output/tseries.csv")
tseries_est <- readRDS("model/tseries_est.rds")

# Generating plots
rmarkdown::render("boot/software/32_TSRibbon.rmd", output_dir = "report")
rmarkdown::render("boot/software/31-1_kobe_uncertainty.rmd", output_dir = "report")

# Round reference point table
refpts_table <- taf2xtab(refpts_table)
refpts_table <- round(refpts_table, 2)
refpts_table[refpts_table > 100] <- round(refpts_table[refpts_table > 100])
refpts_table <- xtab2taf(refpts_table, "Metric")

# Write table
write.taf(refpts_table, dir="report")
