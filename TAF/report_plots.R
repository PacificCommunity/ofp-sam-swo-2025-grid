# Produce plots for report

# Before: report_kobe.Rmd, report_tseries.Rmd
# After:  [kobe], [tseries], report_kobe.html, report_tseries.html (report)

library(TAF)
library(rmarkdown)

mkdir("report")

# Generate plots
render("report_kobe.Rmd", output_dir="report")
render("report_tseries.Rmd", output_dir="report")
