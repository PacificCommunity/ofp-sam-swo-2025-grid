# Produce plots for report

# Before: report_kobe.Rmd, report_tseries.Rmd
# After:  bbmsy.png, ffmsy.png, report_kobe.html, report_tseries.html (report)

library(TAF)
library(rmarkdown)
source("boot/software/utilities_r4ss.R")

mkdir("report")

refpts <- read.taf("output/refpts.csv")

bp <- function(...) bplot(..., df=refpts)

# Influence plot for B/Bmsy
taf.png("bbmsy", width=2400, height=3000, res=220)
par(mfrow=c(2,3))
bp("CPUE", "SBrecent_SBmsy", ylim=c(0, 4.5), ylab="SBrecent / SBmsy",
   col=c("gray60", "gray90"))
bp("Steepness", "SBrecent_SBmsy", ylim=c(0, 4.5), ylab="",
   col=c("gray90", "gray60", "gray90"))
bp("RecProp", "SBrecent_SBmsy", ylim=c(0, 4.5), ylab="",
   col=c("gray60", "gray90"))
bp("Move", "SBrecent_SBmsy", ylim=c(0, 4.5), ylab="SBrecent / SBmsy",
   col=c("gray60", "gray90", "gray90"))
bp("DataWts", "SBrecent_SBmsy", ylim=c(0, 4.5), ylab="",
   col=c(rep("gray90", 4), "gray60"))
bp("Natmort", "SBrecent_SBmsy", ylim=c(0, 4.5), ylab="",
   col=c("gray60", "gray90"))
dev.off()

# Influence plot for F/Fmsy
taf.png("ffmsy", width=2400, height=3000, res=220)
par(mfrow=c(2,3))
bp("CPUE", "Frecent_Fmsy", ylim=c(0, 0.7), ylab="Frecent / Fmsy",
   col=c("gray60", "gray90"))
bp("Steepness", "Frecent_Fmsy", ylim=c(0, 0.7), ylab="",
   col=c("gray90", "gray60", "gray90"))
bp("RecProp", "Frecent_Fmsy", ylim=c(0, 0.7), ylab="",
   col=c("gray60", "gray90"))
bp("Move", "Frecent_Fmsy", ylim=c(0, 0.7), ylab="Frecent / Fmsy",
   col=c("gray60", "gray90", "gray90"))
bp("DataWts", "Frecent_Fmsy", ylim=c(0, 0.7), ylab="",
   col=c(rep("gray90", 4), "gray60"))
bp("Natmort", "Frecent_Fmsy", ylim=c(0, 0.7), ylab="",
   col=c("gray60", "gray90"))
dev.off()

# Kobe plots
# render("report_kobe.Rmd", output_dir="report")

# Ribbon plots
# render("report_tseries.Rmd", output_dir="report")
