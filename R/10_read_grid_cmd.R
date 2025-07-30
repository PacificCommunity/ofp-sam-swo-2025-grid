# Read grid results

# Parse Report.sso using readLines (1.1 sec/file => 10 min/grid)
final_f_fmsy <- function(repfile)
{
  # Read as text
  txt <- readLines(repfile)

  # Locate Kobe table
  from <- which(txt == "Kobe_Plot report:18") + 2
  empty <- which(txt == "")
  to <- min(empty[empty > from]) - 1

  # Read Kobe table
  kobe <- read.table(text=txt[from:to], header=TRUE)
  tail(kobe$F.Fmsy, 1)
}

# Parse Report.sso using shell commands (0.08 sec/model => 40 sec/grid)
fast_f_fmsy <- function(repfile)
{
  # Shell command
  cmd <- paste("sed -n '/^Kobe_Plot report:/,/^SPAWN_RECRUIT report/p'",
               repfile, "| grep '^[Y0-9]'")
  txt <- system(cmd, intern=TRUE)

  # Read Kobe table
  kobe <- read.table(text=txt, header=TRUE)
  tail(kobe$F.Fmsy, 1)
}

folder_1 <- "../Q_10_127_Diag_inputs"
folder_2 <- "../Q_10_128_PICTCPUE_inputs"
folder_3 <- "../Q_10_129_EUCPUE_inputs"
model_runs <- dir(c(folder_1, folder_2, folder_3), full=TRUE)
model_runs <- grepv("Growth-Estimated", model_runs)  # exclude external growth
report_files <- file.path(model_runs, "Report.sso")
## out <- sapply(report_files, final_f_fmsy)
out <- sapply(report_files, fast_f_fmsy)

################################################################################

results <- data.frame(do.call(rbind, strsplit(basename(model_runs), "_")))
names(results) <- sub("-.*", "", strsplit(basename(model_runs)[1], "_")[[1]])
results[] <- sapply(results, sub, pattern=".*-", replacement="")
results$F.Fmsy <- out
results$Growth <- NULL
results <- cbind(CPUE=basename(dirname(model_runs)), results)

results$CPUE[results$CPUE == "Q_10_127_Diag_inputs"] <- "NZ"
results$CPUE[results$CPUE == "Q_10_128_PICTCPUE_inputs"] <- "PICT"
results$CPUE[results$CPUE == "Q_10_129_EUCPUE_inputs"] <- "EU"

################################################################################

bplot <- function(varname, ylim=c(0, 0.7))
{
  z <- split(results[["F.Fmsy"]], results[[varname]])
  boxplot(z, main=varname, ylim=ylim)
  NULL
}

pdf("grid.pdf", width=24, height=8)
par(mfrow=c(1, 6))
sapply(head(names(results), -1), bplot)
dev.off()
