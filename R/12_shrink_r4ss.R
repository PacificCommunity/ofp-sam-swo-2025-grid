# Shrink r4ss models, so 360 models do not require much RAM

shrink4ss <- function(rdsfile)
{
  x <- readRDS(rdsfile)
  select <- c("annual_time_series", "derived_quants", "Dynamic_Bzero",
              "log_det_hessian", "maximum_gradient_component")
  x[select]
}

rds_files <- dir("../rds", full=TRUE)
model_list <- lapply(rds_files, shrink4ss)

long <- basename(rds_files)
long <- sub("\\.rds", "", long)                   # remove .rds
short <- long
short <- gsub("0\\.", "0", short)                 # 0.5 -> 05
short <- sub("Q_10_127_Diag", "NZ", short)        # NZ
short <- sub("Q_10_128_PICTCPUE", "PICT", short)  # PICT
short <- sub("Steepness-", "h", short)            # Steepness -> h
short <- sub("RecProp-", "R", short)              # RecProp -> R
short <- sub("Move-", "V", short)                 # Move -> V
short <- sub("DataWts-", "D", short)              # DataWts -> D
short <- sub("Growth-Estimated_", "", short)      # redundant
short <- sub("Natmort-", "", short)               # Natmort -> M
tiny <- short
tiny <- sub("NZ", "N", tiny)
tiny <- sub("PICT", "P", tiny)
tiny <- sub("RBase", "Rb", tiny)
tiny <- sub("RMoreR2", "R2", tiny)
tiny <- sub("VBase", "Vb", tiny)
tiny <- sub("VHalf1to2", "V1", tiny)
tiny <- sub("VHalf2to1", "V2", tiny)
tiny <- sub("DBase", "Dbas", tiny)
tiny <- sub("D05length", "D05l", tiny)
tiny <- sub("D05weight", "D05w", tiny)
tiny <- sub("D2length", "D20l", tiny)
tiny <- sub("D2weight", "D20w", tiny)
tiny <- sub("M50th", "Mhac", tiny)

names(model_list) <- tiny

dir.create("../model_list", showWarnings=FALSE)
saveRDS(model_list, "../model_list/model_list.rds")
