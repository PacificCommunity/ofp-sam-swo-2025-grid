# Run analysis, write model results

# Before: model_list.rds (boot/data)
# After:  ensemble.rds, estimation_refpts.rds, estimation_ts.rds (model)

library(TAF)
source("boot/software/utilities_r4ss.R")

mkdir("model")

# Read model results
model_list <- readRDS("boot/data/model_list.rds")

# Create ensemble
ensemble <- create_ensemble(model_list)
ensemble <- list(tseries=model_info(ensemble$tseries),
                 refpts=model_info(ensemble$refpts))

# Estimation uncertainty simulation
set.seed(1)
nsim <- 1000
draws <- lapply(model_list, estimation_uncertainty, n=nsim)

# Estimation uncertainty about reference points
BBmsy <- sapply(draws, `[`, "BBmsy")
BBmsy <- unlist(BBmsy, use.names=FALSE)
FFmsy <- sapply(draws, `[`, "FFmsy")
FFmsy <- unlist(FFmsy, use.names=FALSE)
estimation_refpts <- data.frame(FFmsy, BBmsy)

# Estimation uncertainty about time series
message("Simulating time series")
sims <- lapply(model_list, estimation_uncertainty_ts, nsim=nsim)
message("Calculating quantiles")
probs <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
sb <- sapply(sims, `[`, "sb")
sb <- do.call(rbind, sb)
sb <- apply(sb, 2, quantile, probs=probs)
rec <- sapply(sims, `[`, "rec")
rec <- do.call(rbind, rec)
rec <- apply(rec, 2, quantile, probs=probs)
ffmsy <- sapply(sims, `[`, "ffmsy")
ffmsy <- do.call(rbind, ffmsy)
ffmsy <- apply(ffmsy, 2, quantile, probs=probs)
bbmsy <- sapply(sims, `[`, "bbmsy")
bbmsy <- do.call(rbind, bbmsy)
bbmsy <- apply(bbmsy, 2, quantile, probs=probs)
estimation_ts <- list(SB=sb, Rec=rec, FFmsy=ffmsy, BBmsy=bbmsy)

# Save list objects
saveRDS(ensemble, "model/ensemble.rds")
saveRDS(estimation_refpts, "model/estimation_refpts.rds")
saveRDS(estimation_ts, "model/estimation_ts.rds")
