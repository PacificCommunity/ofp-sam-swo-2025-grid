source("utilities_r4ss.R")  # mean_and_quantiles

set.seed(1)
nsim <- 1000

# Read model results
model_list <- readRDS("../model_list/model_list.rds")

# Simulate reference point draws to incorporate estimation uncertainty
draws <- lapply(model_list, estimation_uncertainty, n=nsim)

# Extract simulated reference points
BBmsy <- sapply(draws, `[`, "BBmsy")
BBmsy <- unlist(BBmsy, use.names=FALSE)
FFmsy <- sapply(draws, `[`, "FFmsy")
FFmsy <- unlist(FFmsy, use.names=FALSE)

# Reference point table with estimation uncertainty
tab <- round(t(sapply(data.frame(BBmsy, FFmsy), mean_and_quantiles)), 2)

sims <- lapply(model_list, estimation_uncertainty_ts, nsim=1000)

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

save(sb, rec, ffmsy, bbmsy,
     file="../model_list/estimation_uncertainty_ts.RData")
