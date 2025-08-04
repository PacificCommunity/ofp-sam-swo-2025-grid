source("utilities_r4ss.R")  # mean_and_quantiles

set.seed(1)
nsim <- 1000

# Read model results
model_list <- readRDS("../model_list/model_list.rds")

# Simulate draws to incorporate estimation uncertainty
draws <- lapply(model_list, estimation_uncertainty, n=nsim)

# Extract simulated B/Bmsy and F/Fmsy
BBmsy <- sapply(draws, `[`, "BBmsy")
BBmsy <- unlist(BBmsy, use.names=FALSE)
FFmsy <- sapply(draws, `[`, "FFmsy")
FFmsy <- unlist(FFmsy, use.names=FALSE)

tab <- round(t(sapply(data.frame(BBmsy, FFmsy), mean_and_quantiles)), 2)

print(tab)
