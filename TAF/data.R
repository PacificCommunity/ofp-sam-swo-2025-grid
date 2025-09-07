# Prepare data, write CSV data tables

# Before: model_list.rds (boot/data)
# After:  conv.csv, ensemble.rds (data)

library(TAF)
source("boot/software/utilities_r4ss.R")

mkdir("data")

model_list <- readRDS("../model_list/model_list.rds")

# Convergence
conv <- data.frame(Model=names(model_list))
conv$Gradient <- unlist(sapply(model_list, `[`, "maximum_gradient_component"))
conv$log_det_hessian <- unlist(sapply(model_list, `[`, "log_det_hessian"))
conv$PDH <- conv$log_det_hessian > 1e-6  # safer than > 0.0 floating point
conv <- model_info(conv)

# Create ensemble
ensemble <- create_ensemble(model_list)
ensemble <- list(tseries=model_info(ensemble$tseries),
                 refpts=model_info(ensemble$refpts))

# Write convergence table and ensemble list
write.taf(conv, dir="data")
saveRDS(ensemble, "data/ensemble.rds")
