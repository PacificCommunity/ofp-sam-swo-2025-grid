
setwd(here())

scenario_folders <- list.dirs(GridDir, full.names = FALSE, recursive = FALSE)
results_parallel <- mclapply(scenario_folders, 
                             run_scenario_script, 
                             base_path = GridDir, 
                             mc.cores = detectCores()-1)
