
setwd(here())

scenario_folders <- list.dirs(GridDir, full.names = FALSE, recursive = FALSE)



results_parallel <- mclapply(seq_along(scenario_folders), function(i) {
  run_scenario_script(scenario_folders[i], 
                      base_path = GridDir, 
                      ss3_options = "", 
                      verbose = VERBOSE, 
                      scenario_number = i)
}, mc.cores = 2)

