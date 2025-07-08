
## load functions
source("GridSS/helpers.R")


# Generate scenarios based on settings
if (RUN_FACTORIAL) {
  scenarios <- create_factorial_scenarios(factor_groups, FACTORIAL_FACTORS)
  cat("Generated", length(scenarios), "factorial scenarios\n")
} else {
  scenarios <- create_individual_scenarios(factor_groups)
  cat("Generated", length(scenarios), "individual scenarios\n")
}



#####################################
##### Generate Files ################
#####################################

# Create main directory
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)

# Initialize scenario summary list
scenario_summary <- list(
  metadata = list(
    creation_date = Sys.time(),
    factorial_design = RUN_FACTORIAL,
    factorial_factors = FACTORIAL_FACTORS,
    total_scenarios = length(scenarios),
    base_directory = BaseDir,
    output_directory = main_dir
  ),
  scenarios = list()
)

# Create each scenario
for (scenario_name in names(scenarios)) {
  config <- scenarios[[scenario_name]]
  
  # Create scenario directory
  scenario_dir <- file.path(main_dir, scenario_name)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Read and modify control file
  ctlBase <- SS_readctl(file = base_ctl_file, datlist = base_dat_file)
  ctlBase_modified <- apply_simple_adjustments(ctlBase, config)
  
  # Create detailed change summary
  changes <- create_change_summary(ctlBase_original, ctlBase_modified)
  
  # Add to scenario summary
  scenario_summary$scenarios[[scenario_name]] <- list(
    scenario_name = scenario_name,
    scenario_type = ifelse(RUN_FACTORIAL, "Factorial", "Individual"),
    configuration = config,
    changes = changes,
    directory = scenario_dir
  )
  
  # Write SS3 input files
  SS_writectl(ctlBase_modified, outfile = file.path(scenario_dir, "swo2025.ctl"), 
              overwrite = TRUE, verbose = FALSE)
  
  file.copy(base_dat_file, file.path(scenario_dir, "swo2025.dat"), overwrite = TRUE)
  file.copy(base_forecast_file, file.path(scenario_dir, "forecast.ss"), overwrite = TRUE)
  file.copy(base_starter_file, file.path(scenario_dir, "starter.ss"), overwrite = TRUE)
  
  cat("Created:", scenario_name, "\n")
}

# Save scenario summary as RData
save(scenario_summary, file = file.path(main_dir, "scenario_summary.RData"))

# Also save factor_groups for reference
save(factor_groups, file = file.path(main_dir, "factor_groups.RData"))