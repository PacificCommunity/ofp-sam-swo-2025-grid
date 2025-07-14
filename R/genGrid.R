
setwd(here())

## load functions
source("R/helpers.R")

# Generate scenarios based on settings
if (RUN_FACTORIAL) {
  scenarios <- create_factorial_scenarios(factor_groups, FACTORIAL_FACTORS)
} else {
  scenarios <- create_individual_scenarios(factor_groups)
  cat("Generated", length(scenarios), "individual scenarios\n")
}

# Scenario setup
nscenarios <- length(scenarios)

# Split into batches
batches <- split(scenarios, cut(seq_along(scenarios), batch_count, labels = FALSE))

# Process based on batch_index (including NULL option)
if (is.null(batch_index)) {
  # If batch_index is NULL, use all scenarios
  selected_scenarios <- scenarios
  cat("Generated for all batches: a total of", length(selected_scenarios), "out of", nscenarios, "scenarios\n")
} else {
  # Select specific batch
  if (batch_index > length(batches) || batch_index < 1) {
    stop("Error: batch_index out of range (1 to ", length(batches), ")")
  }
  selected_scenarios <- batches[[batch_index]]
  cat("Generated for batch", batch_index, "of", batch_count, ": a total of", length(selected_scenarios), "out of", nscenarios, "scenarios\n")
}

# Assign selected scenarios to scenarios variable
scenarios <- selected_scenarios

#####################################
##### Generate Files ################
#####################################

# Create main directory
dir.create(GridDir, recursive = TRUE, showWarnings = FALSE)
# 
# # Initialize scenario summary list
scenario_summary <- list(
  metadata = list(
    creation_date = Sys.time(),
    factorial_design = RUN_FACTORIAL,
    factorial_factors = FACTORIAL_FACTORS,
    total_scenarios = length(scenarios),
    base_directory = BaseDir,
    output_directory = GridDir
  ),
  scenarios = list()
)

# Create each scenario
for (scenario_name in names(scenarios)) {
  config <- scenarios[[scenario_name]]

  # Create scenario directory
  scenario_dir <- file.path(GridDir, scenario_name)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)

  # Read and modify control file
  ctlBase <- SS_readctl(file = base_ctl_file, datlist = base_dat_file)
  ctlBase_modified <- apply_simple_adjustments(ctlBase, config)
# 
#   # Create detailed change summary
#   changes <- create_change_summary(ctlBase_original, ctlBase_modified)

  # Add to scenario summary
  scenario_summary$scenarios[[scenario_name]] <- list(
    scenario_name = scenario_name,
    scenario_type = ifelse(RUN_FACTORIAL, "Factorial", "Individual"),
    configuration = config,
    #changes = changes,
    directory = scenario_dir
  )

  # Write SS3 input files
  SS_writectl(ctlBase_modified, outfile = file.path(scenario_dir, "swo2025.ctl"),
              overwrite = TRUE, verbose = FALSE)

  file.copy(base_dat_file, file.path(scenario_dir, "swo2025.dat"), overwrite = TRUE)
  file.copy(base_forecast_file, file.path(scenario_dir, "forecast.ss"), overwrite = TRUE)
  file.copy(base_starter_file, file.path(scenario_dir, "starter.ss"), overwrite = TRUE)
  file.copy(exe_file, file.path(scenario_dir, "ss3"), overwrite = TRUE)

  cat("Created:", scenario_name, "\n")
}

# Save scenario summary as RData
save(scenario_summary, file = file.path(GridDir, "scenario_summary.RData"))

# Also save factor_groups for reference
save(factor_groups, file = file.path(GridDir, "factor_groups.RData"))