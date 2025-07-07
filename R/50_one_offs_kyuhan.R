library(r4ss)
library(here)

# Set working directory to project root
setwd(here())

##################################
## Weight Scenario Setup Script ##
##################################

# Set multiplier values and scenario names
multipliers <- c(2, 5, 10)
scenario_names <- paste0("scenario_mult", multipliers)

BaseDir <- "02_one_offs/base/"

# Base file paths 
base_ctl_file <- paste0(BaseDir, "swo2025.ctl")
base_dat_file <- paste0(BaseDir, "swo2025.dat")
base_forecast_file <- paste0(BaseDir, "forecast.ss")
base_starter_file <- paste0(BaseDir, "starter.ss")

# Main dir where 
main_dir <-"02_one_offs"



##----------------------------------------------------------------------------------------------------------------##

# Output file names configuration
output_ctl_name <- "swo2025.ctl"
output_dat_name <- "swo2025.dat"
output_forecast_name <- "forecast.ss"
output_starter_name <- "starter.ss"


# Create main scenarios directory
dir.create(main_dir, recursive = TRUE)

# Generate ctl files for each multiplier in separate folders
for (i in seq_along(multipliers)) {
  mult <- multipliers[i]
  scenario_name <- scenario_names[i]
  
  # Create scenario-specific directory
  scenario_dir <- file.path(main_dir, scenario_name)
  dir.create(scenario_dir, recursive = TRUE)

  # Read ctl file directly from original location
  ctlBase <- SS_readctl(
    file = base_ctl_file,
    datlist = base_dat_file
  )
  
  # Apply multiplier to Variance_adjustment_list Value
  ctlBase$Variance_adjustment_list$Value <- ctlBase$Variance_adjustment_list$Value * mult
  
  # Generate output file name (within scenario directory)
  outfile_name <- file.path(scenario_dir, output_ctl_name)
  
  # Write ctl file
  SS_writectl(ctlBase,
              outfile = outfile_name,
              overwrite = TRUE,
              verbose = TRUE)
  
  
  # Copy additional files to scenario directory ##
  
  # Copy dat file
  file.copy(base_dat_file, file.path(scenario_dir, output_dat_name), overwrite = TRUE)
  
  # Copy forecast.ss file
  file.copy(base_forecast_file, file.path(scenario_dir, output_forecast_name), overwrite = TRUE)
  
  # Copy starter.ss file
  file.copy(base_starter_file, file.path(scenario_dir, output_starter_name), overwrite = TRUE)

}

