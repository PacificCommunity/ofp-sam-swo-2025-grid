library(r4ss)
library(here)

setwd(here())

source("R/kyuhan_helpers.R")

#####################################
##### Scenario configurations  ######
#####################################

scenarios <- list(
  
  # Global scenarios (no line specified = apply to all likelihood weights)
  scenario_mult0.5 = list(multiplier = 0.5),
  scenario_mult2 = list(multiplier = 2),
  scenario_mult3 = list(multiplier = 3),
  

  #-- if you want to change specific likelihood weights, use this below   --#
  
  
  # # Line-specific scenarios (specify by line number for individual likelihood weights)
  line_scenario_1 = list(
    line_adjustments = data.frame(
      Line = c(2, 3, 17),  # Variance_adjustment_list2, 3, 17
      Multiplier = c(10, 5, 2)
    )
  )
  # 
  # line_scenario_2 = list(
  #   line_adjustments = data.frame(
  #     Line = c(1, 7, 10, 18),  # Multiple line specification
  #     Multiplier = c(0.5, 20, 50, 3)
  #   )
  # ),
  # 
  # # Fleet-based scenarios (specify by Fleet number for likelihood weights)
  # fleet_scenario = list(
  #   fleet_adjustments = data.frame(
  #     Fleet = c(2, 5, 13),
  #     Multiplier = c(10, 5, 3)
  #   )
  # )

  
  )





##########################################
########## Path configurations ###########
##########################################

BaseDir <- "02_one_offs/base/"
main_dir <- "02_one_offs"

# Display current likelihood weight adjustment list with line numbers at the front
ctlBase <- SS_readctl(
  file = paste0(BaseDir, "swo2025.ctl"),
  datlist = paste0(BaseDir, "swo2025.dat")
)

variance_df <- ctlBase$Variance_adjustment_list
variance_df <- cbind(Line = 1:nrow(variance_df), variance_df)
print(variance_df)


# Base file paths 
base_ctl_file <- paste0(BaseDir, "swo2025.ctl")
base_dat_file <- paste0(BaseDir, "swo2025.dat")
base_forecast_file <- paste0(BaseDir, "forecast.ss")
base_starter_file <- paste0(BaseDir, "starter.ss")



##########################################
########## Generate SS input files #######
##########################################


# Create main directory
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)

# Create scenarios
for (scenario_name in names(scenarios)) {
  config <- scenarios[[scenario_name]]
  
  # Create scenario-specific directory
  scenario_dir <- file.path(main_dir, scenario_name)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Read and modify ctl file
  ctlBase <- SS_readctl(file = base_ctl_file, datlist = base_dat_file)
  ctlBase <- apply_adjustments(ctlBase, config)
  
  # Write modified ctl file
  SS_writectl(ctlBase, outfile = file.path(scenario_dir, "swo2025.ctl"), 
              overwrite = TRUE, verbose = FALSE)
  
  # Copy additional files to scenario directory
  file.copy(base_dat_file, file.path(scenario_dir, "swo2025.dat"), overwrite = TRUE)
  file.copy(base_forecast_file, file.path(scenario_dir, "forecast.ss"), overwrite = TRUE)
  file.copy(base_starter_file, file.path(scenario_dir, "starter.ss"), overwrite = TRUE)
}
