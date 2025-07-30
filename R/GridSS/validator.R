#####################################
##### SS3 SCENARIO COMPARISON #######
#####################################
# Author: Kyuhan Kim
# Date: July 8, 2025
# Description: Generate comparison table of SS3 scenarios vs base model

library(r4ss)
library(here)
library(dplyr)

setwd(here())

# Configuration
base_dir <- "02_one_offs/base/"
scenarios_dir <- "02_one_offs/"
base_ctl_file <- file.path(base_dir, "swo2025.ctl")
base_dat_file <- file.path(base_dir, "swo2025.dat")
tolerance <- 1e-5  # Very small tolerance to avoid rounding issues

# Function to extract changes
extract_changes <- function(scenario_name, base_ctl) {
  
  scenario_ctl_file <- file.path(scenarios_dir, scenario_name, "swo2025.ctl")
  
  if (!file.exists(scenario_ctl_file)) {
    return(data.frame(Scenario = scenario_name, Parameter_Type = "ERROR", 
                      Parameter_Name = "File not found", Base_Value = NA, Scenario_Value = NA))
  }
  
  scenario_ctl <- SS_readctl(file = scenario_ctl_file, datlist = base_dat_file)
  
  changes <- data.frame(Scenario = character(), Parameter_Type = character(),
                        Parameter_Name = character(), Base_Value = character(), 
                        Scenario_Value = character(), stringsAsFactors = FALSE)
  
  # Parameter offset approach
  if (base_ctl$parameter_offset_approach != scenario_ctl$parameter_offset_approach) {
    changes <- rbind(changes, data.frame(
      Scenario = scenario_name, Parameter_Type = "Parameter_Offset",
      Parameter_Name = "parameter_offset_approach",
      Base_Value = as.character(base_ctl$parameter_offset_approach),
      Scenario_Value = as.character(scenario_ctl$parameter_offset_approach)
    ))
  }
  
  # Likelihood weights
  base_var <- base_ctl$Variance_adjustment_list
  scenario_var <- scenario_ctl$Variance_adjustment_list
  
  for (i in 1:nrow(base_var)) {
    if (abs(base_var$Value[i] - scenario_var$Value[i]) > tolerance) {
      changes <- rbind(changes, data.frame(
        Scenario = scenario_name, Parameter_Type = "Likelihood_Weight",
        Parameter_Name = paste0("Line_", i),
        Base_Value = as.character(base_var$Value[i]),
        Scenario_Value = as.character(scenario_var$Value[i])
      ))
    }
  }
  
  # MG_parms - NO EXCLUSION, use smart comparison
  base_mg <- base_ctl$MG_parms
  scenario_mg <- scenario_ctl$MG_parms
  
  for (param in rownames(base_mg)) {
    for (col in colnames(base_mg)) {
      if (col %in% colnames(scenario_mg) && 
          !is.na(base_mg[param, col]) && !is.na(scenario_mg[param, col])) {
        
        # Smart comparison function
        values_different <- FALSE
        
        if (is.numeric(base_mg[param, col]) && is.numeric(scenario_mg[param, col])) {
          # For numeric values, use very small tolerance
          if (abs(base_mg[param, col] - scenario_mg[param, col]) > tolerance) {
            values_different <- TRUE
          }
        } else {
          # For non-numeric values (PHASE, etc.), exact comparison
          if (as.character(base_mg[param, col]) != as.character(scenario_mg[param, col])) {
            values_different <- TRUE
          }
        }
        
        if (values_different) {
          changes <- rbind(changes, data.frame(
            Scenario = scenario_name, Parameter_Type = "Biological_Parameter",
            Parameter_Name = paste0(param, "_", col),
            Base_Value = as.character(base_mg[param, col]),
            Scenario_Value = as.character(scenario_mg[param, col])
          ))
        }
      }
    }
  }
  
  # SR_parms
  if (!is.null(base_ctl$SR_parms) && !is.null(scenario_ctl$SR_parms)) {
    base_sr <- base_ctl$SR_parms
    scenario_sr <- scenario_ctl$SR_parms
    
    for (param in rownames(base_sr)) {
      for (col in colnames(base_sr)) {
        if (col %in% colnames(scenario_sr) && 
            !is.na(base_sr[param, col]) && !is.na(scenario_sr[param, col])) {
          
          # Smart comparison function
          values_different <- FALSE
          
          if (is.numeric(base_sr[param, col]) && is.numeric(scenario_sr[param, col])) {
            # For numeric values, use very small tolerance
            if (abs(base_sr[param, col] - scenario_sr[param, col]) > tolerance) {
              values_different <- TRUE
            }
          } else {
            # For non-numeric values, exact comparison
            if (as.character(base_sr[param, col]) != as.character(scenario_sr[param, col])) {
              values_different <- TRUE
            }
          }
          
          if (values_different) {
            changes <- rbind(changes, data.frame(
              Scenario = scenario_name, Parameter_Type = "Stock_Recruitment",
              Parameter_Name = paste0(param, "_", col),
              Base_Value = as.character(base_sr[param, col]),
              Scenario_Value = as.character(scenario_sr[param, col])
            ))
          }
        }
      }
    }
  }
  
  return(changes)
}

# Main process
base_ctl <- SS_readctl(file = base_ctl_file, datlist = base_dat_file)
scenario_names <- basename(list.dirs(scenarios_dir, recursive = FALSE)[!grepl("base", basename(list.dirs(scenarios_dir, recursive = FALSE)))])

all_changes <- data.frame()
for (scenario_name in scenario_names) {
  all_changes <- rbind(all_changes, extract_changes(scenario_name, base_ctl))
}

# Generate table
if (nrow(all_changes) > 0) {
  print(all_changes)
  write.csv(all_changes, file.path(scenarios_dir, "scenario_comparison.csv"), row.names = FALSE)
  cat("Table saved to scenario_comparison.csv\n")
  cat("Total meaningful changes detected:", nrow(all_changes), "\n")
} else {
  cat("No meaningful changes detected\n")
}
