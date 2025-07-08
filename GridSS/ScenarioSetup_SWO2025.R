library(r4ss)
library(here)

setwd(here())

source("GridSS/helpers.R")

#####################################
##### SS3 SCENARIO GENERATOR ########
#####################################
# Author: Kyuhan Kim
# Date: July 8, 2025
# Description: Automated SS3 scenario generation for sensitivity analysis


# QUICK START GUIDE:
# 1. Define what you want to test in 'factor_groups' below
# 2. Set RUN_FACTORIAL = TRUE for all combinations, FALSE for individual tests (aka one-off runs)
# 3. Run the script to generate SS3 input files

# WHAT YOU CAN MODIFY:
# ┌─────────────────────────┬──────────────────────────────────────────────────────┐
# │ Parameter Type          │ What it does                                         │
# ├─────────────────────────┼──────────────────────────────────────────────────────┤
# │ multiplier              │ Scale ALL likelihood weights by this number          │
# │ line_adjustments        │ Change specific likelihood weight lines              │
# │ mg_params               │ Modify biological parameters (growth, M)             │
# │ sr_params               │ Change stock-recruitment parameters                  │
# │ parameter_offset_approach│ Control M, G, CV_G parameter offset method:         │
# │                         │ 1=direct no offset, 2=male=fem*exp(male_parm),       │
# │                         │ 3=male=female*exp(parm) then old=young*exp(parm)     │
# └─────────────────────────┴──────────────────────────────────────────────────────┘

# SIMPLE EXAMPLES:
# 
# Test different natural mortality values:
# natmort = list(
#   low = list(mg_params = list("NatM_p_1_Fem_GP_1" = list(INIT = 0.12))),
#   high = list(mg_params = list("NatM_p_1_Fem_GP_1" = list(INIT = 0.20)))
# )
#
# Test data weighting:
# datawts = list(
#   half = list(multiplier = 0.5),    # Reduce all weights by half
#   double = list(multiplier = 2.0)   # Double all weights
# )
#
# OUTPUT NAMING:
# Individual: "natmort-low", "datawts-half"
# Combined: "natmort-low_datawts-half"

############################################
##### Path Setup and check base files ######
############################################

BaseDir <- "02_one_offs/base/"
main_dir <- "02_one_offs"

base_ctl_file <- paste0(BaseDir, "swo2025.ctl")
base_dat_file <- paste0(BaseDir, "swo2025.dat")
base_forecast_file <- paste0(BaseDir, "forecast.ss")
base_starter_file <- paste0(BaseDir, "starter.ss")

# Read base control file for reference
ctlBase_original <- SS_readctl(file = base_ctl_file, datlist = base_dat_file)
print(ctlBase_original)

###############################
##### Scenario setup ##########
###############################
# Define factor groups for factorial design
factor_groups <- list(
  
  
  # Growth scenarios with parameter_offset_approach control
  Growth = list(
    
    "External" = list(
      mg_params = list(
       
        ## female
        "L_at_Amin_Fem_GP_1" = list(INIT = 86.709, PHASE = -4),
        "L_at_Amax_Fem_GP_1" = list(INIT = 257.982, PHASE = -4),
        "VonBert_K_Fem_GP_1" = list(INIT = 0.153, PHASE = -4),
        "CV_young_Fem_GP_1" = list(INIT = 0.144, PHASE = -3),
        "CV_old_Fem_GP_1" = list(INIT = 0.08, PHASE = -3),
        
        ## male
        "L_at_Amin_Mal_GP_1" = list(INIT = 84.994, PHASE = -4),
        "L_at_Amax_Mal_GP_1" = list(INIT = 206.703, PHASE = -4),
        "VonBert_K_Mal_GP_1" = list(INIT = 0.261, PHASE = -4),
        "CV_young_Mal_GP_1" = list(INIT =  0.173, PHASE = -3),
        "CV_old_Mal_GP_1" = list(INIT = 0.07, PHASE = -3)
        
      ),
      parameter_offset_approach = 1 #direct,
    ),
    
    "MLE" = list(
      mg_params = list(
        "L_at_Amin_Fem_GP_1" = list(INIT = 40, PHASE = -4),
        "L_at_Amax_Fem_GP_1" = list(INIT = 260, PHASE = -4),
        "VonBert_K_Fem_GP_1" = list(INIT = 0.3, PHASE = -4),
        "CV_young_Fem_GP_1" = list(INIT = 0.148, PHASE = -3),
        "CV_old_Fem_GP_1" = list(INIT = 0.148, PHASE = -3)
      ),
      parameter_offset_approach = 2 #male=fem*exp(male_parm),
    ),
    
    "Estimated" = list(
      mg_params = list(
        "L_at_Amin_Fem_GP_1" = list(PHASE = 4),
        "L_at_Amax_Fem_GP_1" = list(PHASE = 4),
        "VonBert_K_Fem_GP_1" = list(PHASE = 4),
        "CV_young_Fem_GP_1" = list(PHASE = 3),
        "CV_old_Fem_GP_1" = list(PHASE = 3)
      ),
      parameter_offset_approach = 2 #male=fem*exp(male_parm),
    )
  ),
  
  
  
  # Steepness scenarios
  Steepness = list(
    
    "0.7" = list(
      sr_params = list(
        "SR_BH_steep" = list(INIT = 0.7)
      )
    ),
    
    "0.8" = list(
      sr_params = list(
        "SR_BH_steep" = list(INIT = 0.8)
      )
    ),
    
    "0.9" = list(
      sr_params = list(
        "SR_BH_steep" = list(INIT = 0.9)
      )
    )
    
  # ),
  # 
  # 
  # # Natural mortality scenarios
  # Natmort = list(
  #   
  #   "Mest" = list(
  #     mg_params = list(
  #       "NatM_p_1_Fem_GP_1" = list(PHASE = 3)
  #     )
  #   ),
  #   
  #   "Mhc" = list(
  #     mg_params = list(
  #       "NatM_p_1_Fem_GP_1" = list(INIT = 0.3, PHASE = -3)
  #     )
  #   ),
  #   
  #   "Mmle" = list(
  #     mg_params = list(
  #       "NatM_p_1_Fem_GP_1" = list(INIT = 0.3, PHASE = -3)
  #     )
  #   ),
  #   
  #   "M10th" = list(
  #     mg_params = list(
  #       "NatM_p_1_Fem_GP_1" = list(INIT = 0.3, PHASE = -3)
  #     )
  #   )
  #   
  # ),
  # 
  # 
  # 
  # # Data weights scenarios
  # DataWts = list(
  #   
  #   "2age" = list(
  #     line_adjustments = data.frame(
  #       Line = c(17),
  #       Multiplier = c(2)
  #     )
  #   ),
  #   
  #   "2weight" = list(
  #     line_adjustments = data.frame(
  #       Line = c(18, 19, 20),
  #       Multiplier = c(2, 2, 2)
  #     )
  #   ),
  #   
  #   
  #   "2length" = list(
  #     line_adjustments = data.frame(
  #       Line = seq(1:17),
  #       Multiplier = rep(2, 17)
  #     )
  #   )
  #   
  # ),
  # 
  # 
  # # Recruitment distribution scenarios
  # RecProp = list(
  #   
  #   "0.3" = list(                
  #     mg_params = list(   
  #       "RecrDist_GP_1_area_1_month_2" = list(INIT = 0.0, PHASE = -2),
  #       "RecrDist_GP_1_area_2_month_2" = list(INIT = 0.0, PHASE = -2)
  #     )
  #   ),
  #   
  #   "0.5" = list(
  #     mg_params = list(
  #       "RecrDist_GP_1_area_1_month_2" = list(INIT = 0.5, PHASE = -2),
  #       "RecrDist_GP_1_area_2_month_2" = list(INIT = -0.5, PHASE = -2)
  #     )
  #   ),
  #   
  #   "0.7" = list(
  #     mg_params = list(
  #       "RecrDist_GP_1_area_1_month_2" = list(INIT = -0.5, PHASE = -2),
  #       "RecrDist_GP_1_area_2_month_2" = list(INIT = 0.5, PHASE = -2)
  #     )
  #   )
  #   
  )

  )

#####################################
##### Configuration Options #########
#####################################

# Automatically set factorial factors based on available factor groups
FACTORIAL_FACTORS <- names(factor_groups)  # Automatically includes all factors
# Or manually select specific factors:
# FACTORIAL_FACTORS <- c("datawts", "natmort")  # Only these factors

# Design option
RUN_FACTORIAL <- T  # Set to TRUE to generate factorial combinations

# Display available factors
cat("Available factor groups:", paste(names(factor_groups), collapse = ", "), "\n")
cat("Current FACTORIAL_FACTORS:", paste(FACTORIAL_FACTORS, collapse = ", "), "\n")


####################################
## running the scenario generator ##
####################################

source("GridSS/runner.R")

source("GridSS/validator.R")

