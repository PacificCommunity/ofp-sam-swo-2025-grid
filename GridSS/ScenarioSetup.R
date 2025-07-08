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

movepar=find_params(base_ctl, "Move")     # Find movement parameters
natm=find_params(base_ctl, "NatM")     # Find natural mortality parameters
vonbert=find_params(base_ctl, "VonBert")  # Find growth parameters
length=find_params(base_ctl, "L_at")     # Find length parameters

# Define factor groups for factorial design
factor_groups <- list(
  
  # Data weights scenarios
  datawts = list(
    
    half = list(multiplier = 0.5),
    
    double = list(multiplier = 2),
    
    age_emphasis = list(
      line_adjustments = data.frame(
        Line = c(17),
        Multiplier = c(1.5)
      )
    ),
    
    weight_emphasis = list(
      line_adjustments = data.frame(
        Line = c(18, 19, 20),
        Multiplier = c(2, 2, 2)
      )
    )
  ),
  
  # Natural mortality scenarios
  natmort = list(
    low = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(INIT = 0.15, PHASE = -3)
      )
    ),
    high = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(INIT = 0.3, PHASE = -3)
      )
    )
  ),
  
  # Steepness scenarios
  steepness = list(
    
    verylow = list(
      sr_params = list(
        "SR_BH_steep" = list(INIT = 0.6)
      )
    ),
    
    low = list(
      sr_params = list(
        "SR_BH_steep" = list(INIT = 0.7)
      )
    ),
    
    high = list(
      sr_params = list(
        "SR_BH_steep" = list(INIT = 0.9)
      )
    )
    
  ),
  
  # Growth scenarios with parameter_offset_approach control
  growth = list(
    fast_direct = list(
      mg_params = list(
        "VonBert_K_Fem_GP_1" = list(INIT = 0.30, PHASE = 4),
        "L_at_Amax_Fem_GP_1" = list(INIT = 260, PHASE = 4)
      ),
      parameter_offset_approach = 1  # Direct, no offset
    ),
    fast_male_offset = list(
      mg_params = list(
        "VonBert_K_Fem_GP_1" = list(INIT = 0.30, PHASE = 4),
        "L_at_Amax_Fem_GP_1" = list(INIT = 260, PHASE = 4)
      ),
      parameter_offset_approach = 2  # Male = female * exp(male_parm)
    )
    ),
  
  
  # Movement scenarios based on current parameter values
  movement = list(
    
    # Low movement scenario (more negative = less movement; not sure please double check before running)
    low = list(
      mg_params = list(
        # Area 1 to 2 - reduce movement
        "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(INIT = -4.0, PHASE = -3),
        
        # Area 2 to 1 - reduce movement
        "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3),
        "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(INIT = -5.0, PHASE = -3)
      )
    ),
    
    # High movement scenario (less negative = more movement; not sure please double check before running)
    high = list(
      mg_params = list(
        # Area 1 to 2 - increase movement
        "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(INIT = -1.5, PHASE = -3),
        
        # Area 2 to 1 - increase movement
        "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3),
        "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(INIT = -2.5, PHASE = -3)
      )
    )
  ),
  
  
  # Recruitment distribution scenarios
  recdist = list(
    equal = list(                ## 0 and 0 means equal recruitment in both areas I assume?? if it's not, please double check
      mg_params = list(   
        "RecrDist_GP_1_area_1_month_2" = list(INIT = 0.0, PHASE = -2),
        "RecrDist_GP_1_area_2_month_2" = list(INIT = 0.0, PHASE = -2)
      )
    ),
    area1_emphasis = list(
      mg_params = list(
        "RecrDist_GP_1_area_1_month_2" = list(INIT = 0.5, PHASE = -2),
        "RecrDist_GP_1_area_2_month_2" = list(INIT = -0.5, PHASE = -2)
      )
    ),
    area2_emphasis = list(
      mg_params = list(
        "RecrDist_GP_1_area_1_month_2" = list(INIT = -0.5, PHASE = -2),
        "RecrDist_GP_1_area_2_month_2" = list(INIT = 0.5, PHASE = -2)
      )
    )
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
RUN_FACTORIAL <- F  # Set to TRUE to generate factorial combinations

# Display available factors
cat("Available factor groups:", paste(names(factor_groups), collapse = ", "), "\n")
cat("Current FACTORIAL_FACTORS:", paste(FACTORIAL_FACTORS, collapse = ", "), "\n")



####################################
## running the scenario generator ##
####################################

source("GridSS/runner.R")

source("GridSS/validator.R")

