library(r4ss)
library(here)
library(parallel)

setwd(here())

source("R/helpers.R")

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

batch_count <- as.integer(Sys.getenv("BATCH_COUNT", 1))
batch_index <- as.integer(Sys.getenv("BATCH_INDEX", 1))
SS3_OPTIONS <- Sys.getenv("SS3_OPTIONS", "-stopph 2 -nohess")  # Get SS3 options from environment variable
VERBOSE <- as.logical(Sys.getenv("VERBOSE", FALSE))  # Default to FALSE if not set
nCORES <- as.integer(Sys.getenv("nCORES", 4))  # Default to detectCores()-2 if not set

if (nzchar(Sys.getenv("BATCH_COUNT"))) {
  cat("Batch processing enabled\n")
  cat("Total batches:", batch_count, "\n")
  cat("Current batch:", batch_index, "\n")
} else {
  cat("Running in single batch mode\n")
}


BaseDir <- "diagnostic/P_10_120_EarlyRecDevs_inputs/"
GridDir <- "grids/"

base_ctl_file <- paste0(BaseDir, "swo2025.ctl")
base_dat_file <- paste0(BaseDir, "swo2025.dat")
base_forecast_file <- paste0(BaseDir, "forecast.ss")
base_starter_file <- paste0(BaseDir, "starter.ss")
exe_file <- "exe/ss_3.30.23.1/ss3"

# Read base control file for reference
 ctlBase_original <- SS_readctl(file = base_ctl_file, datlist = base_dat_file)
 print(ctlBase_original)

###############################
##### Scenario setup ##########
###############################
# Define factor groups for factorial design
factor_groups <- list(

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
    
  ),
  
  
  # Recruitment proportion scenarios
  RecProp = list(
    
    "Base" = list(
      mg_params = list(
        "RecrDist_GP_1_area_2_month_2" = list(INIT = 1.0986)
      )
    ),
    
    "MoreR2" = list(
      mg_params = list(
        "RecrDist_GP_1_area_2_month_2" = list(INIT = 1.3863)
      )
    )
    
 
  ),
  
  
  # Movement scenarios
  Move = list(
    
    "Base"= list(
      mg_params = list(
        # Area 1 to 2 
        "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(INIT = -3.57555),
        
        # Area 2 to 1 
        "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(INIT = -3.05761)
      )
    ),
    
    
    "Half1to2"= list(
      mg_params = list(
        # Area 1 to 2
        "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(INIT = -3.748838),
        "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(INIT = -3.748838),
        
        # Area 2 to 1
        "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(INIT = -3.05761),
        "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(INIT = -3.05761)
      )
    ),
    
    
    "Half2to1"= list(
      mg_params = list(
        # Area 1 to 2
        "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(INIT = -3.57555),
        "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(INIT = -3.57555),
        
        # Area 2 to 1
        "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(INIT = -3.230894),
        "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(INIT = -3.230894)
      )
    )
  
    ),
  
  
  # Data weights scenarios
  DataWts = list(
    
    "Base" = list(
      line_adjustments = data.frame(
        Line = c(1),
        Multiplier = c(1.0)
      )
    ),
    
    
    "2weight" = list(
      line_adjustments = data.frame(
        Line = c(18, 19, 20),
        Multiplier = c(2.0, 2.0, 2.0)
      )
    ),
    
    
    "0.5weight" = list(
      line_adjustments = data.frame(
        Line = c(18, 19, 20),
        Multiplier = c(0.5, 0.5, 0.5)
      )
    ),
    
    
    "2length" = list(
      line_adjustments = data.frame(
        Line = c(3, 11),
        Multiplier = rep(2, 2)
      )
    ),
    
    
    "0.5length" = list(
      line_adjustments = data.frame(
        Line = c(3, 11),
        Multiplier = rep(0.5, 2)
      )
    )
    
  ),
  
  
  

  # Growth scenarios
  Growth = list(
    
    "Estimated" = list(
      mg_params = list(
        "L_at_Amin_Fem_GP_1" = list(PHASE = 4),
        "L_at_Amax_Fem_GP_1" = list(PHASE = 4),
        "VonBert_K_Fem_GP_1" = list(PHASE = 4)
      )
    ),
    
    "External" = list(
      mg_params = list(
        
        ## female
        "L_at_Amin_Fem_GP_1" = list(INIT = 86.7085152834443, PHASE = -4),
        "L_at_Amax_Fem_GP_1" = list(INIT = 257.982055205666, PHASE = -4),
        "VonBert_K_Fem_GP_1" = list(INIT = 0.153478185118902, PHASE = -4)
        
      )
    )
    
  ),
 

  # Natural mortality scenarios
  Natmort = list(

    "Mest" = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(PHASE = 3)
    )
    ),

    "M50th" = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(INIT = 0.2500632, PRIOR=0.3, PR_SD=99, PR_type=0, PHASE = -3)
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
RUN_FACTORIAL <- T  # Set to TRUE to generate factorial combinations

# Display available factors
cat("Available factor groups:", paste(names(factor_groups), collapse = ", "), "\n")
cat("Current FACTORIAL_FACTORS:", paste(FACTORIAL_FACTORS, collapse = ", "), "\n")


####################################
## running the scenario generator ##
####################################

source("R/genGrid.R")

source("R/runSS.R")

