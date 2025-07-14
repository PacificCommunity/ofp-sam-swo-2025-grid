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


BaseDir <- "diagnostic/base/"
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
  

  # Growth scenarios with parameter_offset_approach control
  Growth = list(

    "External" = list(
      mg_params = list(

        ## female
        "L_at_Amin_Fem_GP_1" = list(INIT = 86.7085152834443, PHASE = -4),
        "L_at_Amax_Fem_GP_1" = list(INIT = 257.982055205666, PHASE = -4),
        "VonBert_K_Fem_GP_1" = list(INIT = 0.153478185118902, PHASE = -4),
        "CV_young_Fem_GP_1" = list(INIT = 0.148, PHASE = -3),
        "CV_old_Fem_GP_1" = list(INIT = 0.074, PHASE = -3),

        ## male  (offset values needed)
        "L_at_Amin_Mal_GP_1" = list(INIT = 0.0, PHASE = -4),
        "L_at_Amax_Mal_GP_1" = list(INIT = -0.22160488, PHASE = -4),
        "VonBert_K_Mal_GP_1" = list(INIT = 0.570508, PHASE = -4),
        "CV_young_Mal_GP_1" = list(INIT = 0.156079321, PHASE = -3),
        "CV_old_Mal_GP_1" = list(INIT = -0.055569851, PHASE = -3)

      ),
      parameter_offset_approach = 2
    ),

    "Estimated" = list(
      mg_params = list(
        "L_at_Amin_Fem_GP_1" = list(LO=30.0, HI=150.0, INIT=41.683, PHASE = 4),
        "L_at_Amax_Fem_GP_1" = list(LO=130.0, HI=300.0, INIT=244.063, PHASE = 4),
        "VonBert_K_Fem_GP_1" = list(LO=0.01, HI=0.5, INIT=0.244668, PHASE = 4),
        "CV_young_Fem_GP_1" = list(LO=0.01, HI=0.5, INIT=0.148, PHASE = -3),
        "CV_old_Fem_GP_1" = list(LO=0.01, HI=0.5, INIT=0.074, PHASE = -3),

        "L_at_Amin_Mal_GP_1" = list(INIT = 0.0, PHASE = -4),
        "L_at_Amax_Mal_GP_1" = list(INIT = -0.22160488, PHASE = -4),
        "VonBert_K_Mal_GP_1" = list(INIT = 0.570508, PHASE = -4),
        "CV_young_Mal_GP_1" = list(INIT = 0.156079321, PHASE = -3),
        "CV_old_Mal_GP_1" = list(INIT = -0.055569851, PHASE = -3)

      ),
      parameter_offset_approach = 2
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

  ),


  # Natural mortality scenarios
  Natmort = list(

    "Mest" = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(LO=0.05, HI=0.45, INIT=0.1725436, PRIOR=-1.3860415, PR_SD=0.31, PR_type=3, PHASE = 3),        # NatM_Lorenzen_Fem_GP_1
        "NatM_p_1_Mal_GP_1" = list(LO=-1.0, HI=1.0, INIT=0.107186654, PRIOR=0.3, PR_SD=99.0, PR_type=0, PHASE = -3)         # NatM_Lorenzen_Mal_GP_1
      )
    ),

    "M50th" = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(LO=0.05, HI=0.45, INIT = 0.2500632, PHASE = -3),
        "NatM_p_1_Mal_GP_1" = list(LO=-1.0, HI=1.0, INIT = 0.107186654, PHASE = -3)
      )
    ),

    "M90th" = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(LO=0.05, HI=0.45, INIT = 0.3720375, PHASE = -3),
        "NatM_p_1_Mal_GP_1" = list(LO=-1.0, HI=1.0, INIT = 0.107186654, PHASE = -3)
      )
    ),

    "M10th" = list(
      mg_params = list(
        "NatM_p_1_Fem_GP_1" = list(LO=0.05, HI=0.45, INIT = 0.1725436, PHASE = -3),
        "NatM_p_1_Mal_GP_1" = list(LO=-1.0, HI=1.0, INIT = 0.107186654, PHASE = -3)
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


    "0.2weight" = list(
      line_adjustments = data.frame(
        Line = c(18, 19, 20),
        Multiplier = c(0.2, 0.2, 0.2)
      )
    ),

    "5weight" = list(
      line_adjustments = data.frame(
        Line = c(18, 19, 20),
        Multiplier = c(5.0, 5.0, 5.0)
      )
    ),

    "0.2length" = list(
      line_adjustments = data.frame(
        Line = seq(1:16),
        Multiplier = rep(0.2, 16)
      )
    ),

    "5length" = list(
      line_adjustments = data.frame(
        Line = seq(1:16),
        Multiplier = rep(5.0, 16)
      )
    )

  ),

# Movement scenarios based on current parameter values
movement = list(

  # no movement
  "NoMov"= list(
    mg_params = list(
      # Area 1 to 2 - reduce movement
      "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),

      # Area 2 to 1 - reduce movement
      "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3),
      "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -25.0, PHASE = -3)
    )
  ),

  # middle in the screenshot
  "Base"= list(
    mg_params = list(
      # Area 1 to 2 - reduce movement
      "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),
      "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -2.59027, PHASE = -3),

      # Area 2 to 1 - reduce movement
      "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3),
      "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -3.68888, PHASE = -3)
    )
  ),

  # High movement scenario (less negative = more movement; not sure please double check before running)
  "2Mov"= list(
    mg_params = list(
      # Area 1 to 2 - reduce movement
      "MoveParm_A_seas_1_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_B_seas_1_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_A_seas_2_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_B_seas_2_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_A_seas_3_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_B_seas_3_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_A_seas_4_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),
      "MoveParm_B_seas_4_GP_1_from_1_to_2" = list(LO=-26.0, HI=4.0, INIT = -1.89712, PHASE = -3),

      # Area 2 to 1 - reduce movement
      "MoveParm_A_seas_1_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_B_seas_1_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_A_seas_2_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_B_seas_2_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_A_seas_3_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_B_seas_3_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_A_seas_4_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3),
      "MoveParm_B_seas_4_GP_1_from_2_to_1" = list(LO=-26.0, HI=4.0, INIT = -2.99573, PHASE = -3)
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
# 
# source("R/validator.R")

source("R/runSS.R")



