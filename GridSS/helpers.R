# Find specific parameter patterns
find_params <- function(ctlBase, pattern) {
  mg_names <- rownames(ctlBase$MG_parms)
  matches <- mg_names[grepl(pattern, mg_names, ignore.case = TRUE)]
  
  cat("Parameters matching '", pattern, "':\n", sep = "")
  for (i in seq_along(matches)) {
    cat(sprintf('"%s"\n', matches[i]))
  }
  
  return(matches)
}


# Function to create individual scenarios (one-off)
create_individual_scenarios <- function(factor_groups) {
  scenarios <- list()
  
  for (group_name in names(factor_groups)) {
    group_scenarios <- factor_groups[[group_name]]
    
    for (scenario_name in names(group_scenarios)) {
      # Use hyphen to separate group and level names
      full_name <- paste0(group_name, "-", scenario_name)
      scenarios[[full_name]] <- group_scenarios[[scenario_name]]
    }
  }
  
  return(scenarios)
}

# Function to create factorial combinations
create_factorial_scenarios <- function(factor_groups, selected_factors) {
  
  # Get selected factor groups
  selected_groups <- factor_groups[selected_factors]
  
  # Create all combinations
  factor_names <- lapply(selected_groups, names)
  combinations <- expand.grid(factor_names, stringsAsFactors = FALSE)
  
  scenarios <- list()
  
  for (i in 1:nrow(combinations)) {
    
    # Create scenario name without F_ prefix: group1-level1_group2-level2
    scenario_parts <- paste0(names(combinations), "-", combinations[i, ])
    scenario_name <- paste(scenario_parts, collapse = "_")
    
    # Combine configurations
    combined_config <- list()
    
    for (j in 1:ncol(combinations)) {
      group_name <- names(combinations)[j]
      level_name <- combinations[i, j]
      
      config <- selected_groups[[group_name]][[level_name]]
      
      # Merge configurations
      for (config_type in names(config)) {
        if (is.null(combined_config[[config_type]])) {
          combined_config[[config_type]] <- config[[config_type]]
        } else if (config_type == "line_adjustments") {
          # Combine line adjustments
          combined_config[[config_type]] <- rbind(
            combined_config[[config_type]], 
            config[[config_type]]
          )
        } else if (config_type == "mg_params" || config_type == "sr_params") {
          # Merge parameter lists
          combined_config[[config_type]] <- c(
            combined_config[[config_type]], 
            config[[config_type]]
          )
        } else {
          # For multiplier, keep the last one
          combined_config[[config_type]] <- config[[config_type]]
        }
      }
    }
    
    scenarios[[scenario_name]] <- combined_config
  }
  
  return(scenarios)
}


# Function to apply scenario configurations to control file (FLEXIBLE VERSION)
apply_simple_adjustments <- function(ctlBase, config) {
  
  # Global likelihood weight multiplier
  if (!is.null(config$multiplier)) {
    ctlBase$Variance_adjustment_list$Value <- 
      ctlBase$Variance_adjustment_list$Value * config$multiplier
  }
  
  # Line-specific likelihood weight adjustments
  if (!is.null(config$line_adjustments)) {
    for (i in 1:nrow(config$line_adjustments)) {
      line_num <- config$line_adjustments$Line[i]
      multiplier <- config$line_adjustments$Multiplier[i]
      
      if (line_num <= nrow(ctlBase$Variance_adjustment_list)) {
        ctlBase$Variance_adjustment_list$Value[line_num] <- 
          ctlBase$Variance_adjustment_list$Value[line_num] * multiplier
      }
    }
  }
  
  # Parameter offset approach adjustment
  if (!is.null(config$parameter_offset_approach)) {
    ctlBase$parameter_offset_approach <- config$parameter_offset_approach
  }
  
  # MG_parms adjustments (FLEXIBLE - any column)
  if (!is.null(config$mg_params)) {
    for (param_name in names(config$mg_params)) {
      param_settings <- config$mg_params[[param_name]]
      
      param_row <- which(rownames(ctlBase$MG_parms) == param_name)
      
      if (length(param_row) > 0) {
        # Apply ALL settings provided for this parameter
        for (setting_name in names(param_settings)) {
          if (setting_name %in% colnames(ctlBase$MG_parms)) {
            ctlBase$MG_parms[param_row, setting_name] <- param_settings[[setting_name]]
          }
        }
      }
    }
  }
  
  # SR_parms adjustments (FLEXIBLE - any column)
  if (!is.null(config$sr_params)) {
    for (param_name in names(config$sr_params)) {
      param_settings <- config$sr_params[[param_name]]
      
      param_row <- which(rownames(ctlBase$SR_parms) == param_name)
      
      if (length(param_row) > 0) {
        # Apply ALL settings provided for this parameter
        for (setting_name in names(param_settings)) {
          if (setting_name %in% colnames(ctlBase$SR_parms)) {
            ctlBase$SR_parms[param_row, setting_name] <- param_settings[[setting_name]]
          }
        }
      }
    }
  }
  
  return(ctlBase)
}




# Function to create detailed change summary
create_change_summary <- function(base_ctl, scenario_ctl) {
  changes <- list()
  
  # Variance adjustments
  base_var <- base_ctl$Variance_adjustment_list
  scenario_var <- scenario_ctl$Variance_adjustment_list
  
  variance_changes <- list()
  for (i in 1:nrow(base_var)) {
    if (base_var$Value[i] != scenario_var$Value[i]) {
      variance_changes[[paste0("Line_", i)]] <- list(
        original = base_var$Value[i],
        modified = scenario_var$Value[i],
        change_type = "Variance_adjustment"
      )
    }
  }
  if (length(variance_changes) > 0) {
    changes$variance_adjustments <- variance_changes
  }
  
  # Parameter offset approach changes (NEW)
  if (!is.null(base_ctl$parameter_offset_approach) && !is.null(scenario_ctl$parameter_offset_approach)) {
    if (base_ctl$parameter_offset_approach != scenario_ctl$parameter_offset_approach) {
      changes$parameter_offset_approach <- list(
        original = base_ctl$parameter_offset_approach,
        modified = scenario_ctl$parameter_offset_approach,
        change_type = "parameter_offset_approach"
      )
    }
  }
  
  # MG_parms changes
  base_mg <- base_ctl$MG_parms
  scenario_mg <- scenario_ctl$MG_parms
  
  mg_changes <- list()
  for (param in rownames(base_mg)) {
    for (col in colnames(base_mg)) {
      if (!is.na(base_mg[param, col]) && !is.na(scenario_mg[param, col])) {
        if (base_mg[param, col] != scenario_mg[param, col]) {
          mg_changes[[paste0(param, "_", col)]] <- list(
            parameter = param,
            column = col,
            original = base_mg[param, col],
            modified = scenario_mg[param, col],
            change_type = "MG_parms"
          )
        }
      }
    }
  }
  if (length(mg_changes) > 0) {
    changes$mg_parms <- mg_changes
  }
  
  # SR_parms changes
  if (!is.null(base_ctl$SR_parms) && !is.null(scenario_ctl$SR_parms)) {
    base_sr <- base_ctl$SR_parms
    scenario_sr <- scenario_ctl$SR_parms
    
    sr_changes <- list()
    for (param in rownames(base_sr)) {
      for (col in colnames(base_sr)) {
        if (!is.na(base_sr[param, col]) && !is.na(scenario_sr[param, col])) {
          if (base_sr[param, col] != scenario_sr[param, col]) {
            sr_changes[[paste0(param, "_", col)]] <- list(
              parameter = param,
              column = col,
              original = base_sr[param, col],
              modified = scenario_sr[param, col],
              change_type = "SR_parms"
            )
          }
        }
      }
    }
    if (length(sr_changes) > 0) {
      changes$sr_parms <- sr_changes
    }
  }
  
  return(changes)
}
