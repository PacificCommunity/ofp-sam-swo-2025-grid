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


# Function to create individual scenarios (one-off) - UNCHANGED
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

# Function to create factorial combinations - UNCHANGED
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

# Function to apply scenario configurations to control file (ENHANCED BUT BACKWARDS COMPATIBLE)
apply_simple_adjustments <- function(ctlBase, config) {
  
  # EXISTING FUNCTIONALITY - UNCHANGED
  
  # Global likelihood weight multiplier
  if (!is.null(config$multiplier)) {
    ctlBase$Variance_adjustment_list$value <- 
      ctlBase$Variance_adjustment_list$value * config$multiplier
  }
  
  # Line-specific likelihood weight adjustments
  if (!is.null(config$line_adjustments)) {
    for (i in 1:nrow(config$line_adjustments)) {
      line_num <- config$line_adjustments$Line[i]
      multiplier <- config$line_adjustments$Multiplier[i]
      
      if (line_num <= nrow(ctlBase$Variance_adjustment_list)) {
        ctlBase$Variance_adjustment_list$value[line_num] <- 
          ctlBase$Variance_adjustment_list$value[line_num] * multiplier
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
  
  # NEW GENERIC FUNCTIONALITY - ADDED ON TOP
  
  # Generic parameter table adjustments (Q_parms, size_selex_parms, age_selex_parms, etc.)
  generic_param_tables <- c("Q_parms", "size_selex_parms", "age_selex_parms", 
                            "dirichlet_parms", "tag_parms", "morph_indexing")
  
  for (table_name in generic_param_tables) {
    config_key <- paste0(tolower(table_name))  # e.g., "q_parms", "size_selex_parms"
    
    if (!is.null(config[[config_key]]) && !is.null(ctlBase[[table_name]])) {
      
      for (param_name in names(config[[config_key]])) {
        param_settings <- config[[config_key]][[param_name]]
        
        param_row <- which(rownames(ctlBase[[table_name]]) == param_name)
        
        if (length(param_row) > 0) {
          # Apply ALL settings provided for this parameter
          for (setting_name in names(param_settings)) {
            if (setting_name %in% colnames(ctlBase[[table_name]])) {
              ctlBase[[table_name]][param_row, setting_name] <- param_settings[[setting_name]]
            }
          }
        }
      }
    }
  }
  
  # Generic direct value adjustments (for non-dataframe SS3 elements)
  direct_adjustments <- c("Nfleets", "Nsurveys", "N_areas", "Nages", 
                          "spawn_month", "Ngenders", "Nfishfleets", "Nsurveys", 
                          "Do_AgeKey", "N_platoon", "N_GP", "recr_dist_method",
                          "recr_global_area", "Fcast_years", "Fcast_selex")
  
  for (direct_param in direct_adjustments) {
    config_key <- tolower(direct_param)
    
    if (!is.null(config[[config_key]]) && !is.null(ctlBase[[direct_param]])) {
      ctlBase[[direct_param]] <- config[[config_key]]
    }
  }
  
  # Generic list-based adjustments (for complex SS3 structures)
  if (!is.null(config$custom_adjustments)) {
    for (adjustment_name in names(config$custom_adjustments)) {
      adjustment_config <- config$custom_adjustments[[adjustment_name]]
      
      # Handle different types of custom adjustments
      if (adjustment_config$type == "direct_value") {
        # Direct value replacement
        if (!is.null(ctlBase[[adjustment_config$target]])) {
          ctlBase[[adjustment_config$target]] <- adjustment_config$value
        }
      } else if (adjustment_config$type == "list_element") {
        # List element modification
        if (!is.null(ctlBase[[adjustment_config$target]])) {
          ctlBase[[adjustment_config$target]][[adjustment_config$element]] <- adjustment_config$value
        }
      } else if (adjustment_config$type == "dataframe_modification") {
        # Custom dataframe modifications
        target_df <- ctlBase[[adjustment_config$target]]
        if (!is.null(target_df) && is.data.frame(target_df)) {
          # Apply row/column specific changes
          if (!is.null(adjustment_config$row_condition) && !is.null(adjustment_config$column)) {
            # Find rows matching condition
            if (adjustment_config$row_condition$type == "rowname") {
              target_rows <- which(rownames(target_df) %in% adjustment_config$row_condition$values)
            } else if (adjustment_config$row_condition$type == "column_value") {
              target_rows <- which(target_df[[adjustment_config$row_condition$column]] %in% adjustment_config$row_condition$values)
            }
            
            if (length(target_rows) > 0) {
              target_df[target_rows, adjustment_config$column] <- adjustment_config$value
              ctlBase[[adjustment_config$target]] <- target_df
            }
          }
        }
      }
    }
  }
  
  return(ctlBase)
}

# Function to create detailed change summary (ENHANCED BUT BACKWARDS COMPATIBLE)
create_change_summary <- function(base_ctl, scenario_ctl) {
  changes <- list()
  
  # EXISTING FUNCTIONALITY - UNCHANGED
  
  # Variance adjustments
  base_var <- base_ctl$Variance_adjustment_list
  scenario_var <- scenario_ctl$Variance_adjustment_list
  
  variance_changes <- list()
  for (i in 1:nrow(base_var)) {
    if (base_var$value[i] != scenario_var$value[i]) {
      variance_changes[[paste0("Line_", i)]] <- list(
        original = base_var$value[i],
        modified = scenario_var$value[i],
        change_type = "Variance_adjustment"
      )
    }
  }
  if (length(variance_changes) > 0) {
    changes$variance_adjustments <- variance_changes
  }
  
  # Parameter offset approach changes
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
  
  # NEW GENERIC FUNCTIONALITY - ADDED ON TOP
  
  # Generic parameter table changes detection
  generic_param_tables <- c("Q_parms", "size_selex_parms", "age_selex_parms", 
                            "dirichlet_parms", "tag_parms", "morph_indexing")
  
  for (table_name in generic_param_tables) {
    if (!is.null(base_ctl[[table_name]]) && !is.null(scenario_ctl[[table_name]])) {
      base_table <- base_ctl[[table_name]]
      scenario_table <- scenario_ctl[[table_name]]
      
      if (is.data.frame(base_table) && is.data.frame(scenario_table)) {
        table_changes <- list()
        
        for (param in rownames(base_table)) {
          for (col in colnames(base_table)) {
            if (!is.na(base_table[param, col]) && !is.na(scenario_table[param, col])) {
              if (base_table[param, col] != scenario_table[param, col]) {
                table_changes[[paste0(param, "_", col)]] <- list(
                  parameter = param,
                  column = col,
                  original = base_table[param, col],
                  modified = scenario_table[param, col],
                  change_type = table_name
                )
              }
            }
          }
        }
        
        if (length(table_changes) > 0) {
          changes[[table_name]] <- table_changes
        }
      }
    }
  }
  
  # Generic direct value changes detection
  direct_adjustments <- c("Nfleets", "Nsurveys", "N_areas", "Nages", 
                          "spawn_month", "Ngenders", "Nfishfleets", "Nsurveys", 
                          "Do_AgeKey", "N_platoon", "N_GP", "recr_dist_method",
                          "recr_global_area", "Fcast_years", "Fcast_selex")
  
  for (direct_param in direct_adjustments) {
    if (!is.null(base_ctl[[direct_param]]) && !is.null(scenario_ctl[[direct_param]])) {
      if (base_ctl[[direct_param]] != scenario_ctl[[direct_param]]) {
        changes[[direct_param]] <- list(
          original = base_ctl[[direct_param]],
          modified = scenario_ctl[[direct_param]],
          change_type = "direct_value"
        )
      }
    }
  }
  
  return(changes)
}

# NEW HELPER FUNCTION: Generic SS3 structure analyzer
analyze_ss3_structure <- function(ctl_object) {
  structure_info <- list()
  
  for (element_name in names(ctl_object)) {
    element <- ctl_object[[element_name]]
    
    if (is.data.frame(element)) {
      structure_info[[element_name]] <- list(
        type = "dataframe",
        dimensions = dim(element),
        columns = colnames(element),
        rownames_present = !is.null(rownames(element)),
        sample_rownames = if(!is.null(rownames(element))) head(rownames(element), 5) else NULL
      )
    } else if (is.list(element)) {
      structure_info[[element_name]] <- list(
        type = "list",
        length = length(element),
        names = names(element)
      )
    } else if (is.vector(element)) {
      structure_info[[element_name]] <- list(
        type = "vector",
        length = length(element),
        class = class(element)
      )
    } else {
      structure_info[[element_name]] <- list(
        type = class(element),
        length = if(is.null(length(element))) 1 else length(element)
      )
    }
  }
  
  return(structure_info)
}

# Function to run SS3 script in scenario folders with real-time monitoring
run_scenario_script <- function(scenario_folder, base_path = ".", ss3_options = "", verbose = TRUE, scenario_number = NULL) {
  
  # Robust path construction to handle double slashes
  full_path <- normalizePath(file.path(base_path, scenario_folder), mustWork = FALSE)
  
  # Check if folder exists
  if (!dir.exists(full_path)) {
    return(list(
      scenario = scenario_folder,
      scenario_number = scenario_number,
      status = "error",
      error = paste("Directory does not exist:", full_path)
    ))
  }
  
  # Set up log file path for real-time monitoring
  log_file <- file.path(full_path, "ss3_run.log")
  
  # Test if we can write to the directory before proceeding
  tryCatch({
    test_file <- file.path(full_path, ".write_test")
    cat("test", file = test_file)
    if (file.exists(test_file)) {
      file.remove(test_file)
    }
  }, error = function(e) {
    return(list(
      scenario = scenario_folder,
      scenario_number = scenario_number,
      status = "error",
      error = paste("Cannot write to directory:", full_path, "- Error:", e$message)
    ))
  })
  
  # Record start time
  start_time <- Sys.time()
  
  # Use tryCatch for log file creation
  tryCatch({
    cat("START:", as.character(start_time), "- Scenario:", scenario_folder, "\n", 
        file = log_file, append = FALSE)
  }, error = function(e) {
    return(list(
      scenario = scenario_folder,
      scenario_number = scenario_number,
      status = "error",
      error = paste("Cannot create log file:", log_file, "- Error:", e$message)
    ))
  })
  
  # Print progress to console with scenario number - START TIME ALWAYS SHOWN
  if (!is.null(scenario_number)) {
    if (verbose) {
      cat(sprintf("[%d] Processing scenario: %s in %s\n", scenario_number, scenario_folder, full_path))
    } else {
      cat(sprintf("[%d] Processing scenario: %s\n", scenario_number, scenario_folder))
    }
    cat(sprintf("[%d] Start time: %s\n", scenario_number, as.character(start_time)))
  } else {
    if (verbose) {
      cat("Processing scenario:", scenario_folder, "in", full_path, "\n")
    } else {
      cat("Processing scenario:", scenario_folder, "\n")
    }
    cat("Start time:", as.character(start_time), "\n")
  }
  
  # Save original working directory
  original_dir <- getwd()
  
  tryCatch({
    # Change to scenario folder
    setwd(full_path)
    
    # Create bash script with enhanced logging for real-time monitoring
    script_content <- sprintf('#!/bin/bash

echo "=== Scenario: $(basename $(pwd)) ===" | tee -a ss3_run.log
echo "Start time: $(date)" | tee -a ss3_run.log
echo "Current working directory: $(pwd)" | tee -a ss3_run.log
echo "SS3 options: %s" | tee -a ss3_run.log

# Check if SS3 exists
if [ ! -f "ss3" ]; then
    echo "ERROR: SS3 executable not found" | tee -a ss3_run.log
    exit 1
fi

# Make SS3 executable
chmod 755 ss3

# Run SS3 with real-time output logging
echo "Starting SS3 execution..." | tee -a ss3_run.log
./ss3 %s 2>&1 | tee -a ss3_run.log

# Record completion
echo "SS3 execution completed at: $(date)" | tee -a ss3_run.log
echo "Exit code: $?" | tee -a ss3_run.log
', ss3_options, ss3_options)
    
    # Create temporary script file
    tmp_script <- tempfile(pattern = paste0("scenario_", basename(scenario_folder), "_"), fileext = ".sh")
    writeLines(script_content, tmp_script)
    Sys.chmod(tmp_script, mode = "0755")
    
    # Execute script with optional real-time output to console
    if (verbose) {
      if (!is.null(scenario_number)) {
        cat(sprintf("[%d] === SS3 Execution Output ===\n", scenario_number))
      } else {
        cat("=== SS3 Execution Output ===\n")
      }
      
      # Open connection to script and read output line by line
      conn <- pipe(paste("bash", tmp_script), open = "r")
      all_output <- character()
      
      # Read and display output in real-time
      while (length(line <- readLines(conn, n = 1, warn = FALSE)) > 0) {
        cat(line, "\n")  # Display in R console immediately
        all_output <- c(all_output, line)
      }
      
      # Close connection and get exit code
      exit_code <- close(conn)
      
      if (!is.null(scenario_number)) {
        cat(sprintf("[%d] === SS3 Execution Complete ===\n", scenario_number))
      } else {
        cat("=== SS3 Execution Complete ===\n")
      }
      
    } else {
      # Silent execution - just run the script without real-time output
      result <- system(tmp_script, intern = TRUE)
      exit_code <- attr(result, "status")
      if (is.null(exit_code)) exit_code <- 0
      all_output <- result
    }
    
    # Record completion time and duration
    end_time <- Sys.time()
    duration <- difftime(end_time, start_time, units = "mins")
    
    cat("COMPLETE:", as.character(end_time), "- Duration:", 
        round(as.numeric(duration), 2), "minutes\n", 
        file = log_file, append = TRUE)
    
    # Print completion info to console with scenario number - END TIME ALWAYS SHOWN
    if (!is.null(scenario_number)) {
      if (verbose) {
        cat(sprintf("[%d] Scenario %s completed successfully\n", scenario_number, scenario_folder))
      } else {
        cat(sprintf("[%d] Scenario %s: %s\n", scenario_number, scenario_folder, 
                    if(exit_code == 0) "SUCCESS" else "FAILED"))
      }
      cat(sprintf("[%d] End time: %s\n", scenario_number, as.character(end_time)))
      cat(sprintf("[%d] Duration: %.2f minutes\n", scenario_number, round(as.numeric(duration), 2)))
    } else {
      if (verbose) {
        cat("Scenario", scenario_folder, "completed successfully\n")
      } else {
        cat("Scenario", scenario_folder, ":", 
            if(exit_code == 0) "SUCCESS" else "FAILED", "\n")
      }
      cat("End time:", as.character(end_time), "\n")
      cat("Duration:", round(as.numeric(duration), 2), "minutes\n")
    }
    
    return(list(
      scenario = scenario_folder,
      scenario_number = scenario_number,
      status = if(exit_code == 0) "success" else "error",
      output = all_output,
      path = full_path,
      duration = duration,
      start_time = start_time,
      end_time = end_time,
      exit_code = exit_code
    ))
    
  }, error = function(e) {
    # Log error with timestamp - with additional error handling
    error_time <- Sys.time()
    
    tryCatch({
      cat("ERROR:", as.character(error_time), "- Scenario:", scenario_folder, 
          "- Error:", e$message, "\n", file = log_file, append = TRUE)
    }, error = function(log_error) {
      cat("Could not write to log file:", log_error$message, "\n")
    })
    
    # Print error info to console with scenario number - ERROR TIME ALWAYS SHOWN
    if (!is.null(scenario_number)) {
      if (verbose) {
        cat(sprintf("[%d] Scenario %s failed: %s\n", scenario_number, scenario_folder, e$message))
      } else {
        cat(sprintf("[%d] Scenario %s: FAILED - %s\n", scenario_number, scenario_folder, e$message))
      }
      cat(sprintf("[%d] Error time: %s\n", scenario_number, as.character(error_time)))
    } else {
      if (verbose) {
        cat("Scenario", scenario_folder, "failed:", e$message, "\n")
      } else {
        cat("Scenario", scenario_folder, ": FAILED -", e$message, "\n")
      }
      cat("Error time:", as.character(error_time), "\n")
    }
    
    return(list(
      scenario = scenario_folder,
      scenario_number = scenario_number,
      status = "error",
      error = e$message,
      path = full_path,
      error_time = error_time
    ))
  }, finally = {
    # Return to original directory
    setwd(original_dir)
  })
}
