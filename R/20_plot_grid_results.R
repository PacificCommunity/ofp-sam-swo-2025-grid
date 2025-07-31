# Plot grid results

library(r4ss)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(gridExtra)

rds_dir <- "../rds"     # model results in r4ss format, saved as rds files
plot_dir <- "../plots"
dir.create(plot_dir, showWarnings=FALSE)

model_files <- dir(rds_dir, full=TRUE)
model <- readRDS(model_files[1])
model_list <- list(model_one=model, model_two=model)

#===============================================================================
# REFERENCE POINTS EXTRACTION FUNCTIONS
#===============================================================================

extract_reference_points <- function(model)
{
  annual <- model$annual_time_series
  derived <- model$derived_quants
  dynamic <- model$Dynamic_Bzero[model$Dynamic_Bzero$Era == "TIME",]

  Clatest <- tail(annual$dead_catch_B_an, 1)
  Fmsy <- derived$Value[derived$Label == "annF_MSY"]
  Frecent <- mean(derived$Value[derived$Label %in% paste0("F_", 2019:2022)])
  MSY <- derived$Value[derived$Label == "Dead_Catch_MSY"]
  SB0 <- derived$Value[derived$Label == "SSB_Virgin"]
  SBF0 <- mean(dynamic$SSB_nofishing[dynamic$Yr %in% 2013:2022])
  SBlatest <- derived$Value[derived$Label == "SSB_2023"]
  SBmsy <- derived$Value[derived$Label == "SSB_MSY"]
  SBrecent <- mean(derived$Value[derived$Label %in% paste0("SSB_", 2020:2023)])
  Frecent_Fmsy <- Frecent / Fmsy
  SBlatest_SB0 <- SBlatest / SB0
  SBlatest_SBF0 <- SBlatest / SBF0
  SBlatest_SBmsy <- SBlatest / SBmsy
  SBrecent_SB0 <- SBrecent / SB0
  SBrecent_SBF0 <- SBrecent / SBF0
  SBrecent_SBmsy <- SBrecent / SBmsy

  ref_points <-
    list(Clatest=Clatest,
         Fmsy=Fmsy,
         Frecent=Frecent,
         MSY=MSY,
         SB0=SB0,
         SBF0=SBF0,
         SBlatest=SBlatest,
         SBmsy=SBmsy,
         SBrecent=SBrecent,
         Frecent_Fmsy=Frecent_Fmsy,
         SBlatest_SB0=SBlatest_SB0,
         SBlatest_SBF0=SBlatest_SBF0,
         SBlatest_SBmsy=SBlatest_SBmsy,
         SBrecent_SB0=SBrecent_SB0,
         SBrecent_SBF0=SBrecent_SBF0,
         SBrecent_SBmsy=SBrecent_SBmsy)

  ref_points
}

#===============================================================================
# ENSEMBLE DATA CREATION FUNCTIONS
#===============================================================================

create_ensemble <- function(model_list)
{
  tseries_list <- list()
  refpt_list <- list()

  for(i in seq_along(model_list))
  {
    model <- model_list[[i]]
    run_name <- names(model_list)[i]

    # Extract reference points
    ref_points <- extract_reference_points(model)
    refpt_list[[run_name]] <- data.frame(Run=run_name, ref_points)

    # Get model results
    annual <- model$annual_time_series[model$annual_time_series$Era == "TIME",]
    dynamic <- model$Dynamic_Bzero[model$Dynamic_Bzero$Era == "TIME",]

    # Extract time series
    tseries <- data.frame(Run=run_name, Year=annual$year)
    tseries$F <- annual$"F=Z-M"
    tseries$Rec <- annual$recruits
    tseries$SB <- annual$SSB

    # Calculate ratio time series
    tseries$F_Fmsy <- tseries$F / ref_points$Fmsy
    tseries$SB_SBF0 <- tseries$SB / dynamic$SSB_nofishing
    tseries$SB_SBmsy <- tseries$SB / ref_points$SBmsy

    tseries_list[[i]] <- tseries
  }

  # Combine data
  timeseries <- do.call(rbind, tseries_list)
  ref_points <- do.call(rbind, refpt_list)
  row.names(ref_points) <- NULL

  list(timeseries=timeseries, ref_points=ref_points)
}

#==============================================================================
# RIBBON PLOT FUNCTIONS
#==============================================================================

ribbon_plot <- function(kb_data, variable, title_suffix = "",
                        reference_line = NULL, y_label = NULL) {

  if(!variable %in% names(kb_data)) {
    warning("Variable ", variable, " not found in data")
    return(NULL)
  }

  # Calculate quantiles by year
  ribbon_data <- kb_data %>%
    group_by(year) %>%
    summarise(
      median = median(.data[[variable]], na.rm = TRUE),
      q25 = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      q10 = quantile(.data[[variable]], 0.10, na.rm = TRUE),
      q90 = quantile(.data[[variable]], 0.90, na.rm = TRUE),
      q05 = quantile(.data[[variable]], 0.05, na.rm = TRUE),
      q95 = quantile(.data[[variable]], 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  # Create plot
  p <- ggplot(ribbon_data, aes(x = year)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2, fill = "blue") +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.3, fill = "blue") +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4, fill = "blue") +
    geom_line(aes(y = median), color = "darkblue", size = 1.2) +
    labs(
      title = paste("Ensemble Ribbon Plot:", variable, title_suffix),
      x = "Year",
      y = if(is.null(y_label)) variable else y_label,
      caption = "Ribbons: 5-95% (light), 10-90% (medium), 25-75% (dark); Line: median"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )

  # Add reference line if specified
  if(!is.null(reference_line)) {
    p <- p + geom_hline(yintercept = reference_line, linetype = "dashed",
                        color = "red", size = 1)
  }

  return(p)
}

#==============================================================================
# KOBE PLOT FUNCTIONS
#==============================================================================

kobe_plot <- function(ref_points_list, highlight_recent = TRUE) {

  # Extract F/Fmsy and SB/SBmsy for each model
  kobe_data <- data.frame(
    Model = names(ref_points_list),
    F_Fmsy_recent = sapply(ref_points_list, function(x) {
      if("Frecent_Fmsy" %in% names(x)) x[["Frecent_Fmsy"]] else NA
    }),
    SB_SBmsy_recent = sapply(ref_points_list, function(x) {
      if("SBrecent_SBmsy" %in% names(x)) x[["SBrecent_SBmsy"]] else NA
    }),
    F_Fmsy_latest = sapply(ref_points_list, function(x) {
      if("Flatest_Fmsy" %in% names(x)) x[["Flatest_Fmsy"]] else NA
    }),
    SB_SBmsy_latest = sapply(ref_points_list, function(x) {
      if("SBlatest_SBmsy" %in% names(x)) x[["SBlatest_SBmsy"]] else NA
    })
  )

  # Remove rows with missing data
  kobe_data <- kobe_data[!is.na(kobe_data$F_Fmsy_recent) &
                         !is.na(kobe_data$SB_SBmsy_recent), ]

  if(nrow(kobe_data) == 0) {
    warning("No valid data for Kobe plot")
    return(NULL)
  }

  # Create Kobe plot
  p <- ggplot(kobe_data, aes(x = SB_SBmsy_recent, y = F_Fmsy_recent)) +
    # Add colored quadrants
    annotate("rect", xmin = 0, xmax = 1, ymin = 1, ymax = Inf,
             fill = "red", alpha = 0.3) +
    annotate("rect", xmin = 1, xmax = Inf, ymin = 1, ymax = Inf,
             fill = "yellow", alpha = 0.3) +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "orange", alpha = 0.3) +
    annotate("rect", xmin = 1, xmax = Inf, ymin = 0, ymax = 1,
             fill = "green", alpha = 0.3) +
    # Add reference lines
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +
    # Add points
    geom_point(aes(color = "Recent (2019-2022 & 2020-2023)"),
               size = 3, alpha = 0.7) +
    scale_color_manual(values = c("Recent (2019-2022 & 2020-2023)" = "darkblue")) +
    labs(
      title = "Kobe Plot: Stock Status Across Ensemble Models",
      x = "SB/SBmsy (Recent 2020-2023)",
      y = "F/Fmsy (Recent 2019-2022)",
      color = "Time Period",
      caption = "Green: sustainable; Yellow: overfishing; Orange: overfished; Red: overfishing & overfished"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(xlim = c(0, max(3, max(kobe_data$SB_SBmsy_recent, na.rm = TRUE))),
                    ylim = c(0, max(3, max(kobe_data$F_Fmsy_recent, na.rm = TRUE))))

  # Add latest points if available
  if(highlight_recent && !all(is.na(kobe_data$F_Fmsy_latest)) &&
     !all(is.na(kobe_data$SB_SBmsy_latest))) {
    p <- p + geom_point(aes(x = SB_SBmsy_latest, y = F_Fmsy_latest,
                            color = "Latest (2022-2023)"),
                        size = 4, alpha = 0.8, shape = 17) +
      scale_color_manual(values = c("Recent (2019-2022 & 2020-2023)" = "darkblue",
                                    "Latest (2022-2023)" = "red"))
  }

  return(p)
}

#==============================================================================
# RECRUITMENT ANALYSIS FUNCTIONS
#==============================================================================

recruitment_analysis <- function(kb_data) {

  if(!"Rec" %in% names(kb_data)) {
    warning("Recruitment data not available")
    return(NULL)
  }

  # Calculate recruitment statistics by model
  recr_stats <- kb_data %>%
    group_by(run) %>%
    summarise(
      mean_recr = mean(Rec, na.rm = TRUE),
      sd_recr = sd(Rec, na.rm = TRUE),
      cv_recr = sd_recr / mean_recr,
      min_recr = min(Rec, na.rm = TRUE),
      max_recr = max(Rec, na.rm = TRUE),
      recent_recr = mean(Rec[year >= 2015], na.rm = TRUE),
      .groups = "drop"
    )

  # Create recruitment ribbon plot
  p1 <- create_ribbon_plot(kb_data, "Rec",
                           title_suffix = "Variability Across Models",
                           y_label = "Recruitment")

  # Create recruitment CV plot
  p2 <- ggplot(recr_stats, aes(x = reorder(run, cv_recr), y = cv_recr)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(
      title = "Recruitment Coefficient of Variation by Model",
      x = "Model",
      y = "CV of Recruitment"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

  # Create recent vs historical recruitment comparison
  recr_comparison <- kb_data %>%
    mutate(
      period = case_when(
        year < 2000 ~ "Historical (pre-2000)",
        year >= 2000 & year < 2015 ~ "Recent (2000-2014)",
        year >= 2015 ~ "Latest (2015+)",
        TRUE ~ "Other"
      )
    ) %>%
    filter(period != "Other") %>%
    group_by(run, period) %>%
    summarise(mean_recr = mean(Rec, na.rm = TRUE), .groups = "drop")

  p3 <- ggplot(recr_comparison, aes(x = period, y = mean_recr, fill = period)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_viridis_d() +
    labs(
      title = "Recruitment by Time Period Across Models",
      x = "Time Period",
      y = "Mean Recruitment",
      fill = "Period"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  return(list(
    ribbon = p1,
    cv_plot = p2,
    period_comparison = p3,
    stats = recr_stats
  ))
}

#==============================================================================
# FILTERED MODEL DETECTION AND READING
#==============================================================================

cat("\n=== SCANNING FOR FILTERED MODEL DIRECTORIES ===\n")
cat("Applying filters:", FILTER_PATTERN_1, "AND", FILTER_PATTERN_2, "\n")

# Get all directories and apply filters
all_available_dirs <- list.dirs(model_stem, recursive = FALSE, full.names = FALSE)
all_available_dirs <- all_available_dirs[all_available_dirs != ""]

filtered_dirs <- all_available_dirs  # Needs separate filter step that adds e-3 and PDH into directory names

# filtered_dirs <- all_available_dirs[
#   grepl(FILTER_PATTERN_1, all_available_dirs, ignore.case = TRUE) &
#   grepl(FILTER_PATTERN_2, all_available_dirs, ignore.case = TRUE)
# ]

# cat("Filtered from", length(all_available_dirs), "to", length(filtered_dirs), "directories\n")

if(length(filtered_dirs) == 0) {
  stop("No directories found matching filters: ", FILTER_PATTERN_1, " AND ", FILTER_PATTERN_2)
}

# Validate SS output files
valid_dirs <- character()
valid_paths <- character()

cat("\n=== VALIDATING SS OUTPUT FILES ===\n")
for(dir_name in filtered_dirs) {
  dir_path <- file.path(model_stem, dir_name)

  has_report <- file.exists(file.path(dir_path, "Report.sso"))
  has_par <- file.exists(file.path(dir_path, "ss.par"))

  if(has_report || has_par) {
    valid_dirs <- c(valid_dirs, dir_name)
    valid_paths <- c(valid_paths, dir_path)
    if(VERBOSE_OUTPUT) cat("✓ Valid SS model:", dir_name, "\n")
  } else {
    if(VERBOSE_OUTPUT) cat("✗ No SS files:", dir_name, "\n")
  }
}

cat("Found", length(valid_dirs), "valid SS models\n")

if(length(valid_dirs) == 0) {
  stop("No valid SS models found after filtering!")
}

existing_all_dirs <- valid_dirs
existing_full_paths <- valid_paths

#==============================================================================
# READ MODELS AND CHECK CONVERGENCE
#==============================================================================

cat("\n=== READING SS MODELS AND CHECKING CONVERGENCE ===\n")

model_list <- list()
successful_reads <- character()
convergence_info <- data.frame(
  Model = character(),
  Converged = logical(),
  Hessian_Positive = logical(),
  Max_Gradient = numeric(),
  Convergence_Level = numeric(),
  Reason = character(),
  stringsAsFactors = FALSE
)

# Convergence checking function
check_convergence <- function(model, model_name) {
  converged <- FALSE
  hess_positive <- FALSE
  max_grad <- NA
  convergence_level <- NA
  reason <- "Unknown"

  tryCatch({
    # Get convergence level from Report.sso
    if("inputs" %in% names(model) && "dir" %in% names(model$inputs)) {
      report_path <- file.path(model$inputs$dir, "Report.sso")
      if(file.exists(report_path)) {
        report_lines <- readLines(report_path, n = 50, warn = FALSE)
        for(line in report_lines) {
          if(grepl("^Convergence_Level:", line)) {
            conv_match <- regmatches(line, regexpr("(?<=Convergence_Level: )[0-9.e-]+", line, perl = TRUE))
            if(length(conv_match) > 0) {
              convergence_level <- as.numeric(conv_match)
              break
            }
          }
        }
      }
    }

    # Check hessian
    if("covar" %in% names(model) && !is.null(model$covar)) {
      hess_positive <- TRUE
    } else {
      covar_file <- file.path(model$inputs$dir, "covar.sso")
      hess_positive <- file.exists(covar_file)
    }

    # Determine convergence
    if(hess_positive && !is.na(convergence_level)) {
      if(convergence_level <= 1e-4) {
        converged <- TRUE
        reason <- paste("Excellent convergence (", scientific(convergence_level, 2), ")", sep = "")
      } else if(convergence_level <= 1e-3) {
        converged <- TRUE
        reason <- paste("Good convergence (", scientific(convergence_level, 2), ")", sep = "")
      } else if(convergence_level <= 1e-2) {
        converged <- !CONVERGENCE_FILTER
        reason <- paste("Moderate convergence (", scientific(convergence_level, 2), ")", sep = "")
      } else {
        converged <- FALSE
        reason <- paste("Poor convergence (", scientific(convergence_level, 2), ")", sep = "")
      }
    } else if(hess_positive) {
      converged <- TRUE
      reason <- "Positive hessian"
    } else {
      reason <- "No positive hessian"
    }

  }, error = function(e) {
    reason <- paste("Error:", e$message)
  })

  return(list(
    converged = converged,
    hess_positive = hess_positive,
    max_grad = max_grad,
    convergence_level = convergence_level,
    reason = reason
  ))
}

# Read each model
for(i in seq_along(existing_all_dirs)) {
  cat("Reading model", i, "of", length(existing_all_dirs), ":", existing_all_dirs[i], "\n")

  tryCatch({
    model <- SS_output(existing_full_paths[i], verbose = FALSE, printstats = FALSE, covar = TRUE)
    conv_check <- check_convergence(model, existing_all_dirs[i])

    # Add to convergence info
    convergence_info <- rbind(convergence_info, data.frame(
                                                  Model = existing_all_dirs[i],
                                                  Converged = conv_check$converged,
                                                  Hessian_Positive = conv_check$hess_positive,
                                                  Max_Gradient = conv_check$max_grad,
                                                  Convergence_Level = conv_check$convergence_level,
                                                  Reason = conv_check$reason,
                                                  stringsAsFactors = FALSE
                                                ))

    # Include model based on criteria
    include_model <- conv_check$hess_positive
    if(CONVERGENCE_FILTER && !is.na(conv_check$convergence_level)) {
      include_model <- include_model && (conv_check$convergence_level <= MAX_CONVERGENCE_LEVEL)
    }

    if(include_model) {
      model_list[[length(model_list) + 1]] <- model
      successful_reads <- c(successful_reads, existing_all_dirs[i])
      cat("  ✓ INCLUDED -", conv_check$reason, "\n")
    } else {
      cat("  ✗ EXCLUDED -", conv_check$reason, "\n")
    }

  }, error = function(e) {
    cat("  ✗ Error reading:", e$message, "\n")
  })
}

names(model_list) <- successful_reads

cat("\n=== FILTERING SUMMARY ===\n")
cat("Models meeting criteria:", length(model_list), "out of", length(existing_all_dirs), "\n")

if(length(model_list) == 0) {
  stop("No models passed filtering criteria!")
}

#==============================================================================
# CREATE ENSEMBLE DATA WITH REFERENCE POINTS
#==============================================================================

# Include your enhanced ensemble creation functions here
# (All the functions from the previous artifacts: extract_reference_points,
#  create_ensemble_with_reference_points, etc.)

cat("\n=== CREATING ENSEMBLE DATA WITH REFERENCE POINTS ===\n")

# Create ensemble data with reference points
ensemble_results <- create_ensemble_with_reference_points(model_list)
kb_data <- ensemble_results$timeseries
ref_points_list <- ensemble_results$reference_points

# Check data structure
cat("Ensemble data created with", nrow(kb_data), "rows\n")
cat("Variables available:", names(kb_data), "\n")
cat("Year range:", min(kb_data$year), "to", max(kb_data$year), "\n")
cat("Models included:", length(unique(kb_data$run)), "\n")

# Create reference points table
ref_table <- create_reference_points_table(ref_points_list)
cat("Reference points extracted for", length(ref_points_list), "models\n")

#==============================================================================
# CREATE ESSENTIAL ENSEMBLE PLOTS
#==============================================================================

cat("\n=== CREATING ESSENTIAL ENSEMBLE PLOTS ===\n")

# Create individual ribbon plots for each variable
plot_vars <- c("SB_SBmsy", "F_Fmsy", "SSB", "F", "Rec", "SB_SBF0")
plot_vars <- plot_vars[plot_vars %in% names(kb_data)]

for(var in plot_vars) {
  cat("Creating ribbon plot for variable:", var, "\n")

  # Determine reference line and y-label
  ref_line <- NULL
  y_label <- var

  if(var == "SB_SBmsy") {
    ref_line <- 1
    y_label <- "SB/SBmsy"
  } else if(var == "F_Fmsy") {
    ref_line <- 1
    y_label <- "F/Fmsy"
  } else if(var == "SB_SBF0") {
    y_label <- "SB/SB(F=0)"
  }

  p <- create_ribbon_plot(kb_data, var,
                          reference_line = ref_line,
                          y_label = y_label)

  if(!is.null(p)) {
    ggsave(file.path(plot_dir, paste0("Ribbon_", var, ".png")),
           plot = p, width = 12, height = 8, dpi = 300)
  }
}

# Create comprehensive combined ribbon plot
cat("Creating combined ribbon plot\n")
combined_vars <- c("SB_SBmsy", "F_Fmsy", "SSB", "SB_SBF0")
combined_vars <- combined_vars[combined_vars %in% names(kb_data)]

if(length(combined_vars) >= 4) {
  combined_plots <- list()

  for(var in combined_vars) {
    ref_line <- if(var %in% c("SB_SBmsy", "F_Fmsy")) 1 else NULL
    y_label <- switch(var,
                      "SB_SBmsy" = "SB/SBmsy",
                      "F_Fmsy" = "F/Fmsy",
                      "SB_SBF0" = "SB/SB(F=0)",
                      var)

    p <- create_ribbon_plot(kb_data, var, reference_line = ref_line, y_label = y_label) +
      theme(axis.title.x = element_blank()) # Remove x-axis title for grid
    combined_plots[[var]] <- p
  }

  # Arrange plots in grid
  combined_plot <- do.call(grid.arrange, c(combined_plots, ncol = 2))
  ggsave(file.path(plot_dir, "Combined_Ribbon_Plots.png"),
         plot = combined_plot, width = 16, height = 12, dpi = 300)
}

# Create ONLY the overall Kobe plot
cat("Creating overall Kobe plot\n")
kobe_plot <- create_kobe_plot(ref_points_list)
if(!is.null(kobe_plot)) {
  ggsave(file.path(plot_dir, "Kobe_Plot_Overall.png"),
         plot = kobe_plot, width = 12, height = 10, dpi = 300)
}

# Create recruitment analysis
cat("Creating recruitment analysis\n")
recruitment_analysis <- create_recruitment_analysis(kb_data)
if(!is.null(recruitment_analysis)) {
  ggsave(file.path(plot_dir, "Recruitment_Ribbon.png"),
         plot = recruitment_analysis$ribbon, width = 12, height = 8, dpi = 300)
  ggsave(file.path(plot_dir, "Recruitment_CV.png"),
         plot = recruitment_analysis$cv_plot, width = 10, height = 8, dpi = 300)
  ggsave(file.path(plot_dir, "Recruitment_Periods.png"),
         plot = recruitment_analysis$period_comparison, width = 10, height = 6, dpi = 300)

  # Save recruitment statistics
  write.csv(recruitment_analysis$stats,
            file.path(plot_dir, "Recruitment_Statistics.csv"), row.names = FALSE)
}

#==============================================================================
# CREATE ENHANCED SUMMARY TABLE
#==============================================================================

cat("\n=== CREATING ENHANCED SUMMARY TABLE ===\n")

# Create enhanced summary with reference points and convergence info
enhanced_summary <- merge(convergence_info, ref_table, by = "Model", all.x = TRUE)

# Add stock status categories
enhanced_summary$Stock_Status <- with(enhanced_summary, {
  ifelse(is.na(SBrecent_SBmsy) | is.na(Frecent_Fmsy), "Unknown",
  ifelse(SBrecent_SBmsy >= 1 & Frecent_Fmsy <= 1, "Healthy",
  ifelse(SBrecent_SBmsy >= 1 & Frecent_Fmsy > 1, "Overfishing",
  ifelse(SBrecent_SBmsy < 1 & Frecent_Fmsy <= 1, "Overfished",
         "Overfished_and_Overfishing"))))
})

# Save enhanced summary
write.csv(enhanced_summary, file.path(plot_dir, "Enhanced_Model_Summary.csv"), row.names = FALSE)
write.csv(ref_table, file.path(plot_dir, "Reference_Points_Summary.csv"), row.names = FALSE)

# Print stock status summary
cat("\n=== STOCK STATUS SUMMARY ===\n")
status_counts <- table(enhanced_summary$Stock_Status)
cat("Stock Status Distribution:\n")
print(status_counts)

# Print reference points summary statistics
cat("\nReference Points Summary (across all models):\n")
ref_numeric <- enhanced_summary[, sapply(enhanced_summary, is.numeric)]
key_vars <- c("SBrecent_SBmsy", "SBlatest_SBmsy", "Frecent_Fmsy", "Flatest_Fmsy")

for(var in key_vars) {
  if(var %in% names(ref_numeric)) {
    values <- ref_numeric[[var]][!is.na(ref_numeric[[var]])]
    if(length(values) > 0) {
      cat(sprintf("%s: Mean=%.3f, Median=%.3f, Range=[%.3f, %.3f]\n",
                  var, mean(values), median(values), min(values), max(values)))
    }
  }
}

#==============================================================================
# CREATE EXECUTIVE SUMMARY
#==============================================================================

cat("\n=== CREATING EXECUTIVE SUMMARY ===\n")

executive_summary <- list()

# Stock status summary
if(nrow(ref_table) > 0) {
  stock_status_summary <- ref_table %>%
    summarise(
      n_models = n(),
      n_with_F_data = sum(!is.na(Frecent_Fmsy)),
      n_with_SB_data = sum(!is.na(SBrecent_SBmsy)),
      n_sustainable = sum(Frecent_Fmsy <= 1 & SBrecent_SBmsy >= 1, na.rm = TRUE),
      n_overfishing = sum(Frecent_Fmsy > 1 & SBrecent_SBmsy >= 1, na.rm = TRUE),
      n_overfished = sum(Frecent_Fmsy <= 1 & SBrecent_SBmsy < 1, na.rm = TRUE),
      n_both_issues = sum(Frecent_Fmsy > 1 & SBrecent_SBmsy < 1, na.rm = TRUE),
      median_F_Fmsy = median(Frecent_Fmsy, na.rm = TRUE),
      median_SB_SBmsy = median(SBrecent_SBmsy, na.rm = TRUE),
      range_F_Fmsy = paste0("[", round(min(Frecent_Fmsy, na.rm = TRUE), 2),
                            ", ", round(max(Frecent_Fmsy, na.rm = TRUE), 2), "]"),
      range_SB_SBmsy = paste0("[", round(min(SBrecent_SBmsy, na.rm = TRUE), 2),
                              ", ", round(max(SBrecent_SBmsy, na.rm = TRUE), 2), "]")
    )

  executive_summary$stock_status <- stock_status_summary
}

# Model uncertainty summary
if("SB_SBmsy" %in% names(kb_data)) {
  recent_uncertainty <- kb_data %>%
    filter(year >= 2020) %>%
    group_by(year) %>%
    summarise(
      cv_sb_sbmsy = sd(SB_SBmsy, na.rm = TRUE) / mean(SB_SBmsy, na.rm = TRUE),
      cv_f_fmsy = if("F_Fmsy" %in% names(kb_data)) sd(F_Fmsy, na.rm = TRUE) / mean(F_Fmsy, na.rm = TRUE) else NA,
      .groups = "drop"
    ) %>%
    summarise(
      avg_cv_sb = mean(cv_sb_sbmsy, na.rm = TRUE),
      avg_cv_f = mean(cv_f_fmsy, na.rm = TRUE)
    )

  executive_summary$uncertainty <- recent_uncertainty
}

# Save executive summary
executive_summary_df <- data.frame(
  Metric = c(
    "Total Models Analyzed",
    "Models with F/Fmsy Data",
    "Models with SB/SBmsy Data",
    "Models Showing Sustainable Status",
    "Models Showing Overfishing",
    "Models Showing Overfished",
    "Models with Both Issues",
    "Median F/Fmsy (Recent)",
    "Median SB/SBmsy (Recent)",
    "F/Fmsy Range",
    "SB/SBmsy Range",
    "Average CV for SB/SBmsy (2020+)",
    "Average CV for F/Fmsy (2020+)"
  ),
  Value = c(
    if(!is.null(executive_summary$stock_status)) {
      with(executive_summary$stock_status, c(
                                             n_models, n_with_F_data, n_with_SB_data, n_sustainable,
                                             n_overfishing, n_overfished, n_both_issues,
                                             round(median_F_Fmsy, 3), round(median_SB_SBmsy, 3),
                                             range_F_Fmsy, range_SB_SBmsy
                                           ))
    } else rep(NA, 11),
    if(!is.null(executive_summary$uncertainty)) {
      with(executive_summary$uncertainty, c(
                                            round(avg_cv_sb, 3), round(avg_cv_f, 3)
                                          ))
    } else rep(NA, 2)
  )
)

write.csv(executive_summary_df, file.path(plot_dir, "Executive_Summary.csv"), row.names = FALSE)

#==============================================================================
# SAVE FILTERING AND CONVERGENCE RESULTS
#==============================================================================

# Save filtering summary
filtering_summary <- data.frame(
  Step = c("Total directories", "Name filtered", "SS files present", "Convergence passed", "Final models"),
  Count = c(length(all_available_dirs), length(filtered_dirs), length(valid_dirs),
            sum(convergence_info$Converged), length(model_list)),
  Filters_Applied = c("None", paste(FILTER_PATTERN_1, "&", FILTER_PATTERN_2),
                      "SS output files", "Convergence criteria", "All filters")
)

write.csv(filtering_summary, file.path(plot_dir, "Filtering_Summary.csv"), row.names = FALSE)
write.csv(convergence_info, file.path(plot_dir, "Convergence_Details.csv"), row.names = FALSE)

#==============================================================================
# FINAL SUMMARY AND RESULTS
#==============================================================================

cat("\n🎉 SWO STOCK ASSESSMENT ENSEMBLE ANALYSIS COMPLETE! 🎉\n")
cat("All results saved to:", plot_dir, "\n")

# List all created files
all_files <- list.files(plot_dir, full.names = FALSE)
png_files <- all_files[grepl("\\.png$", all_files)]
csv_files <- all_files[grepl("\\.csv$", all_files)]

cat("\nPlots created (", length(png_files), " files):\n")
cat(paste("  -", png_files, collapse = "\n"))

cat("\n\nData files created (", length(csv_files), " files):\n")
cat(paste("  -", csv_files, collapse = "\n"))

# Print executive summary to console
cat("\n=== EXECUTIVE SUMMARY ===\n")
if(!is.null(executive_summary$stock_status)) {
  with(executive_summary$stock_status, {
    cat(sprintf("• %d models analyzed with complete reference point data\n", n_with_F_data))
    cat(sprintf("• %.0f%% of models indicate sustainable fishing (F≤Fmsy, SB≥SBmsy)\n",
                100 * n_sustainable / n_with_F_data))
    cat(sprintf("• Median stock status: F/Fmsy = %.2f, SB/SBmsy = %.2f\n",
                median_F_Fmsy, median_SB_SBmsy))

    if(median_F_Fmsy <= 1 & median_SB_SBmsy >= 1) {
      cat("• Overall ensemble indicates SUSTAINABLE fishing\n")
    } else if(median_F_Fmsy > 1 & median_SB_SBmsy >= 1) {
      cat("• Overall ensemble indicates OVERFISHING\n")
    } else if(median_F_Fmsy <= 1 & median_SB_SBmsy < 1) {
      cat("• Overall ensemble indicates OVERFISHED stock\n")
    } else {
      cat("• Overall ensemble indicates OVERFISHED stock with OVERFISHING\n")
    }
  })
}

if(!is.null(executive_summary$uncertainty)) {
  with(executive_summary$uncertainty, {
    cat(sprintf("• Model uncertainty (CV): SB/SBmsy = %.1f%%, F/Fmsy = %.1f%%\n",
                100 * avg_cv_sb, 100 * avg_cv_f))

    if(avg_cv_sb < 0.2) {
      cat("• Low uncertainty in stock biomass estimates\n")
    } else if(avg_cv_sb < 0.4) {
      cat("• Moderate uncertainty in stock biomass estimates\n")
    } else {
      cat("• High uncertainty in stock biomass estimates\n")
    }
  })
}

cat("\nKey plots to review:\n")
cat("  1. Kobe_Plot_Overall.png - Overall stock status\n")
cat("  2. Ribbon_SB_SBmsy.png - Stock biomass trajectory\n")
cat("  3. Ribbon_F_Fmsy.png - Fishing mortality trajectory\n")
cat("  4. Ribbon_SB_SBF0.png - Depletion trajectory\n")
cat("  5. Combined_Ribbon_Plots.png - All key metrics together\n")
cat("  6. Recruitment_Ribbon.png - Recruitment variability\n")
cat("  7. Executive_Summary.csv - Key metrics summary\n")

cat("\nConfiguration used:\n")
cat("  Grid folder:", model_stem, "\n")
cat("  Filters:", FILTER_PATTERN_1, "&", FILTER_PATTERN_2, "\n")
cat("  Convergence filter:", ifelse(CONVERGENCE_FILTER, paste("≤", MAX_CONVERGENCE_LEVEL), "OFF"), "\n")
cat("  Models analyzed:", length(model_list), "\n")
cat("  Output folder:", plot_dir, "\n")

# Open results folder
if(.Platform$OS.type == "windows") {
  shell.exec(plot_dir)
} else {
  system(paste("open", plot_dir))  # macOS
}
