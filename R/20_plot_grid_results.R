# Plot grid results

# library(r4ss)
library(dplyr)
library(ggplot2)
# library(tidyr)
# library(viridis)
# library(gridExtra)

source("utilities_r4ss.R")

rds_dir <- "../rds"     # model results in r4ss format, saved as rds files
plot_dir <- "../plots"
dir.create(plot_dir, showWarnings=FALSE)

model_files <- dir(rds_dir, full=TRUE)
model_1 <- readRDS(model_files[1])
model_2 <- readRDS(model_files[2])
model <- model_1
model_list <- list(model_one=model_1, model_two=model_2)
v <- "SB"

# Convergence
data.frame
unname(unlist(sapply(model_list, `[`, "log_det_hessian")))
sapply(model_list, `[`, "maximum_gradient_component")

# Create ensemble
ensemble <- create_ensemble(model_list)

# Plots
plot_vars <- c("SB_SBmsy", "F_Fmsy", "SB", "F", "Rec", "SB_SBF0")

for(v in plot_vars)
{
  cat("Creating ribbon plot for variable:", v, "\n")
  # Determine reference line and y-label
  ref_line <- NULL
  y_label <- v
  if(v == "SB_SBmsy") {
    ref_line <- 1
    y_label <- "SB/SBmsy"
  } else if(v == "F_Fmsy") {
    ref_line <- 1
    y_label <- "F/Fmsy"
  } else if(v == "SB_SBF0") {
    y_label <- "SB/SB(F=0)"
  }
  p <- ribbon_plot(ensemble$tseries, v, ref_line=ref_line, y_label=y_label)
  ggsave(file.path(plot_dir, paste0("Ribbon_", v, ".png")), p,
         width=12, height=8, dpi=300)
}

# Create comprehensive combined ribbon plot
cat("Creating combined ribbon plot\n")
combined_vars <- c("SB_SBmsy", "F_Fmsy", "SB", "SB_SBF0")
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
