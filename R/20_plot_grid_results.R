# Plot grid results

library(dplyr)      # %>%
library(ggplot2)    # ggplot
library(gridExtra)  # grid.arrange

source("utilities_r4ss.R")

plot_dir <- "../plots"
dir.create(plot_dir, showWarnings=FALSE)

model_list <- readRDS("../model_list/model_list.rds")

# Convergence
conv <- data.frame(Model=names(model_list))
conv$Gradient <- unlist(sapply(model_list, `[`, "maximum_gradient_component"))
conv$log_det_hessian <- unlist(sapply(model_list, `[`, "log_det_hessian"))
conv$PDH <- conv$log_det_hessian > 1e-6  # safer than > 0.0 floating point

# Create ensemble
ensemble <- create_ensemble(model_list)

# Reference point table
rtab <- round(t(sapply(ensemble$refpts[-1], mean_and_quantiles)), 2)
rtab <- rtab[c("Clatest", "SBlatest", "SBrecent", "TBlatest", "TBrecent",
               "Flatest", "Frecent", "SBmsy", "MSY", "Fmsy", "Frecent_Fmsy",
               "Flatest_Fmsy", "SBrecent_SBmsy", "SBlatest_SBmsy",
               "SBrecent_SBF0", "SBlatest_SBF0"),]

# Grid axes
ensemble_info <- list(tseries=model_info(ensemble$tseries),
                      refpts=model_info(ensemble$refpts))
par(mfrow=c(2,3))
sapply(head(names(ensemble_info$refpts)), bplot, "Frecent_Fmsy", ylim=c(0, 0.7))
sapply(head(names(ensemble_info$refpts)), bplot, "SBrecent", div=1000,
       ylim=c(0, 120))

# Single ribbon plots
plot_vars <- c("SB_SBmsy", "F_Fmsy", "SB", "F", "Rec", "SB_SBF0")
for(v in plot_vars) {
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
  p <- ribbon_plot(ensemble$tseries, v, ref_line=ref_line, y_label=y_label, y_min=0)
  ggsave(file.path(plot_dir, paste0("Ribbon_", v, ".png")), p,
         width=12, height=8, dpi=300)
}

# Multipanel ribbon plot
cat("Creating multipanel ribbon plot\n")
combined_vars <- c("SB_SBmsy", "F_Fmsy", "SB", "SB_SBF0")
combined_plots <- list()
for(v in combined_vars) {
  ref_line <- if(v %in% c("SB_SBmsy", "F_Fmsy")) 1 else NULL
  y_label <- switch(v,
                    "SB_SBmsy" = "SB/SBmsy",
                    "F_Fmsy" = "F/Fmsy",
                    "SB_SBF0" = "SB/SB(F=0)",
                    v)
  p <- ribbon_plot(ensemble$tseries, v, ref_line=ref_line, y_label=y_label, y_min=0) +
    theme(axis.title.x = element_blank())  # no x-axis title
  combined_plots[[v]] <- p
}
combined_plot <- do.call(grid.arrange, c(combined_plots, ncol=2))
ggsave(file.path(plot_dir, "Combined_Ribbon_Plots.png"),
       combined_plot, width=16, height=12, dpi=300)

# Kobe plot
cat("Creating Kobe plot\n")
p <- kobe_plot(ensemble$refpts)
ggsave(file.path(plot_dir, "Kobe_Plot_Overall.png"),
       p, width=12, height=10, dpi=300)
