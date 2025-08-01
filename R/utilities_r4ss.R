extract_refpts <- function(model)
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

create_ensemble <- function(model_list)
{
  tseries_list <- list()
  refpts_list <- list()

  for(i in seq_along(model_list))
  {
    model <- model_list[[i]]
    run_name <- names(model_list)[i]

    # Extract reference points
    rp <- extract_refpts(model)
    refpts_list[[run_name]] <- data.frame(Model=run_name, rp)

    # Get model results
    annual <- model$annual_time_series[model$annual_time_series$Era == "TIME",]
    dynamic <- model$Dynamic_Bzero[model$Dynamic_Bzero$Era == "TIME",]

    # Extract time series
    ts <- data.frame(Model=run_name, Year=annual$year)
    ts$F <- annual$"F=Z-M"
    ts$Rec <- annual$recruits
    ts$SB <- annual$SSB

    # Calculate ratio time series
    ts$F_Fmsy <- ts$F / rp$Fmsy
    ts$SB_SBF0 <- ts$SB / dynamic$SSB_nofishing
    ts$SB_SBmsy <- ts$SB / rp$SBmsy

    tseries_list[[i]] <- ts
  }

  # Combine data
  tseries <- do.call(rbind, tseries_list)
  refpts <- do.call(rbind, refpts_list)
  row.names(refpts) <- NULL

  list(tseries=tseries, refpts=refpts)
}

ribbon_plot <- function(kb_data, variable, title_suffix = "",
                        ref_line = NULL, y_label = NULL) {

  if(!variable %in% names(kb_data)) {
    warning("Variable ", variable, " not found in data")
    return(NULL)
  }

  # Calculate quantiles by year
  ribbon_data <- kb_data %>%
    group_by(Year) %>%
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
  p <- ggplot(ribbon_data, aes(x = Year)) +
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
  if(!is.null(ref_line)) {
    p <- p + geom_hline(yintercept = ref_line, linetype = "dashed",
                        color = "red", size = 1)
  }

  return(p)
}

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
      recent_recr = mean(Rec[Year >= 2015], na.rm = TRUE),
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
        Year < 2000 ~ "Historical (pre-2000)",
        Year >= 2000 & Year < 2015 ~ "Recent (2000-2014)",
        Year >= 2015 ~ "Latest (2015+)",
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
