extract_refpts <- function(model) {
  annual <- model$annual_time_series
  derived <- model$derived_quants
  dynamic <- model$Dynamic_Bzero[model$Dynamic_Bzero$Era == "TIME",]

  Clatest <- annual$dead_catch_B_an[annual$year == 2023]
  Flatest <- annual$"F=Z-M"[annual$year == 2022]
  Fmsy <- derived$Value[derived$Label == "annF_MSY"]
  Frecent <- mean(annual$"F=Z-M"[annual$year %in% 2019:2022])
  MSY <- derived$Value[derived$Label == "Dead_Catch_MSY"]
  SB0 <- derived$Value[derived$Label == "SSB_Virgin"]
  SBF0 <- mean(dynamic$SSB_nofishing[dynamic$Yr %in% 2013:2022])
  SBlatest <- derived$Value[derived$Label == "SSB_2023"]
  SBmsy <- derived$Value[derived$Label == "SSB_MSY"]
  SBrecent <- mean(derived$Value[derived$Label %in% paste0("SSB_", 2020:2023)])
  TBlatest <- annual$Bio_all_an[annual$year == 2023]
  TBrecent <- mean(annual$Bio_all_an[annual$year %in% 2020:2023])
  Flatest_Fmsy <- Flatest / Fmsy
  Frecent_Fmsy <- Frecent / Fmsy
  SBlatest_SB0 <- SBlatest / SB0
  SBlatest_SBF0 <- SBlatest / SBF0
  SBlatest_SBmsy <- SBlatest / SBmsy
  SBrecent_SB0 <- SBrecent / SB0
  SBrecent_SBF0 <- SBrecent / SBF0
  SBrecent_SBmsy <- SBrecent / SBmsy

  ref_points <-
    list(Clatest=Clatest,
         Flatest=Flatest,
         Fmsy=Fmsy,
         Frecent=Frecent,
         MSY=MSY,
         SB0=SB0,
         SBF0=SBF0,
         SBlatest=SBlatest,
         SBmsy=SBmsy,
         SBrecent=SBrecent,
         TBlatest=TBlatest,
         TBrecent=TBrecent,
         Flatest_Fmsy=Flatest_Fmsy,
         Frecent_Fmsy=Frecent_Fmsy,
         SBlatest_SB0=SBlatest_SB0,
         SBlatest_SBF0=SBlatest_SBF0,
         SBlatest_SBmsy=SBlatest_SBmsy,
         SBrecent_SB0=SBrecent_SB0,
         SBrecent_SBF0=SBrecent_SBF0,
         SBrecent_SBmsy=SBrecent_SBmsy)

  ref_points
}

create_ensemble <- function(model_list) {
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

ribbon_plot <- function(tseries, variable, title_suffix = "",
                        ref_line = NULL, y_label = NULL, y_min = NULL) {
  # Calculate quantiles by year
  ribbon_data <- tseries %>%
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
    ) +
    if(!is.null(y_min)) {
      ylim(0,NA)
    }

  # Add reference line if specified
  if(!is.null(ref_line)) {
    p <- p + geom_hline(yintercept = ref_line, linetype = "dashed",
                        color = "red", size = 1)
  }

  p
}

kobe_plot <- function(refpts, highlight_recent = TRUE) {
  if("refpts" %in% names(refpts))  # if user passed ensemble object
    refpts <- refpts$refpts

  p <- ggplot(refpts, aes(x=SBrecent_SBmsy, y=Frecent_Fmsy)) +
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
    geom_point(aes(color = "Recent"),
               size = 3, alpha = 0.7) +
    scale_color_manual(values = c("Recent" = "darkblue")) +
    labs(
      title = "Kobe Plot: Stock Status Across Ensemble Models",
      x = "SBrecent/SBmsy",
      y = "Frecent/Fmsy",
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
    coord_cartesian(xlim = c(0, max(3, max(refpts$SBrecent_SBmsy, na.rm=TRUE))),
                    ylim = c(0, max(3, max(refpts$Frecent_Fmsy, na.rm=TRUE))))

  # Add latest points if available
  if(highlight_recent && !all(is.na(refpts$Flatest_Fmsy)) &&
     !all(is.na(refpts$SBlatest_SBmsy))) {
    p <- p + geom_point(aes(x = SBlatest_SBmsy, y = Flatest_Fmsy,
                            color = "Latest"),
                        size = 4, alpha = 0.8, shape = 17) +
      scale_color_manual(values = c("Recent" = "darkblue",
                                    "Latest" = "red"))
  }

  p
}

# Add model information to data frame that has a Model column
#
# Before:
# Model                 ...
# N_h07_Rb_Vb_D05l_Mhac ...
#
# After:
# CPUE Steepness RecProp Move  DataWts  Natmort   Model                 ...
# NZ         0.7 Base    Base  05length HamelCope N_h07_Rb_Vb_D05l_Mhac ...
model_info <- function(df)
{
  info <- do.call(rbind, strsplit(df$Model, "_"))
  info <- as.data.frame(info)
  names(info) <- c("CPUE", "Steepness", "RecProp", "Move", "DataWts", "Natmort")
  info$CPUE[info$CPUE == "N"] <- "NZ"
  info$CPUE[info$CPUE == "P"] <- "PICT"
  info$Steepness <- as.numeric(substring(info$Steepness, 2)) / 10
  info$RecProp[info$RecProp == "Rb"] <- "Base"
  info$RecProp[info$RecProp == "R2"] <- "MoreR2"
  info$Move[info$Move == "Vb"] <- "Base"
  info$Move[info$Move == "V1"] <- "Half1to2"
  info$Move[info$Move == "V2"] <- "Half2to1"
  info$DataWts[info$DataWts == "Dbas"] <- "Base"
  info$DataWts[info$DataWts == "D05l"] <- "05length"
  info$DataWts[info$DataWts == "D20l"] <- "20length"
  info$DataWts[info$DataWts == "D05w"] <- "05weight"
  info$DataWts[info$DataWts == "D20w"] <- "20weight"
  info$Natmort[info$Natmort == "Mhac"] <- "HamelCope"
  info$Natmort[info$Natmort == "Mest"] <- "Estimated"
  cbind(info, df)
}

bplot <- function(gridaxis, refpt, df=ensemble_info$refpts, div=1, ...)
{
  df[[refpt]] <- df[[refpt]] / div
  z <- split(df[[refpt]], df[[gridaxis]])
  boxplot(z, main=gridaxis, ...)
  NULL
}
