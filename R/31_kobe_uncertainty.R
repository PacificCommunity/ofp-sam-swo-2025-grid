require(ggplot2)
require(MASS)
require(dplyr)
require(ggdist)
require(ggdensity)
library(dplyr)

source("30_estimation_uncertainty.R")


draws_comb<-bind_rows(draws, .id = "Grid")


# Usage examples
p1 <- create_generic_kobe_plot(draws_comb, 
                                x_label = bquote(SB[recent]/SB[MSY]),
                                y_label = bquote(F[recent]/F[MSY]),
                                point_alpha=0.04,
                                hdr_legend_title = "HDR",
                                perc_decimal_places = 2, 
                                point_size = 1,  
                                #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                scenario_label = "All combined")

 print(p1)
 
 
 
 # ------------------------ Generic HDR Kobe Plot Function ------------------------
 create_generic_kobe_plot <- function(data, 
                                      x_var = "BBmsy", 
                                      y_var = "FFmsy",
                                      x_ref = 1, 
                                      y_ref = 1,
                                      x_label = NULL,
                                      y_label = NULL,
                                      scenario_label = NULL,
                                      xlim_val = c(0, 6), 
                                      ylim_val = c(0, 2),
                                      show_xlab = TRUE, 
                                      show_ylab = TRUE,
                                      quadrant_labels = c("Healthy", "Overfished", "Overfishing", "Critical"),
                                      quadrant_colors = c("#51C87A", "#F7D154", "#FFA500", "#DC143C"),
                                      show_points = TRUE,
                                      show_legend = TRUE,
                                      point_alpha = 0.12,
                                      point_size = 0.6,
                                      point_color = "grey40",
                                      hdr_colors = c("#FDE725", "#35B779", "#31688E", "#440154"),
                                      hdr_alphas = c(0.5, 0.6, 0.7, 0.8),
                                      hdr_legend_title = "HDR",
                                      perc_decimal_places = 3) {

   
   # Set default labels
   if (is.null(x_label)) x_label <- x_var
   if (is.null(y_label)) y_label <- y_var
   
   # Calculate quadrant percentages
   quadrant_stats <- data %>%
     rename(x_axis = !!sym(x_var), y_axis = !!sym(y_var)) %>%
     mutate(region = case_when(
       x_axis > x_ref & y_axis <= y_ref ~ quadrant_labels[1],  # Healthy
       x_axis <= x_ref & y_axis <= y_ref ~ quadrant_labels[2], # Overfished
       x_axis > x_ref & y_axis > y_ref ~ quadrant_labels[3],   # Overfishing
       TRUE ~ quadrant_labels[4]                               # Critical
     )) %>%
     count(region) %>%
     mutate(perc = sprintf(paste0("%.", perc_decimal_places, "f%%"), 100 * n / sum(n)))
   
   # Ensure all regions exist
   all_regions <- quadrant_labels
   quadrant_stats <- data.frame(region = all_regions) %>%
     left_join(quadrant_stats, by = "region") %>%
     mutate(n = ifelse(is.na(n), 0, n),
            perc = ifelse(is.na(perc), paste0("0.", paste(rep("0", perc_decimal_places), collapse = ""), "%"), perc))
   
   # Calculate dimensions
   max_x <- xlim_val[2]
   max_y <- ylim_val[2]
   extended_max_x <- max_x * (if(show_legend) 1.25 else 1.05)
   
   # Create plot
   p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
     
     # Quadrant backgrounds
     annotate("rect", xmin = x_ref, xmax = extended_max_x, ymin = 0, ymax = y_ref, 
              fill = quadrant_colors[1], alpha = 0.35) +
     annotate("rect", xmin = 0, xmax = x_ref, ymin = 0, ymax = y_ref, 
              fill = quadrant_colors[2], alpha = 0.30) +
     annotate("rect", xmin = x_ref, xmax = extended_max_x, ymin = y_ref, ymax = max_y, 
              fill = quadrant_colors[3], alpha = 0.30) +
     annotate("rect", xmin = 0, xmax = x_ref, ymin = y_ref, ymax = max_y, 
              fill = quadrant_colors[4], alpha = 0.28) +
     
     # Legend area background
     {if(show_legend) annotate("rect", xmin = max_x*1.02, xmax = extended_max_x, ymin = 0, ymax = max_y, 
                               fill = "white", alpha = 0.95) else NULL} +
     
     # Data points
     {if(show_points) geom_point(alpha = point_alpha, size = point_size, color = point_color, show.legend = FALSE) else NULL} +
     
     # HDR contours
     geom_hdr(method = "kde", probs = 0.99, alpha = hdr_alphas[1], fill = hdr_colors[1], show.legend = FALSE, 
              xlim = c(-1, max_x*2), ylim = c(-1, max_y*2)) +
     geom_hdr(method = "kde", probs = 0.95, alpha = hdr_alphas[2], fill = hdr_colors[2], show.legend = FALSE, 
              xlim = c(-1, max_x*2), ylim = c(-1, max_y*2)) +
     geom_hdr(method = "kde", probs = 0.80, alpha = hdr_alphas[3], fill = hdr_colors[3], show.legend = FALSE, 
              xlim = c(-1, max_x*2), ylim = c(-1, max_y*2)) +
     geom_hdr(method = "kde", probs = 0.50, alpha = hdr_alphas[4], fill = hdr_colors[4], show.legend = FALSE, 
              xlim = c(-1, max_x*2), ylim = c(-1, max_y*2)) +
     
     # Reference lines
     geom_vline(xintercept = x_ref, linetype = "dashed", color = "black", size = 0.8) +
     geom_hline(yintercept = y_ref, linetype = "dashed", color = "black", size = 0.8) +
     
     # Percentage blocks
     annotate("rect", xmin = 0, xmax = max_x/4, ymin = max_y*0.85, ymax = max_y*0.95, 
              fill = quadrant_colors[2], alpha = 0.8) +
     annotate("rect", xmin = max_x/4, xmax = max_x/2, ymin = max_y*0.85, ymax = max_y*0.95, 
              fill = quadrant_colors[1], alpha = 0.8) +
     annotate("rect", xmin = max_x/2, xmax = max_x*3/4, ymin = max_y*0.85, ymax = max_y*0.95, 
              fill = quadrant_colors[4], alpha = 0.8) +
     annotate("rect", xmin = max_x*3/4, xmax = max_x, ymin = max_y*0.85, ymax = max_y*0.95, 
              fill = quadrant_colors[3], alpha = 0.8) +
     
     # Percentage text
     annotate("text", x = max_x/8, y = max_y*0.9, 
              label = quadrant_stats$perc[quadrant_stats$region==quadrant_labels[2]], 
              size = 3.5, fontface = "bold", color = "black") +
     annotate("text", x = max_x*3/8, y = max_y*0.9, 
              label = quadrant_stats$perc[quadrant_stats$region==quadrant_labels[1]], 
              size = 3.5, fontface = "bold", color = "black") +
     annotate("text", x = max_x*5/8, y = max_y*0.9, 
              label = quadrant_stats$perc[quadrant_stats$region==quadrant_labels[4]], 
              size = 3.5, fontface = "bold", color = "black") +
     annotate("text", x = max_x*7/8, y = max_y*0.9, 
              label = quadrant_stats$perc[quadrant_stats$region==quadrant_labels[3]], 
              size = 3.5, fontface = "bold", color = "black") +
     
     # HDR legend
     {if(show_legend) list(
       annotate("rect", xmin = max_x*1.04, xmax = max_x*1.22, ymin = max_y*0.45, ymax = max_y*0.8, 
                fill = "white", color = "black", linewidth = 0.5),
       annotate("text", x = max_x*1.13, y = max_y*0.75, label = hdr_legend_title, size = 4, fontface = "bold", hjust = 0.5),
       annotate("rect", xmin = max_x*1.06, xmax = max_x*1.09, ymin = max_y*0.68, ymax = max_y*0.71, 
                fill = hdr_colors[1], alpha = 0.9),
       annotate("text", x = max_x*1.105, y = max_y*0.695, label = "99%", size = 3.5, hjust = 0),
       annotate("rect", xmin = max_x*1.06, xmax = max_x*1.09, ymin = max_y*0.62, ymax = max_y*0.65, 
                fill = hdr_colors[2], alpha = 0.9),
       annotate("text", x = max_x*1.105, y = max_y*0.635, label = "95%", size = 3.5, hjust = 0),
       annotate("rect", xmin = max_x*1.06, xmax = max_x*1.09, ymin = max_y*0.56, ymax = max_y*0.59, 
                fill = hdr_colors[3], alpha = 0.9),
       annotate("text", x = max_x*1.105, y = max_y*0.575, label = "80%", size = 3.5, hjust = 0),
       annotate("rect", xmin = max_x*1.06, xmax = max_x*1.09, ymin = max_y*0.50, ymax = max_y*0.53, 
                fill = hdr_colors[4], alpha = 0.9),
       annotate("text", x = max_x*1.105, y = max_y*0.515, label = "50%", size = 3.5, hjust = 0)
     ) else NULL} +
     
     # Coordinate system
     coord_cartesian(xlim = c(0, extended_max_x), ylim = ylim_val, expand = FALSE, clip = "on") +
     scale_x_continuous(limits = c(0, extended_max_x), expand = c(0,0), breaks = pretty(xlim_val)) +
     scale_y_continuous(limits = ylim_val, expand = c(0,0), 
                        sec.axis = if(!is.null(scenario_label)) 
                          sec_axis(~., name = scenario_label, breaks = NULL, labels = NULL) else waiver()) +
     
     # Labels
     labs(x = if(show_xlab) x_label else "", y = if(show_ylab) y_label else "") +
     theme(
       axis.title.x = element_text(size = 10), 
       axis.title.y = element_text(size = 10, margin = margin(r = 5)),
       axis.title.y.right = element_text(size = 13, margin = margin(l = 15), face = "bold", angle = 270),
       axis.text.x = element_text(size = 10), 
       axis.text.y = element_text(size = 10),
       panel.grid.minor = element_blank(), 
       panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
       plot.margin = margin(t = 5, r = 20, b = 3, l = 3, "pt"),
       axis.ticks.x = element_line(), 
       axis.ticks.y = element_line(),
       axis.ticks.length.x = unit(0.1, "cm"), 
       axis.ticks.length.y = unit(0.1, "cm"),
       legend.position = "none",
       panel.grid.major = element_line(color = "grey90", size = 0.3)
     )
   
   return(p)
 }
 
