

library(here)
require(ggplot2)
require(MASS)
require(dplyr)
require(ggdist)
require(ggdensity)
library(dplyr)
library(patchwork)
library(dplyr)

here::here()

source("30_estimation_uncertainty.R")



###############
## steepness ##
###############



draws_h07 <- draws_comb %>% 
  filter(grepl("h07", Grid))

draws_h08 <- draws_comb %>% 
  filter(grepl("h08", Grid))

draws_h09 <- draws_comb %>% 
  filter(grepl("h09", Grid))

draws_comb<-bind_rows(draws, .id = "Grid")


 h07 <- create_generic_kobe_plot(draws_h07, 
                                x_label = bquote(SB[recent]/SB[MSY]),
                                y_label = bquote(F[recent]/F[MSY]),
                                point_alpha=0.04,
                                hdr_legend_title = "HDR",
                                perc_decimal_places = 2, 
                                point_size = 1,  
                                #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                scenario_label = "h=0.7")
 
 
 h08 <- create_generic_kobe_plot(draws_h08, 
                                 x_label = bquote(SB[recent]/SB[MSY]),
                                 y_label = bquote(F[recent]/F[MSY]),
                                 point_alpha=0.04,
                                 hdr_legend_title = "HDR",
                                 perc_decimal_places = 2, 
                                 point_size = 1,  
                                 #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                 scenario_label = "h=0.8")
 
 h09 <- create_generic_kobe_plot(draws_h09, 
                                 x_label = bquote(SB[recent]/SB[MSY]),
                                 y_label = bquote(F[recent]/F[MSY]),
                                 point_alpha=0.04,
                                 hdr_legend_title = "HDR",
                                 perc_decimal_places = 2, 
                                 point_size = 1,  
                                 #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                 scenario_label = "h=0.9")
 
 
 hall <- create_generic_kobe_plot(draws_comb, 
                                x_label = bquote(SB[recent]/SB[MSY]),
                                y_label = bquote(F[recent]/F[MSY]),
                                point_alpha=0.04,
                                hdr_legend_title = "HDR",
                                perc_decimal_places = 2, 
                                point_size = 1,  
                                #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                scenario_label = "All combined")
 
 
 (h07|h08)/(h09|hall)
 
 
 

 ###############
 ## weighting ##
 ###############
 
 
 draws_Base <- draws_comb %>% 
   filter(grepl("Dbas", Grid))
  
 draws_D05l <- draws_comb %>% 
   filter(grepl("D05l", Grid))
 
 draws_D05w <- draws_comb %>% 
   filter(grepl("D05w", Grid))
 
 draws_D20l <- draws_comb %>% 
   filter(grepl("D20l", Grid))
 
 draws_D20w <- draws_comb %>% 
   filter(grepl("D20w", Grid))
 
 draws_Base <- draws_comb %>% 
   filter(grepl("Dbas", Grid))
 

 
 
 
 
 
 
 
 
 Dbase <- create_generic_kobe_plot(draws_Base, 
                                 x_label = bquote(SB[recent]/SB[MSY]),
                                 y_label = bquote(F[recent]/F[MSY]),
                                 point_alpha=0.04,
                                 hdr_legend_title = "HDR",
                                 perc_decimal_places = 2, 
                                 point_size = 1,  
                                 #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                 scenario_label = "Base")
 
 
 D05l <- create_generic_kobe_plot(draws_D05l, 
                                 x_label = bquote(SB[recent]/SB[MSY]),
                                 y_label = bquote(F[recent]/F[MSY]),
                                 point_alpha=0.04,
                                 hdr_legend_title = "HDR",
                                 perc_decimal_places = 2, 
                                 point_size = 1,  
                                 #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                 scenario_label = "0.5Length")
 
 D05w <- create_generic_kobe_plot(draws_D05w, 
                                 x_label = bquote(SB[recent]/SB[MSY]),
                                 y_label = bquote(F[recent]/F[MSY]),
                                 point_alpha=0.04,
                                 hdr_legend_title = "HDR",
                                 perc_decimal_places = 2, 
                                 point_size = 1,  
                                 #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                 scenario_label = "0.5Weight")
 
 
 D20l <- create_generic_kobe_plot(draws_D20l, 
                                  x_label = bquote(SB[recent]/SB[MSY]),
                                  y_label = bquote(F[recent]/F[MSY]),
                                  point_alpha=0.04,
                                  hdr_legend_title = "HDR",
                                  perc_decimal_places = 2, 
                                  point_size = 1,  
                                  #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                  scenario_label = "2Length")
 
 
 
 D20w <- create_generic_kobe_plot(draws_D20w, 
                                  x_label = bquote(SB[recent]/SB[MSY]),
                                  y_label = bquote(F[recent]/F[MSY]),
                                  point_alpha=0.04,
                                  hdr_legend_title = "HDR",
                                  perc_decimal_places = 2, 
                                  point_size = 1,  
                                  #hdr_colors = c("#000004", "#7B1FA2", "#FCA636", "#FCFDBF"),
                                  scenario_label = "2Weight")
 
 
 (hall|Dbase)/(D05l|D20l)/(D05w|D20w)
 
 
 
 
 
 
 
 
 
 
 
 
 
 