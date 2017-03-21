# Sara Williams
# 3/8/2017
# Whale ship strike risk simulation - PLOTS
#   Have to load script: sim_once_to_start.R before running this one
################################################################################

#  Load packages
library(plyr)
library(dplyr)
llibrary(gridExtra)
library(ggthemes)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
################################################################################


#### PLOTS ####

#  Plot initial spatial situation
ras <- probrast
plot(ras)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#5BBCD6") #xlim=c(381000, 470000), ylim = c(645000, 6545000))
#plot(s, add = TRUE)    
plot(ras, add= TRUE, legend.plot = FALSE)
plot(s_big, col = "#9C964A", add = TRUE)
plot(init_locs, pch = 16, col = "#B40F20", add = TRUE)
plot(start_up_bay_xy, add = TRUE)
plot(end_up_bay_xy, add = TRUE)
plot(one_track_up, add = TRUE)
plot(start_down_bay_xy, add = TRUE)
plot(end_down_bay_xy, add = TRUE)
plot(one_track_down, add = TRUE)

#  Plot examples of whale movemements
all_whale_pth <- ggplot() + 
                             geom_point(data = df_XYwhales_up, alpha= 0.5, aes(x = X_whale, y = Y_whale, group = whale_ind_num, colour = factor(whale_ind_num))) + 
                                                  #colour = whale_ind_num, fill = whale_ind_num, size = 1) +
                             coord_equal() +
                             xlab("X (km)") +
                             ylab("Y (km)") +
                             #xlim(426573.5,426585) +
                             #ylim(6461830, 6461843) +
                             theme_bw() +
                              theme(legend.position="none") +
                             theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(2)), 
                             axis.title = element_text(size = rel(2)))
all_whale_pth

one_whale_pth <- ggplot() + 
                               geom_point(data = df_XYwhales_up, alpha= 0.5, aes(x = X_whale, y = Y_whale, group = whale_ind_num, colour = factor(whale_ind_num))) + 
                                                    #colour = whale_ind_num, fill = whale_ind_num, size = 1) +
                               coord_equal() +
                               xlab("X (km)") +
                               ylab("Y (km)") +
                               xlim((df_XY[1,3]-10), (df_XY[1,3]+10)) +
                               ylim((df_XY[1,4]-10), (df_XY[1,4]+10)) +
                               theme_bw() +
                               theme(legend.position="none") +
                               theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(2)), 
                               axis.title = element_text(size = rel(2)))
one_whale_pth




#### Simulate 20 whales, each individually, then see how many many surfacings it is in the ship route buffer?
#### Different whale placement every time? I could rerun prediction model (1 itieration) to get slightly different probabilities?
#### same movement parameters for each whale, or based on behavior?

#### If whales move out of study area, let them keep moving around - they can come back in our stay out.
reach_1_strike_50m_df <- as.data.frame(reach_1_strike_50m) %>%
                                             dplyr::rename(strikes = reach_1_strike_50m)
reach_1_strike_50m_p <- ggplot(data=reach_1_strike_50m_df, aes(reach_1_strike_50m_df$strikes)) +
                                           geom_density(size = 1.25) +
                                           geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                                           xlab("\n Number of iterations to reach first encounter at 50m") +
                                           ylab("Density \n") +
                                           #xlim(c(0, 5)) +
                                           #ylim(c(0, 2.5)) +
                                           theme_bw() +
                                           theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                                           axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                                           #theme(legend.position="none")
reach_1_strike_50m_p



#################################################################################
#################################################################################