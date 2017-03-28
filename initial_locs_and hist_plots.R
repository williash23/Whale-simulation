# Sara Williams
# 3/8/2017
# Whale ship strike risk simulation - PLOTS
#   Have to load script: sim_once_to_start.R before running this one
################################################################################

#  Load packages
library(plyr)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
################################################################################


load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_1yr_10m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_1yr_25m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_1yr_50m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_10yr_10m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_10yr_25m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_10yr_50m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_50yr_10m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_50yr_25m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_13k_50yr_50m.RData")


load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_1yr_10m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_1yr_25m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_1yr_50m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_10yr_10m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_10yr_25m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_10yr_50m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_50yr_10m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_50yr_25m.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/all_strike_loc_20k_50yr_50m.RData")


load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_10m_13k_1yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_25m_13k_1yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_50m_13k_1yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_10m_13k_10yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_25m_13k_10yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_50m_13k_10yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_10m_13k_50yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_25m_13k_50yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_50m_13k_50yr.RData")


load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_10m_20k_1yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_25m_20k_1yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_50m_20k_1yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_10m_20k_10yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_25m_20k_10yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_50m_20k_10yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_10m_20k_50yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_25m_20k_50yr.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_50m_20k_50yr.RData")



load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_10m_13k_100rep.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_25m_13k_100rep.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_50m_13k_100rep.RData")


load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_10m_20k_100rep.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_25m_20k_100rep.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_50m_20k_100rep.RData")


#### PLOTS ####


#  Example initial locations 
study_area_locs <- ggplot(s_big_df) + 
                                    geom_polygon(aes(long, lat, group=group, fill=hole), colour = "grey60") + 
                                    geom_path(aes(long,lat, group=group, fill=hole), color="white") +
                                    #geom_polygon(data = s_df, aes(long, lat, group=group, fill = fill), alpha = 0.7) +
                                    #geom_path(data = s_df, aes(long, lat, group=group, fill=fill), color="black", size = 0.75) +
                                    #geom_path(data = track_df, aes(long, lat, group=group), color="black", linetype = 2, size = 0.5) +
                                    scale_fill_manual(values=c("burlywood4", "skyblue3"), guide="none") +
                                    coord_equal() +
                                    geom_point(data = init_locs_df, aes( x = x, y = y), colour = "firebrick3", size = 2) + 
                                    #xlim((all_strike_loc[1,1]-10000), (all_strike_loc[1,1]+10000)) +
                                    #ylim((all_strike_loc[1,2]-10000), (all_strike_loc[1,2]+10000)) +
                                    #xlim(435000, 450000) +
                                    #ylim(6460000, 6480000) +
                                    xlab("\n Easting (UTM)") +
                                    ylab("Northing (UTM) \n") +
                                    theme_bw() +
                                    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(1)), 
                                    axis.title = element_text(size = rel(1.25)), axis.text.x = element_text(angle = 90), 
                                    panel.background = element_rect(fill = "skyblue3"))
study_area_locs

#  Example imovements
one_whale <- dplyr::filter(df_XYwhales_up, whale_ind_num == "V1")

study_area_one_mov <- ggplot(s_big_df) + 
                                        geom_polygon(aes(long, lat, group=group, fill=hole), colour = "grey60") + 
                                        geom_path(aes(long,lat, group=group, fill=hole), color="white") +
                                        #geom_polygon(data = s_df, aes(long, lat, group=group, fill = fill), alpha = 0.7) +
                                        #geom_path(data = s_df, aes(long, lat, group=group, fill=fill), color="black", size = 0.75) +
                                        #geom_path(data = track_df, aes(long, lat, group=group), color="black", linetype = 2, size = 0.5) +
                                        scale_fill_manual(values=c("burlywood4", "skyblue3"), guide="none") +
                                        coord_equal() +
                                        geom_point(data =one_whale, aes(x = X_whale, y = Y_whale), alpha = 0.5, colour = "firebrick3", size = 2) + 
                                        xlim((one_whale[1,2]-10), (one_whale[1,2]+10)) +
                                        ylim((one_whale[1,3]-10), (one_whale[1,3]+10)) +
                                        #xlim(435000, 450000) +
                                        #ylim(6460000, 6480000) +
                                        xlab("\n Easting (UTM)") +
                                        ylab("Northing (UTM) \n") +
                                        theme_bw() +
                                        theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(1)), 
                                        axis.title = element_text(size = rel(1.25)), axis.text.x = element_text(angle = 90), 
                                        panel.background = element_rect(fill = "skyblue3"))
study_area_one_mov



all_strike_loc_13k_1yr_10m_p <- mutate(all_strike_loc_13k_1yr_10m, mod_name = "13k_1yr_10m")
all_strike_loc_13k_1yr_25m_p <- mutate(all_strike_loc_13k_1yr_25m, mod_name = "13k_1yr_25m")
all_strike_loc_13k_1yr_50m_p <- mutate(all_strike_loc_13k_1yr_50m, mod_name = "13k_1yr_50m")
all_strike_loc_13k_10yr_10m_p <- mutate(all_strike_loc_13k_10yr_10m, mod_name = "13k_10yr_10m")
all_strike_loc_13k_10yr_25m_p <- mutate(all_strike_loc_13k_10yr_25m, mod_name = "13k_10yr_25m")
all_strike_loc_13k_10yr_50m_p <- mutate(all_strike_loc_13k_10yr_50m, mod_name = "13k_10yr_50m")
all_strike_loc_13k_50yr_10m_p <- mutate(all_strike_loc_13k_50yr_10m, mod_name = "13k_50yr_10m")
all_strike_loc_13k_50yr_25m_p <- mutate(all_strike_loc_13k_50yr_25m, mod_name = "13k_50yr_25m")
all_strike_loc_13k_50yr_50m_p <- mutate(all_strike_loc_13k_50yr_50m, mod_name = "13k_50yr_50m")


all_strike_loc_20k_1yr_10m_p <- mutate(all_strike_loc_20k_1yr_10m, mod_name = "20k_1yr_10m")
all_strike_loc_20k_1yr_25m_p <- mutate(all_strike_loc_20k_1yr_25m, mod_name = "20k_1yr_25m")
all_strike_loc_20k_1yr_50m_p <- mutate(all_strike_loc_20k_1yr_50m, mod_name = "20k_1yr_50m")
all_strike_loc_20k_10yr_10m_p <- mutate(all_strike_loc_20k_10yr_10m, mod_name = "20k_10yr_10m")
all_strike_loc_20k_10yr_25m_p <- mutate(all_strike_loc_20k_10yr_25m, mod_name = "20k_10yr_25m")
all_strike_loc_20k_10yr_50m_p <- mutate(all_strike_loc_20k_10yr_50m, mod_name = "20k_10yr_50m")
all_strike_loc_20k_50yr_10m_p <- mutate(all_strike_loc_20k_50yr_10m, mod_name = "20k_50yr_10m")
all_strike_loc_20k_50yr_25m_p <- mutate(all_strike_loc_20k_50yr_25m, mod_name = "20k_50yr_25m")
all_strike_loc_20k_50yr_50m_p <- mutate(all_strike_loc_20k_50yr_50m, mod_name = "20k_50yr_50m")



study_area_strikes <- ggplot(s_big_df) + 
                                    geom_polygon(aes(long, lat, group=group, fill=hole), colour = "grey60") + 
                                    geom_path(aes(long,lat, group=group, fill=hole), color="white") +
                                    #geom_polygon(data = s_df, aes(long, lat, group=group, fill = fill), alpha = 0.7) +
                                    #geom_path(data = s_df, aes(long, lat, group=group, fill=fill), color="black", size = 0.75) +
                                    #geom_path(data = track_df, aes(long, lat, group=group), color="black", linetype = 2, size = 0.5) +
                                    scale_fill_manual(values=c("burlywood4", "skyblue3"), guide="none") +
                                    coord_equal() +
                                    geom_point(data = all_strike_loc_20k_50yr_10m_p, alpha= 0.5, size = 3, aes(x = X_whale, y = Y_whale, group = mod_name), colour = "firebrick3") + 
                                    geom_point(data = all_strike_loc_20k_50yr_25m_p, alpha= 0.5, size = 3, aes(x = X_whale, y = Y_whale, group = mod_name), colour = "darkorange1") + 
                                    geom_point(data = all_strike_loc_20k_50yr_50m_p, alpha= 0.5, size = 3, aes(x = X_whale, y = Y_whale, group = mod_name), colour = "goldenrod2") + 
                                    #scale_fill_manual(values = c("red", "green", "blue")) +
                                    #xlim((all_strike_loc[1,1]-10000), (all_strike_loc[1,1]+10000)) +
                                    #ylim((all_strike_loc[1,2]-10000), (all_strike_loc[1,2]+10000)) +
                                    #xlim(435000, 450000) +
                                    #ylim(6460000, 6480000) +
                                    xlab("\n Easting (UTM)") +
                                    ylab("Northing (UTM) \n") +
                                    theme_bw() +
                                    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(1)), 
                                    axis.title = element_text(size = rel(1.25)), axis.text.x = element_text(angle = 90), 
                                    panel.background = element_rect(fill = "skyblue3"))
study_area_strikes





strike_locs <- all_strike_loc_13k_50yr_50m
coordinates(strike_locs) <- ~ X_whale + Y_whale
crs(strike_locs) <- UTM
strike_ras <- rasterize(strike_locs, predictorstack_tmp)
strikegrid <- as.data.frame(strike_ras, xy=TRUE)
colnames(strikegrid)[1] <- "X"
colnames(strikegrid)[2] <- "Y"
colnames(strikegrid)[3] <- "strikes"
strikegrid$off.set <- 1000^2
strikegrid[is.na(strikegrid)] <- 0
s_big_df$hole <- as.factor(s_big_df$hole)





strikes_p <- ggplot(strikegrid) +
                    geom_tile(aes(x=X, y=Y, fill=strikes)) +
                    coord_equal() + 
                    xlab("\n Easting (UTM)") +
                    ylab("Northing (UTM) \n") +
                    scale_fill_gradientn(colours = red_pal(11),
                                                       labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9" , "10"),
                                                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8,  9 ,10),
                                                       guide = guide_legend(title = "Strikes", keywidth = 2, keyheight = 1,
                                                       title.theme = element_text(size = 12, angle = 0), label.theme = element_text(size = 12, angle = 0))) +
                    geom_polygon(data = s_big_df, aes(long, lat, group=group), color = "burlywood4") + 
                    geom_path(data = s_big_df, aes(long, lat, group=group), color="skyblue4") +
                    geom_polygon(data = s_df, aes(long, lat, group=group), alpha = 0.7) +
                    geom_path(data = s_df, aes(long, lat, group=group), color="black", size = 0.75) +
                    scale_fill_discrete(values=c("burlywood4", "skyblue3"), guide="none") +
                    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(1)), 
                    axis.title = element_text(size = rel(1.25)), axis.text.x = element_text(angle = 90), 
                    panel.background = element_rect(fill = "skyblue3"))
strikes_p












sim_strikes_10m_13k_10yr <- as.data.frame(rep_sim_strikes_10m_13k_10yr) %>%
                                                  mutate(mod_name = "10m_13k_10yr") %>%
                                                  dplyr::rename(num_strikes = rep_sim_strikes_10m_13k_10yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_25m_13k_10yr <- as.data.frame(rep_sim_strikes_25m_13k_10yr) %>%
                                                 mutate(mod_name = "25m_13k_10yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_25m_13k_10yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_50m_13k_10yr <- as.data.frame(rep_sim_strikes_50m_13k_10yr) %>%
                                                 mutate(mod_name = "50m_13k_10yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_50m_13k_10yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_10m_13k_50yr <- as.data.frame(rep_sim_strikes_10m_13k_50yr) %>%
                                                 mutate(mod_name = "10m_13k_50yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_10m_13k_50yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_25m_13k_50yr <- as.data.frame(rep_sim_strikes_25m_13k_50yr) %>%
                                                 mutate(mod_name = "25m_13k_50yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_25m_13k_50yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_50m_13k_50yr <- as.data.frame(rep_sim_strikes_50m_13k_50yr) %>%
                                                 mutate(mod_name = "50m_13k_50yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_50m_13k_50yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_10m_13k_1yr <- as.data.frame(rep_sim_strikes_10m_13k_1yr) %>%
                                                  mutate(mod_name = "10m_13k_100yr") %>%
                                                  dplyr::rename(num_strikes = rep_sim_strikes_10m_13k_1yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_25m_13k_1yr <- as.data.frame(rep_sim_strikes_25m_13k_1yr) %>%
                                                   mutate(mod_name = "25m_13k_100yr") %>%
                                                   dplyr::rename(num_strikes = rep_sim_strikes_25m_13k_1yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_50m_13k_1yr <- as.data.frame(rep_sim_strikes_50m_13k_1yr) %>%
                                                   mutate(mod_name = "50m_13k_100yr") %>%
                                                   dplyr::rename(num_strikes = rep_sim_strikes_50m_13k_1yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())




sim_strikes_10m_20k_10yr <- as.data.frame(rep_sim_strikes_10m_20k_10yr) %>%
                                                  mutate(mod_name = "10m_20k_10yr") %>%
                                                  dplyr::rename(num_strikes = rep_sim_strikes_10m_20k_10yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_25m_20k_10yr <- as.data.frame(rep_sim_strikes_25m_20k_10yr) %>%
                                                 mutate(mod_name = "25m_20k_10yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_25m_20k_10yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_50m_20k_10yr <- as.data.frame(rep_sim_strikes_50m_20k_10yr) %>%
                                                 mutate(mod_name = "50m_20k_10yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_50m_20k_10yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_10m_20k_50yr <- as.data.frame(rep_sim_strikes_10m_20k_50yr) %>%
                                                 mutate(mod_name = "10m_20k_50yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_10m_20k_50yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_25m_20k_50yr <- as.data.frame(rep_sim_strikes_25m_20k_50yr) %>%
                                                 mutate(mod_name = "25m_20k_50yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_25m_20k_50yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_50m_20k_50yr <- as.data.frame(rep_sim_strikes_50m_20k_50yr) %>%
                                                 mutate(mod_name = "50m_20k_50yr") %>%
                                                 dplyr::rename(num_strikes = rep_sim_strikes_50m_20k_50yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_10m_20k_1yr <- as.data.frame(rep_sim_strikes_10m_20k_1yr) %>%
                                                  mutate(mod_name = "10m_20k_100yr") %>%
                                                  dplyr::rename(num_strikes = rep_sim_strikes_10m_20k_1yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_25m_20k_1yr <- as.data.frame(rep_sim_strikes_25m_20k_1yr) %>%
                                                   mutate(mod_name = "25m_20k_100yr") %>%
                                                   dplyr::rename(num_strikes = rep_sim_strikes_25m_20k_1yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())
sim_strikes_50m_20k_1yr <- as.data.frame(rep_sim_strikes_50m_20k_1yr) %>%
                                                   mutate(mod_name = "50m_20k_100yr") %>%
                                                   dplyr::rename(num_strikes = rep_sim_strikes_50m_20k_1yr) %>%
                                                  mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                                  mutate(iter_num = 1:n())



cu_num_strikes_p <- ggplot()+
                                   geom_line(data=sim_strikes_10m_20k_1yr, aes(iter_num, cum_num_strikes), colour = "firebrick3", size = 2) +
                                   geom_line(data=sim_strikes_25m_20k_1yr, aes(iter_num, cum_num_strikes), colour = "darkorange1", size = 2) +
                                   geom_line(data=sim_strikes_50m_20k_1yr, aes(iter_num, cum_num_strikes), colour = "goldenrod2", size = 2) +
                                   #geom_path(aes(colour = as.numeric(date)))
                                   #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                                   xlab("\n Year") +
                                   ylab("Cumulative number of strikes \n") +
                                   #xlim(c(0, 5)) +
                                   ylim(c(0, 300)) +
                                   theme_bw() +
                                   theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                                   axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                                   #theme(legend.position="none")
cu_num_strikes_p







#### If whales move out of study area, let them keep moving around - they can come back in our stay out.
reach_1_strike_10m_13k_df <- as.data.frame(reach_1_strike_10m_13k_1000rep) %>%
                                                    dplyr::rename(strikes = reach_1_strike_10m_13k_1000rep) %>%
                                                    arrange(strikes)
reach_1_strike_25m_13k_df <- as.data.frame(reach_1_strike_25m_13k_1000rep) %>%
                                                    dplyr::rename(strikes = reach_1_strike_25m_13k_1000rep) %>%
                                                    arrange(strikes)
reach_1_strike_50m_13k_df <- as.data.frame(reach_1_strike_50m_13k_1000rep) %>%
                                                    dplyr::rename(strikes = reach_1_strike_50m_13k_1000rep) %>%
                                                    arrange(strikes)
reach_1_strike_10m_20k_df <- as.data.frame(reach_1_strike_10m_20k_1000rep) %>%
                                                    dplyr::rename(strikes = reach_1_strike_10m_20k_1000rep) %>%
                                                    arrange(strikes)
reach_1_strike_25m_20k_df <- as.data.frame(reach_1_strike_25m_20k_1000rep) %>%
                                                    dplyr::rename(strikes = reach_1_strike_25m_20k_1000rep) %>%
                                                    arrange(strikes)
reach_1_strike_50m_20k_df <- as.data.frame(reach_1_strike_50m_20k_1000rep) %>%
                                                    dplyr::rename(strikes = reach_1_strike_50m_20k_1000rep) %>%
                                                    arrange(strikes)


reach_1_strike_p <- ggplot() +
                                  geom_density(data=reach_1_strike_10m_20k_df , aes(reach_1_strike_10m_13k_df $strikes), fill = "firebrick3", colour = "firebrick3") +
                                  geom_density(data=reach_1_strike_25m_20k_df , aes(reach_1_strike_25m_13k_df $strikes), fill = "darkorange1", colour = "darkorange1") +
                                  geom_density(data=reach_1_strike_50m_20k_df , aes(reach_1_strike_50m_13k_df $strikes), fill = "goldenrod2", colour = "goldenrod2") +
                                  #geom_point(data = pt1, aes(x = x, y = y), colour="grey90", size = 1.5, shape = 19) +
                                  #geom_density(size = 1.25) +
                                  #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                                  xlab("\n Number of iterations to reach first encounter ") +
                                  ylab("Density \n") +
                                  xlim(c(0, 2749)) +
                                  #ylim(c(0, 2.5)) +
                                  theme_bw() +
                                  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                                  axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                                  #theme(legend.position="none")
reach_1_strike_p

# tst <- dplyr::filter(reach_1_strike_10m_13k_df, strikes > 2749)
# nrow(tst)
# 95
# tst <- dplyr::filter(reach_1_strike_10m_20k_df, strikes > 2749)
# nrow(tst)
# 218

















prop_strikes_10m_13k_10y <- as.data.frame(prop_strikes_per_year_10m_13k_10y) %>%
                                                 mutate(mod_name = "10m_13k_10y") %>%
                                                 dplyr::rename(num_strikes = prop_strikes_per_year_10m_13k_10y)
prop_strikes_25m_13k_10y <- as.data.frame(prop_strikes_per_year_25m_13k_10y) %>%
                                                 mutate(mod_name = "25m_13k_10y") %>%
                                                 dplyr::rename(num_strikes = prop_strikes_per_year_25m_13k_10y)
prop_strikes_50m_13k_10y <- as.data.frame(prop_strikes_per_year_50m_13k_10y) %>%
                                                 mutate(mod_name = "50m_13k_10y") %>%
                                                 dplyr::rename(num_strikes = prop_strikes_per_year_50m_13k_10y)
prop_strikes_10m_13k_50y <- as.data.frame(prop_strikes_per_year_10m_13k_50y) %>%
                                                 mutate(mod_name = "10m_13k_50y") %>%
                                                 dplyr::rename(num_strikes = prop_strikes_per_year_10m_13k_50y)
prop_strikes_25m_13k_50y <- as.data.frame(prop_strikes_per_year_25m_13k_50y) %>%
                                                 mutate(mod_name = "25m_13k_50y") %>%
                                                 dplyr::rename(num_strikes = prop_strikes_per_year_25m_13k_50y)
prop_strikes_50m_13k_50y <- as.data.frame(prop_strikes_per_year_50m_13k_50y) %>%
                                                 mutate(mod_name = "50m_13k_50y") %>%
                                                 dplyr::rename(num_strikes = prop_strikes_per_year_50m_13k_50y)
prop_strikes_10m_13k_100y <- as.data.frame(prop_strikes_per_year_10m_13k_100y) %>%
                                                  mutate(mod_name = "10m_13k_100y") %>%
                                                  dplyr::rename(num_strikes = prop_strikes_per_year_10m_13k_100y)
prop_strikes_25m_13k_100y <- as.data.frame(prop_strikes_per_year_25m_13k_100y) %>%
                                                   mutate(mod_name = "25m_13k_100y") %>%
                                                   dplyr::rename(num_strikes = prop_strikes_per_year_25m_13k_100y)
prop_strikes_50m_13k_100y <- as.data.frame(prop_strikes_per_year_50m_13k_100y) %>%
                                                   mutate(mod_name = "50m_13k_100y") %>%
                                                   dplyr::rename(num_strikes = prop_strikes_per_year_50m_13k_100y)

all_prop_strike <- as.data.frame(bind_rows(prop_strikes_10m_13k_10y, prop_strikes_25m_13k_10y, prop_strikes_50m_13k_10y,
                                                                       prop_strikes_10m_13k_50y, prop_strikes_25m_13k_50y, prop_strikes_50m_13k_50y,
                                                                       prop_strikes_10m_13k_100y, prop_strikes_25m_13k_100y, prop_strikes_50m_13k_100y)) %>%
                             group_by(mod_name) %>%
                             mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                             mutate(iter_num = 1:n()) %>%
                             ungroup() %>%
                             as.data.frame()

all_prop_strike_10y <- as.data.frame(bind_rows(prop_strikes_10m_13k_10y, prop_strikes_25m_13k_10y, prop_strikes_50m_13k_10y)) %>%
                                    group_by(mod_name) %>%
                                    mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                    mutate(iter_num = 1:n()) %>%
                                    ungroup() %>%
                                    as.data.frame()
all_prop_strike_50y <- as.data.frame(bind_rows(prop_strikes_10m_13k_50y, prop_strikes_25m_13k_50y, prop_strikes_50m_13k_50y)) %>%
                                    group_by(mod_name) %>%
                                    mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                    mutate(iter_num = 1:n()) %>%
                                    ungroup() %>%
                                    as.data.frame()
all_prop_strike_100y <- as.data.frame(bind_rows(prop_strikes_10m_13k_100y, prop_strikes_25m_13k_100y, prop_strikes_50m_13k_100y)) %>%
                                      group_by(mod_name) %>%
                                      mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                      mutate(iter_num = 1:n()) %>%
                                      ungroup() %>%
                                      as.data.frame()


prop_strikes_p <- ggplot(data=all_prop_strike, aes(num_strikes, colour = mod_name)) +
                              geom_density(size = 1.25) +
                              #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                              xlab("\n Number of simulated strikes at given distance and time span") +
                              ylab("Density \n") +
                              #xlim(c(0, 5)) +
                              #ylim(c(0, 2.5)) +
                              theme_bw() +
                              theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                              axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                              #theme(legend.position="none")
prop_strikes_p


prop_strikes_10y_p <- ggplot(data=all_prop_strike_10y, aes(iter_num, num_strikes, colour = mod_name)) +
                                           geom_line(size = 2) +
                                           #geom_path(aes(colour = as.numeric(date)))
                                           #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                                            xlab("\n Iteration") +
                                            ylab("Proportion of cruises in a year with encounter \n") +
                                            #xlim(c(0, 5)) +
                                            #ylim(c(0, 50)) +
                                            theme_bw() +
                                            theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                                            axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                                            #theme(legend.position="none")
prop_strikes_10y_p

prop_strikes_50y_p <- ggplot(data=all_prop_strike_50y, aes(iter_num, num_strikes, colour = mod_name)) +
                                           geom_line(size = 2) +
                                           #geom_path(aes(colour = as.numeric(date)))
                                           #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                                            xlab("\n Iteration") +
                                            ylab("Proportion of cruises in a year with encounter \n") +
                                            #xlim(c(0, 5)) +
                                            #ylim(c(0, 50)) +
                                            theme_bw() +
                                            theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                                            axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                                            #theme(legend.position="none")
prop_strikes_50y_p

prop_strikes_100y_p <- ggplot(data=all_prop_strike_100y, aes(iter_num, num_strikes, colour = mod_name)) +
                                             geom_line(size = 2) +
                                             #geom_path(aes(colour = as.numeric(date)))
                                             #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                                              xlab("\n Iteration") +
                                              ylab("Proportion of cruises in a yearwith encounter \n") +
                                              #xlim(c(0, 5)) +
                                              #ylim(c(0, 50)) +
                                              theme_bw() +
                                              theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                                              axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                                              #theme(legend.position="none")
prop_strikes_100y_p









all_sim_strike <- as.data.frame(bind_rows(sim_strikes_10m_13k_10yr, sim_strikes_25m_13k_10yr, sim_strikes_50m_13k_10yr,
                                                                       sim_strikes_10m_13k_50yr, sim_strikes_25m_13k_50yr, sim_strikes_50m_13k_50yr,
                                                                       sim_strikes_10m_13k_1yr, sim_strikes_25m_13k_1yr, sim_strikes_50m_13k_1yr)) %>%
                             group_by(mod_name) %>%
                             mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                             mutate(iter_num = 1:n()) %>%
                             ungroup() %>%
                             as.data.frame()

all_sim_strike_10yr <- as.data.frame(bind_rows(sim_strikes_10m_13k_10yr, sim_strikes_25m_13k_10yr, sim_strikes_50m_13k_10yr)) %>%
                                    group_by(mod_name) %>%
                                    mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                    mutate(iter_num = 1:n()) %>%
                                    ungroup() %>%
                                    as.data.frame()
all_sim_strike_50yr <- as.data.frame(bind_rows(sim_strikes_10m_13k_50yr, sim_strikes_25m_13k_50yr, sim_strikes_50m_13k_50yr)) %>%
                                    group_by(mod_name) %>%
                                    mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                    mutate(iter_num = 1:n()) %>%
                                    ungroup() %>%
                                    as.data.frame()
all_sim_strike_1yr <- as.data.frame(bind_rows(sim_strikes_10m_13k_1yr, sim_strikes_25m_13k_1yr, sim_strikes_50m_13k_1yr)) %>%
                                      group_by(mod_name) %>%
                                      mutate(cum_num_strikes = cumsum(num_strikes)) %>%
                                      mutate(iter_num = 1:n()) %>%
                                      ungroup() %>%
                                      as.data.frame()


all_strike_loc_10m_20k <- as.data.frame(bind_rows(all_strike_loc_20k_1yr_10m_p,
                                                                                     #all_strike_loc_20k_1yr_25m_p,
                                                                                     #all_strike_loc_20k_1yr_50m_p,
                                                                                     all_strike_loc_20k_10yr_10m_p, 
                                                                                     #all_strike_loc_20k_10yr_25m_p,
                                                                                     #all_strike_loc_20k_10yr_50m_p,
                                                                                     all_strike_loc_20k_50yr_10m_p))
                                                                                     #all_strike_loc_20k_50yr_25m_p,
                                                                                     #all_strike_loc_20k_50yr_50m_p))
all_strike_loc_25m_20k <- as.data.frame(bind_rows(#all_strike_loc_20k_1yr_10m_p,
                                                                                       all_strike_loc_20k_1yr_25m_p,
                                                                                       #all_strike_loc_20k_1yr_50m_p,
                                                                                       #all_strike_loc_20k_10yr_10m_p, 
                                                                                       all_strike_loc_20k_10yr_25m_p,
                                                                                       #all_strike_loc_20k_10yr_50m_p,
                                                                                       #all_strike_loc_20k_50yr_10m_p,
                                                                                       all_strike_loc_20k_50yr_25m_p))
                                                                                       #all_strike_loc_20k_50yr_50m_p))
all_strike_loc_50m_20k <- as.data.frame(bind_rows(#all_strike_loc_20k_1yr_10m_p,
                                                                                       #all_strike_loc_20k_1yr_25m_p,
                                                                                       all_strike_loc_20k_1yr_50m_p,
                                                                                       #all_strike_loc_20k_10yr_10m_p, 
                                                                                       #all_strike_loc_20k_10yr_25m_p,
                                                                                       all_strike_loc_20k_10yr_50m_p,
                                                                                       #all_strike_loc_20k_50yr_10m_p,
                                                                                       #all_strike_loc_20k_50yr_25m_p,
                                                                                       all_strike_loc_20k_50yr_50m_p))
                                                                                       
all_strike_loc_10m_20k$mod_name <- as.factor(all_strike_loc_10m_20k$mod_name)
all_strike_loc_25m_20k$mod_name <- as.factor(all_strike_loc_25m_20k$mod_name)
all_strike_loc_50m_20k$mod_name <- as.factor(all_strike_loc_50m_20k$mod_name)                                                                             



all_strike_loc_10m_13k <- as.data.frame(bind_rows(all_strike_loc_13k_1yr_10m_p,
                                                                                       #all_strike_loc_13k_1yr_25m_p,
                                                                                       #all_strike_loc_13k_1yr_50m_p))
                                                                                       all_strike_loc_13k_10yr_10m_p, 
                                                                                       #all_strike_loc_13k_10yr_25m_p,
                                                                                       #all_strike_loc_13k_10yr_50m_p,
                                                                                       all_strike_loc_13k_50yr_10m_p))
                                                                                       #all_strike_loc_13k_50yr_25m_p,
                                                                                       #all_strike_loc_13k_50yr_50m_p))
all_strike_loc_25m_13k <- as.data.frame(bind_rows(#all_strike_loc_13k_1yr_10m_p,
                                                                                       all_strike_loc_13k_1yr_25m_p,
                                                                                       #all_strike_loc_13k_1yr_50m_p,
                                                                                       #all_strike_loc_13k_10yr_10m_p, 
                                                                                       all_strike_loc_13k_10yr_25m_p,
                                                                                       #all_strike_loc_13k_10yr_50m_p))
                                                                                       #all_strike_loc_13k_50yr_10m_p,
                                                                                       all_strike_loc_13k_50yr_25m_p))
                                                                                       #all_strike_loc_13k_50yr_50m_p))
all_strike_loc_50m_13k <- as.data.frame(bind_rows(#all_strike_loc_13k_1yr_10m_p,
                                                                                       #all_strike_loc_13k_1yr_25m_p,
                                                                                       all_strike_loc_13k_1yr_50m_p,
                                                                                       #all_strike_loc_13k_10yr_10m_p, 
                                                                                       #all_strike_loc_13k_10yr_25m_p,
                                                                                       all_strike_loc_13k_10yr_50m_p,
                                                                                       #all_strike_loc_13k_50yr_10m_p,
                                                                                       #all_strike_loc_13k_50yr_25m_p,
                                                                                       all_strike_loc_13k_50yr_50m_p))
                                                                                       
all_strike_loc_10m_13k$mod_name <- as.factor(all_strike_loc_10m_13k$mod_name)
all_strike_loc_25m_13k$mod_name <- as.factor(all_strike_loc_25m_13k$mod_name)
all_strike_loc_50m_13k$mod_name <- as.factor(all_strike_loc_50m_13k$mod_name)




num_strikes_p <- ggplot(data=all_sim_strike_50yr, aes(num_strikes, colour = mod_name)) +
                              geom_density(size = 1.25) +
                              #geom_vline(aes(xintercept=mean(strikes)), color="black", linetype="dashed", size=1) +
                              xlab("\n Number of simulated strikes at given distance and time span") +
                              ylab("Density \n") +
                              #xlim(c(0, 5)) +
                              #ylim(c(0, 2.5)) +
                              theme_bw() +
                              theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), 
                              axis.text = element_text(size = rel(2)), axis.title = element_text(size = rel(2))) 
                              #theme(legend.position="none")
num_strikes_p



#  Plot initial spatial situation
ras <- probrast
plot(ras)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "skyblue3") #xlim=c(381000, 470000), ylim = c(645000, 6545000))
#plot(s, add = TRUE)    
plot(ras, add= TRUE, legend.plot = FALSE)
plot(s_big, col = "burlywood4", add = TRUE)
plot(init_locs, pch = 16, col = "#B40F20", add = TRUE)
plot(start_up_bay_xy, add = TRUE)
plot(end_up_bay_xy, add = TRUE)
plot(one_track_up, add = TRUE)
plot(start_down_bay_xy, add = TRUE)
plot(end_down_bay_xy, add = TRUE)
plot(one_track_down, add = TRUE)
plot(track, add = TRUE)

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


#################################################################################
#################################################################################