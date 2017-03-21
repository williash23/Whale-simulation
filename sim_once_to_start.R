# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - things that need to be run once first before running 
#   simulation funciton
################################################################################

#  Load packages
library(plyr)
library(dplyr)
library(packcircles)
library(ggmcmc)
library(gridExtra)
library(ggthemes)
library(CircStats)
library(reshape)
library(ggplot2)
library(rjags)
library(rgeos)
library(rgdal)
library(raster)
library(sp)
library(maptools)
################################################################################


#  Load data
# Load output from MCMC posterior distribtuion iterations (JAGS object)
load("C:/Users/sara.williams/Documents/GitHub/Whale-Simulation/data_files/single_fit_dive.RData")
load("C:/Users/sara.williams/Documents/GitHub/Whale-Simulation/data_files/predgrid_plot.RData")
segs_sm <- shapefile("C:/Users/sara.williams/Desktop/Whale-DSM-misc/data/obs_and_segs_dat/segment_centroids_2008on_sm_sst")
s_big2 <- shapefile("C:/Users/sara.williams/Desktop/Whale-DSM-misc/data/study_and_survey_area/SEAK_bound_small")
s2 <- shapefile("C:/Users/sara.williams/Desktop/Whale-DSM-misc/data/study_and_survey_area/survey_area")
track2 <- shapefile("C:/Users/sara.williams/Desktop/Whale-DSM-misc/data/survey_tracks/survey_lines_2008on")
#obssegs_1k_sm <- shapefile("C:/Users/sara.williams/Desktop/Whale-DSM-misc/data/obs_and_segs_dat/obs_segs_2008on_1k_sm_2_14_2")
################################################################################

#  Load functions
rwcauchy <- function(n, mu = 0, rho = 0) {
  u = runif(n)
  V = cos(2 * pi * u)
  c = 2 * rho/(1 + rho^2)
  t <- (sign(runif(n) - 0.5) * acos((V + c)/(1 + c * V)) + mu)%%(2 * pi)
  return(t)
}

#   Generate probability for each grid cell from relative predicted density
sa <- predgrid_plot %>%
         dplyr::select(X, Y, Nhat) 
sa_rast <- rasterFromXYZ(sa)
probrast  <-sa_rast/sum(getValues(sa_rast), na.rm=T)


# Make study area and ship route, movement
UTM <- CRS("+proj=utm +zone=8 +datum=WGS84")
crs(segs_sm) <- UTM
#crs(obs_pts_UTM) <- UTM
e <- extent(segs_sm)

s_big <- crop(s_big2, extent(e))
s_big@data$id <- rownames(s_big@data)
s_big_points <- fortify(s_big, region="id")
s_big_df <- join(s_big_points, s_big@data, by="id")

s <- crop(s2, extent(e))
s@data$id <- rownames(s@data)
s_points <- fortify(s, region="id")
s_df_tmp <- join(s_points, s@data, by="id")
s_df <- s_df_tmp %>%
            mutate(fill = as.logical(ifelse(hole == "TRUE", "FALSE", "TRUE")))

track <- crop(track2, extent(e))
track@data$id <- rownames(track@data)
track_points <- fortify(track, region="id")
track_df2 <- join(track_points, track@data, by="id") 
track_df_up <- track_df2 %>%
                         filter(transct ==  "2013-07-14-S-1") # Sara route up bay
track_df_down <- track_df2 %>%
                              filter(transct ==  "2013-07-14-S-512972") 
                            #transct ==  "2013-08-04-S-168913"| # Sara route down bay - Chatham
                            #transct == "2013-08-04-S-1"| # Sara route up bay
                            #transct ==  "2013-07-24-S-1"|
                            #transct == "2013-07-24-S-309249") # Sara route down bay - Icy

one_track_up_pts <- dplyr::select(track_df_up, long, lat)
one_track_up_pts <- dplyr::select(track_df_up, long, lat)
coordinates(one_track_up_pts) <- ~ long + lat 
one_track_up <- spLines(one_track_up_pts)
crs(one_track_up) <- UTM

one_track_down_pts <- dplyr::select(track_df_down, long, lat)
one_track_down_pts <- dplyr::select(track_df_down, long, lat)
coordinates(one_track_down_pts) <- ~ long + lat 
one_track_down <- spLines(one_track_down_pts)
crs(one_track_down) <- UTM


#  Set ship start and end location for one passage
one_track_up_pts <- as(one_track_up, "SpatialPointsDataFrame")
start_up_bay <- as.data.frame(one_track_up_pts) %>%
                           filter(lat == min(lat)) %>%
                           dplyr::select(long, lat)
end_up_bay <- as.data.frame(one_track_up_pts) %>%
                           filter(lat== max(lat)) %>%
                           dplyr::select(long, lat)
start_up_bay_xy <- SpatialPoints(start_up_bay)
crs(start_up_bay_xy) <- UTM
end_up_bay_xy <- SpatialPoints(end_up_bay)
crs(end_up_bay_xy) <- UTM

one_track_down_pts <- as(one_track_down, "SpatialPointsDataFrame")
start_down_bay <- as.data.frame(one_track_down_pts) %>%
                                filter(lat == max(lat)) %>%
                                dplyr::select(long, lat)
end_down_bay <- as.data.frame(one_track_down_pts) %>%
                              filter(lat == min(lat)) %>%
                              dplyr::select(long, lat)
start_down_bay_xy <- SpatialPoints(start_down_bay)
crs(start_down_bay_xy) <- UTM
end_down_bay_xy <- SpatialPoints(end_down_bay)
crs(end_down_bay_xy) <- UTM



#   Ship speed limits: 20 knots (10.2889 m per sec, 1000 m in 97.2 sec) 
#                         and 13 knots (6.68778 m per sec, 1000 m in 149.5 sec)
#  Time for ship to travel up, down, total at two speeds
track_up_length <- gLength(one_track_up)
track_down_length <- gLength(one_track_down)
tm_sec_up_13k <- (track_up_length)/6.68778
tm_sec_up_20k <- (track_up_length)/10.2889
tm_sec_down_13k <- (track_down_length)/6.68778
tm_sec_down_20k <- (track_down_length)/10.2889
tm_sec_total_13k <- tm_sec_up_13k + tm_sec_down_13k # 482 min, 8 hr
tm_sec_total_20k <- tm_sec_up_20k + tm_sec_down_20k # 313 min, 5.2 hr

#   Time between surfacings, based on data from movement analysyis: median(89 sec), mean (124 sec)
med_surface_int <- 89
#mu_surface_int <- 124

#  Move ship up and down bay along route and determine locations based on distance moved in amount of time
#   between surfacings
dist_ship_move_per_surf_13k <- 6.68778*med_surface_int
dist_ship_move_per_surf_20k <- 10.2889*med_surface_int

num_ship_locs_up_13k <- floor(track_up_length/dist_ship_move_per_surf_13k)
ship_locs_up_13k <- sp::spsample(one_track_up, n = num_ship_locs_up_13k, type = "regular")
ship_locs_up_df_13k_tmp1 <- as.data.frame(ship_locs_up_13k)
ship_locs_up_df_13k_tmp2 <- ship_locs_up_df_13k_tmp1[-1,]
ship_locs_up_df_13k_tmp3 <- ship_locs_up_df_13k_tmp2[-nrow(ship_locs_up_df_13k_tmp2),]
ship_locs_up_df_13k <- ship_locs_up_df_13k_tmp3 %>%
                                        bind_rows(start_up_bay, end_up_bay) %>%
                                        arrange(lat) %>%
                                        mutate(loc_num = 1:n())

num_ship_locs_down_13k <- floor(track_down_length/dist_ship_move_per_surf_13k)
ship_locs_down_13k <- sp::spsample(one_track_down, n = num_ship_locs_down_13k, type = "regular")
ship_locs_down_df_13k_tmp1 <- as.data.frame(ship_locs_down_13k)
ship_locs_down_df_13k_tmp2 <- ship_locs_down_df_13k_tmp1[-1,]
ship_locs_down_df_13k_tmp3 <- ship_locs_down_df_13k_tmp2[-nrow(ship_locs_down_df_13k_tmp2),]
ship_locs_down_df_13k <- ship_locs_down_df_13k_tmp3 %>%
                                            arrange(desc(lat)) %>% 
                                            bind_rows(start_down_bay, end_down_bay) %>%
                                            arrange(desc(lat)) %>%
                                            mutate(loc_num = 1:n())

num_ship_locs_up_20k <- floor(track_up_length/dist_ship_move_per_surf_20k)
ship_locs_up_20k <- sp::spsample(one_track_up, n = num_ship_locs_up_20k, type = "regular")
ship_locs_up_df_20k_tmp1 <- as.data.frame(ship_locs_up_20k)
ship_locs_up_df_20k_tmp2 <- ship_locs_up_df_20k_tmp1[-1,]
ship_locs_up_df_20k_tmp3 <- ship_locs_up_df_20k_tmp2[-nrow(ship_locs_up_df_20k_tmp2),]
ship_locs_up_df_20k <- ship_locs_up_df_20k_tmp3 %>%
                                        bind_rows(start_up_bay, end_up_bay) %>%
                                        arrange(lat) %>%
                                        mutate(loc_num = 1:n())

num_ship_locs_down_20k <- floor(track_down_length/dist_ship_move_per_surf_20k)
ship_locs_down_20k <- sp::spsample(one_track_down, n = num_ship_locs_down_20k, type = "regular")
ship_locs_down_df_20k_tmp1 <- as.data.frame(ship_locs_down_20k)
ship_locs_down_df_20k_tmp2 <- ship_locs_down_df_20k_tmp1[-1,]
ship_locs_down_df_20k_tmp3 <- ship_locs_down_df_20k_tmp2[-nrow(ship_locs_down_df_20k_tmp2),]
ship_locs_down_df_20k <- ship_locs_down_df_20k_tmp3 %>%
                                            arrange(desc(lat)) %>% 
                                            bind_rows(start_down_bay, end_down_bay) %>%
                                            arrange(desc(lat)) %>%
                                            mutate(loc_num = 1:n())

#  Designate number of whale locations during time it takes ship to move up and down bay
nobs_whale_up_13k <- num_ship_locs_up_13k
nobs_whale_down_13k <- num_ship_locs_down_13k
nobs_whale_up_20k <- num_ship_locs_up_20k
nobs_whale_down_20k <- num_ship_locs_down_20k


#  Generate simulated step lengths and turn angles for whale movements from parameter 
#   estimate posterior distritbutions from movement analysis
#   Currently using single movement mode with all data model!!!!
niter <- 11000
nsamp <- 1000
 
keep_1 <- sample(1:niter, nsamp, replace = F)
keep_2 <- sample(1:niter, nsamp, replace = F)
keep_3 <- sample(1:niter, nsamp, replace = F)
chain_1 <- single_fit_dive[[1]]
sims_1 <- chain_1[keep_1, c(2, 3, 4, 1)]
chain_2 <- single_fit_dive[[2]]
sims_2 <- chain_2[keep_2, c(2, 3, 4, 1)]
chain_3 <- single_fit_dive[[3]]
sims_3 <- chain_3[keep_3, c(2, 3, 4, 1)]
sims <- rbind(sims_1, sims_2, sims_3)
steps <- numeric(length = nrow(sims))
turns <- numeric(length = nrow(sims))
for(i in 1:nrow(sims)){
  steps[i] <- rweibull(1, sims[i,3], (1/sims[i,4])^(1/sims[i,3]))
  turns[i] <- rwcauchy(1, sims[i,1], sims[i,2])
 }
post_sims <- as.data.frame(cbind(steps, turns))
post_sims$turns[post_sims$turns>pi]=post_sims$turns[post_sims$turns>pi]-2*pi
################################################################################
################################################################################