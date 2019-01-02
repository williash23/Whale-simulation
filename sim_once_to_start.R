# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - set up code that need to be run once first before running 
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
library(reshape2)
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
load("C:/Users/saraw/Documents/GitHub/Whale-Simulation/data_files/single_fit_dive.RData")
load("C:/Users/saraw/Documents/GitHub/Whale-Simulation/data_files/predgrid_plot.RData")
segs_sm <- shapefile("C:/Users/saraw/Desktop/Whale-DSM-misc/data/obs_and_segs_dat/segment_centroids_2008on_sm_sst")
s_big2 <- shapefile("C:/Users/saraw/Desktop/Whale-DSM-misc/data/study_and_survey_area/SEAK_bound_small")
s2 <- shapefile("C:/Users/saraw/Desktop/Whale-DSM-misc/data/study_and_survey_area/survey_area")
track2 <- shapefile("C:/Users/saraw/Desktop/Whale-DSM-misc/data/survey_tracks/survey_lines_2008on")
#obssegs_1k_sm <- shapefile("C:/Users/saraw/Desktop/Whale-DSM-misc/data/obs_and_segs_dat/obs_segs_2008on_1k_sm_2_14_2")
################################################################################

#  Load functions
#  wrapped cauchy distribution
rwcauchy <- function(n, mu = 0, rho = 0) {
  u = runif(n)
  V = cos(2 * pi * u)
  c = 2 * rho/(1 + rho^2)
  t <- (sign(runif(n) - 0.5) * acos((V + c)/(1 + c * V)) + mu)%%(2 * pi)
  return(t)
}

# Fcuntion to generate relative probability of whale occurence for each grid cell
probsel <- function(probrast, ni){
                    x <- getValues(probrast)
                    x[is.na(x)] <- 0
                    vec_cells <- seq(1:length(probrast))
                    samp <- sample(vec_cells,  ni, replace = F, prob=x)
                    samprast <- raster(probrast)
                    samprast[samp] <- 1 
                    samp_pts <- rasterToPoints(samprast, fun = function(x){x > 0})
                    samp_pts <- SpatialPoints(samp_pts)
                    crs(samp_pts) <- UTM
                    return(samp_pts)
                    }

#   Assign relative probability from each grid cell from relative predicted density
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
s_big_df_tmp <- join(s_big_points, s_big@data, by="id")
s_big_df <- s_big_df_tmp %>%
                   mutate(fill = as.logical(ifelse(hole == "TRUE", "FALSE", "TRUE")))
                   
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
track_df <- track_df2 %>%
                   separate(transct, c("A", "B", "C", "D", "E"), sep="-", remove = FALSE)
track_df$id <- as.numeric(track_df$id)
all_track_df_up <- track_df %>%
                               filter(E < 2) %>%
                                dplyr::select(long, lat, id)
all_track_df_up$new_ID <- all_track_df_up %>% group_indices(id)
all_track_df_down <- track_df %>%
                                    filter(E > 10) %>%
                                    dplyr::select(long, lat, id)
all_track_df_down$new_ID <- all_track_df_down %>% group_indices(id)
################################################################################


#  Generate simulated step lengths and turn angles for whale movements from parameter 
#   estimate posterior distritbutions from movement analysis
#   Currently using single movement mode with all data model
niter <- 11000
nsamp <- 5000
 
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

mod <- as.data.frame(cbind(steps, turns))
mod$turns[mod$turns>pi]=mod$turns[mod$turns>pi]-2*pi
################################################################################
################################################################################
