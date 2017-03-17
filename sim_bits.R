# Sara Williams
# 3/8/2017
# Whale ship strike risk simulation
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
library(rgdal)
library(raster)

################################################################################


#  Load data
# Load output from MCMC posterior distribtuion iterations (JAGS object)
load("C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/movement_mode_models/model_output/single_fit_dive.RData")
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

probsel <- function(probrast, N){
                                  x <- getValues(probrast)
                                  x[is.na(x)] <- 0
                                  vec_cells <- seq(1:length(probrast))
                                  samp <- sample(vec_cells, ni, replace = T, prob=x)
                                  samprast <- raster(probrast)
                                  samprast[samp] <- 1 
                                  samp_pts <- rasterToPoints(samprast, fun = function(x){x > 0})
                                  samp_pts <- SpatialPoints(samp_pts)
                                  crs(samp_pts) <- UTM
                                  return(samp_pts)
}
################################################################################

# Make study area
UTM <- CRS("+proj=utm +zone=8 +datum=WGS84")
crs(obs_pts_UTM) <- UTM
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
track_df <- track_df2 %>%
                   filter(transct ==  "2013-07-14-S-1") # Sara route up bay
                            #transct ==  "2013-08-04-S-168913"| # Sara route down bay - Chatham
                            #transct == "2013-08-04-S-1"| # Sara route up bay
                            #transct ==  "2013-07-24-S-1"|
                            #transct == "2013-07-24-S-309249") # Sara route down bay - Icy

one_track_up_pts <- dplyr::select(track_df, long, lat)
coordinates(one_track_up_pts) <- ~ long + lat 
one_track_up <- spLines(one_track_up_pts)
crs(one_track_up) <- UTM
################################################################################


#### STEP 1 ####
#  Start out with a number of whales in the area of the ship route 
#   These are placed initially within the area of the prediction grid of the DSM (so within 1 km on either side of ship route)
#   Predcited abundance in that area: 20.22758, mean from iterations (mat_predgrid) 20.17342, sd 0.1281257
#  Right now...whales are only placed within that small buffer around ship, and if they leave that buffer, they don't return.
ni <- round(rnorm(1, 20.17342, 0.1281257))

#### STEP 2 ####
#  Place the number of initial whales (ni) in grid cells based on the predicted density of that cells used as probabilities
#   Generate probability for each grid cell from relative predicted density
sa <- predgrid_plot %>%
         dplyr::select(X, Y, Nhat) 
sa_rast <- rasterFromXYZ(sa)
probrast  <-sa_rast/sum(getValues(sa_rast), na.rm=T)
#   Sample ni initial locations for whales using the relative probability of each grid cell
init_locs <- probsel(probrast, ni)

#  Plot
ras <- sa_rast
plot(ras)
plot(s, add = TRUE)
plot(s_big, col = "dark green", add = TRUE)
plot(init_locs, pch = 1, col = "blue", add = TRUE)
plot(one_track_up, add = TRUE)
################################################################################


#### STEP 3 ####
#  Select initial behavior of whale at it's initial sighitng based on proportion observed
#   1489/1671 = 0.89; Transit, 182/1671 = Station
#   Here, 1 = Transit
beh <- as.data.frame(rbinom(ni, 1, 0.89))
colnames(beh) [1] <- "beh_tmp"
init_locs_tmp1 <- as.data.frame(init_locs)
init_locs_tmp2 <- bind_cols(init_locs_tmp, beh)
init_locs_df <- init_locs_tmp2 %>%
                          mutate(beh = ifelse(beh_tmp == 1, 1, 2)) %>%
                         dplyr::select(x, y, beh = beh)
################################################################################





#### STEP 4 ####
#  Set ship start and end location for one passage
start_up_bay <- track_df %>%
                           filter(long == max(long)) %>%
                           dplyr::select(x = long, y = lat)
end_up_bay <- track_df %>%
                           filter(long == min(long)) %>%
                           dplyr::select(x = long, y = lat)
start_up_bay_xy <- SpatialPoints(start_up_bay)
crs(start_up_bay_xy) <- UTM
end_up_bay_xy <- SpatialPoints(end_up_bay)
crs(end_up_bay_xy) <- UTM
################################################################################


#### STEP 5 ####
#  Have ship move step
#   Ship speed limits: 20 knots (10.2889 m per sec) and 13 knots (6.68778 m per sec)



################################################################################


#### STEP 6 ####
#  Generate new whale locations

#  Generate simulated step lengths and turn angles from parameter estimates
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
post_sims_plot <- post_sims %>% 
                              mutate(iter_num = 1:nrow(post_sims))

#  Generate CRW from simulated steps and turns
#   Number of movement paths
npath <- ni
#   Length of walk
nobs <- 10
mod <- post_sims_plot
#   Matrices to hold simulations
mat_X <- matrix(nrow = nobs, ncol = npath)
mat_Y <- matrix(nrow = nobs, ncol = npath)

for(i in 1:npath){
     keep <- sample(1:nrow(mod), nobs, replace = FALSE)
     # make distributed steps
     steps_sim <- mod[keep, 1]
     # make clustered turning angles
     theta_sim <- mod[keep, 2]
     # cumulative angle (absolute orientation)
     phi_sim <- cumsum(theta_sim)
     # step length components 
     dX_sim <- steps_sim*cos(phi_sim)
     dX_sim[1] <- init_locs_df$x[i] # make each whale start at location selected above
     dY_sim <- steps_sim*sin(phi_sim)
     dY_sim[1] <- init_locs_df$y[i] # make each whale start at location selected above
     # actual X-Y values
     X_sim <- as.matrix(cumsum(dX_sim))
     Y_sim <- as.matrix(cumsum(dY_sim))
     mat_X[,i] <- X_sim
     mat_Y[,i] <- Y_sim
   }

#  Data from for locations.
df_X_tmp <- as.data.frame(mat_X)
df_Y_tmp <- as.data.frame(mat_Y)
df_X <- melt(df_X_tmp)
df_Y <- melt(df_Y_tmp)
df_XY_tmp <- cbind(df_X , df_Y, rep(1:nobs))
names(df_XY_tmp) <- c("walk_num", "X", "walk_num_rep", "Y", "location_num")
df_XY <- df_XY_tmp %>%
               dplyr::select(walk_num, location_num, X, Y) %>%
               mutate(model = "Single State")

whale_pth <- ggplot() + 
                       geom_point(data = df_XY, alpha= 0.1, aes(x = X, y = Y, group = walk_num, colour = factor(walk_num))) + 
                                            #colour = walk_num, fill = walk_num, size = 1) +
                       xlab("X (km)") +
                       ylab("Y (km)") +
                       theme_bw() +
                       theme(panel.grid.minor = element_blank(), axis.text = element_text(size = rel(2)), 
                       axis.title = element_text(size = rel(2)))
whale_pth

#### Simulate 20 whales, each individually, then see how many many surfacings it is in the ship route buffer?
#### Different whale placement every time? I could rerun prediction model (1 itieration) to get slightly different probabilities?
#### same movement parameters for each whale, or based on behavior?
#### Looming question of unreported whale strikes??? In this system it porbably isn't an issue (supported by the evidence of historical collisions)
#### If whales move out of study area, let them keep moving around - they can come back in our stay out.




























################################################################################

#  Code below adpated from: 
#   https://scrogster.wordpress.com/2012/04/22/using-r-for-spatial-sampling-with-selection-probabilities-defined-in-a-raster/
# probsel <- function(probrast, N){
                    # x <- getValues(probrast)
                    # x[is.na(x)] <- 0
                    # samp <- sample(nrow(probrast)*ncol(probrast), size=N, prob=x)
                    # samprast <- raster(probrast)
                    # samprast[samp] <- 1 
                    # pts <- rasterToPoints(samprast, fun=function(x){x>0})
                    # pts <- SpatialPoints(pts)
                   # return(pts)
# }



#  My own attempt at writing the sampling and turing into spatial points
# mat <- as.matrix(probrast)
# vec_probs_tmp <- as.vector(mat)
# vec_cells_tmp <- seq(1:length(vec_probs_tmp))
# vec_mat_tmp <- cbind(vec_cells_tmp, vec_probs_tmp)
# vec_mat <- vec_mat_tmp[complete.cases(vec_mat_tmp[,2]),]
# vec_probs <- vec_mat[,2]
# vec_cells <- vec_mat[,1]


# for(i in 1:length(init_locs)){
  # row_init_locs[i] <- floor(init_locs[i]/ncol(mat))
  # col_init_locs[i] <- init_locs[i] - (row_init_locs[i]*ncol(mat))
  # }

# new_mat <- matrix(nrow = nrow(mat), ncol = ncol(mat))
# for(i in 1:length(row_init_locs)){
  # for(j in 1:length(col_init_locs)){
    # new_mat[row_init_locs[i],col_init_locs[j]] <- 1
      # }
    # }




# init_locs <- base::sample(vec_cells, ni, replace = T, prob = vec_probs)


# sa <- predgrid_plot %>%
         # dplyr::select(X, Y, Nhat) 
# probrast <- rasterFromXYZ(sa)
# probrast  <-probrast/sum(getValues(probrast), na.rm=T)
# init_locs <- probsel(probrast, ni)
# crs(init_locs) <- UTM
## Pulling top 20 ---  so same everytime! 


#  Cells - each has a probability - put in rank order in a line
#  For each whale, pull random number and find out where they end up in that line
#  Cumulative sum function - order cells and put into a vector of cumulative sums
#  Pull random u~uniform()
#  Find max value that u is less than up


