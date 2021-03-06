
#  Load packages
library(plyr)
library(packcircles)
library(ggmcmc)
library(gridExtra)
library(ggthemes)
################################################################################

#  Take the short interspout interval turn/step length data only (for now) and start the whale at the 
#  black dot, i.e., at a position that it will be at same place at same time as the ship in 180 seconds.  
#  Then set it loose with the assumption that it will (1) act like your movement model mode based on
#  short interval throughout the 180 second period and (2) that each turn is independent, i.e., if after 
#  the first time step, the whale has turned X degrees, then the probability of its next movement is 
#  independent of its first...(this may be important because we may want to assume that if it turned 
#  right the first turn then the next turn it will continue right....or not)

#   Number of movement paths
npath <- 10000
#   Length of walk
nobs <- 3

mod <- post_sims_su_plot
#   Matrices to hold simulations
mat_X <- matrix(nrow = nobs, ncol = npath)
mat_Y <- matrix(nrow = nobs, ncol = npath)

for(i in 1:npath){
     keep <- sample(1:nrow(mod), nobs, replace = FALSE)
 
       # make distributed steps
       steps_sim <- mod[keep, 1]
       # make clustered turning angles
       theta_sim <- mod[keep, 2]
       theta_sim[1] <- 0
       #theta_sim[2] <- 0
       #theta_sim[2] <- circ[i]
       # cumulative angle (absolute orientation)
       phi_sim <- cumsum(theta_sim)
       # step length components -- 
       dX_sim <- steps_sim*cos(phi_sim)
       dX_sim[1] <- 0 ##MAKE EACH START AT [0,0]
       #dX_sim[2] <- 0
       dY_sim <- steps_sim*sin(phi_sim)
       dY_sim[1] <- 0
       #dY_sim[2] <- 0.5
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

#  Make data suitable for plotting as points
df_XY_pt <- df_XY %>%
                    filter(location_num == 2) 
