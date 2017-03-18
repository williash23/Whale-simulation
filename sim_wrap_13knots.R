# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - function to run simulaiton for 13 knot ship speed
#   Have to load script: sim_once_to_start.R before running this one
################################################################################

# Looming question of unreported whale strikes??? In this system it porbably isn't an issue 
#   (supported by the evidence of historical collisions)


sim_ship_strike_fun <- function(mod, strike_dist){

            #### STEP 1 ####
            #  Start out with a number of whales in the area of the ship route 
            #   These are placed initially within the area of the prediction grid of the DSM (so within 1 km on either side of ship route)
            #   Predcited abundance in that area: 20.22758, mean from iterations (mat_predgrid) 20.17342, sd 0.1281257
            #  Right now...whales are only placed within that small buffer around ship, and if they leave that buffer, they don't return.
            ni <- round(rnorm(1, 20.17342, 0.1281257))

            #### STEP 2 ####
            #  Place the number of initial whales (ni) in grid cells based on the predicted density of that cells used as probabilities

            #   Sample ni initial locations for whales using the relative probability of each grid cell
            init_locs <- probsel(probrast, ni)
            init_locs_df <- as.data.frame(init_locs)
            ################################################################################


            #### STEP 3 #### use this so switch between movement models for different behaviors
            #  Select initial behavior of whale at it's initial sighitng based on proportion observed
            #   1489/1671 = 0.89; Transit, 182/1671 = Station
            #   Here, 1 = Transit
            #beh <- as.data.frame(rbinom(ni, 1, 0.89))
            #colnames(beh) [1] <- "beh_tmp"
            #init_locs_tmp1 <- as.data.frame(init_locs)
            #init_locs_tmp2 <- bind_cols(init_locs_tmp1, beh)
            #init_locs_df <- init_locs_tmp2 %>%
                                     #mutate(beh = ifelse(beh_tmp == 1, 1, 2)) %>%
                                     #dplyr::select(x, y, beh = beh)
            ################################################################################


            #### STEP 4 ####


            #  Generate CRW from simulated steps and turns for movement UP bay for 13k speed restriction
            #   Matrices to hold simulations for whale movement UP bay 13k
            mat_X_up <- matrix(nrow = nobs_whale_up_13k, ncol = ni)
            mat_Y_up <- matrix(nrow = nobs_whale_up_13k, ncol = ni)

            for(j in 1:ni){
                 keep <- sample(1:nrow(mod), nobs_whale_up_13k, replace = FALSE)
                 # make distributed steps
                 steps_sim <- mod[keep, 1]
                 # make clustered turning angles
                 theta_sim <- mod[keep, 2]
                 # cumulative angle (absolute orientation)
                 phi_sim <- cumsum(theta_sim)
                 # step length components 
                 dX_sim <- steps_sim*cos(phi_sim)
                 dX_sim[1] <- init_locs_df$x[j] # make each whale start at location selected above
                 dY_sim <- steps_sim*sin(phi_sim)
                 dY_sim[1] <- init_locs_df$y[j] # make each whale start at location selected above
                 # actual X-Y values
                 X_sim <- as.matrix(cumsum(dX_sim))
                 Y_sim <- as.matrix(cumsum(dY_sim))
                 mat_X_up[,j] <- X_sim
                 mat_Y_up[,j] <- Y_sim
               }

            #  Data from for locations for whales moving UP bay
            df_X_up_tmp <- as.data.frame(mat_X_up)
            df_Y_up_tmp <- as.data.frame(mat_Y_up)
            df_X_up <- melt(df_X_up_tmp)
            df_Y_up <- melt(df_Y_up_tmp)
            df_XYwhales_up_tmp <- cbind(df_X_up, df_Y_up, rep(1:nobs_whale_up_13k))
            names(df_XYwhales_up_tmp) <- c("whale_ind_num", "X_whale", "walk_num_rep", "Y_whale", "loc_num_whale")
            df_XYwhales_up <- df_XYwhales_up_tmp %>%
                                            dplyr::select(whale_ind_num, loc_num_whale, X_whale, Y_whale) %>%
                                            mutate(model = "Single State") %>%
                                            arrange(loc_num_whale)

            #   Matrices to hold simulations for whale movement DOWN bay 13k
            mat_X_down <- matrix(nrow = nobs_whale_down_13k, ncol = ni)
            mat_Y_down <- matrix(nrow = nobs_whale_down_13k, ncol = ni)

            for(k in 1:ni){
                 keep <- sample(1:nrow(mod), nobs_whale_down_13k, replace = FALSE)
                 # make distributed steps
                 steps_sim <- mod[keep, 1]
                 # make clustered turning angles
                 theta_sim <- mod[keep, 2]
                 # cumulative angle (absolute orientation)
                 phi_sim <- cumsum(theta_sim)
                 # step length components 
                 dX_sim <- steps_sim*cos(phi_sim)
                 dX_sim[1] <- init_locs_df$x[k] # make each whale start at location selected above
                 dY_sim <- steps_sim*sin(phi_sim)
                 dY_sim[1] <- init_locs_df$y[k] # make each whale start at location selected above
                 # actual X-Y values
                 X_sim <- as.matrix(cumsum(dX_sim))
                 Y_sim <- as.matrix(cumsum(dY_sim))
                 mat_X_down[,k] <- X_sim
                 mat_Y_down[,k] <- Y_sim
               }

            #  Data from for locations for whales moving DOWN bay
            df_X_down_tmp <- as.data.frame(mat_X_down)
            df_Y_down_tmp <- as.data.frame(mat_Y_down)
            df_X_down <- melt(df_X_down_tmp)
            df_Y_down <- melt(df_Y_down_tmp)
            df_XYwhales_down_tmp <- cbind(df_X_down, df_Y_down, rep(1:nobs_whale_down_13k))
            names(df_XYwhales_down_tmp) <- c("whale_ind_num", "X_whale", "walk_num_rep", "Y_whale", "loc_num_whale")
            df_XYwhales_down <- df_XYwhales_down_tmp %>%
                                                dplyr::select(whale_ind_num, loc_num_whale, X_whale, Y_whale) %>%
                                                mutate(model = "Single State") %>%
                                                arrange(loc_num_whale)
################################################################################


            #### STEP 5 ####
            #  Combine whale movement and ship movement
            df_XYship_up <- ship_locs_up_df_13k[rep(1:nrow(ship_locs_up_df_13k),each=(ni)),] 
            df_XYboth_up <- cbind(df_XYwhales_up, df_XYship_up) %>%
                                        dplyr::rename(X_ship = long, Y_ship = lat, loc_num_ship = loc_num)

            df_XYship_down <- ship_locs_down_df_13k[rep(1:nrow(ship_locs_down_df_13k),each=(ni)),] 
            df_XYboth_down <- cbind(df_XYwhales_down, df_XYship_down) %>%
                                             dplyr::rename(X_ship = long, Y_ship = lat, loc_num_ship = loc_num)
            ################################################################################


            #### STEP 6 ####
            #  Count overlaps within certain distance!!!!
            sim_strikes_up <- df_XYboth_up %>%
                                          mutate(X_overlap = ifelse((abs(X_whale - X_ship)) <= strike_dist, 1, 0)) %>%
                                          mutate(Y_overlap = ifelse((abs(Y_whale - Y_ship)) <= strike_dist, 1, 0)) %>%
                                          mutate(sim_strike = ifelse(X_overlap == 1 & Y_overlap == 1, 1, 0))

            sim_strikes_down <- df_XYboth_down %>%
                                              mutate(X_overlap = ifelse((abs(X_whale - X_ship)) <= strike_dist, 1, 0)) %>%
                                              mutate(Y_overlap = ifelse((abs(Y_whale - Y_ship)) <= strike_dist, 1, 0)) %>%
                                              mutate(sim_strike = ifelse(X_overlap == 1 & Y_overlap == 1, 1, 0))

            tot_sim_strikes_up <- sum(sim_strikes_up$sim_strike, na.rm = T)
            tot_sim_strikes_down <- sum(sim_strikes_down$sim_strike, na.rm = T)

            tot_sim_strikes <- tot_sim_strikes_up + tot_sim_strikes_down
            return(tot_sim_strikes)
}
################################################################################

#  Call function and plot
nrep <- 10
rep_sim_strikes <- as.vector(rep(0, nrep))


for(i in 1:nrep){
  rep_sim_strikes[i] <- sim_ship_strike_fun(mod = post_sims, strike_dist = 50)
  }
  
hist(rep_sim_strikes)
#################################################################################