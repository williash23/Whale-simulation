# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - function to run simulaiton for 20 knot ship speed
#   Have to load script: sim_once_to_start.R before running this one
################################################################################

# Looming question of unreported whale strikes??? In this system it porbably isn't an issue 
#   (supported by the evidence of historical collisions)


sim_ship_strike_fun_20k_loc <- function(strike_dist){

            #### STEP 1 ####
            #  Start out with a number of whales in the area of the ship route - determined in function call
            #   These are placed initially within the area of the prediction grid of the DSM (so within 1 km on either side of ship route)
            #   Predcited abundance in that area: 20.22758, mean from iterations (mat_predgrid) 20.17342, sd 0.1281257
            #  Right now...whales are only placed within that small buffer around ship, and if they leave that buffer, they don't return.
            #ni_tmp <- rnorm(1, 20.17342, 0.1281257)
            #ni <- round(ni_tmp)
            
            #### STEP 2 ####
            #  Place the number of initial whales (ni) in grid cells based on the predicted density of that cells used as probabilities
              #   Sample ni initial locations for whales using the relative probability of each grid cell
              # probsel <- function(probrast, ni){
                                                # x <- getValues(probrast)
                                                # x[is.na(x)] <- 0
                                                # vec_cells <- seq(1:length(probrast))
                                                # samp <- sample(vec_cells,  ni, replace = F, prob=x)
                                                # samprast <- raster(probrast)
                                                # samprast[samp] <- 1 
                                                # samp_pts <- rasterToPoints(samprast, fun = function(x){x > 0})
                                                # samp_pts <- SpatialPoints(samp_pts)
                                                # crs(samp_pts) <- UTM
                                                # return(samp_pts)
              # }
              init_locs <- probsel(probrast, 20)
              init_locs_df <- as.data.frame(init_locs)
            #plot(init_locs_df$x, init_locs_df$y)
            ################################################################################

            #### STEP 3 ####
            #  Set ship locations
            #  Pick a ship transect for up and down bay
            rand_up <- sample(1:300, 1)
            one_track_df_up <- all_track_df_up %>%
                                             filter(new_ID == rand_up) %>%
                                             dplyr::select(long, lat)
            rand_down <- sample(1:300, 1)
            one_track_df_down <- all_track_df_down %>%
                                                 filter(new_ID == rand_down) %>%
                                                dplyr::select(long, lat)
            one_track_pts_up <- SpatialPoints(one_track_df_up)
            crs(one_track_pts_up) <- UTM
            one_track_pts_down <- SpatialPoints(one_track_df_down)
            crs(one_track_pts_down) <- UTM
            
            #  Set ship start and end location for one passage
           start_up_bay <- as.data.frame(one_track_df_up) %>%
                                       filter(lat == min(lat)) %>%
                                       dplyr::select(long, lat)
            end_up_bay <- as.data.frame(one_track_df_up) %>%
                                       filter(lat== max(lat)) %>%
                                       dplyr::select(long, lat)
            start_up_bay_xy <- SpatialPoints(start_up_bay)
            crs(start_up_bay_xy) <- UTM
            end_up_bay_xy <- SpatialPoints(end_up_bay)
            crs(end_up_bay_xy) <- UTM

            start_down_bay <- as.data.frame(one_track_df_down) %>%
                                            filter(lat == max(lat)) %>%
                                            dplyr::select(long, lat)
            end_down_bay <- as.data.frame(one_track_df_down) %>%
                                          filter(lat == min(lat)) %>%
                                          dplyr::select(long, lat)
            start_down_bay_xy <- SpatialPoints(start_down_bay)
            crs(start_down_bay_xy) <- UTM
            end_down_bay_xy <- SpatialPoints(end_down_bay)
            crs(end_down_bay_xy) <- UTM

            #   Ship speed limits: 20 knots (10.2889 m per sec)
            #  Time for ship to travel up, down, total at two speeds
            one_track_up_ln <- spLines(one_track_pts_up)
            crs(one_track_up_ln) <- UTM
            track_up_length <- gLength(one_track_up_ln)
            one_track_down_ln <- spLines(one_track_pts_down)
            crs(one_track_down_ln) <- UTM
            track_down_length <- gLength(one_track_down_ln)
            tm_sec_up_20k <- (track_up_length)/10.2889
            tm_sec_down_20k <- (track_down_length)/10.2889
            tm_sec_total_20k <- tm_sec_up_20k + tm_sec_down_20k # 482 min, 8 hr

            #   Time between surfacings, based on data from movement analysyis: median(89 sec), mean (124 sec)
            med_surface_int <- 89
            #mu_surface_int <- 124

            #  Move ship up and down bay along route and determine locations based on distance moved in amount of time
            #   between surfacings
             dist_ship_move_per_surf_20k <- 10.2889*med_surface_int

            num_ship_locs_up_20k <- floor(track_up_length/dist_ship_move_per_surf_20k)
            ship_locs_up_20k <- sp::spsample(one_track_up_ln, n = num_ship_locs_up_20k, type = "regular")
            ship_locs_up_df_20k_tmp1 <- as.data.frame(ship_locs_up_20k)
            ship_locs_up_df_20k_tmp2 <- ship_locs_up_df_20k_tmp1[-1,]
            ship_locs_up_df_20k_tmp3 <- ship_locs_up_df_20k_tmp2[-nrow(ship_locs_up_df_20k_tmp2),]
            ship_locs_up_df_20k <- ship_locs_up_df_20k_tmp3 %>%
                                                    bind_rows(start_up_bay, end_up_bay) %>%
                                                    arrange(lat) %>%
                                                    mutate(loc_num = 1:n())

            num_ship_locs_down_20k <- floor(track_down_length/dist_ship_move_per_surf_20k)
            ship_locs_down_20k <- sp::spsample(one_track_down_ln, n = num_ship_locs_down_20k, type = "regular")
            ship_locs_down_df_20k_tmp1 <- as.data.frame(ship_locs_down_20k)
            ship_locs_down_df_20k_tmp2 <- ship_locs_down_df_20k_tmp1[-1,]
            ship_locs_down_df_20k_tmp3 <- ship_locs_down_df_20k_tmp2[-nrow(ship_locs_down_df_20k_tmp2),]
            ship_locs_down_df_20k <- ship_locs_down_df_20k_tmp3 %>%
                                                        arrange(desc(lat)) %>% 
                                                        bind_rows(start_down_bay, end_down_bay) %>%
                                                        arrange(desc(lat)) %>%
                                                        mutate(loc_num = 1:n())

            #  Designate number of whale locations during time it takes ship to move up and down bay
            nobs_whale_up_20k <- num_ship_locs_up_20k
            nobs_whale_down_20k <- num_ship_locs_down_20k
            ################################################################################

            #### STEP 4 #### use this so switch between movement models for different behaviors
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


            #### STEP 5 ####
           
            # #  Generate simulated step lengths and turn angles for whale movements from parameter 
            # #   estimate posterior distritbutions from movement analysis
            # #   Currently using single movement mode with all data model!!!!
            # niter <- 11000
            # nsamp <- 5000
             
            # keep_1 <- sample(1:niter, nsamp, replace = F)
            # keep_2 <- sample(1:niter, nsamp, replace = F)
            # keep_3 <- sample(1:niter, nsamp, replace = F)
            # chain_1 <- single_fit_dive[[1]]
            # sims_1 <- chain_1[keep_1, c(2, 3, 4, 1)]
            # chain_2 <- single_fit_dive[[2]]
            # sims_2 <- chain_2[keep_2, c(2, 3, 4, 1)]
            # chain_3 <- single_fit_dive[[3]]
            # sims_3 <- chain_3[keep_3, c(2, 3, 4, 1)]
            # sims <- rbind(sims_1, sims_2, sims_3)
            # steps <- numeric(length = nrow(sims))
            # turns <- numeric(length = nrow(sims))
            
            # for(i in 1:nrow(sims)){
              # steps[i] <- rweibull(1, sims[i,3], (1/sims[i,4])^(1/sims[i,3]))
              # turns[i] <- rwcauchy(1, sims[i,1], sims[i,2])
             # }
            
            # mod <- as.data.frame(cbind(steps, turns))
            # mod$turns[mod$turns>pi]=mod$turns[mod$turns>pi]-2*pi

            #  Generate CRW from simulated steps and turns for movement UP bay for 20k speed restriction
            #   Matrices to hold simulations for whale movement UP bay 20k
            mat_X_up <- matrix(nrow = nobs_whale_up_20k, ncol = 20)
            mat_Y_up <- matrix(nrow = nobs_whale_up_20k, ncol = 20)

            for(j in 1:20){
                 keep <- base::sample(1:nrow(mod), nobs_whale_up_20k, replace = FALSE)
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
            loc_num_whale_u <- as.data.frame(rep(1:nobs_whale_up_20k, 20))
            df_XYwhales_up_tmp <- dplyr::bind_cols(df_X_up, df_Y_up, loc_num_whale_u)
            names(df_XYwhales_up_tmp) <- c("whale_ind_num", "X_whale", "walk_num_rep", "Y_whale", "loc_num_whale")
            df_XYwhales_up <- df_XYwhales_up_tmp %>%
                                            dplyr::select(whale_ind_num, X_whale, Y_whale, loc_num_whale) %>%
                                            dplyr::arrange(loc_num_whale)
            #plot(df_XYwhales_up$X_whale, df_XYwhales_up$Y_whale)

            #  Generate CRW from simulated steps and turns for movement UP bay for 20k speed restriction
            #   Matrices to hold simulations for whale movement DOWN bay 20k
            mat_X_down <- matrix(nrow = nobs_whale_down_20k, ncol = 20)
            mat_Y_down <- matrix(nrow = nobs_whale_down_20k, ncol = 20)

            for(k in 1:20){
                 keep <- base::sample(1:nrow(mod), nobs_whale_down_20k, replace = FALSE)
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
            loc_num_whale_d <- as.data.frame(rep(1:nobs_whale_down_20k, 20))
            df_XYwhales_down_tmp <- dplyr::bind_cols(df_X_down, df_Y_down, loc_num_whale_d)
            names(df_XYwhales_down_tmp) <- c("whale_ind_num", "X_whale", "walk_num_rep", "Y_whale", "loc_num_whale")
            df_XYwhales_down <- df_XYwhales_down_tmp %>%
                                                 dplyr::select(whale_ind_num, X_whale, Y_whale, loc_num_whale) %>%
                                                 dplyr::arrange(loc_num_whale)
           ################################################################################


            #### STEP 6 ####
            #  Combine whale movement and ship movement
            df_XYship_up <- ship_locs_up_df_20k[rep(1:nrow(ship_locs_up_df_20k),each=(20)),]
            df_XYboth_up <- bind_cols(df_XYwhales_up, df_XYship_up) %>%
                                        dplyr::rename(X_ship = long, Y_ship = lat)

            df_XYship_down <- ship_locs_down_df_20k[rep(1:nrow(ship_locs_down_df_20k),each=(20)),]
            df_XYboth_down <- bind_cols(df_XYwhales_down, df_XYship_down) %>%
                                             dplyr::rename(X_ship = long, Y_ship = lat)
            ################################################################################


            #### STEP 7 ####
            #  Count overlaps within certain distance!!!!
            up_dists_whales <- cbind(df_XYboth_up$X_whale, df_XYboth_up$Y_whale)
            up_dists_ship <- cbind(df_XYboth_up$X_ship, df_XYboth_up$Y_ship)
            up_dists <- as.data.frame(raster::pointDistance(up_dists_whales, up_dists_ship, longlat = FALSE))
            colnames(up_dists)[1] <- "dist_btw"

            sim_strikes_up <- df_XYboth_up %>%
                                          bind_cols(up_dists) %>%
                                          mutate(sim_strike = ifelse(dist_btw <= strike_dist, 1, 0)) %>%
                                          dplyr::filter(sim_strike==1) %>%
                                          dplyr::select(X_whale, Y_whale)

            down_dists_whales <- cbind(df_XYboth_down$X_whale, df_XYboth_down$Y_whale)
            down_dists_ship <- cbind(df_XYboth_down$X_ship, df_XYboth_down$Y_ship)
            down_dists <- as.data.frame(raster::pointDistance(down_dists_whales, down_dists_ship, longlat = FALSE))
            colnames(down_dists)[1] <- "dist_btw"

            sim_strikes_down <- df_XYboth_down %>%
                                              bind_cols(down_dists) %>%
                                              mutate(sim_strike = ifelse(dist_btw <= strike_dist, 1, 0)) %>%
                                              dplyr::filter(sim_strike==1) %>%
                                              dplyr::select(X_whale, Y_whale)

            strike_locs <- as.data.frame(bind_rows(sim_strikes_up, sim_strikes_down))
            return(strike_locs)
}
################################################################################
#################################################################################