# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - functions calls to get numbers of simulated strike at various 
#  distance limits and over different period lengths (number of years)
#   Have to load scripts: sim_once_to_start.R and sim_wrap-... before running this one
################################################################################

#  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 275 #ships per year - 275
niter <- 100 # number of years

#  10m; 13k ship speed
rep_sim_strikes_10m_13k <- as.vector(rep(0, nrep))
prop_strikes_per_year_10m_13k <- as.vector(rep(0, niter))

for(j in 1:niter){
  for(i in 1:nrep){
    rep_sim_strikes_10m_13k[i] <- sim_ship_strike_fun_13k(mod = post_sims, strike_dist = 10)
    }
  prop_strikes_per_year_10m_13k[j] <- sum(rep_sim_strikes_10m_13k)/nrep
}

#  10m; 20k ship speed
rep_sim_strikes_10m_20k <- as.vector(rep(0, nrep))
prop_strikes_per_year_10m_20k <- as.vector(rep(0, niter))

for(j in 1:niter){
  for(i in 1:nrep){
    rep_sim_strikes_10m_20k[i] <- sim_ship_strike_fun_20k(mod = post_sims, strike_dist = 10)
    }
  prop_strikes_per_year_10m_20k[j] <- sum(rep_sim_strikes_10m_20k)/nrep
}

#  25m; 13k ship speed
rep_sim_strikes_25m_13k <- as.vector(rep(0, nrep))
prop_strikes_per_year_25m_13k <- as.vector(rep(0, niter))

for(j in 1:niter){
  for(i in 1:nrep){
    rep_sim_strikes_25m_13k[i] <- sim_ship_strike_fun_13k(mod = post_sims, strike_dist = 25)
    }
  prop_strikes_per_year_25m_13k[j] <- sum(rep_sim_strikes_25m_13k)/nrep
}

#  25m; 20k ship speed
rep_sim_strikes_25m_20k <- as.vector(rep(0, nrep))
prop_strikes_per_year_25m_20k <- as.vector(rep(0, niter))

for(j in 1:niter){
  for(i in 1:nrep){
    rep_sim_strikes_25m_20k[i] <- sim_ship_strike_fun_20k(mod = post_sims, strike_dist = 25)
    }
  prop_strikes_per_year_25m_20k[j] <- sum(rep_sim_strikes_25m_20k)/nrep
}

#  50m; 13k ship speed
rep_sim_strikes_50m_13k <- as.vector(rep(0, nrep))
prop_strikes_per_year_50m_13k <- as.vector(rep(0, niter))

for(j in 1:niter){
  for(i in 1:nrep){
    rep_sim_strikes_50m_13k[i] <- sim_ship_strike_fun_13k(mod = post_sims, strike_dist = 50)
    }
  prop_strikes_per_year_50m_13k[j] <- sum(rep_sim_strikes_50m_13k)/nrep
}

#  50m; 20k ship speed
rep_sim_strikes_50m_20k <- as.vector(rep(0, nrep))
prop_strikes_per_year_50m_20k <- as.vector(rep(0, niter))

for(j in 1:niter){
  for(i in 1:nrep){
    rep_sim_strikes_50m_20k[i] <- sim_ship_strike_fun_20k(mod = post_sims, strike_dist = 50)
    }
  prop_strikes_per_year_50m_20k[j] <- sum(rep_sim_strikes_50m_20k)/nrep
 }
################################################################################


#   Save simulations for 10 year period
# rep_sim_strikes_10m_13k_10y <- rep_sim_strikes_10m_13k
# rep_sim_strikes_10m_20k_10y <- rep_sim_strikes_10m_20k
# rep_sim_strikes_25m_13k_10y <- rep_sim_strikes_25m_13k
# rep_sim_strikes_25m_20k_10y <- rep_sim_strikes_25m_20k
# rep_sim_strikes_50m_13k_10y <- rep_sim_strikes_50m_13k
# rep_sim_strikes_50m_20k_10y <- rep_sim_strikes_50m_20k

# prop_strikes_per_year_10m_13k_10y <- prop_strikes_per_year_10m_13k
# prop_strikes_per_year_10m_20k_10y <- prop_strikes_per_year_10m_20k
# prop_strikes_per_year_25m_13k_10y <- prop_strikes_per_year_25m_13k
# prop_strikes_per_year_25m_20k_10y <- prop_strikes_per_year_25m_20k
# prop_strikes_per_year_50m_13k_10y <- prop_strikes_per_year_50m_13k
# prop_strikes_per_year_50m_20k_10y <- prop_strikes_per_year_50m_20k


#   Save simulations for 50 year period
# rep_sim_strikes_10m_13k_50y <- rep_sim_strikes_10m_13k
# rep_sim_strikes_10m_20k_50y <- rep_sim_strikes_10m_20k
# rep_sim_strikes_25m_13k_50y <- rep_sim_strikes_25m_13k
# rep_sim_strikes_25m_20k_50y <- rep_sim_strikes_25m_20k
# rep_sim_strikes_50m_13k_50y <- rep_sim_strikes_50m_13k
# rep_sim_strikes_50m_20k_50y <- rep_sim_strikes_50m_20k

# prop_strikes_per_year_10m_13k_50y <- prop_strikes_per_year_10m_13k
# prop_strikes_per_year_10m_20k_50y <- prop_strikes_per_year_10m_20k
# prop_strikes_per_year_25m_13k_50y <- prop_strikes_per_year_25m_13k
# prop_strikes_per_year_25m_20k_50y <- prop_strikes_per_year_25m_20k
# prop_strikes_per_year_50m_13k_50y <- prop_strikes_per_year_50m_13k
# prop_strikes_per_year_50m_20k_50y <- prop_strikes_per_year_50m_20k


# #   Save simulations for 100 year period
# rep_sim_strikes_10m_13k_100y <- rep_sim_strikes_10m_13k
# rep_sim_strikes_10m_20k_100y <- rep_sim_strikes_10m_20k
# rep_sim_strikes_25m_13k_100y <- rep_sim_strikes_25m_13k
# rep_sim_strikes_25m_20k_100y <- rep_sim_strikes_25m_20k
# rep_sim_strikes_50m_13k_100y <- rep_sim_strikes_50m_13k
# rep_sim_strikes_50m_20k_100y <- rep_sim_strikes_50m_20k

# prop_strikes_per_year_10m_13k_100y <- prop_strikes_per_year_10m_13k
# prop_strikes_per_year_10m_20k_100y <- prop_strikes_per_year_10m_20k
# prop_strikes_per_year_25m_13k_100y <- prop_strikes_per_year_25m_13k
# prop_strikes_per_year_25m_20k_100y <- prop_strikes_per_year_25m_20k
# prop_strikes_per_year_50m_13k_100y <- prop_strikes_per_year_50m_13k
# prop_strikes_per_year_50m_20k_100y <- prop_strikes_per_year_50m_20k
# ################################################################################


# save(rep_sim_strikes_10m_13k_10y, file = "rep_sim_strikes_10m_13k_10y.RData") 
# save(rep_sim_strikes_25m_13k_10y, file = "rep_sim_strikes_25m_13k_10y.RData") 
# save(rep_sim_strikes_50m_13k_10y, file = "rep_sim_strikes_50m_13k_10y.RData") 
# save(rep_sim_strikes_10m_20k_10y, file = "rep_sim_strikes_10m_20k_10y.RData") 
# save(rep_sim_strikes_25m_20k_10y, file = "rep_sim_strikes_25m_20k_10y.RData") 
# save(rep_sim_strikes_50m_20k_10y, file = "rep_sim_strikes_50m_20k_10y.RData") 

# save(prop_strikes_per_year_10m_13k_10y, file = "prop_strikes_per_year_10m_13k_10y.RData") 
# save(prop_strikes_per_year_25m_13k_10y, file = "prop_strikes_per_year_25m_13k_10y.RData") 
# save(prop_strikes_per_year_50m_13k_10y, file = "prop_strikes_per_year_50m_13k_10y.RData") 
# save(prop_strikes_per_year_10m_20k_10y, file = "prop_strikes_per_year_10m_20k_10y.RData") 
# save(prop_strikes_per_year_25m_20k_10y, file = "prop_strikes_per_year_25m_20k_10y.RData") 
# save(prop_strikes_per_year_50m_20k_10y, file = "prop_strikes_per_year_50m_20k_10y.RData") 

# save(rep_sim_strikes_10m_13k_50y, file = "rep_sim_strikes_10m_13k_50y.RData") 
# save(rep_sim_strikes_25m_13k_50y, file = "rep_sim_strikes_25m_13k_50y.RData") 
# save(rep_sim_strikes_50m_13k_50y, file = "rep_sim_strikes_50m_13k_50y.RData") 
# save(rep_sim_strikes_10m_20k_50y, file = "rep_sim_strikes_10m_20k_50y.RData") 
# save(rep_sim_strikes_25m_20k_50y, file = "rep_sim_strikes_25m_20k_50y.RData") 
# save(rep_sim_strikes_50m_20k_50y, file = "rep_sim_strikes_50m_20k_50y.RData") 

# save(prop_strikes_per_year_10m_13k_50y, file = "prop_strikes_per_year_10m_13k_50y.RData") 
# save(prop_strikes_per_year_25m_13k_50y, file = "prop_strikes_per_year_25m_13k_50y.RData") 
# save(prop_strikes_per_year_50m_13k_50y, file = "prop_strikes_per_year_50m_13k_50y.RData") 
# save(prop_strikes_per_year_10m_20k_50y, file = "prop_strikes_per_year_10m_20k_50y.RData") 
# save(prop_strikes_per_year_25m_20k_50y, file = "prop_strikes_per_year_25m_20k_50y.RData") 
# save(prop_strikes_per_year_50m_20k_50y, file = "prop_strikes_per_year_50m_20k_50y.RData") 

# save(rep_sim_strikes_10m_13k_100y, file = "rep_sim_strikes_10m_13k_100y.RData") 
# save(rep_sim_strikes_25m_13k_100y, file = "rep_sim_strikes_25m_13k_100y.RData") 
# save(rep_sim_strikes_50m_13k_100y, file = "rep_sim_strikes_50m_13k_100y.RData") 
# save(rep_sim_strikes_10m_20k_100y, file = "rep_sim_strikes_10m_20k_100y.RData") 
# save(rep_sim_strikes_25m_20k_100y, file = "rep_sim_strikes_25m_20k_100y.RData") 
# save(rep_sim_strikes_50m_20k_100y, file = "rep_sim_strikes_50m_20k_100y.RData") 

# save(prop_strikes_per_year_10m_13k_100y, file = "prop_strikes_per_year_10m_13k_100y.RData") 
# save(prop_strikes_per_year_25m_13k_100y, file = "prop_strikes_per_year_25m_13k_100y.RData") 
# save(prop_strikes_per_year_50m_13k_100y, file = "prop_strikes_per_year_50m_13k_100y.RData") 
# save(prop_strikes_per_year_10m_20k_100y, file = "prop_strikes_per_year_10m_20k_100y.RData") 
# save(prop_strikes_per_year_25m_20k_100y, file = "prop_strikes_per_year_25m_20k_100y.RData") 
# save(prop_strikes_per_year_50m_20k_100y, file = "prop_strikes_per_year_50m_20k_100y.RData") 
# #################################################################################





#  Number of iterations it takes to reach 1 strike
nrep <- 100

#  10m; 13k ship speed
reach_1_strike_10m_13k <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun_13k(mod = post_sims, strike_dist = 10)
        if(sing_rep > 0) break
      } 
      reach_1_strike_10m_13k[j] <- iter
    }

#  10m; 20k ship speed
reach_1_strike_10m_20k <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun_20k(mod = post_sims, strike_dist = 10)
        if(sing_rep > 0) break
      } 
      reach_1_strike_10m_20k[j] <- iter
    }


#  25m; 13k ship speed
reach_1_strike_25m_13k <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun_13k(mod = post_sims, strike_dist = 25)
        if(sing_rep > 0) break
      } 
      reach_1_strike_25m_13k[j] <- iter
    }

#  25m; 20k ship speed
reach_1_strike_25m_20k <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun_20k(mod = post_sims, strike_dist = 25)
        if(sing_rep > 0) break
      } 
      reach_1_strike_25m_20k[j] <- iter
    }


#  50m; 13k ship speed
reach_1_strike_50m_13k <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun_13k(mod = post_sims, strike_dist = 50)
        if(sing_rep > 0) break
      } 
      reach_1_strike_50m_13k[j] <- iter
    }

#  25m; 20k ship speed
nrep <- 1000
reach_1_strike_50m_20k <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun_20k(mod = post_sims, strike_dist = 50)
        if(sing_rep > 0) break
      } 
      reach_1_strike_50m_20k[j] <- iter
    }
################################################################################

save(reach_1_strike_10m_13k, file = "reach_1_strike_10m_13k.RData")
save(reach_1_strike_25m_13k, file = "reach_1_strike_25m_13k.RData")
save(reach_1_strike_50m_13k, file = "reach_1_strike_50m_13k.RData") 
save(reach_1_strike_10m_20k, file = "reach_1_strike_10m_20k.RData") 
save(reach_1_strike_25m_20k, file = "reach_1_strike_25m_20k.RData") 
save(reach_1_strike_50m_20k, file = "reach_1_strike_50m_20k.RData") 
#################################################################################
#################################################################################















# tot_sim_strikes_10m_13k_10y <- sum(rep_sim_strikes_10m_13k)
# tot_sim_strikes_10m_20k_10y <- sum(rep_sim_strikes_10m_20k)
# tot_sim_strikes_25m_13k_10y <- sum(rep_sim_strikes_25m_13k)
# tot_sim_strikes_25m_20k_10y <- sum(rep_sim_strikes_25m_20k)
# tot_sim_strikes_50m_13k_10y <- sum(rep_sim_strikes_50m_13k)
# tot_sim_strikes_50m_20k_10y <- sum(rep_sim_strikes_50m_20k)

# tot_sim_strikes_10m_13k_50y <- sum(rep_sim_strikes_10m_13k)
# tot_sim_strikes_10m_20k_50y <- sum(rep_sim_strikes_10m_20k)
# tot_sim_strikes_25m_13k_50y <- sum(rep_sim_strikes_25m_13k)
# tot_sim_strikes_25m_20k_50y <- sum(rep_sim_strikes_25m_20k)
# tot_sim_strikes_50m_13k_50y <- sum(rep_sim_strikes_50m_13k)
# tot_sim_strikes_50m_20k_50y <- sum(rep_sim_strikes_50m_20k)

# tot_sim_strikes_10m_13k_100y <- sum(rep_sim_strikes_10m_13k)
# tot_sim_strikes_10m_20k_100y <- sum(rep_sim_strikes_10m_20k)
# tot_sim_strikes_25m_13k_100y <- sum(rep_sim_strikes_25m_13k)
# tot_sim_strikes_25m_20k_100y <- sum(rep_sim_strikes_25m_20k)
# tot_sim_strikes_50m_13k_100y <- sum(rep_sim_strikes_50m_13k)
# tot_sim_strikes_50m_20k_100y <- sum(rep_sim_strikes_50m_20k)

# save(tot_sim_strikes_10m_13k_10y, file = "tot_sim_strikes_10m_13k_10y.RData") 
# save(tot_sim_strikes_25m_13k_10y, file = "tot_sim_strikes_25m_13k_10y.RData") 
# save(tot_sim_strikes_50m_13k_10y, file = "tot_sim_strikes_50m_13k_10y.RData") 
# save(tot_sim_strikes_10m_20k_10y, file = "tot_sim_strikes_10m_20k_10y.RData") 
# save(tot_sim_strikes_25m_20k_10y, file = "tot_sim_strikes_25m_20k_10y.RData") 
# save(tot_sim_strikes_50m_20k_10y, file = "tot_sim_strikes_50m_20k_10y.RData") 

# save(tot_sim_strikes_10m_13k_50y, file = "tot_sim_strikes_10m_13k_50y.RData") 
# save(tot_sim_strikes_25m_13k_50y, file = "tot_sim_strikes_25m_13k_50y.RData") 
# save(tot_sim_strikes_50m_13k_50y, file = "tot_sim_strikes_50m_13k_50y.RData") 
# save(tot_sim_strikes_10m_20k_50y, file = "tot_sim_strikes_10m_20k_50y.RData") 
# save(tot_sim_strikes_25m_20k_50y, file = "tot_sim_strikes_25m_20k_50y.RData") 
# save(tot_sim_strikes_50m_20k_50y, file = "tot_sim_strikes_25m_20k_50y.RData") 

# save(tot_sim_strikes_10m_13k_100y, file = "tot_sim_strikes_10m_13k_100y.RData") 
# save(tot_sim_strikes_25m_13k_100y, file = "tot_sim_strikes_25m_13k_100y.RData") 
# save(tot_sim_strikes_50m_13k_100y, file = "tot_sim_strikes_50m_13k_100y.RData") 
# save(tot_sim_strikes_10m_20k_100y, file = "tot_sim_strikes_10m_20k_100y.RData") 
# save(tot_sim_strikes_25m_20k_100y, file = "tot_sim_strikes_25m_20k_100y.RData") 
# save(tot_sim_strikes_50m_20k_100y, file = "tot_sim_strikes_25m_20k_100y.RData") 




#load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_100k_sims.RData") # within 50m
#load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_10m_13km.RData") # within 50m

