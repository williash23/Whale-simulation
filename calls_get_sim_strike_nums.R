# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - functions calls to get numbers of simulated strike at various 
#  distance limits and over different period lengths (number of years)
#   Have to load scripts: sim_once_to_start.R and sim_wrap-... before running this one
################################################################################
setwd("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output")


#### 13k speed ####


#  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 275 #ships per year - 275 * # number of years

rep_sim_strikes_10m_13k_1yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_25m_13k_1yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_50m_13k_1yr <- as.vector(rep(NA, nrep))

  for(i in 1:nrep){
    rep_sim_strikes_10m_13k_1yr[i] <- sim_ship_strike_fun_13k(strike_dist = 10)
    rep_sim_strikes_25m_13k_1yr[i] <- sim_ship_strike_fun_13k(strike_dist = 25)
    rep_sim_strikes_50m_13k_1yr[i] <- sim_ship_strike_fun_13k(strike_dist = 50)
    }


#  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 2750  #ships per year - 275 * 10 # number of years
rep_sim_strikes_10m_13k_10yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_25m_13k_10yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_50m_13k_10yr <- as.vector(rep(NA, nrep))

  for(i in 1:nrep){
    rep_sim_strikes_10m_13k_10yr[i] <- sim_ship_strike_fun_13k(strike_dist = 10)
    rep_sim_strikes_25m_13k_10yr[i] <- sim_ship_strike_fun_13k(strike_dist = 25)
    rep_sim_strikes_50m_13k_10yr[i] <- sim_ship_strike_fun_13k(strike_dist = 50)
    }


#  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 13750 #ships per year - 275 * 50 # number of years
rep_sim_strikes_10m_13k_50yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_25m_13k_50yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_50m_13k_50yr <- as.vector(rep(NA, nrep))

  for(i in 1:nrep){
    rep_sim_strikes_10m_13k_50yr[i] <- sim_ship_strike_fun_13k(strike_dist = 10)
    rep_sim_strikes_25m_13k_50yr[i] <- sim_ship_strike_fun_13k(strike_dist = 25)
    rep_sim_strikes_50m_13k_50yr[i] <- sim_ship_strike_fun_13k(strike_dist = 50)
    }
 

 
 
 #### 20k speed ####


#  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 275 #ships per year - 275 * 1 # number of years

rep_sim_strikes_10m_20k_1yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_25m_20k_1yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_50m_20k_1yr <- as.vector(rep(NA, nrep))

  for(i in 1:nrep){
    rep_sim_strikes_10m_20k_1yr[i] <- sim_ship_strike_fun_20k(strike_dist = 10)
    rep_sim_strikes_25m_20k_1yr[i] <- sim_ship_strike_fun_20k(strike_dist = 25)
    rep_sim_strikes_50m_20k_1yr[i] <- sim_ship_strike_fun_20k(strike_dist = 50)
    }
 
 
 #  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 2750 #ships per year - 275* 10 # number of years

rep_sim_strikes_10m_20k_10yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_25m_20k_10yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_50m_20k_10yr <- as.vector(rep(NA, nrep))

  for(i in 1:nrep){
    rep_sim_strikes_10m_20k_10yr[i] <- sim_ship_strike_fun_20k(strike_dist = 10)
    rep_sim_strikes_25m_20k_10yr[i] <- sim_ship_strike_fun_20k(strike_dist = 25)
    rep_sim_strikes_50m_20k_10yr[i] <- sim_ship_strike_fun_20k(strike_dist = 50)
    }


  #  Iterations over number of ships (nrep = 1 up and down bay) per year and number of years 
nrep <- 13750 #ships per year - 275*50 # number of years

rep_sim_strikes_10m_20k_50yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_25m_20k_50yr <- as.vector(rep(NA, nrep))
rep_sim_strikes_50m_20k_50yr <- as.vector(rep(NA, nrep))

  for(i in 1:nrep){
    rep_sim_strikes_10m_20k_50yr[i] <- sim_ship_strike_fun_20k(strike_dist = 10)
    rep_sim_strikes_25m_20k_50yr[i] <- sim_ship_strike_fun_20k(strike_dist = 25)
    rep_sim_strikes_50m_20k_50yr[i] <- sim_ship_strike_fun_20k(strike_dist = 50)
    }
################################################################################


save(rep_sim_strikes_10m_13k_1yr, file = "rep_sim_strikes_10m_13k_1yr.RData") 
save(rep_sim_strikes_25m_13k_1yr, file = "rep_sim_strikes_25m_13k_1yr.RData") 
save(rep_sim_strikes_50m_13k_1yr, file = "rep_sim_strikes_50m_13k_1yr.RData") 
save(rep_sim_strikes_10m_13k_10yr, file = "rep_sim_strikes_10m_13k_10yr.RData") 
save(rep_sim_strikes_25m_13k_10yr, file = "rep_sim_strikes_25m_13k_10yr.RData") 
save(rep_sim_strikes_50m_13k_10yr, file = "rep_sim_strikes_50m_13k_10yr.RData") 
save(rep_sim_strikes_10m_13k_50yr, file = "rep_sim_strikes_10m_13k_50yr.RData") 
save(rep_sim_strikes_25m_13k_50yr, file = "rep_sim_strikes_25m_13k_50yr.RData") 
save(rep_sim_strikes_50m_13k_50yr, file = "rep_sim_strikes_50m_13k_50yr.RData") 


save(rep_sim_strikes_10m_20k_1yr, file = "rep_sim_strikes_10m_20k_1yr.RData") 
save(rep_sim_strikes_25m_20k_1yr, file = "rep_sim_strikes_25m_20k_1yr.RData") 
save(rep_sim_strikes_50m_20k_1yr, file = "rep_sim_strikes_50m_20k_1yr.RData") 
save(rep_sim_strikes_10m_20k_10yr, file = "rep_sim_strikes_10m_20k_10yr.RData") 
save(rep_sim_strikes_25m_20k_10yr, file = "rep_sim_strikes_25m_20k_10yr.RData") 
save(rep_sim_strikes_50m_20k_10yr, file = "rep_sim_strikes_50m_20k_10yr.RData") 
save(rep_sim_strikes_10m_20k_50yr, file = "rep_sim_strikes_10m_20k_50yr.RData") 
save(rep_sim_strikes_25m_20k_50yr, file = "rep_sim_strikes_25m_20k_50yr.RData") 
save(rep_sim_strikes_50m_20k_50yr, file = "rep_sim_strikes_50m_20k_50yr.RData") 
#################################################################################





#  Number of iterations it takes to reach 1 strike
nrep <- 1000
# break at 275*10 years 

#  10m; 13k ship speed
reach_1_strike_10m_13k_100rep <- as.vector(rep(NA, nrep))
sing_rep <- as.vector(rep(0, 2750))

for(j in 1:nrep){
    iter <- 0
    for(i in 1:2750){
        iter <-iter +1
        sing_rep[i] <- sim_ship_strike_fun_13k(strike_dist = 10)
        if(sing_rep[i] > 0) break
      } 
      reach_1_strike_10m_13k_100rep[j] <- iter
    }

#  25m; 13k ship speed
reach_1_strike_25m_13k_100rep <- as.vector(rep(NA, nrep))
sing_rep <- as.vector(rep(0, 2750))

for(j in 1:nrep){
    iter <- 0
    for(i in 1:2750){
        iter <-iter +1
        sing_rep[i]<- sim_ship_strike_fun_13k(strike_dist = 25)
        if(sing_rep[i] > 0) break
      } 
      reach_1_strike_25m_13k_100rep[j] <- iter
    }

#  50m; 13k ship speed
reach_1_strike_50m_13k_100rep <- as.vector(rep(NA, nrep))
sing_rep <- as.vector(rep(0, 2750))

for(j in 1:nrep){
    iter <- 0
    for(i in 1:2750){
        iter <-iter +1
        sing_rep[i] <- sim_ship_strike_fun_13k(strike_dist = 25)
        if(sing_rep[i] > 0) break
      } 
      reach_1_strike_50m_13k_100rep[j] <- iter
    }


#  10m; 20k ship speed
reach_1_strike_10m_20k_100rep <- as.vector(rep(NA, nrep))
sing_rep <- as.vector(rep(0, 2750))

for(j in 1:nrep){
    iter <- 0
    for(i in 1:2750){
        iter <-iter +1
        sing_rep[i] <- sim_ship_strike_fun_20k(strike_dist = 10)
        if(sing_rep[i] > 0) break
      } 
      reach_1_strike_10m_20k_100rep[j] <- iter
    }

#  25m; 20k ship speed
reach_1_strike_25m_20k_100rep <- as.vector(rep(NA, nrep))
sing_rep <- as.vector(rep(0, 2750))

for(j in 1:nrep){
    iter <- 0
    for(i in 1:2750){
        iter <-iter +1
        sing_rep[i] <- sim_ship_strike_fun_20k(strike_dist = 25)
        if(sing_rep[i] > 0) break
      } 
      reach_1_strike_25m_20k_100rep[j] <- iter
    }

#  50m; 20k ship speed
reach_1_strike_50m_20k_100rep <- as.vector(rep(NA, nrep))
sing_rep <- as.vector(rep(0, 2750))

for(j in 1:nrep){
    iter <- 0
    for(i in 1:2750){
        iter <-iter +1
        sing_rep[i] <- sim_ship_strike_fun_20k(strike_dist = 25)
        if(sing_rep[i] > 0) break
      } 
      reach_1_strike_50m_20k_100rep[j] <- iter
    }

################################################################################

save(reach_1_strike_10m_13k_100rep, file = "reach_1_strike_10m_13k_100rep.RData")
save(reach_1_strike_25m_13k_100rep, file = "reach_1_strike_25m_13k_100rep.RData")
save(reach_1_strike_50m_13k_100rep, file = "reach_1_strike_50m_13k_100rep.RData")
save(reach_1_strike_10m_20k_100rep, file = "reach_1_strike_10m_20k_100rep.RData")
save(reach_1_strike_25m_20k_100rep, file = "reach_1_strike_25m_20k_100rep.RData")
save(reach_1_strike_50m_20k_100rep, file = "reach_1_strike_50m_20k_100rep.RData")


save(reach_1_strike_10m_13k_1000rep, file = "reach_1_strike_10m_13k_1000rep.RData")
save(reach_1_strike_25m_13k_1000rep, file = "reach_1_strike_25m_13k_1000rep.RData")
save(reach_1_strike_50m_13k_1000rep, file = "reach_1_strike_50m_13k_1000rep.RData")
save(reach_1_strike_10m_20k_1000rep, file = "reach_1_strike_10m_20k_1000rep.RData")
save(reach_1_strike_25m_20k_1000rep, file = "reach_1_strike_25m_20k_1000rep.RData")
save(reach_1_strike_50m_20k_1000rep, file = "reach_1_strike_50m_20k_1000rep.RData")
#################################################################################
#################################################################################




















#load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/rep_sim_strikes_100k_sims.RData") # within 50m
#load("C:/Users/sara.williams/Documents/GitHub/Whale-simulation/output/reach_1_strike_10m_13km.RData") # within 50m

