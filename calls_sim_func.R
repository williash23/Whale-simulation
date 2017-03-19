# Sara Williams
# 3/18/2017
# Whale ship strike risk simulation - calls for simulation function #  Call functions
#   Have to load scripts: sim_once_to_start.R and sim_wrap-... before running this one
################################################################################



nrep <- 5 #275 * 10 # 275 ships per year for 10 years
rep_sim_strikes <- as.vector(rep(0, nrep))
#pb <- tkProgressBar(title = "progress bar", min = 0, max = nrep, width = 300)

for(i in 1:nrep){
  rep_sim_strikes[i] <- sim_ship_strike_fun(mod = post_sims, strike_dist = 50)
   #setTkProgressBar(pb, i, label=paste( round(i/total*100, 0), "% done"))
  }

save(rep_sim_strikes_100k_sims, file = "rep_sim_strikes_100k_sims.RData") # within 50m


nrep <- 1000
reach_1_strike <- as.vector(rep(0, nrep))

for(j in 1:nrep){
    iter <- 0
    while(TRUE){
        iter <-iter +1
        sing_rep <- sim_ship_strike_fun(mod = post_sims, strike_dist = 50)
        if(sing_rep > 0) break
      } 
      reach_1_strike[j] <- iter
    }

save(reach_1_strike, file = "reach_1_strike_25m.RData")
    
#################################################################################