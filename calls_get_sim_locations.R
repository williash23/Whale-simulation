# Sara Williams
# 3/21/2017
# Whale ship strike risk simulation - functions calls to get simulated strike locations at various 
#  distance limits and period lengths (number of years)
#   Have to load script: sim_once_to_start.R before running this one
################################################################################

# Looming question of unreported whale strikes??? In this system it porbably isn't an issue 
#   (supported by the evidence of historical collisions)

#  13 knot ship speed functions
# 1 year
nrep <- 275 #ships per year - 275
strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 50)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_1yr_50m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 25)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_1yr_25m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 10)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_1yr_10m = as.data.frame(do.call(rbind, strike_loc_list))


# 10 years
nrep <- 275*10  #ships per year - 275
strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 50)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_10yr_50m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 25)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_10yr_25m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 10)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_10yr_10m = as.data.frame(do.call(rbind, strike_loc_list))


# 50 years
nrep <- 275*50  #ships per year - 275
strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 50)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_50yr_50m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 25)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_50yr_25m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_13k_loc(mod = post_sims, strike_dist = 10)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_13k_50yr_10m = as.data.frame(do.call(rbind, strike_loc_list))
################################################################################

save(all_strike_loc_13k_1yr_10m, file = "all_strike_loc_13k_1yr_10m.RData") 
save(all_strike_loc_13k_1yr_25m, file = "all_strike_loc_13k_1yr_25m.RData") 
save(all_strike_loc_13k_1yr_50m, file = "all_strike_loc_13k_1yr_50m.RData") 
save(all_strike_loc_13k_10yr_10m, file = "all_strike_loc_13k_10yr_10m.RData") 
save(all_strike_loc_13k_10yr_25m, file = "all_strike_loc_13k_10yr_25m.RData") 
save(all_strike_loc_13k_10yr_50m, file = "all_strike_loc_13k_10yr_50m.RData") 
save(all_strike_loc_13k_50yr_10m, file = "all_strike_loc_13k_50yr_10m.RData") 
save(all_strike_loc_13k_50yr_25m, file = "all_strike_loc_13k_50yr_25m.RData") 
save(all_strike_loc_13k_50yr_50m, file = "all_strike_loc_13k_50yr_50m.RData") 
################################################################################




#  20 knot ship speed functions
# 1 year
nrep <- 275 #ships per year - 275
strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 50)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_1yr_50m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 25)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_1yr_25m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 10)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_1yr_10m = as.data.frame(do.call(rbind, strike_loc_list))


# 10 years
nrep <- 275*10  #ships per year - 275
strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 50)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_10yr_50m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 25)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_10yr_25m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 10)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_10yr_10m = as.data.frame(do.call(rbind, strike_loc_list))


# 50 years
nrep <- 275*50  #ships per year - 275
strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 50)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_50yr_50m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 25)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_50yr_25m = as.data.frame(do.call(rbind, strike_loc_list))

strike_loc_list = list()
for (i in 1:nrep) {
    strike_loc <- sim_ship_strike_fun_20k_loc(mod = post_sims, strike_dist = 10)
    strike_loc_list[[i]] <- strike_loc 
}
all_strike_loc_20k_50yr_10m = as.data.frame(do.call(rbind, strike_loc_list))
################################################################################


save(all_strike_loc_20k_1yr_10m, file = "all_strike_loc_20k_1yr_10m.RData") 
save(all_strike_loc_20k_1yr_25m, file = "all_strike_loc_20k_1yr_25m.RData") 
save(all_strike_loc_20k_1yr_50m, file = "all_strike_loc_20k_1yr_50m.RData") 
save(all_strike_loc_20k_10yr_10m, file = "all_strike_loc_20k_10yr_10m.RData") 
save(all_strike_loc_20k_10yr_25m, file = "all_strike_loc_20k_10yr_25m.RData") 
save(all_strike_loc_20k_10yr_50m, file = "all_strike_loc_20k_10yr_50m.RData") 
save(all_strike_loc_20k_50yr_10m, file = "all_strike_loc_20k_50yr_10m.RData") 
save(all_strike_loc_20k_50yr_25m, file = "all_strike_loc_20k_50yr_25m.RData") 
save(all_strike_loc_20k_50yr_50m, file = "all_strike_loc_20k_50yr_50m.RData") 
################################################################################
#################################################################################