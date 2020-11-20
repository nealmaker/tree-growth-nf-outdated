########## NOTE!!!! ##########
# One call to partial (ice = T) takes about 80 min with full training set.
# One model's worth (16 vars) should take ~22 hrs.

memory.limit(size=56000)

#######################
## Example ICE plot 
#######################

# Too slow! Can I use arg "train" to calculate for a sample of the training data?
dbh_pdp_cr <- pdp::partial(dbh_growth_model_full, pred.var = "cr", ice = T)
# Yes!!! This works and is way faster:
dbh_pdp_cr_samp <- pdp::partial(dbh_growth_model_full, pred.var = "cr", ice = T, 
                            train = dbh_growth_model_full$
                              trainingData[sample(1:nrow(dbh_growth_model_full$
                                                           trainingData),
                                                  30), -17])

# Sample from pdp object so there aren't so many lines on plot (for full model)
ids <- sample(unique(dbh_pdp_dbh$yhat.id), 200, replace = F)
dbh_pdp_dbh_samp2 <- dbh_pdp_dbh[dbh_pdp_dbh$yhat.id %in% ids,]

# using lattice graph. autoplot() does ggplot2, but has no alpha arg
pdp::plotPartial(dbh_pdp_dbh_samp2, pdp.lwd = 4, pdp.col = "#386cb0", alpha = .3)


#########################################
## DBH Growth (inches per year)
#########################################

dbh_ice <- vector(mode = "list", length = 16)
dbh_ice_c <- vector(mode = "list", length = 16)
dbh_pdp_s <- vector(mode = "list", length = 16)

# ICE plots
# 4 minutes run time
for(i in 1:16){
  dbh_ice[[i]] <- 
    pdp::partial(dbh_growth_model_full, 
                 pred.var = names(dbh_growth_model_full$trainingData)[[i]], 
                 ice = T, 
                 train = dbh_growth_model_full$
                   trainingData[sample(1:nrow(dbh_growth_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Centered ICE plots
for(i in 1:16){
  dbh_ice_c[[i]] <- 
    pdp::partial(dbh_growth_model_full, 
                 pred.var = names(dbh_growth_model_full$trainingData)[[i]], 
                 ice = T, center = T,
                 train = dbh_growth_model_full$
                   trainingData[sample(1:nrow(dbh_growth_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Single factor pdps
for(i in 1:16){
  dbh_pdp_s[[i]] <- 
    pdp::partial(dbh_growth_model_full, 
                 pred.var = names(dbh_growth_model_full$trainingData)[[i]], 
                 train = dbh_growth_model_full$
                   trainingData[sample(1:nrow(dbh_growth_model_full$trainingData),
                                       1000), -17]) # 1000 lines averaged
}

save(dbh_ice, dbh_ice_c, dbh_pdp_s, file = "rda/dbh-ice.rda")


#########################################
## Height (feet)
#########################################

ht_ice <- vector(mode = "list", length = 16)
ht_ice_c <- vector(mode = "list", length = 16)
ht_pdp_s <- vector(mode = "list", length = 16)

# ICE plots
for(i in 1:16){
  ht_ice[[i]] <- 
    pdp::partial(ht_model_full, 
                 pred.var = names(ht_model_full$trainingData)[[i]], 
                 ice = T, 
                 train = ht_model_full$
                   trainingData[sample(1:nrow(ht_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Centered ICE plots
for(i in 1:16){
  ht_ice_c[[i]] <- 
    pdp::partial(ht_model_full, 
                 pred.var = names(ht_model_full$trainingData)[[i]], 
                 ice = T, center = T,
                 train = ht_model_full$
                   trainingData[sample(1:nrow(ht_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Single factor pdps
for(i in 1:16){
  ht_pdp_s[[i]] <- 
    pdp::partial(ht_model_full, 
                 pred.var = names(ht_model_full$trainingData)[[i]], 
                 train = ht_model_full$
                   trainingData[sample(1:nrow(ht_model_full$trainingData),
                                       1000), -17]) # 1000 lines averaged
}

save(ht_ice, ht_ice_c, ht_pdp_s, file = "rda/ht-ice.rda")


#########################################
## Height Growth (feet per year)
#########################################

ht_growth_ice <- vector(mode = "list", length = 16)
ht_growth_ice_c <- vector(mode = "list", length = 16)
ht_growth_pdp_s <- vector(mode = "list", length = 16)

# ICE plots
# 4 minutes run time
for(i in 1:16){
  ht_growth_ice[[i]] <- 
    pdp::partial(ht_growth_model_full, 
                 pred.var = names(ht_growth_model_full$trainingData)[[i]], 
                 ice = T, 
                 train = ht_growth_model_full$
                   trainingData[sample(1:nrow(ht_growth_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Centered ICE plots
for(i in 1:16){
  ht_growth_ice_c[[i]] <- 
    pdp::partial(ht_growth_model_full, 
                 pred.var = names(ht_growth_model_full$trainingData)[[i]], 
                 ice = T, center = T,
                 train = ht_growth_model_full$
                   trainingData[sample(1:nrow(ht_growth_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Single factor pdps
for(i in 1:16){
  ht_growth_pdp_s[[i]] <- 
    pdp::partial(ht_growth_model_full, 
                 pred.var = names(ht_growth_model_full$trainingData)[[i]], 
                 train = ht_growth_model_full$
                   trainingData[sample(1:nrow(ht_growth_model_full$trainingData),
                                       1000), -17]) # 1000 lines averaged
}

save(ht_growth_ice, ht_growth_ice_c, ht_growth_pdp_s, file = "rda/ht-growth-ice.rda")


#########################################
## CR Growth (change in percent per year)
#########################################

cr_ice <- vector(mode = "list", length = 16)
cr_ice_c <- vector(mode = "list", length = 16)
cr_pdp_s <- vector(mode = "list", length = 16)

# ICE plots
for(i in 1:16){
  cr_ice[[i]] <- 
    pdp::partial(cr_growth_model_full, 
                 pred.var = names(cr_growth_model_full$trainingData)[[i]], 
                 ice = T, 
                 train = cr_growth_model_full$
                   trainingData[sample(1:nrow(cr_growth_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Centered ICE plots
for(i in 1:16){
  cr_ice_c[[i]] <- 
    pdp::partial(cr_growth_model_full, 
                 pred.var = names(cr_growth_model_full$trainingData)[[i]], 
                 ice = T, center = T,
                 train = cr_growth_model_full$
                   trainingData[sample(1:nrow(cr_growth_model_full$trainingData),
                                       200), -17]) # 200 lines per plot
}

# Single factor pdps
for(i in 1:16){
  cr_pdp_s[[i]] <- 
    pdp::partial(cr_growth_model_full, 
                 pred.var = names(cr_growth_model_full$trainingData)[[i]], 
                 train = cr_growth_model_full$
                   trainingData[sample(1:nrow(cr_growth_model_full$trainingData),
                                       1000), -17]) # 1000 lines averaged
}

save(cr_ice, cr_ice_c, cr_pdp_s, file = "rda/cr-ice.rda")


#########################################
## Mortality (prob. of dying)
#########################################

mort_ice <- vector(mode = "list", length = 17)
mort_ice_c <- vector(mode = "list", length = 17)
mort_pdp_s <- vector(mode = "list", length = 17)

# ICE plots
for(i in 1:17){
  mort_ice[[i]] <- 
    pdp::partial(mort_model_full, 
                 pred.var = names(mort_model_full$trainingData)[[i]], 
                 ice = T, prob = T,
                 train = mort_model_full$
                   trainingData[sample(1:nrow(mort_model_full$trainingData),
                                       200), -18]) # 200 lines per plot
}

# Centered ICE plots
for(i in 1:17){
  mort_ice_c[[i]] <- 
    pdp::partial(mort_model_full, 
                 pred.var = names(mort_model_full$trainingData)[[i]], 
                 ice = T, center = T, prob = T,
                 train = mort_model_full$
                   trainingData[sample(1:nrow(mort_model_full$trainingData),
                                       200), -18]) # 200 lines per plot
}

# Single factor pdps
for(i in 1:17){
  mort_pdp_s[[i]] <- 
    pdp::partial(mort_model_full, 
                 pred.var = names(mort_model_full$trainingData)[[i]], 
                 prob = T,
                 train = mort_model_full$
                   trainingData[sample(1:nrow(mort_model_full$trainingData),
                                       1000), -18]) # 1000 lines averaged
}

save(mort_ice, mort_ice_c, mort_pdp_s, file = "rda/mort-ice.rda")
