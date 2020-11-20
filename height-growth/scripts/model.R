#### SLOW. RUN OVERNIGHT. ####

library("tidyverse")
library("caret")
library("Rborist")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived", !is.na(ht_rate)) %>% 
  select(ht_rate, spp, dbh_mid, cr_mid, crown_class_s, tree_class_s,
         ba_mid, bal_mid, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev, ba_ash:`bal_yellow birch`,
         plot) %>% 
  rename(dbh = dbh_mid, cr = cr_mid, crown_class = crown_class_s,
         tree_class = tree_class_s, ba = ba_mid, bal = bal_mid,
         forest_type = forest_type_s, stocking = stocking_s)

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(nf_fia$plot), 
                     size = round(test_size*length(unique(nf_fia$plot))), 
                     replace = FALSE)

index <- which(nf_fia$plot %in% test_plots)
train <- nf_fia[-index,]
ht_growth_test <- nf_fia[index,]

# save(ht_growth_test, file = "../big-rdas/ht-growth-test.rda")

x <- select(train, -plot, -ht_rate)
y <- train[,1]


#####################################################################
# Train full model
#####################################################################

set.seed(1)
ht_growth_model_full <- 
  train(x, y,
        method = "ranger",
        preProcess = c("center", "scale", "YeoJohnson"),
        num.trees = 200,
        importance = 'impurity',
        tuneGrid = data.frame(mtry = seq(2, 14, by = 2),
                              splitrule = rep("variance", 7),
                              min.node.size = rep(5, 7)))


#####################################################################
# Results full model
#####################################################################

# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

ht_growth_model_full$results

plot(ht_growth_model_full)

varImp(ht_growth_model_full, scale = F)


#####################################################################
# Train operational model
#####################################################################

x2 <- select(x, 
             -landscape, 
             -crown_class, 
             -tree_class, 
             -aspect, 
             -slope, 
             -stocking)

set.seed(1)
ht_growth_model_op <- 
  train(x2, y,
        method = "ranger",
        preProcess = c("center", "scale", "YeoJohnson"),
        num.trees = 200,
        importance = 'impurity',
        tuneGrid = data.frame(mtry = seq(2, 10, by = 2),
                              splitrule = rep("variance", 5),
                              min.node.size = rep(5, 5)))


#####################################################################
# Results operational model
#####################################################################

ht_growth_model_op$results

plot(ht_growth_model_op)

varImp(ht_growth_model_op, scale = F)


#####################################################################
# Save
#####################################################################

save(ht_growth_model_full, file = "../big-rdas/ht-growth-model-sppspec.rda")
save(ht_growth_model_op, file = "../big-rdas/ht-growth-model-op.rda")
