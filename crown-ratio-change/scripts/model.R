library("tidyverse")
library("caret")
library("Rborist")

# load data from GitHub
temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(cr_rate, spp, dbh_mid, cr_mid, crown_class_s, tree_class_s,
         ba_mid, bal_mid, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev,  ba_ash:`bal_yellow birch`,
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
cr_growth_test <- nf_fia[index,]

# save(cr_growth_test, file = "../big-rdas/cr-growth-test.rda")

x <- select(train, -cr_rate, -plot)
y <- train[,1]


#####################################################################
# Train full model
#####################################################################

set.seed(1)
cr_growth_model_full <- train(x, y,
                  method = "ranger",
                  preProcess = c("center", "scale", "YeoJohnson"),
                  num.trees = 200,
                  importance = 'impurity',
                  tuneGrid = data.frame(mtry = seq(2, 10, by = 2),
                                        splitrule = rep("variance", 5),
                                        min.node.size = rep(5, 5)))


#####################################################################
# Results full
#####################################################################

cr_growth_model_full$results

plot(cr_growth_model_full)

varImp(cr_growth_model_full, scale = F)


#####################################################################
# Prediction full
#####################################################################

df <- data.frame(spp = factor(rep("white pine", 4), levels = levels(train$spp)),
                 dbh_mid = rep(12, 4),
                 cr_mid = seq(10, 70, 20),
                 crown_class_s = c(5, 4, 3, 2),
                 tree_class_s = rep(2, 4),
                 ba_mid = rep(150, 4),
                 bal_mid = seq(225, 0, -75),
                 forest_type_s = factor(rep("White pine", 4),
                                        levels = levels(train$forest_type_s)),
                 stocking_s = rep(2, 4),
                 landscape = factor(rep("rolling uplands", 4),
                                    levels = levels(train$landscape)),
                 site_class = rep(5, 4),
                 slope = rep(0, 4),
                 aspect = rep(0, 4),
                 lat = rep(44.7, 4),
                 lon = rep(-73.6, 4),
                 elev = rep(1400, 4))

df_pred <- df %>% 
  mutate(y_hat = predict(cr_growth_model_full, newdata = df))


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
cr_growth_model_op <- 
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

cr_growth_model_op$results

plot(cr_growth_model_op)

varImp(cr_growth_model_op, scale = F)


#####################################################################
# Train operational linear model
#####################################################################

train2 <- select(train, 
                 -landscape,
                 -forest_type,
                 -spp,
                 -crown_class, 
                 -tree_class, 
                 -site_class,
                 -aspect, 
                 -slope, 
                 -stocking,
                 -plot)

set.seed(1)
train2 <- train2[sample(nrow(train2), 10000),]

set.seed(1)
cr_growth_model_op_glm <- glm(cr_rate ~ ., data = train2)


#####################################################################
# Results operational linear model
#####################################################################

summary(cr_growth_model_op_glm)

varImp(cr_growth_model_op_glm, scale = F)


#####################################################################
# Save
#####################################################################

# STOP! Too big to fit on GitHub
save(cr_growth_model_full, file = "../big-rdas/cr-growth-model-sppspec.rda")
save(cr_growth_model_op, file = "../big-rdas/cr-growth-model-op.rda")
