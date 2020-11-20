library("tidyverse")
library("survival")
library("caret")
library("ranger")
library("randomForest")
library("Rborist")

# load data from GitHub
temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that were cut and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change != "cut") %>%
  mutate(died = as.character(status_change),
         died = as.factor(died)) %>% 
  select(died, interval, spp, dbh_s, cr_s, crown_class_s, tree_class_s,
         ba_s, bal_s, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev, ba_ash:`bal_yellow birch`,
         plot) %>% 
  rename(dbh = dbh_s, cr = cr_s, crown_class = crown_class_s,
         tree_class = tree_class_s, ba = ba_s, bal = bal_s,
         forest_type = forest_type_s, stocking = stocking_s)

unlink(temp)

# # Converts outcome to intervals for interval-censored model:
# nf_fia <- nf_fia %>% 
#   mutate(start = ifelse(died == 1, 0, interval),
#          end = ifelse(died == 1, interval, Inf)) %>% 
#   select(-died, -interval)


# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(nf_fia$plot), 
                     size = round(test_size*length(unique(nf_fia$plot))), 
                     replace = FALSE)

index <- which(nf_fia$plot %in% test_plots)
train <- nf_fia[-index,]
test <- nf_fia[index,]
train_sub <- train[sample(1:nrow(train), 200),]

#Surv now gives an interval censored object, I think,
#but ranger doesn't seem to use it correctly
#or should I make ranges that go from interval to infinity for live trees?
#time = 0 or interval; time2 = interval or infinity; event = 3 for all
# x <- select(train, -plot, -start, -end)
# y <- Surv(rep(0, nrow(train)), time = train$interval, event = train$died)
# 
# x_sub <- select(train_sub, -plot, -start, -end)
# y_sub <- Surv(time = train_sub$start, 
#               time2 = train_sub$end, 
#               event = rep(3, nrow(train_sub)), 
#               type = "interval")

x <- select(train, -died, -plot)
y <- train[,1]

x_sub <- select(train_sub, -died, -plot)
y_sub <- train_sub[,1]


#####################################################################
# Train model
#####################################################################

# Model testing
# This one needs y to be a factor with two levels (not named "0" and "1")
# to predict class probabilities: predict(model, newdata, type = "prob")
# ranger_mod_test <- train(x_sub, y_sub,
#                          method = "ranger",
#                          preProcess = c("center", "scale", "YeoJohnson"),
#                          trControl = trainControl(classProbs = T),
#                          num.trees = 50,
#                          tuneGrid = data.frame(.mtry = seq(2, 14, by = 4),
#                                                .splitrule = rep("gini", 4),
#                                                .min.node.size = rep(3, 4)),
#                          importance = 'impurity')

# this is set up as right-censored, should be interval;
# looks like ranger only works for right censored data though...
# chf shows prob of death at various times for each observation
# and unique.death.times are the times that correspond to those probs.
# ranger_test <- ranger(y_sub ~ spp + dbh + cr, data = train_sub)
# 
# randomForest_mod_test <- train(x_sub, y_sub,
#                                method = "rf",
#                                preProcess = c("center", "scale", "YeoJohnson"),
#                                ntree = 200,
#                                importance = T,
#                                tuneGrid = data.frame(mtry = seq(2, 14, by = 4)))

# "rf" works on subsample, but "cannot allocate vector of 2.3 Gb" 
# with full (80%) training set. Try decreasing size of training set.
# still fails at 60% training set "cannot allocate ... 1.7 Gb" (>24 hrs)
# Could also reduce ntree to 200 (default is 500), or maybe
# increase R's memory limit (eg: memory.limit(size=56000))

#memory.limit(size=56000)
set.seed(10)
mort_model_full <- train(x, y,
                         method = "ranger",
                         preProcess = c("center", "scale", "YeoJohnson"),
                         trControl = trainControl(classProbs = T),
                         num.trees = 200,
                         tuneGrid = data.frame(.mtry = seq(2, 14, by = 4),
                                               .splitrule = rep("gini", 4),
                                               .min.node.size = rep(10, 4)),
                         importance = 'impurity')

# causes R to abort!
# Rborist_mod_test <- train(x_sub, y_sub,
#                           method = "Rborist",
#                           preProcess = c("center", "scale", "YeoJohnson"),
#                           tuneGrid = data.frame(predFixed = seq(2, 8, by = 2),
#                                                 minNode = rep(2, 4)))


# Rborist version
# set.seed(1)
# mort_model_2 <- train(x, y,
#                     method = "Rborist",
#                     tuneGrid = data.frame(predFixed = seq(2, 8, by = 2),
#                                           minNode = rep(2, 4)))

# validation$census can be used to get probabilities
# validation$yPred seems to have changed factor levels
# mort_model <- Rborist(x = x, y = y, 
#                       predFixed = 7,
#                       minNode = 2)


#####################################################################
# Results full
#####################################################################

y_hat_train <- predict(mort_model_full, newdata = x)
y_hat_train <- if_else(y_hat_train$yPred == 1, 0, 1)
y_hat_train <- as.factor(y_hat_train)

confusionMatrix(data = y_hat_train, reference = y, positive = "lived")

caret::F_meas(data = y_hat_train, reference = y, positive = "lived")


#####################################################################
# Prediction full
#####################################################################

df <- data.frame(spp = factor(c("hard maple", "paper birch"), 
                              levels = levels(train$spp)),
                 dbh_s = c(10, 19),
                 cr_s = c(60, 10),
                 ba_mid = c(60, 250),
                 bal_s = c(0, 200),
                 forest_type_s = factor(c("Northern hardwood", "Cedar-hardwood"),
                                        levels = levels(train$forest_type_s)),
                 lat = c(44.7, 44.7),
                 lon = c(-73.6, -73.6))

predict(mort_model_full, newdata = df, type = "prob")

#this will give a probability of death (over next 5ish years)
# predict(mort_model_full, newdata = df_trans)$census[,2]/500


#####################################################################
# Test
#####################################################################

# y_hat_test <- predict(mort_model, newdata = test_tran[,-1])$yPred
# y_hat_test <- as.factor(if_else(y_hat_test == 1, 0, 1))
# 
# confusionMatrix(data = y_hat_test, reference = test$died, positive = "1")


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

memory.limit(size=56000)
set.seed(1)
mort_model_op <- 
  train(x2, y,
        method = "ranger",
        preProcess = c("center", "scale", "YeoJohnson"),
        trControl = trainControl(classProbs = T),
        num.trees = 200,
        importance = 'impurity',
        tuneGrid = data.frame(.mtry = seq(2, 10, by = 2),
                              .splitrule = rep("variance", 5),
                              .min.node.size = rep(5, 5)))


#####################################################################
# Results operational model
#####################################################################

mort_model_op$results

plot(mort_model_op)

varImp(mort_model_op, scale = F)


#####################################################################
# Save
#####################################################################

# STOP! Too big to fit on GitHub
save(mort_model_full, file = "../big-rdas/mort-model-sppspec.rda")
save(mort_model_op, file = "../big-rdas/mort-model-op.rda")
