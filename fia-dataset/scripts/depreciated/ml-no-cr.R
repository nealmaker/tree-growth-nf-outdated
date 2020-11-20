
load("rda/nf-fia.rda")

library(tidyverse)


# Fix problems -----------------------------------------------------------

bad_plots <- unique((filter(nf_fia, is.na(FORTYPCD)))$PLT_CN) #w/o condition data

nf_fia <- nf_fia  %>%
  # reformat factors:
  mutate(FORTYPCD = factor(FORTYPCD),
         PHYSCLCD = factor(PHYSCLCD),
         STATECD = factor(STATECD)) %>%
  # remove observations missing key predictors:
  filter(!is.na(DIA_MIDPT),
         !(PLT_CN %in% bad_plots),
         STATECD %in% c("33", "36")) %>%
  # remove empty & almost empty vectors:
  select(ANN_DIA_GROWTH:BALIVE, LAT:SPCD, CCLCD, DIA_MIDPT)

nf_fia$SPCD <- plyr::mapvalues(x = nf_fia$SPCD, 
                               from = levels(nf_fia$SPCD), 
                               to = c("balsam fir", "atlantic white cedar", "redcedar", "larch", "tamarack", 
                                      "norway spruce", "white spruce", "black spruce", "blue spruce", 
                                      "red spruce", "jack pine", "table mtn pine", "red pine", "pitch pine", 
                                      "white pine", "scots pine", "austrian pine", "douglas fir", "baldcypress", 
                                      "eastern white cedar", "hemlock", "maple spp", "boxelder", "black maple",
                                      "red maple", "silver maple", "sugar maple", "norway maple", "ohio buckeye",
                                      "birch spp", "yellow birch", "sweet birch", "river birch", "paper birch", 
                                      "gray birch", "hickory spp", "bitternut hickory", "pignut hickory",
                                      "shagbark hickory", "mockernut hickory", "chestnut", "hackberry",
                                      "flowering dogwood", "beech", "ash spp", "white ash", "black ash",
                                      "green ash", "blue ash", "honeylocust", "butternut", "black walnut",
                                      "tulip poplar", "cucumbertree", "frasier magnolia", "mulberry spp",
                                      "blackgum", "paulownia", "sycamore", "balsam poplar", "cottonwood",
                                      "bigtooth aspen", "swamp cottonwood", "quaking aspen", "black cherry",
                                      "white oak", "swamp white oak", "scarlet oak", "burr oak", "chestnut oak",
                                      "northern red oak", "black oak", "black locust", "willow spp", "black willow",
                                      "basswood spp", "american basswood", "elm spp", "american elm", "slippery elm",
                                      "rock elm"))

index <- createDataPartition(nf_fia$ANN_DIA_GROWTH, times = 1, p = .1, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]

#index_sub <- createDataPartition(train$ANN_DIA_GROWTH, p = .1, list = FALSE)

#train_sub <- train[index_sub,]

fit_rf <- train(ANN_DIA_GROWTH ~ ., data = train, method = "Rborist", 
                tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))