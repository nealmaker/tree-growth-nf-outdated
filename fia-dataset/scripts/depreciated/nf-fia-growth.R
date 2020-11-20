# Checks for, installs, & loads packages (no warning prior to install):
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  if(length(need) > 0){ 
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
}

using("tidyverse", "laselva", "maps", "caret", "Rborist")
# laselva fetches FIA data: https://github.com/ropenscilabs/laselva




##############################
# Import FIA data
##############################


# Define States & counties (FIPS codes) in Northern Forest region --------

states <- c("VT", "NH", "NY", "ME")

vt_counties <- c(11, 19, 9, 7, 15, 5, 23, 1, 17, 13)
nh_counties <- c(7, 9, 3)
ny_counties <- c(75, 65, 49, 45, 89, 43, 35, 41, 33, 31, 19, 113)
me_counties <- c(17, 7, 25, 1, 11, 27, 9, 29, 19, 21, 3)


# Fetch FIA tree, growth, plot, & condition data for Northern Forest states 
# (this may take a few minutes)

nf_trees <- fia_fetch(state = states)
nf_growth <- fia_fetch(states, "TREE_GRM_COMPONENT")
nf_plots <- fia_fetch(states, "PLOT")
nf_conds <- fia_fetch(states, "COND")


# Filter nf_trees to keep only data from Northern Forest counties in each state 

nf_trees$VT_tree <- nf_trees$VT_tree %>% 
  filter(COUNTYCD %in% vt_counties)
nf_trees$NH_tree <- nf_trees$NH_tree %>%
  filter(COUNTYCD %in% nh_counties)
nf_trees$NY_tree <- nf_trees$NY_tree %>%
  filter(COUNTYCD %in% ny_counties)
nf_trees$ME_tree <- nf_trees$ME_tree %>%
  filter(COUNTYCD %in% me_counties)


# Calculates overtopping basal area (BAL) assuming all input trees are in same plot
# and ba is adjusted based on tpa:
pbal <- function(dbh, ba){
  sapply(dbh, function(x){
    index <- dbh > x
    return(sum(ba[index]))
  })
}


# For each FIA table, combine states' data, filter, format and select vectors
# that might be useful

nf_trees <- rbind(nf_trees$VT_tree, 
                  nf_trees$NH_tree, 
                  nf_trees$NY_tree, 
                  nf_trees$ME_tree) %>%
  # keep live, non-cull, non-seedling trees:
  filter(DIAHTCD == 1, 
         TREECLCD == 2, 
         STATUSCD == 1) %>% 
  mutate(TRE_CN = CN,
         # create unique county code from state and county codes:
         COUNTY = paste(factor(STATECD), factor(COUNTYCD), sep = "_"),
         SPCD = factor(SPCD),
         COUNTY = factor(COUNTY)) %>%
  select(TRE_CN, PLT_CN, COUNTY, SPCD, HT, CCLCD, TREEGRCD, CULL, UNCRCD, 
         CR, CDENCD, CDIEBKCD, TRANSCD, TREECLCD_NERS, DAMLOC1, DAMLOC2, 
         DAMTYP1, DAMTYP2, DAMSEV1, DAMSEV2, STATECD, INVYR, DIA)

nf_growth <- rbind(nf_growth$VT_TREE_GRM_COMPONENT, 
                   nf_growth$NH_TREE_GRM_COMPONENT, 
                   nf_growth$NY_TREE_GRM_COMPONENT, 
                   nf_growth$ME_TREE_GRM_COMPONENT) %>%
  select(TRE_CN, DIA_BEGIN, DIA_MIDPT, DIA_END, ANN_DIA_GROWTH) 

nf_plots <- rbind(nf_plots$VT_PLOT, 
                  nf_plots$NH_PLOT, 
                  nf_plots$NY_PLOT, 
                  nf_plots$ME_PLOT) %>%
  mutate(PLT_CN = CN) %>%
  select(PLT_CN, LAT, LON)

nf_conds <- rbind(nf_conds$VT_COND,
                  nf_conds$NH_COND,
                  nf_conds$NY_COND,
                  nf_conds$ME_COND) %>%
  select(FORTYPCD, SITECLCD, SLOPE, ASPECT, PHYSCLCD, GSSTKCD,
         BALIVE, LIVE_CANOPY_CVR_PCT, NBR_LIVE_STEMS, PLT_CN)


# Join FIA tables -------------------------------------------------------

nf_fia <- inner_join(nf_trees, nf_growth, by = "TRE_CN") 

nf_fia <- right_join(nf_plots, nf_fia, by = "PLT_CN")

nf_fia <- right_join(nf_conds, nf_fia, by = "PLT_CN")


# filter out observations w/o diameter growth measurements (response variable),
# remove database key, bring response to front

nf_fia <- nf_fia %>% filter(!is.na(ANN_DIA_GROWTH)) %>%
  select(-(TRE_CN)) %>% 
  select(ANN_DIA_GROWTH, everything())


# clean up ---------------------------------------------------------------

remove(nf_conds, nf_growth, nf_plots, nf_trees, states, vt_counties, 
       nh_counties, ny_counties, me_counties, pbal, using)




##############################
# EDA & related fixes
##############################


# # Examine vectors ----------------------------------------------------------
# 
# str(nf_fia)
# 
# summary(nf_fia)
# 
# nf_fia <- nf_fia %>%
#   arrange(ANN_DIA_GROWTH)
# 
# View(head(nf_fia))
# 
# View(tail(nf_fia))
# 
# levels(nf_fia$COUNTY) # 10 VT counties, 3 NH, 12 NY, 11 ME
# 
# 
# # Distribution of growth rates -------------------------------------------
# 
# nf_fia %>%
#   group_by(SPCD) %>%
#   filter(n()>=20) %>%
#   ungroup() %>%
#   ggplot(aes(ANN_DIA_GROWTH)) +
#   geom_density(bw = .008, fill = "dark gray") +
#   geom_vline(xintercept = median(nf_fia$ANN_DIA_GROWTH),
#              col = "dark green", size = 1)
# 
# nf_fia %>% ggplot(aes(sample = ANN_DIA_GROWTH)) + geom_qq() 
# 
# # Grouped by species:
# nf_fia %>%
#   group_by(SPCD) %>%
#   filter(n()>=20) %>%
#   ungroup() %>%
#   ggplot(aes(ANN_DIA_GROWTH)) +
#   geom_density(bw = .008, fill = "dark gray") +
#   geom_vline(xintercept = median(nf_fia$ANN_DIA_GROWTH),
#              col = "dark green", size = 1) +
#   facet_wrap(~ SPCD, ncol = 5)
# 
# 
# # Look for plots w/o condition data --------------------------------------
# 
# View(nf_fia %>%
#        filter(is.na(FORTYPCD)) %>%
#        group_by(PLT_CN) %>%
#        summarize(nasite = sum(is.na(SITECLCD))/n(),
#                  naslope = sum(is.na(SLOPE))/n(),
#                  naaspect = sum(is.na(ASPECT))/n(),
#                  naphys = sum(is.na(PHYSCLCD))/n(),
#                  nastock = sum(is.na(GSSTKCD))/n(),
#                  naba = sum(is.na(BALIVE))/n()))
# 
# View(nf_fia %>%
#        filter(!is.na(FORTYPCD)) %>%
#        group_by(PLT_CN) %>%
#        summarize(nasite = sum(is.na(SITECLCD))/n(),
#                  naslope = sum(is.na(SLOPE))/n(),
#                  naaspect = sum(is.na(ASPECT))/n(),
#                  naphys = sum(is.na(PHYSCLCD))/n(),
#                  nastock = sum(is.na(GSSTKCD))/n(),
#                  naba = sum(is.na(BALIVE))/n()))
# 
# View(unique((nf_fia %>% filter(is.na(FORTYPCD)))$COUNTY))
# # Plots w/o condition data exist in all states


# Fix problems -----------------------------------------------------------

bad_plots <- unique((filter(nf_fia, is.na(FORTYPCD)))$PLT_CN) #w/o condition data

nf_fia <- nf_fia  %>%
  # reformat factors:
  mutate(FORTYPCD = factor(FORTYPCD),
         PHYSCLCD = factor(PHYSCLCD),
         STATECD = factor(STATECD)) %>%
  filter(STATECD %in% c("33", "36")) %>%
  # remove observations missing key predictors:
  filter(!is.na(DIA_MIDPT),
         #!is.na(bal),
         !(PLT_CN %in% bad_plots)) %>%
  # remove empty & almost empty vectors:
  select(ANN_DIA_GROWTH:BALIVE, LAT:SPCD, CCLCD, CR, STATECD:INVYR, DIA_MIDPT)


# Make names and factor levels more intuitive ----------------------------

species_codes <- c(12, 43, 68, 70, 71, 91, 94, 95, 96, 97, 105, 123, 125, 126, 129, 130, 136, 
                   202, 221, 241, 261, 310, 313, 314, 315, 316, 317, 318, 319, 320, 331, 341, 
                   355, 356, 357, 367, 370, 371, 372, 373, 375, 379, 391, 400, 402, 403, 407, 
                   409, 421, 462, 491, 500, 531, 540, 541, 543, 544, 546, 552, 601, 602, 621, 
                   651, 655, 660, 661, 663, 680, 693, 701, 712, 731, 741, 742, 743, 744, 746, 
                   760, 761, 762, 763, 764, 771, 802, 804, 806, 816, 823, 832, 833, 837, 901, 
                   920, 922, 923, 926, 934, 935, 936, 937, 950, 951, 970, 972, 975, 977, 999)

species <- c("balsam fir", "atlantic white cedar", "redcedar", "larch", "tamarack", 
             "norway spruce", "white spruce", "black spruce", "blue spruce", 
             "red spruce", "jack pine", "table mtn pine", "red pine", "pitch pine", 
             "white pine", "scots pine", "austrian pine", "douglas fir", "baldcypress", 
             "northern white cedar", "hemlock", "maple spp", "boxelder", "black maple", 
             "striped maple", "red maple", "silver maple", "sugar maple", "moutain maple", 
             "norway maple", "ohio buckeye", "ailanthus", "european alder", "serviceberry spp", 
             "common serviceberry", "pawpaw", "birch spp", "yellow birch", "sweet birch", 
             "river birch", "paper birch", "gray birch", "carpinus", "hickory spp", 
             "bitternut hickory", "pignut hickory", "shagbark hickory", "mockernut hickory", 
             "chestnut", "hackberry", "flowering dogwood", "hawthorn", "beech", "ash spp", 
             "white ash", "black ash", "green ash", "blue ash", "honeylocust", "butternut", 
             "black walnut", "tulip poplar", "cucumbertree", "frasier magnolia", "apple", 
             "oregon crab apple", "crab apple", "mulberry", "blackgum", "hophornbeam", 
             "paulownia", "sycamore", "balsam poplar", "cottonwood", "bigtooth aspen", 
             "swamp cottonwood", "quaking aspen", "cherry spp", "pin cherry", "black cherry",
             "chokecherry", "peach", "sweet cherry", "white oak", "swamp white oak", 
             "scarlet oak", "scrub oak", "bur oak", "chestnut oak", "northern red oak", 
             "black oak", "black locust", "willow spp", "black willow", "bebb willow", 
             "balsam willow", "mountain-ash spp", "american mountain-ash", 
             "european mountain-ash", "northern mountain-ash", "basswood spp", 
             "american basswood", "elm spp", "american elm", "slippery elm", "rock elm", 
             "other")
  
spp <- data.frame(SPCD = factor(species_codes), spp = species)

nf_fia <- left_join(nf_fia, spp, by = "SPCD")


forest_type_codes <- c(101, 102, 103, 104, 105, 121, 122, 123, 124, 125, 126, 127, 167, 171, 
                       381, 384, 385, 401, 402, 409, 503, 505, 509, 512, 513, 516, 517, 519, 
                       520, 701, 702, 703, 704, 705, 706, 707, 708, 709, 801, 802, 805, 809, 
                       901, 902, 903, 904, 905, 962, 995, 999)

forest_types <- c("jack pine", "red pine", "white pine", "white pine-hemlock", "hemlock",
                  "fir", "white spruce", "red spruce", "spruce-fir", "black spruce",
                  "tamarack", "white cedar", "pitch pine", "redcedar", "scotch pine", 
                  "norway spruce", "larch", "pine-oak-ash", "redcedar-hardwood",
                  "pine-hardwood", "oak-hickory", "red oak", "bur oak", "black walnut",
                  "black locust", "cherry-ash-tulip tree", "elm-ash-locust", "red maple-oak",
                  "mixed upland hardwoods", "ash-elm-red maple", "river birch-sycamore",
                  "cottonwood", "willow", "sycamore-pecan-elm", "sugarberry-hackberry-elm",
                  "silver maple-elm", "red maple-lowland", "cottonwood-willow", 
                  "maple-beech-birch", "cherry", "hard maple-basswood", "red maple-upland",
                  "aspen", "paper birch", "gray birch", "balsam poplar", "pin cherry",
                  "other hardwoods", "other exotic hardwoods", "nonstocked")

forests <- data.frame(FORTYPCD = factor(forest_type_codes), forest_type = forest_types)

nf_fia <- left_join(nf_fia, forests, by = "FORTYPCD")


landscape_codes <- c(11, 12, 13, 21, 22, 23, 24, 25, 29, 31, 32, 34, 39)

landscapes <- c("dry tops", "dry slopes", "deep sands", "flatwoods", "rolling uplands",
                "moist slopes & coves", "narrow floodplains/bottomlands",
                "broad floodplains/bottomlands", "other mesic", "swamps/bogs", "small drains",
                "beaver ponds", "other hydric")

lands <- data.frame(PHYSCLCD = factor(landscape_codes), landscape = landscapes)

nf_fia <- left_join(nf_fia, lands, by = "PHYSCLCD")


# stocking_codes <- 1:5
# 
# stocking_levels <- c("overstocked", "fully stocked", "medium stocked", "poorly stocked",
#                      "nonstocked")
# 
# stock <- data.frame(GSSTKCD = stocking_codes, stocking = stocking_levels)
# 
# nf_fia <- left_join(nf_fia, stock, by = "GSSTKCD")
# 
# 
# crown_codes <- 1:5
# 
# crown_classes <- c("open grown", "dominant", "codominant", "intermediate", "overtopped")
# 
# crowns <- data.frame(CCLCD = crown_codes, crown_class = crown_classes)
# 
# nf_fia <- left_join(nf_fia, crowns, by = "CCLCD")
# 
# remove(bad_plots, species_codes, species, spp, forest_type_codes, forest_types, forests,
#        landscape_codes, landscapes, lands, stocking_codes, stocking_levels, stock,
#        crown_codes, crown_classes, crowns)

nf_fia <- nf_fia %>%
  mutate(stocking = GSSTKCD, crown_class = CCLCD)

nf_fia <- nf_fia %>%
  select(-SPCD, -FORTYPCD, -PHYSCLCD, -GSSTKCD, -CCLCD) %>%
  rename(diam_growth = ANN_DIA_GROWTH, site_class = SITECLCD, slope = SLOPE, aspect = ASPECT,
         ba = BALIVE, lat = LAT, lon = LON, county = COUNTY, dbh = DIA_MIDPT)


# # PLot relationships to growth -------------------------------------------
# 
# nf_fia %>%
#   filter(!is.na(forest_type)) %>%
#   mutate(forest_type = reorder(forest_type, -diam_growth, FUN = median)) %>%
#   ggplot(aes(forest_type, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# nf_fia %>%
#   filter(!is.na(site_class)) %>%
#   mutate(site_class = factor(site_class)) %>%
#   ggplot(aes(site_class, diam_growth)) +
#   geom_boxplot(fill = "gray")
# # Why isn't there a clear relationship to site class?
# 
# nf_fia %>%
#   filter(!is.na(site_class)) %>%
#   mutate(site_class = factor(site_class)) %>%
#   ggplot(aes(site_class, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   facet_wrap(~ spp)
# # not enough to show underlying influence of site class
# 
# nf_fia %>%
#   filter(!is.na(slope)) %>%
#   ggplot(aes(slope, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# 
# nf_fia %>%
#   filter(!is.na(aspect)) %>%
#   ggplot(aes(aspect, diam_growth)) +
#   geom_jitter(alpha = .05) +
#   geom_smooth(col = "red")
# 
# nf_fia %>%
#   filter(!is.na(landscape)) %>%
#   mutate(landscape = reorder(landscape, -diam_growth, FUN = median)) %>%
#   ggplot(aes(landscape, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # almost no relationship, but what's there is unexpected
# 
# nf_fia %>%
#   group_by(landscape) %>%
#   summarize(n = n())
# # doesn't seem to be a regularization problem
# 
# nf_fia %>%
#   filter(!is.na(landscape)) %>%
#   ggplot(aes(landscape, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_wrap(~ stocking)
# # this fixes it
# 
# nf_fia %>%
#   filter(!is.na(stocking)) %>%
#   ggplot(aes(stocking, diam_growth)) +
#   geom_boxplot(fill = "gray")
# 
# nf_fia %>%
#   filter(!is.na(ba)) %>%
#   ggplot(aes(ba, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# # What's with the plots w/ BA>500?
# 
# nf_fia %>%
#   filter(ba > 500) %>%
#   group_by(forest_type) %>%
#   summarize(n = n(), mean_growth = mean(diam_growth))
# # high ba are mostly white pine stands
# 
# nf_fia %>%
#   filter(ba < 30) %>%
#   group_by(forest_type) %>%
#   summarize(n = n(), mean_growth = mean(diam_growth))
# # low ba are mostly sp/fr types
# 
# nf_fia %>%
#   ggplot(aes(lat, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# 
# nf_fia %>%
#   ggplot(aes(lon, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red")
# 
# nf_fia %>%
#   mutate(county = reorder(county, -diam_growth, FUN = median)) %>%
#   ggplot(aes(county, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # Only have good data from NY & 3 counties in NH. ME & VT growth is all 0.
# 
# View(nf_fia %>% group_by(county) %>% summarize(n = n()))
# 
# nf_fia %>%
#   mutate(spp = reorder(spp, -diam_growth, FUN = median)) %>%
#   ggplot(aes(spp, diam_growth)) +
#   geom_boxplot(fill = "gray") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# View(nf_fia %>%
#        mutate(spp = reorder(spp, -diam_growth, FUN = mean))%>% 
#        group_by(spp) %>% 
#        summarize(mean_growth = mean(diam_growth),
#                  sd_growth = sd(diam_growth)))
# 
# nf_fia %>%
#   ggplot(aes(crown_class, diam_growth)) +
#   geom_boxplot(fill = "gray")
# 
# # why is open-grown slow growing?
# nf_fia %>%
#   filter(crown_class == "open grown") %>%
#   group_by(site_class) %>%
#   summarize(n = n(), mean_growth = mean(diam_growth))
# # maybe they are poor situations not captured by site class...
# 
# nf_fia %>%
#   filter(!is.na(cr)) %>%
#   ggplot(aes(cr, diam_growth)) +
#   geom_jitter(width = 2.5, alpha = .07) +
#   geom_smooth(col = "red")
# 
# nf_fia %>%
#   filter(!is.na(dbh)) %>%
#   group_by(spp) %>%
#   filter(n() > 100) %>%
#   ungroup() %>%
#   ggplot(aes(dbh, diam_growth)) +
#   geom_jitter(alpha = .07) +
#   geom_smooth(col = "red") +
#   facet_wrap(~ spp)
# 
# nf_fia %>%
#   filter(!is.na(bal),
#          bal > 0) %>%
#   ggplot(aes(bal, diam_growth)) +
#   geom_jitter(alpha = .05) +
#   geom_smooth(col = "red")
# 
# nf_fia %>%
#   ggplot(aes(INVYR)) +
#   geom_density(bw = .8, fill = "gray")
# 
# 
# # Plot relationships between predictors ---------------------------------
# 
# nf_fia %>%
#   ggplot(aes(lon, lat)) +
#   geom_polygon(data = map_data("state"),
#                aes(x = long, y = lat, group = group),
#                fill = NA, col = "dark gray") +
#   geom_point(size = .2) +
#   coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank())
# 
# nf_fia %>%
#   filter(diam_growth < .3) %>% # removes high outliers that obscure relationship
#   ggplot(aes(lon, lat)) +
#   geom_polygon(data = map_data("state"),
#                aes(x = long, y = lat, group = group),
#                fill = NA, col = "dark gray") +
#   geom_point(size = 1, aes(col = diam_growth)) +
#   scale_color_gradientn(colors = rev(rainbow(5))) +
#   coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank())


# Remove vectors that aren't predictors or response variable -------------

nf_fia <- nf_fia %>%
  select(-STATECD, -INVYR)




##############################
# Partition data
##############################


index <- createDataPartition(nf_fia$diam_growth, times = 1, p = .1, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]




##############################
# Train algorithms
##############################


# Leaving live crown ratio as predictor (limiting geographic extent) -----

# This may take up to an hour:
fit_rf <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR))), method = "Rborist", 
                tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

# # Training knn takes hours on laptop!:
# fit_knn <- train(diam_growth ~ ., data = (train %>% filter(!is.na(cr))), method = "knn", 
#                  tunegrid = data.frame(k = seq(2, 22, by = 4)))
# 
min(fit_rf$results$RMSE)
# min(fit_knn$results$RMSE)

# RF leaving out BAL,
# This may take up to an hour:
fit_rf_no_bal <- train(diam_growth ~ ., data = (train %>% 
                                                 filter(!is.na(cr)) %>% 
                                                 select(-bal)), 
                      method = "Rborist", 
                      tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

# min(fit_rf_no_bal$results$RMSE)

# Without live crown ratio (larger geographic extent) ---------------------

# This may take up to an hour:
fit_rf_no_cr <- train(diam_growth ~ ., data = (train %>% select(-cr)), method = "Rborist", 
                tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

# min(fit_rf_no_cr$results$RMSE)


# Which is more accurate over whole region? -------------------------------

# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

RMSE(train$diam_growth, predict(fit_rf, newdata = train))
RMSE(train$diam_growth, predict(fit_rf_no_cr, newdata = train))


# Examine residuals ------------------------------------------------------

outcomes <- train %>%
  mutate(actual = diam_growth,
         predicted = predict(fit_rf_no_cr, newdata = train),
         naive = mean(diam_growth),
         error = actual - predicted,
         naive_error = actual - naive)

outcomes %>% ggplot(aes(error)) +
  geom_density(bw = .008, fill = "dark gray") +
  scale_x_continuous(limits = c(-.1,.3))

outcomes %>% ggplot(aes(naive_error)) +
  geom_density(bw = .008, fill = "dark gray") +
  scale_x_continuous(limits = c(-.1,.3))

View(top_n(outcomes %>% arrange(error), 30))

outcomes %>% ggplot(aes(diam_growth)) + geom_density()

##############################
# Estimate accuracy of final
##############################


RMSE(test$diam_growth, predict(fit_rf_no_cr, newdata = test))




##############################
# Retrain on entire dataset
##############################

y <- nf_fia$diam_growth

x <- nf_fia %>% select(-diam_growth) %>% filter(!is.na(cr))

nf_growth <- Rborist(x, y, 
                     predFixed = fit_rf$bestTune$predFixed,
                     minNode = fit_rf$bestTune$minNode)

# data.frame(y = y, y_hat = predict(nf_growth, newdata = x)$yPred) %>% mutate(err = y - y_hat) %>%
#   ggplot(aes(err)) + geom_density()


##############
# a few other fits, without certain predictors
##############

fit_rf_no_cr <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR)) %>% select(-CR)), 
                method = "Rborist", 
                tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

min(fit_rf_no_cr$results$RMSE)



fit_rf_no_crown <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR)) %>% select(-crown_class)), 
                      method = "Rborist", 
                      tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

min(fit_rf_no_crown$results$RMSE)




fit_rf_no_crcrown <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR)) %>% select(-crown_class, -CR)), 
                         method = "Rborist", 
                         tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

min(fit_rf_no_crcrown$results$RMSE)




fit_rf_no_stock <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR)) %>% select(-stocking)), 
                           method = "Rborist", 
                           tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

min(fit_rf_no_stock$results$RMSE)




fit_rf_no_crcrownstock <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR)) %>% select(-crown_class, -CR, -stocking)), 
                           method = "Rborist", 
                           tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

min(fit_rf_no_crcrownstock$results$RMSE)



fit_rf_no_site <- train(diam_growth ~ ., data = (train %>% filter(!is.na(CR)) %>% select(-site_class)), 
                                method = "Rborist", 
                                tuneGrid = data.frame(predFixed = 2, minNode = seq(2, 22, by = 4)))

min(fit_rf_no_site$results$RMSE)
