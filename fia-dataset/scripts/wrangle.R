#' There are numerous actions that could filter out some trees in a plot without 
#' removing them all. The best thing would be to do all plot-level calculations
#' first (including spp spec ba & bal, which entails doing our species grouping 
#' and renaming at beginning). 


library("tidyverse")
library("lubridate")


################################################################################
# Import FIA data
################################################################################

# Define States & counties (FIPS codes) in Northern Forest region --------

states <- c("NY", "VT", "NH", "ME")

NY_counties <- c(75, 65, 49, 45, 89, 43, 35, 41, 33, 31, 19, 113)
VT_counties <- c(11, 19, 9, 7, 15, 5, 23, 1, 17, 13)
NH_counties <- c(7, 9, 3)
ME_counties <- c(17, 7, 25, 1, 11, 27, 9, 29, 19, 21, 3)


# Fetch FIA tree, growth, plot, & condition data for Northern Forest states 
# and filter to keep only northern forest counties
# (this may take a while; ~140MB of downloads + reading)

temp <- tempfile()

for(state in states){
  download.file(paste("https://apps.fs.usda.gov/fia/datamart/CSV/", 
                      state, "_TREE.zip", sep = ""),
                temp, mode = "wb")
  unzip(temp, paste(state, "_TREE.csv", sep = ""))
}

TREE <- lapply(states, function(x){
  read.csv(paste(x, "_TREE.csv", sep = ""), header = T) %>% 
    filter(COUNTYCD %in% eval(as.name(paste(x, "_counties", sep = ""))),
           DIAHTCD == 1) %>% # excludes seedlings measured at root collar
    select(CN, PLT_CN, SUBP, PREV_TRE_CN, CONDID, DIA, SPCD, STATUSCD, 
           MORTYR, CR, CCLCD, TREECLCD, HT) %>%
    mutate(ba_ac = if_else(DIA >= 5, 
                           # poles & larger from 24' radius subplots
                           # saplings from 6.8' radius microplots
                           0.005454*DIA^2*(43560/(pi*24^2)),  
                           0.005454*DIA^2*(43560/(pi*6.8^2)))) 
})

## SUBPLOTS MATTER!!!!
# Subplots may have different sizes depending on the plot design, found in 
# PLOT$DESIGNCD (see database guide, appendix i)
# I can just keep DESIGNCD == 1 (the main standard) and lose some data, 
# or I can account for the various designs when I calculate TREE$ba_ac
# (above; which would mean calculating ba_ac after combining
# states' data and joining nf_trees to nf_plots).
# Update: it's probably harder b/c they severed the plot codes to older 
# inventories to keep people from seeing real coordinates

# DESIGN CODES:
# 1:4 used 1999 - present
# 11:15 used 1994 - 1996
# 100 used 1982 & 1983
# 101:104 used 1991 - 1998
# 101 was continued through 2008
# 105:120 variousy used 1991 - 1993


for(state in states){
  download.file(paste("https://apps.fs.usda.gov/fia/datamart/CSV/", state, 
                      "_PLOT.zip", sep = ""),
                temp, mode = "wb")
  unzip(temp, paste(state, "_PLOT.csv", sep = ""))
}

PLOT <- lapply(states, function(x){
  read.csv(paste(x, "_PLOT.csv", sep = ""), header = T) %>% 
    filter(COUNTYCD %in% eval(as.name(paste(x, "_counties", sep = "")))) %>% 
    select(CN, PREV_PLT_CN, DESIGNCD, MEASYEAR, MEASMON, 
           MEASDAY, LAT, LON, ELEV) %>%
    rename(PLT_CN = CN)
})


for(state in states){
  download.file(paste("https://apps.fs.usda.gov/fia/datamart/CSV/", state, 
                      "_COND.zip", sep = ""),
                temp, mode = "wb")
  unzip(temp, paste(state, "_COND.csv", sep = ""))
}

COND <- lapply(states, function(x){
  read.csv(paste(x, "_COND.csv", sep = ""), header = T) %>% 
    filter(COUNTYCD %in% eval(as.name(paste(x, "_counties", sep = "")))) %>% 
    select(PLT_CN, CONDID, FORTYPCD, ALSTKCD, SITECLCD, 
           PHYSCLCD, SLOPE, ASPECT)
})


for(state in states){
  download.file(paste("https://apps.fs.usda.gov/fia/datamart/CSV/", state, 
                      "_TREE_GRM_COMPONENT.zip", sep = ""),
                temp, mode = "wb")
  unzip(temp, paste(state, "_TREE_GRM_COMPONENT.csv", sep = ""))
}
  
GRM <- lapply(states, function(x){
  read.csv(paste(x, "_TREE_GRM_COMPONENT.csv", sep = ""), header = T) %>% 
    filter(!is.na(ANN_DIA_GROWTH)) %>% 
    select(TRE_CN, STATECD, DIA_BEGIN, DIA_MIDPT, DIA_END, ANN_DIA_GROWTH,
           HT_BEGIN, HT_MIDPT, HT_END, ANN_HT_GROWTH)
})

# Combine states' data

nf_trees <- do.call(rbind, TREE)
nf_plots <- do.call(rbind, PLOT)
nf_conds <- do.call(rbind, COND)
nf_grms <- do.call(rbind, GRM)

# delete temporary objects and downloaded files

unlink(temp)

remove(TREE, PLOT, COND, GRM, temp, state)

for(state in states){
  file.remove(paste(state, "_TREE.csv", sep = ""))
  file.remove(paste(state, "_PLOT.csv", sep = ""))
  file.remove(paste(state, "_COND.csv", sep = ""))
  file.remove(paste(state, "_TREE_GRM_COMPONENT.csv", sep = ""))
}


################################################################################
# Group species
################################################################################

# Make names and factor levels more intuitive ------------------------------

species_codes <- 
  c(12, 43, 68, 70, 71, 91, 94, 95, 96, 97, 105, 123, 125, 126, 129, 
    130, 136, 202, 221, 241, 261, 310, 313, 314, 315, 316, 317, 318, 
    319, 320, 331, 341, 355, 356, 357, 367, 370, 371, 372, 373, 375, 
    379, 391, 400, 402, 403, 407, 409, 421, 462, 491, 500, 531, 540, 
    541, 543, 544, 546, 552, 601, 602, 621, 651, 655, 660, 661, 663, 
    680, 693, 701, 712, 731, 741, 742, 743, 744, 746, 760, 761, 762, 
    763, 764, 771, 802, 804, 806, 816, 823, 832, 833, 837, 901, 920, 
    922, 923, 926, 934, 935, 936, 937, 950, 951, 970, 972, 975, 977, 
    999)

species <- 
  c("fir", "other softwood", "cedar", "tamarack", "tamarack", 
    "norway spruce", "spruce", "spruce", "spruce", "spruce", "other softwood", 
    "other softwood", "red pine", "other softwood", "white pine", "scots pine", 
    "other softwood", "other softwood", "other softwood", "cedar", "hemlock", 
    "other hardwood", "soft maple", "hard maple", "striped maple", "soft maple",
    "soft maple", "hard maple", "other hardwood", "hard maple", 
    "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
    "other hardwood", "other hardwood", "other hardwood", "yellow birch", 
    "other hardwood", "other hardwood", "paper birch", "other hardwood", 
    "other hardwood", "hickory", "hickory", "hickory", "hickory", "hickory", 
    "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
    "beech", "ash", "ash", "ash", "ash", "ash", "other hardwood", "butternut", 
    "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
    "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
    "other hardwood", "hophornbeam", "other hardwood", "other hardwood", 
    "aspen", "cottonwood", "aspen", "cottonwood", "aspen", "other hardwood", 
    "other hardwood", "black cherry", "other hardwood", "other hardwood", 
    "other hardwood", "white oak", "white oak", "red oak", "white oak", 
    "white oak", "white oak", "red oak", "red oak", "other hardwood", 
    "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
    "other hardwood", "other hardwood", "other hardwood", "other hardwood", 
    "basswood", "basswood", "elm", "elm", "elm", "elm", "other hardwood")

names(species) <- as.character(species_codes)

nf_trees$SPCD <- factor(unname(species[as.character(nf_trees$SPCD)]),
                     levels = levels(factor(species))) # standardize levels



################################################################################
# Calculate BAL & plot BA for each tree
################################################################################

# Calculates overtopping basal area (BAL) assuming all input trees are in 
# same plot and ba is adjusted based on tpa:
pbal <- function(dbh, ba){
  sapply(dbh, function(x){
    index <- dbh > x
    return(sum(ba[index]))
  })
}


# Add BAL
nf_trees <- nf_trees %>%
  mutate(bal = NA,
         ba = NA)

# Note that this only calculates ending basal areas for trees that lived
nf_trees[nf_trees$STATUSCD == 1,] <- nf_trees[nf_trees$STATUSCD == 1,] %>% 
  group_by(PLT_CN, SUBP) %>% 
  mutate(bal = pbal(DIA, ba_ac),
         ba = sum(ba_ac, na.rm = T)) %>% 
  ungroup()


################################################################################
# Calculate species-specific BAL & plot BA for each tree
################################################################################

# BA ---------------------------------------------------------------------------
sppba <- lapply(levels(nf_trees$SPCD), function(i) {
  temp <- nf_trees %>% 
    group_by(PLT_CN, SUBP) %>% 
    mutate(sppba = sum(ba_ac[SPCD == i & STATUSCD == 1])) %>% 
    ungroup()
  return(temp$sppba)
})

temp <- as.data.frame(do.call(cbind, sppba))
names(temp) <- paste0("ba_", levels(nf_trees$SPCD))
nf_trees <- cbind(nf_trees, temp)


# BAL --------------------------------------------------------------------------

# Calculates overtopping basal area (BAL) of one species only, assuming all 
# input trees are in same plot and ba is adjusted based on tpa:
sbal <- function(dbh, ba, spp, stat, sppref){
  sapply(dbh, function(x){
    index <- dbh > x & spp == sppref & stat == 1
    return(sum(ba[index]))
  })
}

sppbal <- lapply(levels(nf_trees$SPCD), function(i) {
  temp <- nf_trees %>% group_by(PLT_CN, SUBP) %>% 
    mutate(sppbal = sbal(DIA, 
                         ba_ac,
                         spp = SPCD, 
                         stat = STATUSCD, 
                         sppref = i)) %>% 
    ungroup()
  return(temp$sppbal)
})

temp <- as.data.frame(do.call(cbind, sppbal))
names(temp) <- paste0("bal_", levels(nf_trees$SPCD))
nf_trees <- cbind(nf_trees, temp)


################################################################################
# Combine FIA tables & reformat
################################################################################

# get before and after data and remeasurement period for each tree and -----
# add initial cond and plot data -------------------------------------------

nf_end <- nf_trees %>%
  left_join(nf_plots, by = "PLT_CN") %>%
  left_join(nf_conds, by = c("PLT_CN", "CONDID")) %>%
  rename(cn_e = CN, 
         plt_cn_e = PLT_CN, 
         condid_e = CONDID, 
         dbh_e = DIA, 
         statuscd_e = STATUSCD,
         mortyr_e = MORTYR, 
         cr_e = CR, 
         crown_class_e = CCLCD, 
         tree_class_e = TREECLCD, 
         MEASYEAR_E = MEASYEAR, 
         MEASMON_E = MEASMON, 
         MEASDAY_E = MEASDAY, 
         ba1_e = ba,
         bal1_e = bal, 
         ht_e = HT,
         forest_type_e = FORTYPCD, 
         stocking_e = ALSTKCD, 
         site_class_e = SITECLCD,
         landscape_e = PHYSCLCD, 
         slope_e = SLOPE, 
         aspect_e = ASPECT, 
         designcd_e = DESIGNCD) %>%
  select(-SPCD, -starts_with("ba_"), -starts_with("bal_"), -SUBP)

nf_start <- nf_trees %>%
  filter(CN %in% nf_end$PREV_TRE_CN) %>% 
  left_join(nf_plots, by = "PLT_CN") %>% 
  left_join(nf_conds, by = c("PLT_CN", "CONDID")) %>%
  rename(cn_s = CN, 
         plt_cn_s = PLT_CN, 
         condid_s = CONDID, 
         dbh_s = DIA, 
         statuscd_s = STATUSCD, 
         mortyr_s = MORTYR, 
         cr_s = CR, 
         crown_class_s = CCLCD, 
         tree_class_s = TREECLCD, 
         MEASYEAR_S = MEASYEAR, 
         MEASMON_S = MEASMON, 
         MEASDAY_S = MEASDAY, 
         ba1_s = ba, 
         bal1_s = bal, 
         ht_s = HT, 
         forest_type_s = FORTYPCD, 
         stocking_s = ALSTKCD, 
         site_class_s = SITECLCD,
         landscape_s = PHYSCLCD, 
         slope_s = SLOPE, 
         aspect_s = ASPECT, 
         designcd_s = DESIGNCD) %>%
  select(-PREV_TRE_CN, -PREV_PLT_CN, -LAT, -LON, -ELEV)

nf_fia <- nf_end %>%
  left_join(nf_start, by = c("PREV_TRE_CN" = "cn_s")) %>%
  filter(statuscd_s == 1, # only trees that started live
         statuscd_e != 0, # remove trees that were remeasured incorrectly
         cr_s >= 0,       # only trees that had cr at start 
         designcd_s == 1, # only those with current plot design 
         designcd_e == 1) %>%   
  mutate(MEASMON_E = formatC(MEASMON_E, width = 2, format = "d", flag = "0"), 
         MEASMON_S = formatC(MEASMON_S, width = 2, format = "d", flag = "0"),
         MEASDAY_E = formatC(MEASDAY_E, width = 2, format = "d", flag = "0"),
         MEASDAY_S = formatC(MEASDAY_S, width = 2, format = "d", flag = "0"),
         #make month and day codes 2 digits
         date_s = ymd(paste0(MEASYEAR_S, MEASMON_S, MEASDAY_S)),
         date_e = ymd(paste0(MEASYEAR_E, MEASMON_E, MEASDAY_E))) %>%
  # remove incorrectly entered dates (eg. Feb 31)
  filter(!is.na(date_e), !is.na(date_s)) %>% 
  mutate(interval = as.double(as.period(date_e - date_s), unit = "years"),
         cr_rate = (cr_e - cr_s)/interval,
         cr_mid = (cr_e + cr_s)/2,
         dbh_rate = (dbh_e - dbh_s)/interval,
         dbh_mid = (dbh_e + dbh_s)/2,
         ba1_mid = (ba1_e + ba1_s)/2,
         bal1_mid = (bal1_e + bal1_s)/2,
         ht_mid = (ht_e + ht_s)/2,
         ht_rate = (ht_e - ht_s)/interval,
         status_change = case_when(statuscd_e == 1 ~ "lived",
                                   statuscd_e == 2 ~ "died",
                                   statuscd_e == 3 ~ "cut",
                                   TRUE ~ "error"),
         status_change = as.factor(status_change),
         SPCD = as.factor(SPCD),
         plt_cn_e = as.factor(plt_cn_e)) %>%
  rename(ba1_ac = ba_ac) %>% 
  select(cn_e, spp = SPCD, dbh_e, dbh_rate, cr_s, cr_mid, 
         cr_e, cr_rate, crown_class_s, crown_class_e, tree_class_s, 
         tree_class_e, ba1_s, ba1_mid, ba1_e,
         bal1_s, bal1_mid, bal1_e, ht_s, ht_mid, ht_e, ht_rate,
         forest_type_s, forest_type_e, stocking_s, stocking_e, 
         landscape_s, landscape_e, site_class_s, site_class_e, 
         slope_s, slope_e, aspect_s, aspect_e, lat = LAT, lon = LON, 
         elev = ELEV, date_s, date_e, interval, status_change,
         plot = plt_cn_e, ba1_ac, plt_cn_s, SUBP,
         starts_with("ba_"), starts_with("bal_")) %>% 
  # mortality year was all null and was removed
  inner_join(nf_grms, by = c("cn_e" = "TRE_CN")) %>% 
  # use these dbh's b/c inconsistencies have been resolved (eg. measurements at different heights)
  rename(dbh_s = DIA_BEGIN, dbh_mid = DIA_MIDPT, 
         dbh_rate_fia = ANN_DIA_GROWTH, state = STATECD) 
  

remove(nf_start, nf_end, nf_conds, nf_plots, nf_trees, nf_grms, states, 
       VT_counties, NH_counties, NY_counties, ME_counties, pbal, state,
       sppba, sppbal, temp, species, species_codes, sbal)


# Keep starting values for fixed variables & ------------------------------
# only keep records with all neccessary fields ----------------------------

nf_fia <- nf_fia %>%
  rename(landscape = landscape_s,
         site_class = site_class_s,
         slope = slope_s,
         aspect = aspect_s) %>%
  select(-landscape_e, -site_class_e, -slope_e, -aspect_e) %>%
  filter(!is.na(spp), # only keep records with all neccessary fields
         !is.na(dbh_s), # could these affect some trees in a plot but not all?
         !is.na(crown_class_s),
         !is.na(tree_class_s),
         !is.na(ba1_s),
         !is.na(bal1_s),
         !is.na(forest_type_s),
         !is.na(forest_type_e),
         !is.na(stocking_s),
         !is.na(stocking_e),
         !is.na(landscape),
         !is.na(site_class),
         !is.na(slope),
         !is.na(aspect),
         !is.na(lat),
         !is.na(lon),
         !is.na(elev),
         !is.na(status_change),
         xor(is.na(bal1_e), status_change == "lived"))


################################################################################
# Make names and factor levels more intuitive 
################################################################################

# Forest types -----------------------------------------------------------------

forest_type_codes <- 
  c(101, 102, 103, 104, 105, 121, 122, 123, 124, 125, 126, 127, 
    167, 171, 381, 384, 385, 401, 402, 409, 503, 505, 509, 512, 
    513, 515, 516, 517, 519, 520, 701, 702, 703, 704, 705, 706, 
    707, 708, 709, 801, 802, 805, 809, 901, 902, 903, 904, 905, 
    962, 995, 999)

forest_types <- 
  c("Red pine", "Red pine", "White pine", "Mixed softwood", "Hemlock",
    "Spruce-fir", "Spruce-fir", "Spruce-fir", "Spruce-fir", "Spruce-fir",
    "Larch", "Cedar", "Mixed softwood", "Mixed softwood", "Scots pine", 
    "Norway spruce", "Larch", "Pine-hardwood", "Mixedwood",
    "Pine-hardwood", "Oak-hickory", "Oak-hickory", "Oak-hickory", 
    "Transition hardwood", "Transition hardwood", "Oak-hickory", 
    "Transition hardwood", "Transition hardwood", "Northern hardwood", 
    "Northern hardwood", "Northern hardwood", "Transition hardwood", 
    "Cottonwood", "Other", "Other", "Other", "Northern hardwood", 
    "Northern hardwood", "Cottonwood", "Northern hardwood", 
    "Northern hardwood", "Northern hardwood", "Northern hardwood", 
    "Northern hardwood", "Northern hardwood", "Northern hardwood", 
    "Northern hardwood", "Northern hardwood", "Other", "Other", 
    "Nonstocked")

names(forest_types) <- as.character(forest_type_codes)

nf_fia$forest_type_s <- 
  factor(unname(forest_types[as.character(nf_fia$forest_type_s)]),
         levels = levels(factor(forest_types)))

nf_fia$forest_type_e <- 
  factor(unname(forest_types[as.character(nf_fia$forest_type_e)]),
         levels = levels(factor(forest_types)))

# Landscapes -------------------------------------------------------------------

landscape_codes <- # add 19 & 33
  c(11, 12, 13, 19, 21, 22, 23, 24, 25, 29, 31, 32, 33, 34, 39)

landscapes <- 
  c("dry tops", "dry slopes", "deep sands", "other xeric", "flatwoods", 
    "rolling uplands", "moist slopes & coves", "narrow floodplains/bottomlands",
    "broad floodplains/bottomlands", "other mesic", "swamps/bogs", 
    "small drains", "small drains", "beaver ponds", "other hydric")

names(landscapes) <- as.character(landscape_codes)

nf_fia$landscape <- factor(unname(landscapes[as.character(nf_fia$landscape)]),
                           levels = levels(factor(landscapes)))


################################################################################
# Finish up
################################################################################

remove(forest_type_codes, forest_types, landscape_codes, landscapes)

# put columns in order
nf_fia <- nf_fia %>%
  select(spp, dbh_s, dbh_mid, dbh_e, dbh_rate, dbh_rate_fia, cr_s, cr_mid, 
         cr_e, cr_rate, crown_class_s, crown_class_e, tree_class_s, 
         tree_class_e, ba1_s, ba1_mid, ba1_e, bal1_s, bal1_mid, bal1_e, ht_s, 
         ht_mid, ht_e, ht_rate, forest_type_s, forest_type_e,
         stocking_s, stocking_e, landscape, site_class, slope, aspect,
         lat, lon, elev, state, date_s, date_e, interval, status_change,
         plot, ba1_ac, plt_cn_s, SUBP,
         starts_with("ba_"), starts_with("bal_")) %>% 
  rename(ba_s = ba1_s, ba_mid = ba1_mid, ba_e = ba1_e,
         bal_s = bal1_s, bal_mid = bal1_mid, bal_e = bal1_e,
         ba_ac = ba1_ac)


# Save ---------------------------------------------------------------------

save(nf_fia, file = "rda/nf-fia.rda")

