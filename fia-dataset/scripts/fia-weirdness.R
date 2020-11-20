library("tidyverse")
library("lubridate")

##################
## Just VT data
## for sharing
#################
########## NEED PLOT TABLE FOR MEAS YEAR/MO/DAY & DESIGNCD !!!!!!!!!!!!!!!!!!!
temp <- tempfile()


# TREE table ________________________

download.file("https://apps.fs.usda.gov/fia/datamart/CSV/VT_TREE.zip",
                temp, mode = "wb")

unzip(temp, "VT_TREE.csv")

TREE <- read.csv("VT_TREE.csv", header = T) %>% 
  filter(DIAHTCD == 1) # excludes seedlings measured at root collar


# Plot table ________________________

download.file("https://apps.fs.usda.gov/fia/datamart/CSV/VT_PLOT.zip",
              temp, mode = "wb")

unzip(temp, "VT_PLOT.csv")

PLOT <- read.csv("VT_PLOT.csv", header = T) %>%
  rename(PLT_CN = CN) 


# GRM Component table _________________

download.file("https://apps.fs.usda.gov/fia/datamart/CSV/VT_TREE_GRM_COMPONENT.zip",
                temp, mode = "wb")

unzip(temp, "VT_TREE_GRM_COMPONENT.csv")

GRM <- read.csv("VT_TREE_GRM_COMPONENT.csv", header = T) %>% 
    filter(!is.na(ANN_DIA_GROWTH))


# GRM ESTN table (for remeasurement period) _________________

download.file("https://apps.fs.usda.gov/fia/datamart/CSV/VT_TREE_GRM_ESTN.zip",
              temp, mode = "wb")

unzip(temp, "VT_TREE_GRM_ESTN.csv")

GRM_ESTN <- read.csv("VT_TREE_GRM_ESTN.csv", header = T)


# Combine them _______________________

trees_end <- TREE %>%
  filter(PREV_TRE_CN > 0) %>% 
  left_join(PLOT, by = "PLT_CN") %>%
  rename(cn_e = CN, 
         plt_cn_e = PLT_CN, 
         dbh_e = DIA, 
         statuscd_e = STATUSCD,
         ht_e = HT, 
         MEASYEAR_E = MEASYEAR, 
         MEASMON_E = MEASMON, 
         MEASDAY_E = MEASDAY,
         designcd_e = DESIGNCD)

trees_start <- TREE %>% 
  filter(CN %in% trees_end$PREV_TRE_CN) %>% 
  left_join(PLOT, by = "PLT_CN") %>%
  rename(cn_s = CN, 
         plt_cn_s = PLT_CN, 
         dbh_s = DIA, 
         statuscd_s = STATUSCD,
         ht_s = HT, 
         MEASYEAR_S = MEASYEAR, 
         MEASMON_S = MEASMON, 
         MEASDAY_S = MEASDAY,
         designcd_S = DESIGNCD) %>% 
  select(-SPCD)

fia_vt <- trees_end %>%
  left_join(trees_start, by = c("PREV_TRE_CN" = "cn_s")) %>%
  filter(statuscd_s == 1, # only trees that started live
         statuscd_e != 0) %>% # remove trees that were remeasured incorrectly
  mutate(MEASMON_E = formatC(MEASMON_E, width = 2, format = "d", flag = "0"), 
         MEASMON_S = formatC(MEASMON_S, width = 2, format = "d", flag = "0"),
         MEASDAY_E = formatC(MEASDAY_E, width = 2, format = "d", flag = "0"),
         MEASDAY_E = formatC(MEASDAY_S, width = 2, format = "d", flag = "0"),
         #make month and day codes 2 digits
         date_s = ymd(paste(MEASYEAR_S, MEASMON_S, MEASDAY_S, sep = "")),
         date_e = ymd(paste(MEASYEAR_E, MEASMON_E, MEASDAY_E, sep = ""))) %>%
  filter(!is.na(date_e), !is.na(date_s)) %>% 
  # remove incorrectly entered dates (eg. Feb 31)
  mutate(interval = as.double(as.period(date_e - date_s), unit = "years"),
         dbh_rate = (dbh_e - dbh_s)/interval,
         dbh_mid = (dbh_e + dbh_s)/2,
         ht_mid = (ht_e + ht_s)/2,
         ht_rate = (ht_e - ht_s)/interval,
         status_change = case_when(statuscd_e == 1 ~ "lived",
                                   statuscd_e == 2 ~ "died",
                                   statuscd_e == 3 ~ "cut",
                                   TRUE ~ "error"),
         status_change = as.factor(status_change),
         SPCD = as.factor(SPCD),
         plt_cn_e = as.factor(plt_cn_e)) %>%
  inner_join(GRM, by = c("cn_e" = "TRE_CN")) %>% 
  left_join(select(GRM_ESTN, TRE_CN, REMPER), by = c("cn_e" = "TRE_CN"))


# Diameter Change _________________________________________

dbh_table <- fia_vt %>% 
  select(TRE_CN = cn_e, dbh_s, DIA_BEGIN, dbh_e, DIA_END, interval, REMPER, 
         ANN_DIA_GROWTH) %>% 
  mutate(DIA_GROWTH_CALC = (DIA_END - DIA_BEGIN)/REMPER,
         dbh_growth = (dbh_e - dbh_s)/interval)


# Height change ___________________________________________

ht_table <- fia_vt %>% 
  select(TRE_CN = cn_e, ht_s, HT_BEGIN, ht_e, HT_END, interval, 
         REMPER, ANN_HT_GROWTH) %>% 
  mutate(HT_GROWTH_CALC = (HT_END - HT_BEGIN)/REMPER,
         ht_growth = (ht_e - ht_s)/interval)
  