library("tidyverse")

weiskittel <- 
  tibble(spp = c("beech",
                 "ash",
                 "black cherry",
                 "fir",
                 "hemlock",
                 "other hardwood",
                 "other softwood",
                 "aspen",
                 "soft maple",
                 "red pine",
                 "red oak",
                 "hard maple",
                 "spruce",
                 "cedar",
                 "white pine",
                 "yellow birch"),
         ht_mean = c(18.48, 22.67, 20.05, 17.21, 19.59, 19.39, 21.47, NA,
                     21.39, 23.00, NA, 23.40, 18.96, NA, 23.25, 21.37),
         dbh_growth_mean = c(.3, .3, NA, .31, .3, .23, .23, .36, .25,
                             .2, .35, .26, .18, .17, .38, .25),
         ht_growth_mean = c(.23, .22, .16, .29, .17, .22, .2, .21,
                            .19, .21, .13, .16, .14, .19, .21, .16),
         ht_rmse = c(2.9021, 2.8813, 2.6475, 2.8830, 2.7909, 3.7033,
                     4.5155, NA, 2.7841, 2.7722, NA, 2.9732, 2.5105,
                     NA, 3.1214, 2.7534),
         dbh_growth_rmse = c(.1508, .2030, NA, .1972, .1437, .1685,
                             .1234, .2122, .1445, .1259, .2218, .1616,
                             .1259, .1108, .2197, .1595),
         ht_growth_rmse = c(.1640, .1555, .1399, .1537, .1337, .1820,
                            .1471, .2232, .1801, .1636, .1420, .1436,
                            .1235, .1123, .1596, .1277),
         mort_auc = c(.7740, .6962, NA, .6936, .6301, .7313, .6767, .7304,
                      .6926, .9483, .7962, .7453, .7034, .8157, .798, .7298))

weiskittel <- weiskittel %>% 
  mutate(ht_nrmse = ht_rmse/ht_mean,
         dbh_growth_nrmse = dbh_growth_rmse/dbh_growth_mean,
         ht_growth_nrmse = ht_growth_rmse/ht_growth_mean)

weis_dbh_growth <- weiskittel %>% 
  mutate(dbh_growth_mean = dbh_growth_mean * 0.39370, 
         dbh_growth_rmse = dbh_growth_rmse * 0.39370,
         dbh_growth_nrmse = dbh_growth_nrmse * 0.39370) %>% 
  select(spp, dbh_growth_mean, dbh_growth_rmse, dbh_growth_nrmse)
