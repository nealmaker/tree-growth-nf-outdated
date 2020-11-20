
load("rda/nf-fia.rda")

library(tidyverse)
library(maps)


##############################
# EDA
##############################


# Examine vectors ---------------------------------------------------------

str(nf_fia)

summary(nf_fia)

lived <- filter(nf_fia, status_change == "lived")
died <- filter(nf_fia, status_change != "lived")

lived <- lived %>%
  arrange(desc(dbh_rate))

View(head(lived, 20))

View(tail(lived, 20))

nrow(filter(lived, dbh_rate < 0))

summary(lived)


# Distributions -----------------------------------------------------------

View(nf_fia %>% 
       group_by(spp) %>% 
       summarize(n = n(), percent = round(100*(n/nrow(nf_fia)))) %>%
       arrange(desc(n)))

View(nf_fia %>% 
       group_by(forest_type_s) %>% 
       summarize(n = n(), percent = round(100*(n/nrow(nf_fia)))) %>%
       arrange(desc(n)))

View(nf_fia %>% 
       group_by(forest_type_e) %>% 
       summarize(n = n(), percent = round(100*(n/nrow(nf_fia)))) %>%
       arrange(desc(n)))

View(nf_fia %>% 
       group_by(landscape) %>% 
       summarize(n = n(), percent = round(100*(n/nrow(nf_fia)))) %>%
       arrange(desc(n)))

nf_fia %>% ggplot() + 
  geom_density(aes(dbh_s), col = "blue") +
  geom_density(aes(dbh_mid), col = "purple") +
  geom_density(aes(dbh_e), col = "green")

nf_fia %>% ggplot(aes(dbh_rate)) + 
  geom_density(bw = .02) + 
  scale_x_continuous(limits = c(-.2, .6))

nf_fia %>% ggplot() + 
  geom_density(aes(cr_s), bw = 10, col = "blue") +
  geom_density(aes(cr_mid), bw = 10, col = "purple") +
  geom_density(aes(cr_e), bw = 10, col = "green")

nf_fia %>% ggplot(aes(cr_rate)) + 
  geom_density(bw = 1) + 
  scale_x_continuous(limits = c(-12, 12))

nf_fia %>% ggplot() + 
  geom_density(aes(ht_s), col = "blue") +
  geom_density(aes(ht_mid), col = "purple") +
  geom_density(aes(ht_e), col = "green")

nf_fia %>% ggplot(aes(dbh_rate)) + 
  geom_density(bw = .01) 

nf_fia %>% ggplot() +
  geom_histogram(aes(crown_class_s), fill = "blue", alpha = .2) +
  geom_histogram(aes(crown_class_e), fill = "red", alpha = .2)
  
nf_fia %>% ggplot() +
  geom_histogram(aes(tree_class_s), fill = "blue", alpha = .2) +
  geom_histogram(aes(tree_class_e), fill = "red", alpha = .2)

nf_fia %>% ggplot() + 
  geom_density(aes(ba_s), bw = 10, col = "blue") +
  geom_density(aes(ba_mid), bw = 10, col = "purple") +
  geom_density(aes(ba_e), bw = 10, col = "green")

nf_fia %>% ggplot() + 
  geom_density(aes(bal_s), bw = 10, col = "blue") +
  geom_density(aes(bal_mid), bw = 10, col = "purple") +
  geom_density(aes(bal_e), bw = 10, col = "green")

nf_fia %>% ggplot() +
  geom_histogram(aes(stocking_s), fill = "blue", alpha = .2, bins = 5) +
  geom_histogram(aes(stocking_e), fill = "red", alpha = .2, bins = 5)

nf_fia %>% ggplot() +
  geom_histogram(aes(site_class), bins = 5)

nf_fia %>% ggplot() +
  geom_density(aes(slope), bw = 5)

nf_fia %>% ggplot() +
  geom_density(aes(aspect), bw = 5)

nf_fia %>% ggplot() +
  geom_density(aes(lat), bw = .2)

nf_fia %>% ggplot() +
  geom_density(aes(lon), bw = .2)

nf_fia %>% ggplot() +
  geom_density(aes(elev), bw = 100)

nf_fia %>% ggplot() +
  geom_density(aes(date_s), bw = 500, col = "blue") +
  geom_density(aes(date_e), bw = 500, col = "red")

nf_fia %>% ggplot() +
  geom_density(aes(interval), bw = .20)

length(unique(nf_fia$plot))


# Distribution of growth rates --------------------------------------------

lived %>%
  group_by(spp) %>%
  filter(n()>=20) %>%
  ungroup() %>%
  ggplot(aes(dbh_rate)) +
  geom_density(bw = .01, fill = "dark gray") +
  geom_vline(xintercept = median(nf_fia$dbh_rate, na.rm = T), 
             col = "dark green", size = 1) +
  scale_x_continuous(limits = c(-.1, .6)) +
  facet_wrap(~ spp, ncol = 5)

View(lived %>% 
  group_by(spp) %>% 
  summarize(n = n(),
          dbh_rate = mean(dbh_rate),
          cr_rate = mean(cr_rate),
          dbh = mean(dbh_mid)) %>%
    arrange(dbh_rate))


# Look for plots w/o condition data ---------------------------------------

#number of rows with some na's somewhere
dim(nf_fia)[1] - dim(nf_fia[rowSums(is.na(nf_fia)) == 0,])[1] 

# should all be trees that died and are ligitimately missing end measures.
missing <- nf_fia[rowSums(is.na(nf_fia)) > 0,]
summary(missing)


# PLot relationships to dbh_rate ------------------------------------------

lived %>% 
  mutate(forest_type_s = reorder(forest_type_s, -dbh_rate, FUN = median)) %>%
  ggplot(aes(forest_type_s, dbh_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  mutate(site_class = factor(site_class),
         site_class = reorder(site_class, -dbh_rate, FUN = median)) %>%
  ggplot(aes(site_class, dbh_rate)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-.25, 1))
# Why isn't there a clear relationship to site class for sites 3:6? 

lived %>%
  mutate(site_class = factor(site_class),
         site_class = reorder(site_class, -dbh_rate, FUN = median)) %>%
  ggplot(aes(site_class, dbh_rate)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-.25, 1)) +
  facet_wrap(~ spp)
# not enough to show underlying influence of site class

lived %>%
  ggplot(aes(slope, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  ggplot(aes(slope, dbh_rate)) +
  geom_smooth(col = "red") 
        
lived %>%
  ggplot(aes(aspect, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  ggplot(aes(aspect, dbh_rate)) +
  geom_smooth(col = "red") 

lived %>%
  mutate(landscape = reorder(landscape, -dbh_rate, FUN = median)) %>%
  ggplot(aes(landscape, dbh_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-.25, 1))
# almost no relationship, but what's there is unexpected

lived %>% 
  group_by(landscape) %>%
  summarize(n = n(),
            mean_cr = mean(cr_s),
            mean_stocking = mean(stocking_s),
            mean_ba = mean(ba_mid))
# not a regularization problem
# it's b/c beaver ponds have lowest mean ba and highest mean cr

lived %>% 
  filter(landscape == "deep sands") %>% 
  group_by(spp) %>%
  summarize(n = n(), mean_growth = mean(dbh_rate))
# deep sands look good because they have so much pine 

lived %>% 
  filter(landscape == "beaver ponds") %>% 
  group_by(spp) %>%
  summarize(n = n(), mean_growth = mean(dbh_rate))
# beaver ponds have fir and soft maple (both med growing)

lived %>%
  mutate(stocking_s = factor(stocking_s)) %>%
  ggplot(aes(stocking_s, dbh_rate)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  ggplot(aes(ba_mid, dbh_rate)) +
  geom_smooth(col = "red")

lived %>%
  ggplot(aes(ba_mid, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1))
# What's with the plots w/ BA>500?

lived %>%
  filter(ba_mid > 500) %>%
  group_by(forest_type_s) %>%
  summarize(n = n(), mean_growth = mean(dbh_rate))
# high ba are mostly white pine & northern hardwood stands

lived %>%
  filter(ba_mid > 500) %>%
  ggplot(aes(ba_mid, dbh_rate, col = forest_type_s)) +
  geom_point()

View(filter(nf_fia, ba_mid > 950)) # it's one plot with a lot of ash

# these all look like mistakes:
filter(nf_fia, ba_mid > 500) %>% 
  mutate(ba_ac = if_else(dbh_mid >= 5, 
                         0.005454*dbh_mid^2*(43560/(pi*24^2)),
                         0.005454*dbh_mid^2*(43560/(pi*6.8^2)))) %>%
  group_by(plot) %>%
  summarize(ba_calc = sum(ba_ac))

lived %>%
  filter(ba_mid < 30) %>%
  group_by(forest_type_s) %>%
  summarize(n = n(), mean_growth = mean(dbh_rate)) %>%
  arrange(mean_growth)
# low ba are mostly nhw & sp/fr types

lived %>%
  ggplot(aes(lat, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1)) 

lived %>%
  ggplot(aes(lat, dbh_rate)) +
  geom_smooth(col = "red")

lived %>%
  ggplot(aes(lon, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  ggplot(aes(lon, dbh_rate)) +
  geom_smooth(col = "red")

lived %>% 
  mutate(spp = reorder(spp, -dbh_rate, FUN = median)) %>%
  ggplot(aes(spp, dbh_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  mutate(crown_class_s = factor(crown_class_s)) %>%
  ggplot(aes(crown_class_s, dbh_rate)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  ggplot(aes(cr_mid, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red")

lived %>%
  ggplot(aes(cr_mid, dbh_rate)) +
  geom_smooth(col = "red")

lived %>%
  ggplot(aes(bal_mid, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1))

lived %>%
  ggplot(aes(bal_mid, dbh_rate)) +
  geom_smooth(col = "red")

lived %>%
  ggplot(aes(dbh_mid, dbh_rate)) +
  geom_jitter(alpha = .06) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.25, 1)) +
  facet_wrap(~ spp)


# Plot relationships between predictors ---------------------------------

nf_fia %>%
  ggplot(aes(lon, lat)) +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group),
               fill = NA, col = "dark gray") +
  geom_point(size = .2) +
  coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

lived %>%
  filter(dbh_rate < .3) %>%
  ggplot(aes(lon, lat)) +
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group),
               fill = NA, col = "dark gray") +
  geom_point(size = 1, aes(col = dbh_rate)) +
  scale_color_gradientn(colors = rev(rainbow(5))) +
  coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Cuts continuous var into bins & returns midpoint value:
cut2 <- function(x, breaks) {
  r <- range(x)
  b <- seq(r[1], r[2], length=2*breaks+1)
  brk <- b[0:breaks*2+1]
  mid <- b[1:breaks*2]
  brk[1] <- brk[1]-0.01
  k <- cut(x, breaks=brk, labels=FALSE)
  mid[k]
}

# 2d bin smoothed mapping of diameter growth
lived %>%
  mutate(groupx = cut2(lon, 30), #binning lat & lon
         groupy = cut2(lat, 20)) %>%
  group_by(groupx, groupy) %>%
  summarize(growth = mean(dbh_rate, na.rm = T)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = growth), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 60") +
  scale_fill_viridis_c(option = "plasma", name = "in/year") +
  coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

nf_fia %>%
  group_by(status_change) %>%
  summarize(mean_dbh = mean(dbh_s),
            cr = mean(cr_s),
            crown_class = mean(crown_class_s),
            ba = mean(ba_s),
            bal = mean(bal_s),
            stocking = mean(stocking_s))

nf_fia %>%
  ggplot(aes(dbh_s, fill = status_change)) +
  geom_density(alpha = .7) + 
  scale_x_continuous(limits = c(0, 25)) +
  facet_grid(rows = factor(crown_class), cols = status_change)


######################################
## Height and Height Growth
######################################
ht_data <- nf_fia %>% 
  filter(!is.na(ht_e)) %>% 
  mutate(ht_rate_calc = (ht_e - ht_s)/interval,
         ht_rate_err = ht_rate - ht_rate_calc)

summary(select(ht_data, ht_s, ht_e, ht_rate, ht_rate_calc, ht_rate_err))

mean(ht_data$ht_rate_calc < 0, na.rm = T)


######################################
## DBH and DBH Growth
######################################
dbh_data <- nf_fia %>% 
  filter(!is.na(dbh_e)) %>% 
  mutate(dbh_rate_calc = (dbh_e - dbh_s)/interval,
         dbh_rate_err = dbh_rate - dbh_rate_calc)

summary(select(dbh_data, dbh_s, dbh_e, dbh_rate, dbh_rate_calc, dbh_rate_err))

dbh_data %>% ggplot(aes(dbh_rate_err)) + 
  geom_density() + 
  scale_x_continuous(limits = c(-.5, .2))

mean(dbh_data$dbh_rate_calc < 0)
mean(dbh_data$dbh_rate_err == 0)
