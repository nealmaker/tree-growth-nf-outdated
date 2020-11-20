library("tidyverse")
library("maps")
library("caret")
library("Rborist")
library("extrafont")
library("knitr")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(dbh_rate, spp, dbh_mid, cr_mid, crown_class_s, tree_class_s,
         ba_mid, bal_mid, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev, state)

# test set is 20% of full dataset
test_size <- .2

set.seed(10)
index <- createDataPartition(nf_fia$dbh_rate, times = 1, p = test_size, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]


#######################
# distributions
#######################

summary(train)

View(train %>% 
  group_by(spp, dbh_rate > .4) %>% 
  summarize(n = n(),
            dbh = mean(dbh_mid),
            cr = mean(cr_mid)))

View(train %>% 
       group_by(spp, dbh_rate > -.5) %>% 
       summarize(n = n(),
                 dbh = mean(dbh_mid),
                 cr = mean(cr_mid)))

View(train %>% 
       filter(dbh_rate > .4) %>% 
       group_by(spp) %>% 
       summarize(n = n(),
                 cr = mean(cr_mid)) %>% 
       arrange(-n))

train %>% ggplot(aes(dbh_rate)) +
  geom_density(bw = .02) +
  scale_x_continuous(limits = c(-.2, .6))

View(train %>% 
       group_by(spp) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 cr = mean(cr_mid),
                 crown_class = mean(crown_class_s),
                 bal = mean(bal_mid),
                 site_class = mean(site_class),
                 slope = mean(slope),
                 aspect = mean(aspect),
                 elev = mean(elev)) %>% 
       arrange(-dbh_rate))

# what if we control for some things?
# Adk trees 10 - 12" on site class 6
View(train %>% 
       filter(lon < -73.4,
              dbh_mid > 10,
              dbh_mid < 12,
              site_class == 6) %>% 
       group_by(spp) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 cr = mean(cr_mid),
                 crown_class = mean(crown_class_s),
                 bal = mean(bal_mid),
                 site_class = mean(site_class),
                 slope = mean(slope),
                 aspect = mean(aspect),
                 elev = mean(elev)) %>% 
       arrange(-dbh_rate))

# ME trees 16 - 20" on site class 6
View(train %>% 
       filter(lon > -70.8,
              dbh_mid > 16,
              dbh_mid < 20,
              site_class == 6) %>% 
       group_by(spp) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 cr = mean(cr_mid),
                 crown_class = mean(crown_class_s),
                 bal = mean(bal_mid),
                 site_class = mean(site_class),
                 slope = mean(slope),
                 aspect = mean(aspect),
                 elev = mean(elev)) %>% 
       arrange(-dbh_rate))

train %>% ggplot(aes(dbh_mid)) +
  geom_density(bw = 1) +
  scale_x_continuous(limits = c(0, 40))

train %>% ggplot(aes(cr_mid)) +
  geom_density(bw = 10) 

View(train %>% 
       group_by(crown_class_s) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 bal = mean(bal_mid))) 

View(train %>% 
       group_by(tree_class_s) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 bal = mean(bal_mid)))

train %>% ggplot(aes(ba_mid)) +
  geom_density(bw = 10)

train %>% ggplot(aes(bal_mid)) +
  geom_density(bw = 5)

View(train %>% 
       group_by(forest_type_s) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 ba = mean(ba_mid),
                 site_class = mean(site_class),
                 elev = mean(elev)) %>% 
       arrange(-dbh_rate)) 

View(train %>% 
       group_by(stocking_s) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 bal = mean(bal_mid)))

View(train %>% 
       group_by(landscape) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 ba = mean(ba_mid),
                 site_class = mean(site_class),
                 elev = mean(elev)) %>% 
       arrange(-dbh_rate))

View(train %>% 
       group_by(site_class) %>% 
       summarize(n = n(),
                 dbh_rate = mean(dbh_rate),
                 dbh = mean(dbh_mid),
                 bal = mean(bal_mid)))

train %>% ggplot(aes(slope)) +
  geom_density(bw = 1) 

train %>% ggplot(aes(aspect)) +
  geom_density(bw = 5) 

train %>% ggplot(aes(lat)) +
  geom_density(bw = .1)

train %>% ggplot(aes(lon)) +
  geom_density(bw = .2)

train %>% ggplot(aes(elev)) +
  geom_density(bw = 80)


#######################
# growth relationships
#######################

train %>%
  mutate(spp = reorder(spp, -dbh_rate, FUN = mean)) %>%
  ggplot(aes(spp, dbh_rate)) +
  geom_boxplot(fill = "gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "species group")

train %>%
  ggplot(aes(dbh_mid, dbh_rate)) +
  geom_smooth(col = "red")

# slow and useless
train %>%
  ggplot(aes(dbh_mid, dbh_rate)) +
  geom_jitter(alpha = .03) +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = c(-.2, .7))

# arbitrary spp, nice looking
train %>% 
  filter(spp %in% c("fir", "spruce", "soft maple",
                    "hard maple", "cedar", "hemlock",
                    "yellow birch", "white pine"),
         dbh_rate > -1) %>%
  mutate(spp = reorder(spp, -dbh_rate, FUN = mean)) %>%
  ggplot(aes(dbh_mid, dbh_rate, col = spp)) +
  geom_smooth(method.args = list(gamma = 35)) +
  scale_x_continuous(name = "dbh (in)") +
  scale_y_continuous(name = expression(Delta~'dbh (in/yr)')) +
  scale_color_brewer(type = "qual",
                     name = "species")

train %>% 
  filter(spp %in% c("hemlock", "tamarack", "fir", "spruce", 
                    "cedar", "white pine", "red pine"),
         dbh_rate > -1) %>%
  mutate(spp = reorder(spp, -dbh_rate, FUN = mean)) %>%
  ggplot(aes(dbh_mid, dbh_rate, col = spp)) +
  geom_smooth(method.args = list(gamma = 35)) +
  scale_x_continuous(name = "dbh (in)") +
  scale_y_continuous(name = expression(Delta~'dbh (in/yr)')) +
  scale_color_brewer(type = "qual",
                     name = "species")

train %>% 
  filter(spp %in% c("red oak", "aspen", "ash", "hard maple", 
                    "beech", "soft maple", "yellow birch",
                    "paper birch"),
         dbh_rate > -1) %>%
  mutate(spp = reorder(spp, -dbh_rate, FUN = mean)) %>%
  ggplot(aes(dbh_mid, dbh_rate, col = spp)) +
  geom_smooth(method.args = list(gamma = 35)) +
  scale_x_continuous(name = "dbh (in)") +
  scale_y_continuous(name = expression(Delta~'dbh (in/yr)')) +
  scale_color_brewer(type = "qual",
                     name = "species")

train %>% 
  filter(spp == "soft maple") %>% 
  ggplot(aes(dbh_mid, dbh_rate)) +
  geom_jitter(alpha = .06, width = 2, height = .005) +
  geom_smooth()

train %>% 
  filter(spp %in% c("red oak", "cedar")) %>%
  ggplot(aes(dbh_mid, dbh_rate, col = factor(state))) +
  geom_smooth(method.args = list(gamma = 5)) +
  scale_x_continuous(name = "dbh (in)", limits = c(0, 25)) +
  scale_y_continuous(name = expression(Delta~'dbh (in/yr)')) +
  scale_color_brewer(type = "qual",
                     direction = -1) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(~ spp)

train %>% 
  filter(spp %in% c("fir", "white pine", "cedar", "red oak")) %>%
  mutate(spp = reorder(spp, -dbh_rate, FUN = median)) %>%
  ggplot(aes(dbh_mid, dbh_rate, col = factor(state))) +
  geom_smooth(method.args = list(gamma = 35)) +
  scale_x_continuous(name = "dbh (in)", limits = c(0, 40)) +
  scale_y_continuous(name = expression(Delta~'dbh (in/yr)')) +
  scale_color_brewer(type = "qual",
                     name = "species") +
  facet_wrap(~ spp)

train %>%
  ggplot(aes(cr_mid, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 200))

# slow but nice
train %>%
  ggplot(aes(cr_mid, dbh_rate)) +
  geom_jitter(alpha = .02, width = 5, height = .02) +
  geom_smooth(col = "red",
              method.args = list(gamma = 200)) +
  scale_y_continuous(limits = c(-.05, .4))

train %>%
  ggplot(aes(factor(crown_class_s), dbh_rate)) +
  geom_boxplot(fill = "gray") +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "crown class")

train %>% 
  ggplot(aes(crown_class_s, dbh_rate)) +
  geom_jitter() +
  geom_smooth(method.args = list(gamma = 200),
              k = .1)

train %>%
  ggplot(aes(factor(tree_class_s), dbh_rate)) +
  geom_boxplot(fill = "gray") +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "tree class")

train %>%
  ggplot(aes(ba_mid, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 80))

# slow but nice
train %>%
  ggplot(aes(ba_mid, dbh_rate)) +
  geom_jitter(alpha = .02, width = 0, height = .02) +
  geom_smooth(col = "red",
              method.args = list(gamma = 80)) +
  scale_y_continuous(limits = c(-.05, .4))

train %>%
  ggplot(aes(bal_mid, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 200))

# slow but nice
train %>%
  ggplot(aes(bal_mid, dbh_rate)) +
  geom_jitter(alpha = .02, width = 0, height = .02) +
  geom_smooth(col = "red",
              method.args = list(gamma = 200)) +
  scale_y_continuous(limits = c(-.05, .4))

train %>%
  mutate(forest_type_s = reorder(forest_type_s, -dbh_rate, FUN = mean)) %>%
  ggplot(aes(forest_type_s, dbh_rate)) +
  geom_boxplot(fill = "gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "Forest type")

train %>%
  ggplot(aes(factor(stocking_s), dbh_rate)) +
  geom_boxplot(fill = "gray") +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "stocking")

train %>%
  mutate(landscape = reorder(landscape, -dbh_rate, FUN = mean)) %>%
  ggplot(aes(landscape, dbh_rate)) +
  geom_boxplot(fill = "gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "Landscape type")

train %>% group_by(landscape == "deep sands") %>% 
  summarize(pct_pine = 100*sum(spp == "white pine" | spp == "red pine")/n(),
            dbh = mean(dbh_mid))

View(train %>% group_by(landscape) %>% 
  summarize(rate = mean(dbh_rate), cr = mean(cr_mid), crown_class = mean(crown_class_s),
            dbh = mean(dbh_mid), stocking = mean(stocking_s), ba = mean(ba_mid),
            site_class = mean(site_class)) %>% 
  arrange(desc(rate)))

train %>% filter(landscape == "beaver ponds") %>% 
  group_by(spp) %>% 
  summarize(n = n(), pct = 100*(n/sum(train$landscape == "beaver ponds"))) %>% 
  arrange(desc(n))

train %>% filter(landscape != "beaver ponds") %>% 
  group_by(spp) %>% 
  summarize(n = n(), pct = 100*(n/sum(train$landscape != "beaver ponds"))) %>% 
  arrange(desc(n))

train %>%
  ggplot(aes(factor(site_class), dbh_rate)) +
  geom_boxplot(fill = "gray") +
  scale_y_continuous(trans = "log",
                     breaks = c(.01, .02, .04, .08, .16, .32, .64, 1.28),
                     name = "diameter growth (in/yr)") +
  scale_x_discrete(name = "site class")

train %>%
  ggplot(aes(slope, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 30)) +
  scale_y_continuous(limits = c(0, .15))

train %>%
  ggplot(aes(aspect, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 20)) +
  scale_y_continuous(limits = c(0, .15))

train %>%
  ggplot(aes(lat, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 10)) +
  scale_y_continuous(limits = c(0, .15))

train %>%
  ggplot(aes(lon, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 10)) +
  scale_y_continuous(limits = c(0, .15))

train %>%
  ggplot(aes(elev, dbh_rate)) +
  geom_smooth(col = "red",
              method.args = list(gamma = 10)) +
  scale_y_continuous(limits = c(0, .15))


#######################
# maps
#######################

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
train %>%
  mutate(groupx = cut2(lon, 14), #binning lat & lon
         groupy = cut2(lat, 14)) %>%
  group_by(groupx, groupy) %>%
  summarize(growth = mean(dbh_rate)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = growth), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_fill_viridis_c(option = "plasma", 
                       name = expression(Delta*'dbh (in/yr)')) +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua")) 

# Map of growth rates by spp
train %>%
  filter(spp %in% c("cedar", "soft maple", "hard maple", "fir")) %>% 
  mutate(groupx = cut2(lon, 13), #binning lat & lon
         groupy = cut2(lat, 13)) %>%
  group_by(spp, groupx, groupy) %>%
  summarize(growth = mean(dbh_rate)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = growth), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_fill_viridis_c(#direction = -1,
                       option = "plasma", 
                       name = expression(Delta*'dbh (in/yr)')) +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua")) +
facet_wrap(~ spp)

# Maps of spp relative abundance
train %>%
  mutate(groupx = cut2(lon, 14), #binning lat & lon
         groupy = cut2(lat, 14)) %>%
  group_by(groupx, groupy) %>%
  mutate(trees_bin = n()) %>% 
  ungroup() %>% 
  group_by(spp, groupx, groupy) %>% 
  summarize(pct = n()/mean(trees_bin)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = pct), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1,
                       name = expression(Delta*'relative /nabundance')) +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua")) +
facet_wrap(~ spp)


#######################
# correlations
#######################

library("PerformanceAnalytics")

cor_matrix <- cor(select(train, -dbh_rate, -spp, -forest_type_s, -landscape))

cor_comp <- cor(select(train, cr_mid, crown_class_s, ba_mid, bal_mid, stocking_s))

chart.Correlation(cor_comp, histogram=TRUE, pch=19)