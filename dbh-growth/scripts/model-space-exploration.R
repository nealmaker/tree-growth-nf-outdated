library("caret")
library("tidyverse")
load("../big-rdas/dbh-growth-model-op.rda")
load("../big-rdas/ht-model-op.rda")
load("../big-rdas/mort-model-op.rda")

######################
## Make fake data
######################

trees <- expand.grid(spp = c("hard maple", "paper birch", "hemlock"),
                     dbh = seq(2, 26, by = 4),
                     cr = seq(10, 80, by = 10),
                     ba = seq(0, 200, by = 40),
                     bal = seq(0, 120, by = 40),
                     forest_type = "Northern hardwood",
                     site_class = 5,
                     lat = 44.45,
                     lon = -72.65,
                     elev = 1200,
                     stringsAsFactors = FALSE) %>% 
  mutate(spp = factor(spp, 
                      levels = levels(dbh_growth_model_op$trainingData$spp)),
         forest_type = 
           factor(forest_type,
                  levels = levels(dbh_growth_model_op$trainingData$forest_type)))

trees <- trees %>% 
  mutate(dbh_rate = predict(dbh_growth_model_op, newdata = trees))
trees <- trees %>% 
  mutate(ht = predict(ht_model_op, newdata = trees))
trees <- trees %>% 
  mutate(dbh_rate = predict(dbh_growth_model_op, newdata = trees))

# Big version
trees2 <- expand.grid(spp = c("fir",
                              "spruce",
                              "soft maple",
                              "hard maple",
                              "cedar",
                              "beech",
                              "yellow birch",
                              "paper birch",
                              "hemlock",
                              "white pine",
                              "red oak",
                              "black cherry"),
                     dbh = seq(2, 26, by = 4),
                     cr = seq(10, 80, by = 10),
                     ba = seq(10, 220, by = 30),
                     rbal = seq(0, 90, by = 10),
                     forest_type = "Northern hardwood",
                     site_class = 5,
                     lat = seq(44, 47, by = .5),
                     lon = seq(-75, -68, by = 1),
                     elev = 1000,
                     stringsAsFactors = FALSE) %>% 
  mutate(spp = factor(spp, 
                      levels = levels(dbh_growth_model_op$trainingData$spp)),
         forest_type = 
           factor(forest_type,
                  levels = levels(dbh_growth_model_op$trainingData$forest_type)),
         bal = (rbal / 100) * ba) #%>% 
  filter(bal <= ba)

trees2 <- trees2 %>% 
  mutate(rem = case_when(lat > 45 & lon < -70 ~ TRUE,
                         lat == 47 & lon == -70 ~ TRUE,
                         lat < 44.5 & lon > -70 ~ TRUE,
                         TRUE ~ FALSE))

trees2 <- trees2 %>% filter(rem == FALSE) %>% select(-rem) #%>% 
  mutate(rbal = round(bal/ba, 2))

trees2 <- trees2 %>% 
  mutate(dbh_rate = predict(dbh_growth_model_op, newdata = trees2))
trees2 <- trees2 %>% 
  mutate(ht = predict(ht_model_op, newdata = trees2))
trees2 <- trees2 %>% 
  mutate(surv = predict(mort_model_op, 
                        newdata = mutate(trees2, interval = 5), 
                        type = "prob")[,2])

######################
## Explore
######################

trees %>% filter(bal == 0) %>% 
  mutate(ba = as.factor(ba)) %>% 
  ggplot(aes(cr, dbh_rate, color = ba)) +
  geom_line() + 
  facet_grid(dbh ~ spp) +
  labs(title = "DBH growth model space",
       subtitle = "For three species, faceted by DBH")

trees %>% filter(bal == 0,
                 spp == "hard maple") %>% 
  mutate(ba = as.factor(ba)) %>% 
  ggplot(aes(cr, dbh_rate, color = ba)) +
  geom_line() + 
  facet_wrap(~ dbh) +
  labs(title = "DBH growth model space",
       subtitle = "Hard maples, faceted by DBH")

trees %>% filter(bal == 0,
                 spp == "hemlock") %>% 
  mutate(ba = as.factor(ba)) %>% 
  ggplot(aes(cr, dbh_rate, color = ba)) +
  geom_line() + 
  facet_wrap(~ dbh) +
  labs(title = "DBH growth model space",
       subtitle = "Hemlocks, faceted by DBH")

trees %>% filter(bal == 0,
                 spp == "paper birch") %>% 
  mutate(ba = as.factor(ba)) %>% 
  ggplot(aes(cr, dbh_rate, color = ba)) +
  geom_line() + 
  facet_wrap(~ dbh) +
  labs(title = "DBH growth model space",
       subtitle = "Paper birches, faceted by DBH")

trees %>% filter(bal == 0,
                 spp == "hard maple",
                 cr == 30) %>% 
  ggplot(aes(ba, dbh_rate)) +
  geom_line() + 
  facet_wrap(~ dbh) +
  labs(title = "DBH growth model space",
       subtitle = "Hard maples with 30 % cr, faceted by DBH")



trees %>% filter(ba == 80,
                 spp == "hard maple") %>% 
  mutate(cr = as.factor(cr)) %>% 
  ggplot(aes(bal, dbh_rate, color = cr)) +
  geom_line() + 
  facet_wrap(~ dbh) +
  labs(title = "DBH growth model space",
       subtitle = "Hard maples, faceted by DBH")


# Simulate thinning from above

trees_release <- trees %>% 
  mutate(release = case_when(trees$ba == 120 & trees$bal == 80 ~ 0,
                             trees$ba == 80 & trees$bal == 40 ~ 1,
                             trees$ba == 40 & trees$bal == 0 ~ 2,
                             trees$ba == 0 & trees$bal == 0 ~ 3,
                             TRUE ~ 4)) %>% 
  filter(release != 4)

trees_release %>% filter(spp == "hard maple") %>% 
  mutate(cr = as.factor(cr)) %>% 
  ggplot(aes(release, dbh_rate, color = cr)) +
  geom_line() +
  facet_wrap(~ dbh)


trees %>% filter(spp == "hard maple",
                 dbh == 10, 
                 cr == 30, 
                 lat == 44.5,
                 lon == -73) %>% 
  ggplot(aes(ba, bal, fill = dbh_rate)) +
  geom_tile() + 
  scale_fill_viridis_c(option = "E",
                       name = "dbh growth (in/yr)")

trees2 %>% filter(spp == "hard maple",
                 dbh == 10, 
                 cr == 30, 
                 lat == 44.5,
                 lon == -73) %>% 
  ggplot(aes(ba, rbal, fill = dbh_rate)) +
  geom_tile() + 
  scale_fill_viridis_c(option = "E",
                       name = "dbh growth (in/yr)")
