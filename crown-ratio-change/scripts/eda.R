summary(train)

train %>% 
  ggplot(aes(cr_rate)) +
  geom_density(bw = .5, fill = "dark gray") +
  labs(title = "Distribution of Crown Ratio Change Rates") +
  xlab ("crown ratio change rate (% increase/yr)")


# Main effects ----------

train %>% 
  ggplot(aes(dbh_mid, cr_rate)) +
  geom_point(alpha = .01) +
  geom_smooth() +
  labs(title = "DBH Main Effect") +
  xlab("dbh (in)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  ggplot(aes(cr_mid, cr_rate)) +
  geom_jitter(width = 2, height = 2, alpha = .01) +
  geom_smooth() +
  labs(title = "Crown Ratio Main Effect") +
  xlab("crown ratio (%)") +
  ylab ("crown ratio change rate (% increase/yr)") 

train %>% 
  ggplot(aes(ba_mid, cr_rate)) +
  geom_jitter(width = 1, alpha = .01) +
  geom_smooth() +
  labs(title = "Basal Area Main Effect") +
  xlab("basal area (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  ggplot(aes(bal_mid, cr_rate)) +
  geom_point(alpha = .01) +
  geom_smooth() +
  labs(title = "Overtopping Basal Area Main Effect") +
  xlab("bal (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  ggplot(aes(lat, cr_rate)) +
  geom_smooth() #+ 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  ggplot(aes(lon, cr_rate)) +
  geom_smooth() #+ 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  mutate(spp = reorder(spp, -cr_rate, FUN = mean)) %>% 
  ggplot(aes(spp, cr_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Species Main Effect") +
  ylab ("crown ratio change rate (% increase/yr)") +
  xlab("species")

train %>% 
  mutate(forest_type_s = reorder(forest_type_s, -cr_rate, FUN = mean)) %>% 
  ggplot(aes(forest_type_s, cr_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Forest Type Main Effect") +
  ylab ("crown ratio change rate (% increase/yr)") +
  xlab("forest type")


# Interactions ----------

# Slice by dbh
train %>% 
  mutate(dbh = cut(train$dbh_mid, c(0, 6, 12, 16, 20, 60), ordered_result = T)) %>% 
  ggplot(aes(cr_mid, cr_rate, col = dbh)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ dbh) +
  labs(title = "Effect of Crown Ratio, Sliced by DBH") +
  xlab("crown ratio (%)") +
  ylab ("crown ratio change rate (% increase/yr)")
    
train %>% 
  mutate(dbh = cut(train$dbh_mid, c(0, 6, 12, 16, 20, 60), ordered_result = T)) %>% 
  ggplot(aes(ba_mid, cr_rate, col = dbh)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ dbh) +
  labs(title = "Effect of Basal Area, Sliced by DBH") +
  xlab("basal area (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(dbh = cut(train$dbh_mid, c(0, 6, 12, 16, 20, 60), ordered_result = T)) %>% 
  ggplot(aes(bal_mid, cr_rate, col = dbh)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ dbh) +
  labs(title = "Effect of Overtopping Basal Area, Sliced by DBH") +
  xlab("bal (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(dbh = cut(train$dbh_mid, c(0, 6, 12, 16, 20, 60), ordered_result = T)) %>% 
  ggplot(aes(lat, cr_rate, col = dbh)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ dbh) +
  labs(title = "Effect of Latitude, Sliced by DBH") +
  xlab("latitude (dec. deg.)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(dbh = cut(train$dbh_mid, c(0, 6, 12, 16, 20, 60), ordered_result = T)) %>% 
  ggplot(aes(lon, cr_rate, col = dbh)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ dbh) +
  labs(title = "Effect of Longitude, Sliced by DBH") +
  xlab("longitude (dec. deg.)") +
  ylab ("crown ratio change rate (% increase/yr)")

# Slice by cr
train %>% 
  mutate(cr = cut(train$cr_mid, c(0, 20 , 40, 60, 80, 100), ordered_result = T)) %>% 
  ggplot(aes(dbh_mid, cr_rate, col = cr)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ cr) +
  labs(title = "Effect of DBH, Sliced by Crown Ratio") +
  xlab("dbh (in)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(cr = cut(train$cr_mid, c(0, 20 , 40, 60, 80, 100), ordered_result = T)) %>% 
  ggplot(aes(ba_mid, cr_rate, col = cr)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ cr) +
  labs(title = "Effect of Basal Area, Sliced by Crown Ratio") +
  xlab("basal area (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(cr = cut(train$cr_mid, c(0, 20 , 40, 60, 80, 100), ordered_result = T)) %>% 
  ggplot(aes(bal_mid, cr_rate, col = cr)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ cr) +
  labs(title = "Effect of Overtopping Basal Area, Sliced by Crown Ratio") +
  xlab("bal (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(cr = cut(train$cr_mid, c(0, 20 , 40, 60, 80, 100), ordered_result = T)) %>% 
  ggplot(aes(lat, cr_rate, col = cr)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ cr) +
  labs(title = "Effect of Latitude, Sliced by Crown Ratio") +
  xlab("latitude (dec. deg.)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(cr = cut(train$cr_mid, c(0, 20 , 40, 60, 80, 100), ordered_result = T)) %>% 
  ggplot(aes(lon, cr_rate, col = cr)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ cr) +
  labs(title = "Effect of Longitude, Sliced by Crown Ratio") +
  xlab("longitude (dec. deg.)") +
  ylab ("crown ratio change rate (% increase/yr)")
           
# Slice by ba
train %>% 
  mutate(ba = cut(train$ba_mid, c(0, 60, 120, 180, 500), ordered_result = T)) %>% 
  ggplot(aes(dbh_mid, cr_rate, col = ba)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
facet_wrap(~ ba) +
  labs(title = "Effect of DBH, Sliced by Basal Area") +
  xlab("dbh (in)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(ba = cut(train$ba_mid, c(0, 60, 120, 180, 500), ordered_result = T)) %>% 
  ggplot(aes(cr_mid, cr_rate, col = ba)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
facet_wrap(~ ba) +
  labs(title = "Effect of Crown Ratio, Sliced by Basal Area") +
  xlab("crown ratio (%)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(ba = cut(train$ba_mid, c(0, 60, 120, 180, 500), ordered_result = T)) %>% 
  ggplot(aes(bal_mid, cr_rate, col = ba)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ ba) +
  labs(title = "Effect of Overtopping Basal Area, Sliced by Basal Area") +
  xlab("bal (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(ba = cut(train$ba_mid, c(0, 60, 120, 180, 500), ordered_result = T)) %>% 
  ggplot(aes(lat, cr_rate, col = ba)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ ba) +
  labs(title = "Effect of Latitude, Sliced by Basal Area") +
  xlab("latitude (dec. deg.)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(ba = cut(train$ba_mid, c(0, 60, 120, 180, 500), ordered_result = T)) %>% 
  ggplot(aes(lon, cr_rate, col = ba)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ ba) +
  labs(title = "Effect of Longitude, Sliced by Basal Area") +
  xlab("longitude (dec. deg.)") +
  ylab ("crown ratio change rate (% increase/yr)")

# Slice by bal
train %>% 
  mutate(bal = cut(train$bal_mid, c(0, 10, 60, 120, 180, 500), 
                   ordered_result = T, include.lowest = T)) %>% 
  ggplot(aes(dbh_mid, cr_rate, col = bal)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ bal) +
  labs(title = "Effect of DBH, Sliced by Overtopping Basal Area") +
  xlab("dbh (in)") +
  ylab ("crown ratio change rate (% increase/yr)") 

train %>% 
  mutate(bal = cut(train$bal_mid, c(0, 10, 60, 120, 180, 500), 
                   ordered_result = T, include.lowest = T)) %>% 
  ggplot(aes(cr_mid, cr_rate, col = bal)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ bal) +
  labs(title = "Effect of Crown Ratio, Sliced by Overtopping Basal Area") +
  xlab("crown ratio (%)") +
  ylab ("crown ratio change rate (% increase/yr)") 

train %>% 
  mutate(bal = cut(train$bal_mid, c(0, 10, 60, 120, 180, 500), 
                   ordered_result = T, include.lowest = T)) %>% 
  ggplot(aes(ba_mid, cr_rate, col = bal)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ bal) +
  labs(title = "Effect of Basal Area, Sliced by Overtopping Basal Area") +
  xlab("ba (sq ft/ac)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(bal = cut(train$bal_mid, c(0, 10, 60, 120, 180, 500), 
                   ordered_result = T, include.lowest = T)) %>% 
  ggplot(aes(lat, cr_rate, col = bal)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ bal) +
  labs(title = "Effect of Latitude, Sliced by Overtopping Basal Area") +
  xlab("latitude (dec deg)") +
  ylab ("crown ratio change rate (% increase/yr)")

train %>% 
  mutate(bal = cut(train$bal_mid, c(0, 10, 60, 120, 180, 500), 
                   ordered_result = T, include.lowest = T)) %>% 
  ggplot(aes(lon, cr_rate, col = bal)) +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  facet_wrap(~ bal) +
  labs(title = "Effect of Longitude, Sliced by Overtopping Basal Area") +
  xlab("longitude (dec deg)") +
  ylab ("crown ratio change rate (% increase/yr)")


# Maps -------------
library("maps")

cut2 <- function(x, breaks) {
  r <- range(x)
  b <- seq(r[1], r[2], length=2*breaks+1)
  brk <- b[0:breaks*2+1]
  mid <- b[1:breaks*2]
  brk[1] <- brk[1]-0.01
  k <- cut(x, breaks=brk, labels=FALSE)
  mid[k]
}

# 2d bin smoothed mapping of cr growth
train %>%
  mutate(groupx = cut2(lon, 14), #binning lat & lon
         groupy = cut2(lat, 14)) %>%
  group_by(groupx, groupy) %>%
  summarize(cr_change = mean(cr_rate)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = cr_change), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_fill_viridis_c(option = "plasma", 
                       name = expression(Delta*'cr (%/yr)')) +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua")) +
  labs(title = "Latitude/Longitude Interaction")

# 2d bin smoothed mapping of cr growth, sliced by ba
train %>%
  mutate(groupba = as.factor(cut2(ba_mid, 5)),
         groupx = cut2(lon, 14), #binning lat & lon
         groupy = cut2(lat, 14)) %>%
  group_by(groupba, groupx, groupy) %>%
  summarize(cr_change = mean(cr_rate)) %>%
  ggplot(aes(groupx, groupy)) +
  geom_raster(aes(fill = cr_change), interpolate = TRUE) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_fill_viridis_c(option = "plasma", 
                       name = expression(Delta*'cr (%/yr)')) +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua")) +
  labs(title = "Latitude/Longitude Interaction, Sliced by Basal Area") +
  facet_wrap(~ groupba)
