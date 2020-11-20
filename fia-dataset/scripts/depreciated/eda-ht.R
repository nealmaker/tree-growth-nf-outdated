library("gridExtra")
library("maps")

# n for each height measure
data.frame(ht_measure = 
             c("ht", "actualht", "sawht", "boleht"),
           observations = 
             c(nrow(filter(nf_fia_with_ht, !is.na(ht_s))),
               nrow(filter(nf_fia_with_ht, !is.na(actualht_s))),
               nrow(filter(nf_fia_with_ht, !is.na(sawht_s))),
               nrow(filter(nf_fia_with_ht, !is.na(boleht_s)))))


# Height distributions
ht <- nf_fia_with_ht %>% 
  ggplot(aes(ht_s)) +
  geom_density(fill = "dark gray", bw = 2) +
  scale_x_continuous(limits = c(0, 175))

sawht <- nf_fia_with_ht %>% 
  ggplot(aes(sawht_s)) +
  geom_density(fill = "dark gray", bw = 1) +
  scale_x_continuous(limits = c(0, 175))

boleht <- nf_fia_with_ht %>% 
  ggplot(aes(boleht_s)) +
  geom_density(fill = "dark gray", bw = 1) +
  scale_x_continuous(limits = c(0, 175))

actualht <- nf_fia_with_ht %>% 
  ggplot(aes(actualht_s)) +
  geom_density(fill = "dark gray", bw = 2) +
  scale_x_continuous(limits = c(0, 175))

grid.arrange(ht, actualht, sawht, boleht, nrow = 2)

# Is there bole height data accross species, dbh, etc?

nf_fia_ht <- filter(nf_fia_with_ht, !is.na(boleht_s))

View(nf_fia_ht %>% group_by(spp) %>% 
       summarize(n = n(), mean = mean(boleht_s)) %>% 
       arrange(-n))

View(nf_fia_ht %>% group_by(forest_type_s) %>% 
       summarize(n = n(), mean = mean(boleht_s)) %>% 
       arrange(-n))

View(nf_fia_ht %>% group_by(state) %>% 
       summarize(n = n(), mean = mean(boleht_s)) %>% 
       arrange(-n))

nf_fia %>% ggplot(aes(lon, lat)) +
  geom_point() +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 60") +
  coord_fixed(xlim = c(-77, -67), ylim = c(42.4, 47.6), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Relationships ----------

# Categorical vars.
nf_fia_ht %>%  
  mutate(spp = reorder(spp, -boleht_s, FUN = median)) %>%
  ggplot(aes(spp, boleht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nf_fia_ht %>%  
  mutate(forest_type_s = reorder(forest_type_s, -boleht_s, FUN = median)) %>%
  ggplot(aes(forest_type_s, boleht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nf_fia_ht %>%  
  mutate(landscape = reorder(landscape, -boleht_s, FUN = median)) %>%
  ggplot(aes(landscape, boleht_s)) +
  geom_boxplot(fill = "dark gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Continuous vars.
nf_fia_ht %>% 
  ggplot(aes(dbh_s, boleht_s)) +
  geom_point(alpha = .05) +
  geom_smooth()

nf_fia_ht %>% 
  filter(spp %in% c("fir", "spruce", "soft maple",
                    "hard maple", "cedar", "hemlock",
                    "yellow birch", "white pine")) %>%
  mutate(spp = reorder(spp, -boleht_s, FUN = median)) %>%
  ggplot(aes(dbh_s, boleht_s, col = spp)) +
  geom_smooth() +
  scale_color_brewer(type = "qual",
                     name = "species")

nf_fia_ht %>% 
  ggplot(aes(cr_s, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(ba_s, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(bal_s, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(ba_s, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(slope, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(aspect, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(lat, boleht_s)) +
  geom_smooth()

nf_fia_ht %>% 
  ggplot(aes(lon, boleht_s)) +
  geom_smooth()

# Discrete numeric vars.
nf_fia_ht %>%  
  ggplot(aes(as.factor(crown_class_s), boleht_s)) +
  geom_boxplot(fill = "dark gray") 

nf_fia_ht %>%  
  ggplot(aes(as.factor(stocking_s), boleht_s)) +
  geom_boxplot(fill = "dark gray") 

nf_fia_ht %>%  
  ggplot(aes(as.factor(site_class), boleht_s)) +
  geom_boxplot(fill = "dark gray") 
