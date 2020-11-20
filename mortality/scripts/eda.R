summary(train)

sum(train$died)/nrow(train)
# Only a 4% mortality rate. Will this make prediction hard?
# Do I need a different model to deal with it?

View(filter(train, died))

summary(filter(train, died))

# main effects of numeric predictors
# boxplots
train %>% 
  ggplot(aes(died, dbh_s)) + 
  geom_boxplot()

train %>% 
  ggplot(aes(died, cr_s)) + 
  geom_boxplot()

train %>% 
  ggplot(aes(died, ba_mid)) + 
  geom_boxplot()

train %>% 
  ggplot(aes(died, bal_s)) + 
  geom_boxplot()

train %>% 
  ggplot(aes(died, lat)) + 
  geom_boxplot()

train %>% 
  ggplot(aes(died, lon)) + 
  geom_boxplot()

# faceted smooth densities
train %>% 
  ggplot(aes(dbh_s)) +
  geom_density(fill = "dark gray") +
  facet_grid(died ~ .)

train %>% 
  ggplot(aes(cr_s)) +
  geom_density(bw = 4, fill = "dark gray") +
  facet_grid(died ~ .)

train %>% 
  ggplot(aes(ba_mid)) +
  geom_density(bw = 7, fill = "dark gray") +
  facet_grid(died ~ .)

train %>% 
  ggplot(aes(bal_s)) +
  geom_density(bw = 7, fill = "dark gray") +
  facet_grid(died ~ .)

train %>% 
  ggplot(aes(lat)) +
  geom_density(bw = .1, fill = "dark gray") +
  facet_grid(died ~ .)

train %>% 
  ggplot(aes(lon)) +
  geom_density(bw = .2, fill = "dark gray") +
  facet_grid(died ~ .)

# Main effects of categorical factors
View(train %>% 
  group_by(spp) %>% 
  summarize(n = n(),
            pct_mort = round(100*(sum(died)/n), 3)) %>% 
  arrange(pct_mort))

View(train %>% 
       group_by(forest_type_s) %>% 
       summarize(n = n(),
                 pct_mort = round(100*(sum(died)/n), 3)) %>% 
       arrange(pct_mort))

# CHECK OUT SOME INTERACTIONS