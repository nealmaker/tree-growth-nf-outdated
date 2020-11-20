simple_hm <- train %>% 
  filter(spp == "hard maple") %>% 
  select(dbh_rate, dbh_mid, cr_mid)

train_glm_hm <- train(dbh_rate ~ .,
                      data = simple_hm,
                      method = 'glm')

train_glm_hm$results

train_glm2_hm <- train(dbh_rate ~ dbh_mid * cr_mid,
                      data = simple_hm,
                      method = 'glm')

train_glm2_hm$results