library("pdp")
library("gridExtra")

varImp(model_dbh_full, scale = F)

plot(varImp(model_dbh_full), scale = F)

##########################
# Partial dependence plots 
##########################
# Very slow
# (can I un-transform the predictors first?)

pdp::partial(model_dbh_full, pred.var = "cr_mid", plot = T)

# dbh vs cr
pd <- pdp::partial(model_dbh_full, pred.var = c("cr_mid", "dbh_mid"), 
                   grid.resolution = 30)

pdp::plotPartial(pd, levelplot = FALSE, zlab = "dbh rate", 
            drape = TRUE, colorkey = TRUE)

# bal vs cr
pd2 <- pdp::partial(model_dbh_full, pred.var = c("cr_mid", "bal_mid"), 
                    grid.resolution = 20)

pdp::plotPartial(pd2, levelplot = FALSE, zlab = "dbh rate", 
                 drape = TRUE, colorkey = TRUE)

# bal vs ba
pd3 <- pdp::partial(model_dbh_full, pred.var = c("ba_mid", "bal_mid"), 
                    grid.resolution = 20)

pdp::plotPartial(pd3, levelplot = FALSE, zlab = "dbh rate", 
                 drape = TRUE, colorkey = TRUE)

# lat vs lon
pd4 <- pdp::partial(model_dbh_full, pred.var = c("lat", "lon"), 
                    grid.resolution = 20)

pdp::plotPartial(pd4, levelplot = FALSE, zlab = "dbh rate", 
                 drape = TRUE, colorkey = TRUE,
                 ylim = rev(range(pd4$lat)))

# cr vs ba
pd5 <- pdp::partial(model_dbh_full, pred.var = c("cr_mid", "ba_mid"), 
                    grid.resolution = 20)

pdp::plotPartial(pd5, levelplot = FALSE, zlab = "dbh rate", 
                 drape = TRUE, colorkey = TRUE,
                 ylim = rev(range(pd5$ba_mid)))


#####################
# Errors & accuracies
#####################

errors_full <- test %>% 
  mutate(pred = predict(dbh_growth_model_full, newdata = test),
         error = dbh_rate - pred)

errors_op <- test %>% 
  mutate(pred = predict(model_dbh_op, newdata = test_tran),
         error = dbh_rate - pred)

# r^2
r2_full <- 1-(sum(errors_full$error^2)/
                sum((errors_full$dbh_rate-mean(errors_full$dbh_rate))^2))

r2_opp <- 1-(sum(errors_op$error^2)/
                sum((errors_op$dbh_rate-mean(errors_op$dbh_rate))^2))

# Error distributions
e1 <- errors_full %>% ggplot(aes(error)) +
  geom_density(fill = "dark gray") +
  labs(title = "Full Model") +
  xlab("error (in/yr)") +
  scale_y_continuous(limits = c(0, 250)) +
  theme(text = element_text(family = "Perpetua"))

e2 <- errors_op %>% ggplot(aes(error)) +
  geom_density(fill = "dark gray") +
  labs(title = "Operational Model") +
  xlab("error (in/yr)") +
  scale_y_continuous(limits = c(0, 250)) +
  theme(text = element_text(family = "Perpetua"),
        axis.title.y = element_blank())

grid.arrange(e1, e2, ncol = 2)

# Error histograms by species
errors_op %>%
  group_by(spp) %>% 
  mutate(err_mean = mean(error)) %>% 
  ungroup() %>% 
  ggplot(aes(error)) +
  geom_density(fill = "dark gray") +
  geom_vline(xintercept = 0, col = "#386cb0", size = 1) +
  geom_vline(aes(xintercept = err_mean), col = "#bf5b17", size = 1) +
  facet_wrap(~ spp, ncol = 4) +
  scale_x_continuous(name = "error (in/yr)",
                     breaks = c(-0.025, 0, 0.025),
                     limits = c(-0.025, 0.025)) +
  scale_y_continuous(limits = c(0, 500)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(text = element_text(family = "Perpetua"))

errors_full %>%
  group_by(spp) %>% 
  mutate(err_mean = mean(error)) %>% 
  ungroup() %>% 
  ggplot(aes(error)) +
  geom_density(fill = "dark gray") +
  geom_vline(xintercept = 0, col = "#386cb0", size = 1) +
  geom_vline(aes(xintercept = err_mean), col = "#bf5b17", size = 1) +
  facet_wrap(~ spp, ncol = 4) +
  scale_x_continuous(name = "error (in/yr)",
                     breaks = c(-0.025, 0, 0.025),
                     limits = c(-0.025, 0.025)) +
  scale_y_continuous(limits = c(0, 500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "Perpetua"))

# Species' error table with both models
err_op_spp <- errors_op %>% 
  group_by(spp) %>% 
  summarize(error_op = round(mean(error), 5))

View(errors_full %>% group_by(spp) %>% 
       summarize(n = n(), error_full = round(mean(error), 5)) %>% 
       arrange(desc(n)) %>% 
       left_join(err_op_spp))

# Species-specific RMSEs
spp_RMSE <- data.frame(spp = unique(errors_full$spp),
                       nRMSE = rep(0, length(unique(errors_full$spp))))

for (i in 1:length(unique(errors_full$spp))){
  x <- filter(errors_full, spp == unique(errors_full$spp)[i])
  spp_RMSE[i, 2] <- RMSE(x$dbh_rate, x$pred)/mean(x$dbh_rate)
}
