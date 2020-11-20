library("tidyverse")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(cr_rate, spp, dbh_mid, cr_mid, 
         ba_mid, bal_mid, plot) %>% 
  rename(dbh = dbh_mid, cr = cr_mid, ba = ba_mid, bal = bal_mid)

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(nf_fia$plot), 
                     size = round(test_size*length(unique(nf_fia$plot))), 
                     replace = FALSE)

index <- which(nf_fia$plot %in% test_plots)
train <- nf_fia[-index,]
test <- nf_fia[index,]


# gets coefficients for any data frame
get_coefs <- function(df) {
  mod <- lm(cr_rate ~ dbh + bal + ba + cr, data = df)
  
  coef(mod)
}

# split into species-specific data frames and get coefficients
by_spp <- split(train, train$spp)

coefs_list <- by_spp %>% map(get_coefs)
cr_growth_coef <- as.data.frame(do.call(rbind, coefs_list))

# save
save(cr_growth_coef, file = "cr-growth-coef.rda")
write.csv(cr_growth_coef, file = "cr-growth-coef.csv")


###################
## TEST
###################
# results <- test %>% mutate(cr_rate_pred = cr_growth_coef[as.character(spp),1] + 
#                              cr_growth_coef[as.character(spp),2] * dbh +
#                              cr_growth_coef[as.character(spp),3] * bal + 
#                              cr_growth_coef[as.character(spp),4] * ba + 
#                              cr_growth_coef[as.character(spp),5] * cr,
#                            err = cr_rate_pred - cr_rate)
# 
# # RMSE
# sqrt(sum(results$err^2)/nrow(results))
