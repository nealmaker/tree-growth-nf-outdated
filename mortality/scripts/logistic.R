library("tidyverse")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change != "cut") %>%
  mutate(died = ifelse(status_change == "died", 1, 0)) %>%
  select(died, spp, dbh_s, cr_s, 
         ba_s, bal_s, interval, plot) %>% 
  rename(dbh = dbh_s, cr = cr_s, ba = ba_s, bal = bal_s)

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


# gets model for any data frame
get_mod <- function(df) {
  glm(died ~ dbh + log(dbh) + cr + bal + sqrt(ba) + interval, 
             data = df, family = "binomial")
}

# split into species-specific data frames and get coefficients
by_spp <- split(train, train$spp)

mod_list <- by_spp %>% map(get_mod)
coefs_list <- mod_list %>% map(coef)
mort_coef <- as.data.frame(do.call(rbind, coefs_list))

# save
save(mort_coef, file = "mort-coef.rda")
write.csv(mort_coef, file = "mort-coef.csv")