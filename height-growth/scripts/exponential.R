library("tidyverse")
library("minpack.lm")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived",
         !is.na(ht_rate),
         !is.na(ht_mid)) %>% 
  # rudely make all ht_growth positive
  mutate(ht_rate = ifelse(ht_rate <= 0, .00001, ht_rate)) %>% 
  select(ht_rate, spp, dbh_mid, cr_mid, 
         ba_mid, bal_mid, ht_mid, plot) %>% 
  rename(dbh = dbh_mid, cr = cr_mid, ba = ba_mid, bal = bal_mid, ht = ht_mid)

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

# initialize coefficients using Weiskittel nomenclature & values from a model
# we made that didn't differentiate species.
start <- list(b40 = -2.5,
              b41 = -.11,
              b42 = .8261,
              b43 = -.0583,
              b44 = -.0121,
              b45 = -.0049,
              b46 = .03741,
              b47 = .01)


# gets coefficients for any data frame
get_coefs <- function(df) {
  mod <- nlsLM(ht_rate ~ exp(b40 + b41*ht + b42*log(ht) + b43*(dbh/ht) + 
                              b44*bal + b45*ba + b46*log(ba) + b47*log(cr)),
             start = start, data = df)
  
  coef(mod)
}

# split into species-specific data frames and get coefficients
by_spp <- split(train, train$spp)

coefs_list <- by_spp %>% map(get_coefs)
ht_growth_coef <- as.data.frame(do.call(rbind, coefs_list))

# save
save(ht_growth_coef, file = "dbh-growth-coef.rda")
write.csv(ht_growth_coef, file = "ht-growth-coef.csv")
