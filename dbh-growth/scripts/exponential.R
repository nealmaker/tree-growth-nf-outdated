library("tidyverse")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(dbh_rate, spp, dbh_mid, cr_mid, 
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

# initialize coefficients using Weiskittel nomenclature & values from a model
# we made that didn't differentiate species.
start <- list(b30 = -6,
              b31 = .6,
              b32 = -.05,
              b33 = -.003,
              b34 = -.01,
              b35 = 1)


# gets coefficients for any data frame
get_coefs <- function(df) {
  mod <- nls(dbh_rate ~ exp(b30 + b31*log(dbh) + b32*dbh + b33*bal + 
                              b34*sqrt(ba) + b35*log(cr)),
             start = start, data = df)
  
  coef(mod)
}

# split into species-specific data frames and get coefficients
by_spp <- split(train, train$spp)

coefs_list <- by_spp %>% map(get_coefs)
dbh_growth_coef <- as.data.frame(do.call(rbind, coefs_list))

# save
save(dbh_growth_coef, file = "dbh-growth-coef.rda")
