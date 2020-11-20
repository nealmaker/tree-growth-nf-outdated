# tree-growth-nf
Results and analysis of US Northern Forest tree growth submodels, using non-parametric methods on FIA data

## Some notes
- dbh_rate is calculated by fia. It is essentially a prediction of growth based on the starting diameter, crown ratio, etc., etc. and is not (dbh_e - dbh_s)/interval.
  - (dbh_e - dbh_s)/interval gives negative rates for about 2% of observations, but fia's dbh_rate is always positive
  - Using fia's rate gave a very acurate model because we were basically doing a really good job recreating the model the FIA program used.
  - Using (dbh_e - dbh_s)/interval (as we do here) gives an unbiased model with accuracies similar to the best published models (like Weiskittel  et al's Adirondack model).
- ht_rate as calculated by fia does equal (ht_e - ht_s)/interval, and there's a lot of measurement error.
  - 30% of observations have negative ht growth rates.
  - The model has very low accuracy (r^2 of .1) and 15% of predictions are negative.
- bal is calculated based on all trees in each subplot. Does that include big and little trees (do the regen and overstory plots share a subplot ID), or are we getting weird numbers because we need to combine data from regen and overstory plots?
- Independent test data is reserved by holding back data from 20% of unique plot IDs (PLT_CN for plot at end of remeasurement period), but some of those could be plots in the training data, measured at a different time (each remeasurement gets a new plot ID). We should probably figure out which plots were remeasured multiple times and account for it explicitly. 

## Things to try:
- Add predictors for percent of basal area each species accounts for within each plot, which will greatly increase the number of predictors, but might give some fine-scale resolution to help the accuracy (Charlie Canham says that the species of competators affects growth a lot).
- Add predictors for percent of bal each species accounts for for each tree.
- Build out models like Weiskettle's and Teck's, but use our data to get more direct comparisons between the methods.
- Look at accuracies for situations of interest (like growth rates of very high quality trees released and not released) to see if the non-parametric model does outperform the others in those situations. (Maybe the overall accuracy isn't much different, but it's better in an important minority of cases.)
