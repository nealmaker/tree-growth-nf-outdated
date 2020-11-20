# mortality
Tree mortality model for the Northern Forest

This model has not been tested against independent data yet.

## To Do:

* More exploritory analysis, especially examination of possible interactive effects.
* Better lit review
* Exploration of other model types
  * Are non-parametric models best?
  * Would disagregation models be useful & possible? (see Zhang et al. 2011)
  * Look into splitting criteria for tree-based models. Hellinger is supposedly great for class imbalances (ranger used to? have it as an option, other common RFs don't. Gini and entropy aren't great for imbalances; extratrees isn't either, based on my experimentation.
* A comparison of methods would be nice, using a subset of the training data to ease computation and comparing Kappas and AUROCs
  * Logistic regression (simplist approach)
  * Wiskittel (originally Chao?) approach with recursion to estimate when in interval each tree died
  * Cox model w/ or w/out survival
  * Random Forest with some weighted sampling scheme to deal with class imbalance and using interval as predictor
  * Support Vector Machine w/ or w/out survival
  * Disagregation model?
* Construct ROC curves and calculate Areas Under Curve (AUC) so results are comperable w/ Wiskittel and others.
* Is AUC really better than Kappa? Why? Does AUC depend on defining a 'positive' class?
* Do other studies treat survival as the "possitive" outcome? (only matters if they're using precission, recall, F1, maybe AUC.)

## Possiblities For Dealing With Censoring:

* Use a Cox model, which is semi-parametric, and definately has implementations that work with interval-censored data.
* Find a non-parametric model that can handle interval-censored data
  * Random survival forests for R look like a dead end: have looked, but haven't found any that can handle interval censoring
  * Bagged trees for survival?
  * Support vector machines?
  * Survival neural networks? (These definitley exist for Python. Not sure about R. Are they even applicable?)
* Avoid censoring altogether by using a binary outcome (lived or died) and using the remeasurement period as a predictor. Then you just put in the period you want when doing predictions (as long as it's in the range of the training data). (This is the current approach, with ranger.)

## Dealing With Class Imbalances

With the non-censored RF, can't find existing implementations with splitting criteria that work well for the big class imbalance. There are a few possibilities:

* Stick with non-censored RF, but use a sampling-based approach. 
  * Some implementations have a class weight option that chooses bootstrap samples with weight proportional to class prevalance, which could make a big difference.
  * Could undersample "lived" or oversample "died" in training data.
  * There are more complex synthetic sampling techniques which make new "died" observations based on k-nn of existing observations.
* Try SVM, which can also use balanced class weights and might have implementations that deal with censoring directly.
