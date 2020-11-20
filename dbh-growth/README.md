# dbh-growth
Diameter growth models for trees in the United States Northern Forest region.  
Constructed using US Forest Service [FIA data](https://github.com/nealmaker/fia-data-nf) and Wright's 'ranger' implementation of Random Forest. 

## Purpose
The models developed in this project are appropriate for predicting diameter growth of trees in the the New York Adirondacks and in northern Vermont, New Hampshire, and Maine. They were created to increase the accuracy of predictions over existing, parametric models; especially in diverse, multi-aged forests with complex growth dynamics. 

## Use
View the [report](dbh-growth-nf.pdf) for details related to the data and model.  

The [model.R](scripts/model.R) script can be run in full to download the data and build two random forest models: 1) the full model with 16 predictors, and 2) an "operational" model with only 8 predictors which is better suited to making predictions for day-to-day forest inventory data.  

The [eda.R](scripts/eda.R) and [results.R](scripts/results.R) scripts contain multiple visualizations, and are best used interactively, to explore the data and findings.  

The [glm.R](scripts/glm.R) script contains species-specific general linear models of diameter growth, trained on the same data as the random forests, for use as a point of comparison.

## Contact
Inquiries can be directed to Neal Maker at neal@pekinbranch.com.
