# fia-data-nf
Development of an FIA dataset to model tree-level changes in the Northern Forest

## Purpose
US Forest Service Forest Inventory and Analysis data is extensive and multifaceted. This project fetches the appropriate tree remeasurement data and wrangles it into a form suitable for use in modeling tree-level changes in the New York Adirondacks, northern Vermont, northern New Hampshire, and northern Maine. Specifically, the dataset was built to support the creation of individual tree models of diameter growth, height, height growth, crown ratio change, and survival.

## Use
Read the [report](nf-dataset-creation.pdf) for more information about the FIA data, the purpose of this dataset, and how it was created.

The full dataset is available as an [rda file](rda/nf-fia.rda), and the [wrangle.R](scripts/wrangle.R) script can be run in full to fetch up-to-date data from the FIA databse and manipulate it to rebuild the dataset.

The [eda.R](scripts/eda.R) script contains visualizations of the data, and is best used interactively.

## Contact
Inquiries can be directed to Neal Maker at neal@pekinbranch.com.
