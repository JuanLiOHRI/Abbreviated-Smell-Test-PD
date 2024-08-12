# Abbreviated Smell Test to Identify Patients with PD/DLB

Code for analysis in the manuscript entitled "Development of a Simplified Smell Test to Identify Patients with Typical Parkinson’s as Informed by Multiple Cohorts, Machine Learning and External Validation". 
medRxiv preprint doi: https://doi.org/10.1101/2024.08.09.24311696

## `Data preparation.DeNoPa.Rmd`

Code for cut and prepare data for the study. For accessting raw data, please contact the corresponding authors. The cut data (and data dictionary) can be accessed via zenodo (link to be updated) upon request.

## `Abbreviated Smell Test DeNoPa.qmd`

The Quarto file for the analysis.

## Supporting functions

There are three supporting functions, see `Abbreviated Smell Test DeNoPa.qmd` for more details.

- `itemAUC.R`: calculate each scent's AUC values and rank them, using cross-validation
- `boruta.ranking.R`: rank each scent by their variable importance values using the Boruta algorithm.
- `subsetAUC.R`: calculate each subset's AUC values.

