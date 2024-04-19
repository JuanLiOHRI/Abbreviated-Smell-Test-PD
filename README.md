# Abbreviated SST-ID

Code for analysis in the manuscript entitled "Simplified Olfaction Testing to Identify Patients with Parkinson’s Using Longitudinal Cohort Study, Machine Learning and External Validation". Links of preprint and the published version to be updated.

## `Data preparation.DeNoPa.Rmd`

Code for cut and prepare data for the study. For accessting raw data, please contact the corresponding authors. The cut data (and data dictionary) can be accessed via zenodo ([here](https://zenodo.org/records/10998698)) upon request.

## `Abbreviated Smell Test DeNoPa.qmd`

The Quarto file for the analysis.

## Supporting functions

There are three supporting functions, see `Abbreviated Smell Test DeNoPa.qmd` for more details.

- `itemAUC.R`: calculate each scent's AUC values and rank them, using cross-validation
- `boruta.ranking.R`: rank each scent by their variable importance values using the Boruta algorithm.
- `subsetAUC.R`: calculate each subset's AUC values.

