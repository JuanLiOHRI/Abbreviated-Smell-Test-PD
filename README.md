# Abbreviated Smell Test to Identify Patients with PD/DLB

Code for analysis in the manuscript entitled "Development of a Simplified Smell Test to Identify Patients with Typical Parkinson’s as Informed by Multiple Cohorts, Machine Learning and External Validation". 

medRxiv preprint doi: https://doi.org/10.1101/2024.08.09.24311696

## Data files

Datasets of the three cohorts used in this study can be accessed via zenodo (link to be updated) upon request.

1. `Data dictionary.xlsx`
2. Ottawa (PREDIGT) Trial (baseline): `Ottawa_cut.csv`
3. Prognostic Biomarkers in Parkinson’s Disease Study (PROBE; baseline): `PROBE_cut.csv`
4. “_De Novo_ Parkinson disease study” (DeNoPa; baseline, 48-month, and 72-month follow-up visits): `DeNoPa_cut.csv`

## `Abbreviated Smell Test DeNoPa.qmd`

The Quarto file for the analysis.

## Supporting functions

There are three supporting functions, see `Abbreviated Smell Test DeNoPa.qmd` for more details.

- `itemAUC.R`: calculate each scent's AUC values and rank them, using cross-validation
- `boruta.ranking.R`: rank each scent by their variable importance values using the Boruta algorithm.
- `subsetAUC.R`: calculate each subset's AUC values.

