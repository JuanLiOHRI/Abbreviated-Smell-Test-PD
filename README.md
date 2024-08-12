# Abbreviated Smell Test to Identify Patients with PD/DLB

Code for analysis in the manuscript entitled "_Development of a Simplified Smell Test to Identify Patients with Typical Parkinson’s as Informed by Multiple Cohorts, Machine Learning and External Validation_". 

medRxiv preprint doi: https://doi.org/10.1101/2024.08.09.24311696

## Data files

Datasets of the 3 cohorts used in this study can be accessed via zenodo (link to be updated) upon request. __Note:__ please place the data files within the same working directory after downloading them from zenodo.

1. `Data dictionary.xlsx`
2. `Ottawa_cut.csv`: Ottawa (PREDIGT) Trial (baseline)
3. `PROBE_cut.csv`: Prognostic Biomarkers in Parkinson’s Disease Study (PROBE; baseline)
4. `DeNoPa_cut.csv`: “_De Novo_ Parkinson disease study” (DeNoPa; baseline, 48-month, and 72-month follow-up visits)

## Supportive data

Included in the `Supportive data` folder in this repo:

1. `diagnosis colors.csv`: colors representing each diagnostic groups for plotting.
2. `Published rankings.csv`: 8 previously published scent rankings, 4 for SST-ID, 4 for UPSIT.
3. `scent_shared.csv`: 11 scents that are shared by SST-ID and UPSIT. (__Note:__ this file is generated by `4. Combine UPSIT and SST-ID.qmd`, but is also required by `1. Tables, distributions, ROCs.qmd` for plotting. So I included it here to avoid running error.)
4. `SST_ID_option.csv`: Options provided for each SST-ID scent, used for generating ICCs.
5. `UPSIT_key.csv`: answer keys of UPSIT.
6. `UPSIT_option.csv`: Options provided for each UPSIT scent, used for generating ICCs.

## Code files

1. `1. Tables, distributions, ROCs.qmd`
   1. Code for
      - Table 1: demographic and diagnostic characteristics of the 3 cohorts
      - Table 2: AUC, sensitivity and specificity of SST-ID and UPSIT
      - Supplemental Table 2: relationship between SST-ID/UPSIT scores with age, sex, and diagnostic groups
      - Figure 2: distribution and AUC values of SST-ID and UPSIT
      - Supplemental Figure 1: distribution and AUC values of SST-TH and SST-DS
   2. Need to read
      - `Ottawa_cut.csv`
      - `PROBE_cut.csv`
      - `DeNoPa_cut.csv`
      - `Supportive data/diagnosis colors.csv`
2. `2. Abbreviated Smell Test SST-ID.qmd`
   1. Code for
      - Figure 3 (a,b): rankings of SST-ID scents, percentages of correct scent identification in each group
      - Supplemental Figure 2 (a): Percentage differences of correct scent identification between HC and PD/DLB groups (% HC - % PD/DLB) in DeNoPa (SST-ID)
      - Figure 4 (a): comparing SST-ID rankings of this study with 4 previously published ones
      - Figure 6 (a)-(c): validation of the SST-ID subsets
      - Figure 5, Supplemental Figure 3, Supplemental Figure 4: ICCs of SST-ID scents
   2. Need to read
      - `DeNoPa_cut.csv`
      - `Supportive data/Published rankings.csv`
      - `Supportive data/scent_shared.csv`
      - `Supportive data/SST_ID_option.csv`
   3. Generate files: the intermediate results will be saved to a folder called `Generated files`. __Note:__ you need to create such folder within your working directory.
      - `SST_ID_rankings.csv`: rankings of SST-ID scents
      - `df_sex_D_M.rds`, `df_sex_D_F.rds`, `df_sex_D.rds`: relationship between scent identification and sex of SST-ID in DeNoPa
      - `df_fine_D.rds`, `df_age_D.rds`: relationship between scent identification and age of SST-ID in DeNoPa
3. `3. Abbreviated Smell Test UPSIT.qmd`
   1. Code for
      - Figure 3 (c,d): rankings of UPSIT scents, percentages of correct scent identification in each group
      - Supplemental Figure 2 (b): Percentage differences of correct scent identification between HC and PD/DLB groups (% HC - % PD/DLB) in Ottawa Trial (UPSIT)
      - Figure 4 (b): comparing UPSIT rankings of this study with 4 previously published ones
      - Figure 6 (d,e): validation of the UPSIT subsets
      - Figure 5, Supplemental Figure 3, Supplemental Figure 5: ICCs of UPSIT scents
   2. Need to read
      - `Ottawa_cut.csv`
      - `PROBE_cut.csv`
      - `Supportive data/diagnosis colors.csv`
      - `Supportive data/Published rankings.csv`
      - `Supportive data/scent_shared.csv`
      - `Supportive data/UPSIT_key.csv`
      - `Supportive data/UPSIT_option.csv`
   3. Generate files: the intermediate results will be saved to a folder called `Generated files`. __Note:__ you need to create such folder within your working directory.
      - `UPSIT_rankings.csv`: rankings of UPSIT scents
      - `df_sex_O_M.rds`, `df_sex_O_F.rds`, `df_sex_O.rds`: relationship between scent identification and sex of UPSIT in Ottawa trial
      - `df_fine_O.rds`, `df_age_O.rds`: relationship between scent identification and age of UPSIT in Ottawa trial
      - `df_sex_P_M.rds`, `df_sex_P_F.rds`, `df_sex_P.rds`: relationship between scent identification and sex of UPSIT in PROBE
      - `df_fine_P.rds`, `df_age_P.rds`: relationship between scent identification and age of UPSIT in PROBE
4. `4. Combine UPSIT and SST-ID.qmd`
   1. Code for
      - Table 3: rankings of the 11 scents shared by SST-ID and UPSIT
      - Table 4: performance of the 7-scent abbreviated smell test
      - Figure 7: relationship of scent identification with sex and age
   2. Need to read
      - `Ottawa_cut.csv`
      - `PROBE_cut.csv`
      - `DeNoPa_cut.csv`
      - `Supportive data/diagnosis colors.csv`
      - Intermediate results generated by `2. Abbreviated Smell Test SST-ID.qmd` and `3. Abbreviated Smell Test UPSIT.qmd`
   3. Generate files: the intermediate results will be saved to a folder called `Generated files`. __Note:__ you need to create such folder within your working directory.
      - `scent_shared.csv`: 11 scents that are shared by SST-ID and UPSIT.

## Supportive function

Supportive functions are placed in the `R` folder. These functions need to be sourced in `2. Abbreviated Smell Test SST-ID.qmd` and `4. Combine UPSIT and SST-ID.qmd`.

1. `itemAUC.R`: calculate each scent's AUC values and rank them, using cross-validation
2. `subsetAUC.R`: calculate each subset's AUC values.
3. Modified `TestGardener` functions to generate ICCs: `make.dataList.R`, `Wbinsmth.R`, `eval_surp.R`, `ICC.plot.R`

