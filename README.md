# Labor Markets and Incarceration: Effects of the China Shock on American Punishment

This repository contains replication materials for "Labor Markets and Incarceration: Effects of the China Shock on American Punishment". 

## FOLDERS

+ **code** / contains code to replicate main results, as well as code to generate all supplementary results
+ **crosswalks** / files used to crosswalk CZ to counties and to other identifiers, and to retrieve metadata
+ **meta** / metadata for this project, relied on by main code
+ **repdata** / contains replication dataset for the main regression 

## .R files in /code

+ **00_replicate.R** / replicates the main results
+ **03_runregs.R** / runs all regressions on all dvs
+ **04_runregs_tables.R** / generates tables based on regression results
+ **05_runregs_plots.R** / generates figures based on regression results
+ **20_cfactuals.R** / runs counterfactual
+ **21_cfactuals_rates.R** / converts counterfactual output to annual rates
+ **22_cfactuals_output.R** / output figures based on cfactual results
+ **40_felersenses.R** / explains differences in result from Feler and Senses (2017)
+ **99_figures.R** / outputs .tex fragment for figures
+ **99_tables.R** / outputs .tex fragment for tables
+ **footnotes.R** / adds footnotes to figures/tables
+ **functions.R** / misc functions used
+ **predict_ivreg.R** / takes an ivreg model object and generates predictions

To run 00_replicate.R, which replicates the main results, the files in this repository are sufficient. But if you would like to replicate anything else, follow these steps: 

1. Create a 'files' folder in the main directory
2. Download '02_prepped.RData' into that folder (via [this link](https://www.dropbox.com/s/gpjx11jb2eyg7wz/02_prepped.RData?dl=0))
3. Create an 'output' folder in the main directory
4. Run all numbered .R files, in order
