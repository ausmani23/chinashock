# Job Loss and Incarceration: Local Labor Market Effects of a Natural Experiment

This repository contains replication materials for "Job Loss and Incarceration: Local Labor Market Effects of a Natural Experiment" by John Clegg and Adaner Usmani. 

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
+ **99_figures.R** / outputs .tex fragment for figures
+ **99_tables.R** / outputs .tex fragment for tables
+ **footnotes.R** / adds footnotes to figures/tables
+ **functions.R** / misc functions used
+ **predict_ivreg.R** / takes an ivreg model object and generates predictions

## QUESTIONS? 

If you encounter any issues with this code or with reproducing my results, please let me know at adaner.usmani[at]gmail.com. 
