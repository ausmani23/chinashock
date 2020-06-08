# Job Loss and Incarceration: Local Labor Market Effects of a Natural Experiment

This repository contains replication materials for "Job Loss and Incarceration: Local Labor Market Effects of a Natural Experiment", by John Clegg, Adaner Usmani and Jacob Kang-Brown

## What is in here?

+ appendix: Contains the supplementary appendix
+ code: Contains all the R scripts that read data, run the models and generate output
+ data: Contains data inputs to the code (i.e., starting dataset), except where proprietary
+ meta: Contains .csv's that some of the scripts consult, when running
+ output: Stores output created by the scripts

## How to use it? 

To run this code successfully, please follow the instructions below:

1. Before you run the scripts, load 'china shock.Rproj'. This ensures that the default working directory is the "code" subfolder.
2. Run the code sequentially (in order of filename). If you run a later file without having run an earlier file, the script will probably throw an error.   
3.  Figures are output as .pdf's and tables as .csv's, but if you'd like standalone .tex files (or .tex) fragments with the explanatory foototes, run 99_figures.R and 99_tables.R. 99_figures.R will also create a folder called 'upload' in the 'output' folder, to which it will copy all figures that we use in the paper and supplementary appendix

Below I describe briefly describe what each file does, and which of the main figures and tables it generates.

+ 00_replicate.R: This file generates the main results. If all you want is to reproduce the main regression results or play around with the replication data, focus on this file. 
+ 01_prep*: These files take the original data, usually in levels, and generates first-differenced versions for the relevant periods. 
+ 02_prepdf.R: This file takes all the constituent data and makes our dataset. 
+ 03_runregs.R: This files run all the different models we consider, including all robustness checks.  
+ 04_runregs_tables.R: This file generates Tables X and Y
+ 05_runregs_plots.R: This file generates Figures X, Y, Z.. 
+ 20_cfactuals.R: This file runs the counterfactual scenarios we discuss in Section X of the paper.  
+ 21_cfactuals_rates.R: This file takes those counterfactuals, imputes intervening years, and generates the weighted, national-level average on which we rely for our discussion in the paper.
+ 22_cfactuals_output.R: This file generates Figures X, Y, and Z
+ 30_describe.R: This file generates Figures X, Y and Z
+ 99_figures.R: Creates .tex files for figures, with footnotes
+ 99_tables.R: Creates .tex files for figures, with footnotes
