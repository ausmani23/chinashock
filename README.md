# Job Loss and Incarceration: Local Labor Market Effects of a Natural Experiment

This repository contains replication materials for "Job Loss and Incarceration: Local Labor Market Effects of a Natural Experiment", by John Clegg, Adaner Usmani and Jacob Kang-Brown

## What is in here?

+ appendix: Contains the supplementary appendix
+ code: Contains all the R scripts that read data, run the models and generate output
+ data: Contains all data inputs to the code (i.e., starting dataset)
+ meta: Contains .csv's that some of the scripts consult, when running
+ files: Stores files created by the scripts, as they run
+ output: Stores output created by the scripts

## How to use it? 

To run this code successfully, please follow the instructions below:

1. Before you run the scripts, load 'china shock.Rproj'. This ensures that the default working directory is the "code" subfolder.
2. Run the code sequentially (in order of filename). If you run a later file without having run an earlier file, the script will probably throw an error.  
3.  Figures are output as .pdf's and tables as .csv's, but if you'd like standalone .tex files (or .tex) fragments with the explanatory foototes, run 99_figures.R and 99_tables.R. 99_figures.R will also create a folder called 'upload' in the 'output' folder, to which it will copy all figures that we use in the paper and supplementary appendix

