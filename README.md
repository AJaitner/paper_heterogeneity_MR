# Exploring and accounting for genetically driven effect heterogeneity in Mendelian Randomization 

R code for all analysis and simulations in the manuscript "Exploring and accounting for genetically driven effect heterogeneity in Mendelian Randomization"
## Table of contents
- [Authors](#Authors)
- [Installation](#installation)
- [Folder Structure](#folder-structure)

## Authors
Annika Jaitner, Krasimira Tsaneva-Atanasova, Rachel M. Freathy, Jack Bowden

## Installation
The conda_env_spec_file.txt can be used to build an identical conda environment to the one we used for our analysis using
conda create --name myenv --conda_env_spec_file.txt

The code was run with R version 4.2.2. 
Some of the simulation where run on in a high performance computing environment using Slurm and allowing to run several scenarios in parallel. Those simulations used R version 4.2.1. 

## Folder Structure
```
├── ALSPAC_applied_work <- All scripts and results for the applied anlysis.
│   ├── Results 
├── Assumptions_method_1 <- Investigate required assumptions for method 1, plots for the supplementary material.
│   ├── Results 
├── Assumptions_method_2 <- Investigate required assumptions for method 2, plots for the supplementary material.
│   ├── Results 
├── sample_size_sim <- Investigate performance of method 1 and method 2 for different Sample sizes, main paper plots.
│   ├── Low_effect_diff <- Calculate power for different sample sizes with small genetically driven effect heterogeneity, plot in the supplementary material.
|   |    ├── Results
|   ├── Results_method_1
|   ├── Results_method_1
├── .gitignore <- Files to ignore for git.  
├── README.md <- The top-level README.
├── conda_env_spec_file.txt <- File to build conda environment. 
├── run_applied_anlaysis.sh <- Runs applied analysis and plots its results.
├── run_sample_size_sim_main.sh <- Runs sample size simulation and plots its results.
├── run_sim_low_effect_diff.sh <- Runs simualtion with small genetically driven heterogeneity effect and plots the power for method 1 and method 2.
├── run_simulation_assumptions_meth1_meth2.sh <- Runs the simulation with different parameter values to investigate assumptions for method 1 and method 2.
```
Note that we provide some summary results which can be used to regenerate the plots. But we do not provide the results from each simulation run individually. 
