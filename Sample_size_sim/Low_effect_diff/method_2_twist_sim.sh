#!/bin/bash
## #############################################################
## Specific settings for high performance computing (HPC) using Slurm. 
## Code runs several scenarios in parallel 
## For other HPC enviroments modifications might need to be made
## ##############################################################
#SBATCH --export=ALL    # export environment variables (PBS -V)
#SBATCH -D .    # set working directory to . (PBS -d)
#SBATCH -p mrcq # queue (PBS -q)
#SBATCH -A Research_Project   # research project to run under
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=meth_2
#SBATCH --array=1-28	# ARRAY VARIABLE: $SLURM_ARRAY_TASK_ID
#SBATCH --output=%x.%A-%a.out	# FOR ARRAY JOBS
#SBATCH --error=%x.%A-%a.err	# FOR ARRAY JOBS

echo "running on"; hostname

module load R/4.2.1-foss-2022a
 
Rscript "./Sample_size_sim/Low_effect_diff/run_sim_sample_size_low_effect_meth2.R" $SLURM_ARRAY_TASK_ID 5000
