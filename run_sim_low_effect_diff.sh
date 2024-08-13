#!/bin/bash
echo "Start sim_low_effect_diff.sh"
# Project RUN Settings
run_simulation="TRUE"
combine_result_files="TRUE"
plot_results="TRUE"

if [ ${run_simulation} = "TRUE" ];then 
	echo "run run_simulation is TRUE"
	echo "run simulation for increasing sample sizes and small genetically driven effect heterogeneity"
    sbatch ./Sample_size_sim/Low_effect_diff/method_1_twist_sim.sh
    sbatch ./Sample_size_sim/Low_effect_diffmethod_2_twist_sim.sh
fi

if [ ${combine_result_files} = "TRUE" ];then 
	echo "run combine_result_files is TRUE"
	echo "Combine result files into one file with one scenario per row"
    awk 'FNR==1 && NR!=1{next;}{print}' ./Sample_size_sim/Low_effect_diff/Results/*_res_table_method_1.csv >> ./Sample_size_sim/Low_effect_diff/Results/sample_size_low_effect_meth1.csv   
    awk 'FNR==1 && NR!=1{next;}{print}' ./Sample_size_sim/Low_effect_diff/Results/*_res_table_method_2.csv >> ./Sample_size_sim/Low_effect_diff/Results/sample_size_low_effect_meth2.csv   
fi

# Load R
module load Anaconda3
source activate tidyverse_twist

if [ ${plot_results} = "TRUE" ];then 
	echo "run combine_result_files is TRUE"
	echo "Combine result files into one file with one scenario per row"
    Rscript "./Sample_size_sim/Low_effect_diff/plot_results_low_effect.R"   
fi


