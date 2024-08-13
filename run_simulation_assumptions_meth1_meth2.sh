#!/bin/bash
echo "Start run_simulation_assumptions_meth1_meth2.sh"

# Project RUN Settings
sim_meth_1="TRUE"
plot_res_meth_1="TRUE"
sim_meth_2="TRUE"
plot_res_meth_2="TRUE"

# Load R
module load Anaconda3
source activate tidyverse_twist

# Run all scripts in this directory
if [ ${sim_meth_1} = "TRUE" ];then 
	echo "run sim_meth_1 is TRUE"
    echo "Generate data with different parameter values to investigate mean estimates from method 1 under different assumptions"
	Rscript "./Assumptions_method_1/run_param_sim_method_1.R"
fi

if [ ${plot_res_meth_1} = "TRUE" ];then 
	echo "run plot_res_meth_1 is TRUE"
    echo "Plot density of mean estimates from method 1 for different scenarios"
	Rscript "./Assumptions_method_1/plot_results_param_sim_method_1.R"
fi

if [ ${sim_meth_2} = "TRUE" ];then 
	echo "run sim_meth_1 is TRUE"
    echo "Generate data with different parameter values to investigate mean estimates from method 2 under different assumptions"
	Rscript "./Assumptions_method_2/run_param_sim_method_2.R"
fi

if [ ${plot_res_meth_2} = "TRUE" ];then 
	echo "run plot_res_meth_1 is TRUE"
    echo "Plot density of mean estimates from method 2 for different scenarios"
	Rscript "./Assumptions_method_2/plot_results_param_sim_method_2.R"
fi