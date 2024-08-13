#!/bin/bash
echo "Start run_sample_size_sim_main.sh"

# Project RUN Settings
sim_meth_1="TRUE"
sim_meth_2="TRUE"
prepare_data_for_plotting="TRUE"
plot_mean_estimates="TRUE"
plot_coverage="TRUE"
plot_power="TRUE"

# Load R
module load Anaconda3
source activate tidyverse_twist

# Run all scripts in this directory
if [ ${sim_meth_1} = "TRUE" ];then 
	echo "run sim_meth_1 is TRUE"
    echo "Generate data with different sample sizes to investigate mean estimates from method 1"
	Rscript "./Sample_size_sim/run_sim_sample_size_meth1.R"
fi

if [ ${sim_meth_2} = "TRUE" ];then 
	echo "run sim_meth_2 is TRUE"
    echo "Generate data with different sample sizes to investigate mean estimates from method 2"
	Rscript "./Sample_size_sim/run_sim_sample_size_meth2.R"
fi

if [ ${prepare_data_for_plotting} = "TRUE" ];then 
	echo "run prepare_data_for_plotting is TRUE"
    echo "Reformat simulation data for plotting"
	Rscript "./Sample_size_sim/modify_data_results_meth1.R"
    Rscript "./Sample_size_sim/modify_data_results_meth2.R"
fi

if [ ${plot_mean_estimates} = "TRUE" ];then 
	echo "run plot_mean_estimates is TRUE"
    echo "Generate plot with mean estimates for method 1 and 2"
	Rscript "./Assumptions_method_1/plot_mean_estimates.R"
fi

if [ ${plot_coverage} = "TRUE" ];then 
	echo "run plot_coverage is TRUE"
    echo "Generate plot with coverage for method 1 and 2"
	Rscript "./Assumptions_method_2/plot_coverage.R"
fi

if [ ${plot_power} = "TRUE" ];then 
	echo "run plot_power is TRUE"
    echo "Generate plot with power for method 1 and 2"
	Rscript "./Assumptions_method_2/plot_power.R"
fi