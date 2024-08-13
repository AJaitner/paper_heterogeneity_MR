#!/bin/bash
echo "Start run_applied_analysis.sh"
# Project RUN Settings
meth1_meth_2_prepreg_sm="TRUE"
meth1_meth_2_1st3m_sm="TRUE"
observational="TRUE"
standard_MR="TRUE"
one_plot_all_results="TRUE"

# Load R
module load Anaconda3
source activate tidyverse_twist

# Run all scripts in this directory
if [ ${meth1_meth_2_prepreg_sm} = "TRUE" ];then 
	echo "run meth1_meth_2_prepreg_sm is TRUE"
	echo "Apply method 1 and method 2 to the ALSPAC data using pre-pregnancy smoking as the exposure"
	Rscript "./ALSPAC_applied_work/run_applied_ananlyis_different_methods_b663.R"
fi

if [ ${meth1_meth_2_1st3m_sm} = "TRUE" ];then 
	echo "run meth1_meth_2_1st3m_sm is TRUE"
	echo "Apply method 1 and method 2 to the ALSPAC data using smoking in the 1st 3 month as the exposure"
	Rscript "./ALSPAC_applied_work/run_applied_ananlyis_different_methods_b665.R"
fi

if [ ${observational} = "TRUE" ];then 
	echo "run observational is TRUE"
	echo "Investiagte the observation association between smoking exposures and birth weight in ALSPAC data"
	Rscript "./ALSPAC_applied_work/run_applied_analysis_observational.R"
fi

if [ ${standard_MR} = "TRUE" ];then 
	echo "run standard_MR is TRUE"
	echo "Perform and standard one-sample MR analysis with smoking varibales as the exposure and birth weight as the outcome in ALSPAC data"
	Rscript "./ALSPAC_applied_work/standard_MR.R"
fi

if [ ${one_plot_all_results} = "TRUE" ];then 
	echo "run one_plot_all_results is TRUE"
	echo "Show all results from the ALPSAC data in one plot, which is shown in the published manuscript"
	Rscript "./ALSPAC_applied_work/combine_plots_methods_observational.R"
fi