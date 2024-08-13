run_analysis_with_different_conf = function(exp, Conf_exp, Conf_out, res_name, PATH, dataset){
        source('different_method_fct.R')
        PCs = "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10"
        if(is.na(Conf_exp)){
            Z_exp = PCs
        }else{
            Z_exp = paste0(PCs, "+", Conf_exp)
        }

        if(is.na(Conf_out)){
            Z_out = PCs
        }else{
            Z_out = paste0(PCs, "+", Conf_out)
        }

        res_list_method_1_G01 = method_1_fct(Y = "kz030", S = exp, G= "chr15.78894339_G_A_01", G2 = "weighted_allele_score_sminit", Z_exp, Z_out, data = dataset)
        res_list_method_2_g01 = method_2_fct(Y = "kz030", S = exp, G= "chr15.78894339_G_A_01", G2 = "weighted_allele_score_sminit", Z_exp, Z_out, data = dataset)

        # create dataframe with results 
        source('create_result_dataframe_plotting_fct.R')
        res_pre_preg_G01_weightedG2 = create_results_dataframe_plotting(res_list_method_1_G01, res_list_method_2_g01, exp)
        
        write.table(res_pre_preg_G01_weightedG2[[1]], file = paste(PATH, "Results/", res_name, "_stage1.csv", sep = ""), row.names = FALSE)
        write.table(res_pre_preg_G01_weightedG2[[2]], file = paste(PATH, "Results/", res_name, "_stage2.csv", sep = ""), row.names = FALSE)

        # plot
        source('plot_dataframe_results_fct.R')
        plot_dataframe_results(res_pre_preg_G01_weightedG2[[2]], path = paste(PATH, "Plots/", sep = ""), plot_name = paste(res_name,".png", sep =""))

}
