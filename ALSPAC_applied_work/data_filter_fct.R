data_filter_fct = function(dataset){
    # ''' filter data for specific characteristics 
    #     Input: dataset for be filterd
    #     Output: dataset after filtering
    # '''
    data_filter = dataset %>%
                    filter(birth_weight > 0 | is.na(birth_weight))%>%                     filter(livebirth == 1) %>% # 1 Yes
                    filter(multiple_births == 1) %>% # 1 Singleton

                    filter(child_per_mother == 'A') %>% # A 1 child per mother
                    filter(gestational_duration >= 37) %>%
                    tidydim()
    return(data_filter)
}