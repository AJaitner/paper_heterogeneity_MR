plot_dataframe_results = function(result_dataframe, path, plot_name){

result_dataframe$label_name = paste0(round(result_dataframe$Est_value, 0)," (SE: ", round(result_dataframe$SE, 0), ")" )

cols_1 <- c("Method_1" = "#B32070","Method_2" = "#360FC5")

p = ggplot(result_dataframe, aes(Est_value, Est_name)) +
    geom_point(aes(color=factor(Method, c("Method_2", "Method_1"))), position=position_dodge(width=1), size=2) +
    geom_errorbarh(data = result_dataframe, aes(xmin=Lower_CI, xmax=Higher_CI, color=factor(Method, c("Method_2", "Method_1"))),  height=.3, position=position_dodge(width=1)) + # position=position_dodge(width=1),
    geom_vline(xintercept=0, linetype="dashed") +
    geom_text(aes(label = label_name, color=factor(Method, c("Method_2", "Method_1"))),position=position_dodge(width=1),  #hjust = 0.5,
           vjust = -1.5, hjust = 0.35,
           #nudge_y = 0, nudge_x = 5,  #vjust = -1.5,
           show.legend = FALSE) +
    #scale_y_discrete("", limits=rev(levels(result_dataframe$Exposure))) +
    scale_x_continuous("Estimates") +
    guides(color=guide_legend(reverse = TRUE, title="Method")) +
    theme_classic() + scale_color_manual(values=cols_1) + # "#A6A6A6" "#767171"
    theme(plot.margin=margin(10,10,10,10)) + 
    theme(text=element_text(size=18)) #+ 
    labs(title = "",
       #subtitle = "Adjusted for offspring sex, gestational age, PC,\ngenotype batch (moba only)", 
       caption = "")


ggsave(paste(path, plot_name, sep =""), p, width=10, height=4, dpi=300)

}