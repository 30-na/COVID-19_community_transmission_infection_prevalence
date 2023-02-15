
# density plots
density_plot = function(data,
                        x,
                        group,
                        xlab = "",
                        title = "",
                        filename){
  # fig 01
  
  x = data[[x]]
  group = data[[group]]
  df = data.frame(x = x,
                  group = group)
  fig = ggplot(df)+
    geom_density(aes(x=x,
                     y=..density..,
                     fill=group,
                     group=group),
                 color="grey",
                 alpha=0.6,
                 bw=0.01)+
    #scale_x_continuous(trans = "log10")+
    scale_linetype("")+
    scale_color_manual("CDC\nRisk Level",
                       values=c("DodgerBlue","Yellow","Orange","Red"))+
    scale_fill_manual("CDC\nRisk Level",
                      values=c("DodgerBlue","Yellow","Orange","Red"))+
    theme_bw()+
    theme(legend.position = "top",
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 12))+
    labs(title = title)+
    xlab(xlab)+
    ylab("Frequency")+
    annotation_logticks(side="b")
  
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}
