
library(ggplot2)


# density plots
densityPlot = function(data,
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
                 alpha=0.6)+
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
    annotation_logticks(side="b")
  
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}


# point plots
pointPlot = function(data, x, y, group, xlab = "", ylab="", title = "", filename){
  x = data[[x]]
  y = data[[y]]
  group = data[[group]]
  df = data.frame(x = x,
                  y = y,
                  group = group)
  
  fig = ggplot(df)+
    geom_point(aes(x=x,
                   y=y,
                   color=group,
                   group=group),
               alpha=.7)+
    theme_bw()+
    labs(title = title)+
    xlab(xlab)+
    ylab(ylab)+
    ylim(c(0,3))+
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}



##bar plot
barPlot = function(data, x, y, group, xlab = "", ylab="", title = "", filename){
  x = data[[x]]
  df = data.frame(x = x)
  fig = ggplot(df) +
  geom_bar(aes(x = x),stat = "count")+
  theme_bw()+
  theme(legend.position="top")+
  labs(title = title)+
  scale_fill_grey()+
  xlab(xlab)+
  ylab(ylab)
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}

boxPlot = function(data, x, y, xlab = "", ylab="", title = "", filename){
  x = data[[x]]
  y = data[[y]]
  
  df = data.frame(x = x, y = y)
  fig = ggplot(df)+
    geom_boxplot(aes(x=x, y=y, fill=x),alpha=.6)+
    theme_bw()+
    labs(title = title)+
    xlab(xlab)+
    ylab(ylab)
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}




# line plots
linePlot = function(data, x, y, group = NA, xlab = "", ylab="", title = "", filename){

  if(!is.na(group)){
    x = data[[x]]
    y = data[[y]]
    group = data[[group]]
    df = data.frame(x = x,
                    y = y,
                    group = group)
    
    fig = ggplot(df)+
      geom_point(aes(x=x,
                     y=y,
                     color=group,
                     group=group),
                 alpha=.2)+
      geom_line(aes(x=x,
                    y=y,
                    color=group,
                    group=group),
                alpha=.8)+
      theme_bw()+
      labs(title = title)+
      xlab(xlab)+
      ylab(ylab)+
      ylim(c(0,3))+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  }
    
    
  
  if(is.na(group)){
      x = data[[x]]
      y = data[[y]]
      df = data.frame(x = x,
                      y = y)
      
      fig = ggplot(df)+
        geom_point(aes(x=x,
                       y=y),
                   alpha=.2)+
        geom_line(aes(x=x,
                      y=y),
                  alpha=.8)+
        theme_bw()+
        labs(title = title)+
        xlab(xlab)+
        ylab(ylab)+
        ylim(c(0,3))+
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  }
    ggsave(paste0("Figures/", filename, ".jpg"),
           fig, 
           height=6,
           width=8,
           scale=1.65)
}
