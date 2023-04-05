library(ggplot2)
library(dplyr)


# load ddata
load("ProcessedData/rt_counties_TranRisk.RDA")

source("Function/plot_functions.R")


names(rt_counties_TranRisk)
boxPlot(data=na.omit(rt_counties_TranRisk),
        x="community_transmission_level",
        y="Rt",
        xlab = "",
        ylab="",
        title = "",
        "rt_transmissionRisk_46counties")


rt_counties_TranRisk %>%
  group_by(community_transmission_level) %>%
  count()




pointPlot(data=na.omit(rt_counties_TranRisk),
          x="date",
          y="Rt",
          group="community_transmission_level",
          xlab = "",
          ylab="",
          title = "",
          "rt_transmissionRisk_46counties")



plot(rt_counties_TranRisk %>% 
  group_by(date) %>%
  summarize(meanTotalRt = mean(Rt)))







# Density Plot limited rt 3 weeks later
names(rt_counties_TranRisk)
fig = ggplot(data = rt_counties_TranRisk)+
  geom_density(aes(x=Rt_next3weeks,
                   y=..density..,
                   fill=community_transmission_level,
                   group=community_transmission_level),
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
  labs(title = "Density of 3 weeks later Reproduction Number for Transmission Risk Level groups")+
  xlab("Transmision Risk Level")+
  annotation_logticks(side="b")

ggsave("Figures/rt_transmissionRisk_46counties_density.jpg",
       fig, 
       height=6,
       width=8,
       scale=1.65)




# Density Plot limited rt 3 weeks later
fig = ggplot(data = rt_counties_TranRisk)+
  geom_density(aes(x=Rt_next3weeks,
                   y=..density..,
                   fill=community_transmission_level,
                   group=community_transmission_level),
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
  labs(title = "Density of 3 weeks later Reproduction Number for Transmission Risk Level groups(Rt between 0,3)")+
  xlab("Transmision Risk Level")+
  xlim(c(0,3))+
  annotation_logticks(side="b")

ggsave("Figures/rt_transmissionRisk_46counties_density_limitRt.jpg",
       fig, 
       height=6,
       width=8,
       scale=1.65)





# point for rt 3 weeks later limit between 0 and 3
names(rt_counties_TranRisk)
fig = ggplot(data = rt_counties_TranRisk)+
    geom_point(aes(x=date,
                   y=Rt_next3weeks,
                   color=community_transmission_level,
                   group=county_name),
               alpha=.7)+
    theme_bw()+
    labs(title = "3 weeks later Reproduction Number for Transmission Risk Level groups county level (rt betweem 0 and 3)")+
    xlab("")+
    ylab("")+
    ylim(c(0,3))+
    scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")
  
  ggsave("Figures/rt3week_transmissionRisk_46counties_point_limitedrt.jpg",
         fig, 
         height=6,
         width=8,
         scale=1.65)

  
  
  
# point for rt 3 weeks later
  fig = ggplot(data = rt_counties_TranRisk)+
    geom_point(aes(x=date,
                   y=Rt_next3weeks,
                   color=community_transmission_level,
                   group=county_name),
               alpha=.7)+
    theme_bw()+
    labs(title = "3 weeks later Reproduction Number for Transmission Risk Level groups county level")+
    xlab("")+
    ylab("")+
    #ylim(c(0,3))+
    scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")
  
  ggsave("Figures/rt3week_transmissionRisk_46counties_point.jpg",
         fig, 
         height=6,
         width=8,
         scale=1.65)
