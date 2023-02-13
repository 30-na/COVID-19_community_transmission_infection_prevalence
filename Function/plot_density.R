library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

# loud data from CDC
# Weekly COVID-19 County Level of Community Transmission as Originally Posted

#data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")
data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")



# filter date after "2020-07-01"
start.date = "2020-07-01"
transmission.levels = c("low", "moderate", "substantial", "high")
cdc.transmission = data %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"),
         # change data type
         cases_per_100K_7_day_count_change = as.numeric(cases_per_100K_7_day_count_change),
         # remove negative new case
         cases_per_100K_7_day_count_change = if_else(cases_per_100K_7_day_count_change < 0,
                                                     NA_real_,
                                                     cases_per_100K_7_day_count_change),
         # add IU.per100k, transmission.level for newcase and positive test
         IU.per100k = sqrt(cases_per_100K_7_day_count_change * percent_test_results_reported_positive_last_7_days)*1000,
         transmission.level.newcase = case_when(cases_per_100K_7_day_count_change < 10 ~ "low",
                                                cases_per_100K_7_day_count_change >= 10 &
                                                  cases_per_100K_7_day_count_change < 50 ~ "moderate",
                                                cases_per_100K_7_day_count_change >= 50 &
                                                  cases_per_100K_7_day_count_change < 100 ~ "substantial",
                                                cases_per_100K_7_day_count_change >= 100 ~ "high"),
         transmission.level.positivetest = case_when(percent_test_results_reported_positive_last_7_days < 5 ~ "low",
                                                     percent_test_results_reported_positive_last_7_days >= 5 &
                                                       percent_test_results_reported_positive_last_7_days <8 ~ "moderate",
                                                     percent_test_results_reported_positive_last_7_days >= 8 &
                                                       percent_test_results_reported_positive_last_7_days < 10 ~ "substantial",
                                                     percent_test_results_reported_positive_last_7_days > 10 ~ "high"),
         # change data type to factor
         community_transmission_level = factor(community_transmission_level,
                                               levels = transmission.levels),
         transmission.level.newcase = factor(transmission.level.newcase,
                                               levels = transmission.levels),
         transmission.level.positivetest = factor(transmission.level.positivetest,
                                               levels = transmission.levels)) %>%
  # drop NA values
  drop_na(community_transmission_level,
                transmission.level.newcase,
                transmission.level.positivetest)
  

summary(cdc.transmission)

# fig 01
fig.newcase.density = ggplot(cdc.transmission)+
  
  geom_density(aes(x=IU.per100k,
                   y=..density..*..n..,
                   fill=community_transmission_level,
                   group=community_transmission_level),
               color="grey",
               alpha=0.6,
               bw=0.01)+
  geom_density(aes(x=IU.per100k,
                   y=..density..*..n..), # "true"
               fill=NA,
               color="grey",
               linetype="dotted",
               bw=0.01)+
  # geom_histogram(aes(x = IU.per100k,
  #                    y = ..density..,
  #                    group = community_transmission_level,
  #                    color = "black",
  #                    fill = community_transmission_level),
  #                bins=50,
  #                alpha=.2)+
  scale_x_continuous(trans = "log10")+
  #geom_vline(xintercept=mean(cdc.transmission$IU.per100k),
  #           linetype="dotted")+
  #scale_x_log10(limits=c(1,10000))+
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
  labs(title = "Undiagnosed Infection Density for different Transmission Risk Level")+
  xlab("IU.per100k (log10)")+
  ylab("Frequency")+
  #facet_wrap(~Basis,ncol=1)+
  annotation_logticks(side="b")

  
  ggsave("Figures/fig.newcase.density.jpg",
         fig.newcase.density, 
         height=6,
         width=8,
         scale=1.65)
  
  
  
  
  
  
  
  
  
  # fig 02
  fig.newcase.density.newcase = ggplot(cdc.transmission)+
    
    geom_density(aes(x=IU.per100k,
                     y=..density..*..n..,
                     fill=transmission.level.newcase,
                     group=transmission.level.newcase),
                 color="grey",
                 alpha=0.6,
                 bw=0.01)+
    geom_density(aes(x=IU.per100k,
                     y=..density..*..n..), # "true"
                 fill=NA,
                 color="grey",
                 linetype="dotted",
                 bw=0.01)+

    scale_x_continuous(trans = "log10")+
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
    labs(title = "Undiagnosed Infection Density for different Transmission Risk Level (based on New case only)")+
    xlab("IU.per100k (log10)")+
    ylab("Frequency")+
    #facet_wrap(~Basis,ncol=1)+
    annotation_logticks(side="b")
  
  
  ggsave("Figures/fig.newcase.density.newcase.jpg",
         fig.newcase.density.newcase, 
         height=6,
         width=8,
         scale=1.65)
  

  
  # fig 03 positive case
  fig.newcase.density.positivetest = ggplot(cdc.transmission)+
    
    geom_density(aes(x=IU.per100k,
                     y=..density..*..n..,
                     fill=transmission.level.positivetest,
                     group=transmission.level.positivetest),
                 color="grey",
                 alpha=0.6,
                 bw=0.01)+
    geom_density(aes(x=IU.per100k,
                     y=..density..*..n..), # "true"
                 fill=NA,
                 color="grey",
                 linetype="dotted",
                 bw=0.01)+
    
    scale_x_continuous(trans = "log10")+
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
    labs(title = "Undiagnosed Infection Density for different Transmission Risk Level (based on positive test only)")+
    xlab("IU.per100k (log10)")+
    ylab("Frequency")+
    #facet_wrap(~Basis,ncol=1)+
    annotation_logticks(side="b")
  
  
  ggsave("Figures/fig.newcase.density.positivetest.jpg",
         fig.newcase.density.positivetest, 
         height=6,
         width=8,
         scale=1.65)
  
  
