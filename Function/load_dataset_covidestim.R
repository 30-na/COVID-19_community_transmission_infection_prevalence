library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(emmeans)


estimates_data = fread("RawData/estimates.csv")

estimates = estimates_data %>%
  dplyr::select(
    date,
    fips,
    #Estimate of the effective reproductive number (Rt)
    r_t
  ) %>%
  mutate(
    date = as.Date(date, "%y/%m/%d")
  )

summary(estimate)


CDC_data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_as_Originally_Posted_-_ARCHIVED.csv")

CDC = CDC_data %>%
  select(
    "date" = report_date,
    "fips" = fips_code,
    "county" = county_name,
    "state" = state_name,
    community_transmission_level
  ) %>%
  filter(community_transmission_level != "")%>%
  mutate(
    date = as.Date(date, "%Y/%m/%d"),
    community_transmission_level = factor(community_transmission_level,
                                          level=c("low", "moderate", "substantial", "high"))
  )

summary(CDC)


data = estimates %>%
  inner_join(CDC, by=c("date", "fips"))  %>%
  group_by(county, state)%>%
  mutate(
    Rt1NextWeeks = lead(r_t, 1),
    Rt2NextWeeks = lead(r_t, 2),
    Rt3NextWeeks = lead(r_t, 3),
    Rt1PrevWeeks = lag(r_t, 1),
    Rt2PrevWeeks = lag(r_t, 2),
    Rt3PrevWeeks = lag(r_t, 3)
  )

summary(data)




my_comparisons <- list( c("low", "moderate"),
                        c("moderate", "substantial"),
                        c("substantial", "high") )

boxPlot = function(filename,
                   title,
                   var){
  
  data = data %>%
    rename("targetVar" = var)
  
  fig = ggplot(data)+
    geom_boxplot(aes(x = community_transmission_level,
                     y = targetVar,
                     fill=community_transmission_level),
                 alpha=.6
                 #,outlier.shape = NA
                 )+
    theme_bw()+
    labs(title = title)+
    xlab("CDC Risk level")+
    ylab("Rt")+
    ylim(0, 2)+
    stat_compare_means(aes(x = community_transmission_level,
                           y = targetVar),  # Specify x aesthetic here
                       method = "anova") +
    # stat_compare_means(aes(x = community_transmission_level,
    #                        y = r_t),  # Specify x aesthetic here
    #                    comparisons = my_comparisons) +
    # stat_compare_means(
    #   comparisons = combn(levels(data$community_transmission_level), 2, simplify = FALSE)[c(1, 4, 6)],
    #   method="t.test"
    # )+
    scale_fill_manual("CDC\nRisk Level",
                      values=c("DodgerBlue","Yellow","Orange","Red"))
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1)
}








# density plots
densityPlot = function(title = "",
                       filename){

  fig = ggplot(data)+
    geom_density(aes(x=r_t,
                     y=..density..,
                     fill=community_transmission_level,
                     group=community_transmission_level),
                 color="grey",
                 alpha=0.4)+
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
    xlab("Rt")+
    annotation_logticks(side="b")+
    xlim(0,2)
  
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1)
}



# Comparing Mean
model = aov(r_t ~ community_transmission_level,
            data )
summary(model)
cont = emmeans::emmeans(model,
                       specs = "community_transmission_level")

gEmmeans1 = plot(cont, comparisons = F)+
  theme_bw()+
  labs(title = "Rt means for each CDC risk level")+
  labs( x = "Mean risk level")+
  labs(y = "CDC risk level")+
  labs(caption = "Distribution of Rt across different CDC risk levels. (Same week)")+
  theme(plot.caption = element_text(hjust = 0))

ggsave("Figures/gEmmeans1.jpg",
       gEmmeans1, 
       height=6,
       width=8,
       scale=1)






meanPlot = function(filename,
                   title,
                   var){

  data = data %>%
    rename("targetVar" = var)
  
  
model = aov(targetVar ~ community_transmission_level,
            data )
summary(model)
cont = emmeans::emmeans(model,
                        specs = "community_transmission_level")

gEmmeans1 = plot(cont, comparisons = F)+
  theme_bw()+
  labs(title = title)+
  labs( x = "Mean risk level")+
  labs(y = "CDC risk level")+
  labs(caption = "")+
  theme(plot.caption = element_text(hjust = 0))

ggsave(paste0("Figures/", filename, ".jpg"),
       gEmmeans1, 
       height=6,
       width=8,
       scale=1)
}


meanPlot(filename = "MeanSameWeek_boplot",
        title = "Comparing the Mean Rt number across different CDC risk levels for the same week",
        var = "r_t")

meanPlot(filename = "MeanRt1NextWeeks",
        title = "Comparing the Mean one week later Rt number across different CDC risk levels",
        var = "Rt1NextWeeks")

meanPlot(filename = "MeanRt2NextWeeks",
        title = "Comparing the Mean Two weeks later Rt number across different CDC risk levels",
        var = "Rt2NextWeeks")

meanPlot(filename = "MeanRt3NextWeeks",
        title = "Comparing the Mean Three weeks later Rt number across different CDC risk levels",
        var = "Rt3NextWeeks")

meanPlot(filename = "MeanRt1PrevWeeks",
        title = "Comparing the Mean one week before Rt number across different CDC risk levels",
        var = "Rt1PrevWeeks")

meanPlot(filename = "MeanRt2PrevWeeks",
        title = "Comparing the Mean Two weeks before Rt number across different CDC risk levels",
        var = "Rt2PrevWeeks")

meanPlot(filename = "MeanRt3PrevWeeks",
        title = "Comparing the Mean Three weeks before Rt number across different CDC risk levels",
        var = "Rt3PrevWeeks")



boxPlot(filename = "SameWeek_boplot",
        title = "Comparing the Rt number across different CDC risk levels for the same week",
        var = "r_t")

boxPlot(filename = "Rt1NextWeeks",
        title = "Comparing the one week later Rt number across different CDC risk levels",
        var = "Rt1NextWeeks")

boxPlot(filename = "Rt2NextWeeks",
        title = "Comparing the Two weeks later Rt number across different CDC risk levels",
        var = "Rt2NextWeeks")

boxPlot(filename = "Rt3NextWeeks",
        title = "Comparing the Three weeks later Rt number across different CDC risk levels",
        var = "Rt3NextWeeks")

boxPlot(filename = "Rt1PrevWeeks",
        title = "Comparing the one week before Rt number across different CDC risk levels",
        var = "Rt1PrevWeeks")

boxPlot(filename = "Rt2PrevWeeks",
        title = "Comparing the Two weeks before Rt number across different CDC risk levels",
        var = "Rt2PrevWeeks")

boxPlot(filename = "Rt3PrevWeeks",
        title = "Comparing the Three weeks before Rt number across different CDC risk levels",
        var = "Rt3PrevWeeks")



densityPlot(filename = "SameWeek_density",
        title = "Comparing the Rt number across different CDC risk levels for the same week")


