library(data.table)
library(dplyr)
library(ggplot2)

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




CDC_data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_as_Originally_Posted_-_ARCHIVED.csv")

CDC = CDC_data %>%
  select(
    "date" = report_date,
    "fips" = fips_code,
    "county" = county_name,
    "state" = state_name,
    community_transmission_level
  ) %>%
  mutate(
    date = as.Date(date, "%Y/%m/%d")
  )




data = estimates %>%
  inner_join(CDC, by=c("date", "fips"))

names(data)

boxPlot = function(filename,
                   title){
  
  df = data.frame()
  fig = ggplot(data)+
    geom_boxplot(aes(x = community_transmission_level,
                     y = r_t,
                     fill=community_transmission_level),
                 alpha=.6
                 #,outlier.shape = NA
                 )+
    theme_bw()+
    labs(title = title)+
    xlab("CDC Risk level")+
    ylab("Rt")+
    ylim(0, .5)
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}

boxPlot(filename = "SameWeek",
        title = "Comparing the Rt number across different CDC risk levels for the same week")

