library(data.table)
library(dplyr)

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

boxPlot = function(data,
                   x,
                   y,
                   xlab = "",
                   ylab="",
                   title = "",
                   filename){
  
  df = data.frame()
  fig = ggplot(data)+
    geom_boxplot(aes(x = community_transmission_level,
                     y = r_t,
                     fill=community_transmission_level),alpha=.6,outlier.shape = NA)+
    theme_bw()+
    labs(title = "")+
    xlab(xlab)+
    ylab(ylab)
  
  ggsave(paste0("Figures/", filename, ".jpg"),
         fig, 
         height=6,
         width=8,
         scale=1.65)
}


