library(dplyr)
library(data.table)


# load NCHS


# load CDC transmission Risk Level
CDC.transmission.level = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_as_Originally_Posted_-_ARCHIVED.csv")

trans.lvl = CDC.transmission.level %>%
  mutate(
    date = as.Date(report_date, "%Y/%m/%d"),
    county = gsub(" County", "", county_name)
  ) %>%
  select(
    date,
    county,
    "riskLevel" = community_transmission_level
  )%>%
  dplyr::filter(
    county == target.county
  ) 