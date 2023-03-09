library(ggplot2)
library(dplyr)
library(data.table)


source("Function/plot_functions.R")

# load rt data
load("ProcessedData/out.county.rt.LosAngeles16months.RDA")

r = (out[["estimates"]][["summarised"]]$variable) == "R"
rt.mean = out[["estimates"]][["summarised"]]$mean[r]
date = out[["estimates"]][["summarised"]]$date[r]

rt.df = data.frame(
  date = as.Date(date, "%Y-%m-%d"),
  rt.mean = rt.mean
)




# load CDC transmission Risk Level
CDC.transmission.level = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_as_Originally_Posted_-_ARCHIVED.csv")

target.county = "Los Angeles"
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




## merge data
merged.data = trans.lvl %>%
  dplyr::inner_join(rt.df, by="date")

names(merged.data)
boxPlot(data=merged.data,
        x="riskLevel",
        y="rt.mean",
        xlab = "",
        ylab="",
        title = "",
        "rt_transmissionRisk_LosAngeles")



pointPlot(data=merged.data,
                     x="date",
                     y="rt.mean",
          group="riskLevel",
                     xlab = "",
                     ylab="",
                     title = "",
                     "rt_transmissionRisk_LosAngeles_point")



merged.data %>%
  group_by(riskLevel) %>%
  count()


trans.lvl %>%
  group_by(riskLevel) %>%
  count()





names(CDC.transmission.level)


