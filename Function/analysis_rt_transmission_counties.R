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

