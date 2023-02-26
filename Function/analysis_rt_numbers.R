source("Function/plot_functions.R")
load("ProcessedData/rt.data.RDA")


rt.summary = rt.data %>%
  group_by(date) %>%
  summarize(rt.mean = mean(Mean))

start.date = "2021-11-30"
# last day with estimate type in harvard data
end.date = "2022-03-14"

linePlot(data = subset(rt.data,
                       date > start.date &
                         date < end.date),
         x = "date",
         y = "Mean",
         group = "state",
         xlab = "",
         ylab="",
         title = "",
         filename = "rtNumber")
