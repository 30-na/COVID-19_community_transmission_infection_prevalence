source("Function/plot_functions.R")
load("ProcessedData/rt.data.RDA")
load("ProcessedData/rt.7days.data.RDA")

rt.summary = rt.data %>%
  group_by(date) %>%
  summarize(rt.mean = mean(Mean))

start.date = "2021-11-30"
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
         filename = "rtNumbercheck")


rt.summary = rt.data %>%
  group_by(date) %>%
  summarize(mean = mean(Mean))


linePlot(data = subset(rt.summary,
                       date > start.date &
                         date < end.date),
         x = "date",
         y = "mean",
         xlab = "",
         ylab="",
         title = "",
         filename = "rt_mean_check")




