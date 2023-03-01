source("Function/plot_functions.R")
load("ProcessedData/harvard.rt.RDA")
load("ProcessedData/prevalence.data.RDA")
load("ProcessedData/merged.rtHarvard.NdeffoGithub.RDA")

# plot the data harvard dat
harvard.summary = harvard.rt %>%
  group_by(date,type) %>%
  summarize(mean = mean(mean))

linePlot(data = subset(harvard.summary,
                       date > start.date &
                         date < end.date),
         x = "date",
         y = "mean",
         xlab = "",
         ylab="",
         title = "",
         filename = "rt_harvard_type")

harvard.summary = harvard.rt %>%
  group_by(date,state) %>%
  summarize(mean = mean(mean))

start.date = "2021-11-30"
end.date = "2022-03-14"

linePlot(data = subset(harvard.rt,
                       date > start.date &
                         date < end.date),
         x = "date",
         y = "mean",
         group = "state",
         xlab = "",
         ylab="",
         title = "",
         filename = "rt_harvard_state")



# plot prevalence data
prevalence.data %>%
  group_by(CDCLevelCommTrans) %>%
  count()

barPlot(
  data = prevalence.data
  ,x = "CDCLevelCommTrans"
  ,xlab = ""
  ,title = "Number of states with different transmission risk levels from 2021-11-30 to 2022-03-14"
  ,filename = "tansmission_barplot")



# plot merged data
densityPlot(data = merged.data,
            x = "mean",
            xlab = "Mean Covid-19 Reproduction Number (R)",
            title = "Reproduction Number Density for Transmission Risk Level",
            group = "CDCLevelCommTrans",
            filename = "fig.rt.density.prevalence.transmission")

boxPlot(
  data = merged.data
  ,x = "CDCLevelCommTrans"
  ,y = "mean"
  ,xlab = "Mean Covid-19 Reproduction Number (R)"
  ,title = "Reproduction Number Density for Transmission Risk Level"
  ,filename = "fig.rt.boxplot.prevalence.transmission")

pointPlot(
  data = merged.data
  ,x = "date"
  ,y = "mean"
  ,group = "CDCLevelCommTrans"
  ,filename = "pointplot_Rt_transmision"
)


fit = lm(mean ~ CDCLevelCommTrans, data=merged.data)
TukeyHSD(aov(fit))
plot(TukeyHSD(aov(fit)))


data = merged.data %>%
  group_by(date, CDCLevelCommTrans) %>%
  summarize(rt = mean(mean))

linePlot(
  data = data
  ,x = "date"
  ,y = "rt"
  ,group = "CDCLevelCommTrans"
  ,filename = "lineplot_Rt_transmision"
)