source("Function/plot_functions.R")
load("ProcessedData/prevalence.data.RDA")
load("ProcessedData/rt.7days.data.RDA")


# merge to data set
merged.rt.NdeffoGithub = prevalence.data %>%
  inner_join(
    rt.data
    , by=c("state", "date")
    ) %>%
  na.omit(
    CDCLevelCommTrans
    )

save(merged.rt.NdeffoGithub
     ,file = "ProcessedData/merged.rt.NdeffoGithub.RDA")



# plot transmission risk count in that time interval
merged.rt.NdeffoGithub %>%
  group_by(CDCLevelCommTrans) %>%
  count()

barPlot(
  data = merged.rt.NdeffoGithub
  ,x = "CDCLevelCommTrans"
  ,xlab = ""
  ,title = "Number of states with different transmission risk levels from 2020-07-01 to 2022-10-18"
  ,filename = "tansmission_barplot")

summary(merged.rt.NdeffoGithub$date)


# plot merged data

















densityPlot(data = merged.rt.NdeffoGithub,
            x = "Mean",
            xlab = "Mean Covid-19 Reproduction Number (R)",
            title = "Reproduction Number Density for Transmission Risk Level",
            group = "CDCLevelCommTrans",
            filename = "fig.rt.density.prevalence.transmission01")

boxPlot(
  data = merged.rt.NdeffoGithub
  ,x = "CDCLevelCommTrans"
  ,y = "Mean"
  ,xlab = "Mean Covid-19 Reproduction Number (R)"
  ,title = "Reproduction Number Density for Transmission Risk Level"
  ,filename = "fig.rt.boxplot.prevalence.transmission01")

pointPlot(
  data = merged.rt.NdeffoGithub
  ,x = "date"
  ,y = "Mean"
  ,group = "CDCLevelCommTrans"
  ,filename = "pointplot_Rt_transmision01"
)

linePlot(
  data = merged.rt.NdeffoGithub
  ,x = "date"
  ,y = "Mean"
  ,group = "CDCLevelCommTrans"
  ,filename = "lineplot_Rt_transmision01"
)


fig = ggplot(merged.rt.NdeffoGithub)+
  geom_boxplot(aes(x=CDCLevelCommTrans, y=Mean, fill=CDCLevelCommTrans),alpha=.6,outlier.shape = NA)+
  theme_bw()

ggsave("Figures/gh.jpg",
       fig, 
       height=6,
       width=8,
       scale=1.65)


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






####################################################
# plot the data harvard dat
harvard.summary = rt.data %>%
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



