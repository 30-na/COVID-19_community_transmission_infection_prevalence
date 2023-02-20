#
library(data.table)
library(dplyr)
library(usdata)
###################################
# Load Harvard data
rt.data = fread("RawData/rt.csv")
harvard.rt = rt.data %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y"),
         type = as.factor(type)) %>%
  filter(!state %in% c("American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands")
         #,type == "estimate"
         ) %>%
  dplyr::select(-one_of("strat"))

summary(harvard.rt)

# plot the data
harvard.summary = harvard.rt %>%
  group_by(date,type) %>%
  summarize(mean = mean(mean))

linePlot(data = harvard.summary,
          x = "date",
          y = "mean",
          group = "type",
          xlab = "",
          ylab="",
          title = "",
          filename = "rt_harvard_type")

harvard.summary = harvard.rt %>%
  group_by(date,state) %>%
  summarize(mean = mean(mean))

linePlot(data = harvard.summary,
         x = "date",
         y = "mean",
         group = "state",
         xlab = "",
         ylab="",
         title = "",
         filename = "rt_harvard_state")

###########################################

# load prevalence data set
# filter date after "2020-07-01"
start.date = "2021-11-30"
# last day with estimate type in harvard data
end.date = "2022-03-14"
transmission.levels = c("low", "moderate", "substantial", "high")

load("RawData/Prevalence_Incidence_all.Rdata")




prevalence.data = alldata %>%
  dplyr::select(
    date
    ,type
    ,state
    ,Cases_tau_pct
    ,PositivePct_tau
    ,CDCLevelCommTrans
    ) %>%
  dplyr::filter(
    type == "state"
    ,date > start.date
    ,date < end.date
    ) %>%
  mutate(
    state = abbr2state(state)
  )

summary(alldata)

prevalence.data %>%
  group_by(CDCLevelCommTrans) %>%
  count()

barPlot(
  data = prevalence.data
  ,x = "CDCLevelCommTrans"
  ,xlab = ""
  ,title = "Number of states with different transmission risk levels from 2021-11-30 to 2022-03-14"
  ,filename = "tansmission_barplot")

######################################################################
# merge to data set

merged.data = prevalence.data %>%
  left_join(harvard.rt, by=c("state", "date")) %>%
  na.omit(CDCLevelCommTrans)




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

fit = lm(mean ~ CDCLevelCommTrans, data=merged.data)
TukeyHSD(aov(fit))
