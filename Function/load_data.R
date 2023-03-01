#
library(data.table)
library(dplyr)
library(usdata)
library(tidyr)



# Load Harvard data
rt.data = fread("RawData/rt.csv")
harvard.rt = rt.data %>% 
  mutate(
    date = as.Date(date, format="%m/%d/%Y")
    ,type = as.factor(type)
    ) %>%
  filter(
    !state %in% c("American Samoa"
                  , "Guam"
                  , "Northern Mariana Islands"
                  , "Virgin Islands")
         #,type == "estimate"
         ) %>%
  dplyr::select(
    -one_of("strat")
    )

summary(harvard.rt)
save(harvard.rt
     ,file = "ProcessedData/harvard.rt.RDA")



# load prevalence data set
load("RawData/Prevalence_Incidence_all.Rdata")

# filter date after "2020-07-01"
start.date = "2021-11-30"
# last day with estimate type in harvard data
end.date = "2022-03-14"
transmission.levels = c("low", "moderate", "substantial", "high")

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
    #,date > start.date
    #,date < end.date
    ) %>%
  mutate(
    state = abbr2state(state)
  )

summary(alldata)
save(prevalence.data
     ,file = "ProcessedData/prevalence.data.RDA")



# merge to data set
merged.rtHarvard.NdeffoGithub = prevalence.data %>%
  left_join(harvard.rt, by=c("state", "date")) %>%
  na.omit(CDCLevelCommTrans)
save(merged.rtHarvard.NdeffoGithub
     ,file = "ProcessedData/merged.rtHarvard.NdeffoGithub.RDA")

####################################

# load rt data from GitHub
rtReich.data0404 = fread("RawData/rt.ReichLab2022-04-04.csv")
rtReich.data0207 = fread("RawData/rt.ReichLab2022-02-07.csv")
check = inner_join(rtReich.data0404
           ,rtReich.data0207
           ,by = c("region", "date")
           )%>%
  dplyr::select(
    region
    ,date
    ,mean.x
    ,mean.y
  ) %>%
  gather(
    key = "group"
    ,value = "mean"
    ,c(mean.x, mean.y)
    ) %>%
  group_by(
    date
    ,group
  ) %>%
  summarize(
    mean = mean(mean)
  ) %>%
  mutate(
    group = ifelse(
      group == "mean.x"
      ,"rt.ReichLab2022-04-04"
      ,"rt.ReichLab2022-02-07"
      )
  )



linePlot(
  data = check
  ,x = "date"
  ,y = "mean"
  ,group = "group" 
  ,xlab = "date"
  ,ylab="rt"
  ,title = "Compare rt value in same days in two different data samples from Reich GitHub"
  ,"check1")




############
rtReich.data1206 = fread("RawData/rt.ReichLab2021-12-06.csv")
rtReich.data0117 = fread("RawData/rt.ReichLab2022-01-17.csv")
summary(rtReich.data0207)
check = inner_join(
  rtReich.data1206
  ,rtReich.data0117
  ,by = c("region", "date")
  )%>%
  dplyr::select(
    region
    ,date
    ,mean.x
    ,mean.y
  ) %>%
  gather(
    key = "group"
    ,value = "mean"
    ,c(mean.x, mean.y)
  ) %>%
  group_by(
    date
    ,group
  ) %>%
  summarize(
    mean = mean(mean)
  ) %>%
  mutate(
    group = ifelse(
      group == "mean.x"
      ,"rt.ReichLab2021-12-06"
      ,"rt.ReichLab2022-01-17"
    )
  )



linePlot(
  data = check
  ,x = "date"
  ,y = "mean"
  ,group = "group" 
  ,xlab = "date"
  ,ylab="rt"
  ,title = "Compare rt value in same days in two different data samples from Reich GitHub"
  ,"check2")


##############################################################


summary(rtReich.data)
rtReich = rt.data.reich
  mutate(
    date = as.Date(date, format="%m/%d/%Y")
    ,type = as.factor(type)
  ) %>%
  filter(
    !state %in% c("American Samoa"
                  , "Guam"
                  , "Northern Mariana Islands"
                  , "Virgin Islands")
    #,type == "estimate"
  ) %>%
  dplyr::select(
    -one_of("strat")
  )

summary(harvard.rt)
save(harvard.rt
     ,file = "ProcessedData/harvard.rt.RDA")
            