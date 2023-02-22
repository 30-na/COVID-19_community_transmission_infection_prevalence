#
library(data.table)
library(dplyr)
library(usdata)



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
    ,date > start.date
    ,date < end.date
    ) %>%
  mutate(
    state = abbr2state(state)
  )

summary(alldata)
save(prevalence.data
     ,file = "ProcessedData/prevalence.data.RDA")



# merge to data set
merged.data = prevalence.data %>%
  left_join(harvard.rt, by=c("state", "date")) %>%
  na.omit(CDCLevelCommTrans)
save(merged.data
     ,file = "ProcessedData/merged.rtHarvard.NdeffoGithub.RDA")




            