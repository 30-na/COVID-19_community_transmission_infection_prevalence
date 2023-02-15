library(data.table)
library(dplyr)

# load datasets
cdc.data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")
rt.data = fread("RawData/rt.csv")


# filter date after "2020-07-01"
start.date = "2021-11-30"
end.date = "2022-04-05"
transmission.levels = c("low", "moderate", "substantial", "high")
cdc.transmission = cdc.data %>%
  rename("state" = state_name) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"),
         # change data type
         cases_per_100K_7_day_count_change = as.numeric(cases_per_100K_7_day_count_change),
         # remove negative new case
         cases_per_100K_7_day_count_change = if_else(cases_per_100K_7_day_count_change < 0,
                                                     NA_real_,
                                                     cases_per_100K_7_day_count_change),
         # Transmission.level for newcase and positive test
         transmission.level.newcase = case_when(cases_per_100K_7_day_count_change < 10 ~ "low",
                                                cases_per_100K_7_day_count_change >= 10 &
                                                  cases_per_100K_7_day_count_change < 50 ~ "moderate",
                                                cases_per_100K_7_day_count_change >= 50 &
                                                  cases_per_100K_7_day_count_change < 100 ~ "substantial",
                                                cases_per_100K_7_day_count_change >= 100 ~ "high"),
         transmission.level.positivetest = case_when(percent_test_results_reported_positive_last_7_days < 5 ~ "low",
                                                     percent_test_results_reported_positive_last_7_days >= 5 &
                                                       percent_test_results_reported_positive_last_7_days <8 ~ "moderate",
                                                     percent_test_results_reported_positive_last_7_days >= 8 &
                                                       percent_test_results_reported_positive_last_7_days < 10 ~ "substantial",
                                                     percent_test_results_reported_positive_last_7_days > 10 ~ "high"),
         # Change data type to factor
         community_transmission_level = factor(community_transmission_level,
                                               levels = transmission.levels),
         transmission.level.newcase = factor(transmission.level.newcase,
                                             levels = transmission.levels),
         transmission.level.positivetest = factor(transmission.level.positivetest,
                                                  levels = transmission.levels))  %>%
  filter(state != "",
         date >= start.date,
         date <= end.date)
  # drop NA values
  # drop_na(community_transmission_level,
  #         transmission.level.newcase,
  #         transmission.level.positivetest)

summary(cdc.transmission)


harvard.rt = rt.data %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y"),
         type = as.factor(type)) %>%
  filter(!state %in% c("American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands"),
         type == "estimate") %>%
  dplyr::select(-one_of("strat"))

summary(harvard.rt)
  

# merge two datasets
merged.data = cdc.transmission %>%
  left_join(harvard.rt, by = c("date", "state"))


names(merged.data)
density_plot(data = merged.data,
             x = "mean",
             xlab = "Mean Covid-19 Reproduction Number (R)",
             title = "Reproduction Number Density for different Transmission Risk Level",
             group = "community_transmission_level",
             filename = "fig.rt.density.transmission")
  
  
density_plot(data = merged.data,
             x = "mean",
             xlab = "Mean Covid-19 Reproduction Number (R)",
             title = "Reproduction Number Density for different Transmission Risk Level (only based on positive test)",
             group = "transmission.level.positivetest",
             filename = "fig.rt.density.transmission.positivetest")
  

density_plot(data = merged.data,
             x = "mean",
             xlab = "Mean Covid-19 Reproduction Number (R)",
             title = "Reproduction Number Density for different Transmission Risk Level (only based on new case)",
             group = "transmission.level.newcase",
             filename = "fig.rt.density.transmission.newcas")
