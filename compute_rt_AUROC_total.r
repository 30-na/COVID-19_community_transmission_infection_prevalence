library(dplyr)
library(data.table)

# Load CovidActNow Data and NCHS Data
load("RawData/CovidActNow.rda")
load("ProcessedData/county.NCHS.RDA")


# Clean and Merged Data
data = covid_data %>%
  dplyr::select(
    date,
    state,
    county,
    "fips_code" = fips,
    # R_t, or the estimated number of infections arising from a typical case
    "infection_rate" = metrics.infectionRate,
    #  Enum: 0 1 2 3 4 Community transmission level for region, calculated using the CDC definition.
    # Possible values:  0: Low - 1: Moderate - 2: Substantial - 3: High - 4: Unknown
    cdcTransmissionLevel
  ) %>%
  dplyr::mutate(
    cdcTransmissionLevel = case_when(
      cdcTransmissionLevel == 0 ~ "Low",
      cdcTransmissionLevel == 1 ~ "Moderate",
      cdcTransmissionLevel == 2 ~ "Substantial",
      cdcTransmissionLevel == 3 ~ "High",
      cdcTransmissionLevel == 4 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    cdcTransmissionLevel = factor(cdcTransmissionLevel,
                                  levels = c("Low", "Moderate", "Substantial", "High")), 
    date = as.Date(date, format = "%y-%m-%d"),
    cdcTransmissionLevel = factor(
      cdcTransmissionLevel,
      levels = c("Low", "Moderate", "Substantial", "High")
    )
  ) %>%
  left_join(county.NCHS,
            by = c("fips_code", "state")) %>%
  group_by(county, state) %>%
  mutate(mean_last_7_days = zoo::rollmean(infection_rate, k = 7, fill = NA, align = "right"),
         Rt3NextWeeks = lead(mean_last_7_days, 21),
         risk_level = case_when(cdcTransmissionLevel == "Low"  ~ 1,
                                cdcTransmissionLevel == "Moderate"  ~ 2,
                                cdcTransmissionLevel == "Substantial"  ~ 3,
                                cdcTransmissionLevel == "High"  ~ 4),
         stateFips = paste(state, fips_code, sep = ",")
  ) %>%
  filter(!is.na(Rt3NextWeeks),
         !is.na(risk_level))




dates = sort(unique(data$date))
columnNames = c("date",
                "state",
                "county",
                "fips_code",
                "infection_rate",
                "cdcTransmissionLevel",
                "UR_category",
                "POPPCT_URBAN",
                "POPPCT_RURAL",
                "POP_URBAN",
                "POP_RURAL",
                "pop_2020",
                "mean_last_7_days",
                "Rt3NextWeeks",
                "risk_level",
                "stateFips",
                "target_county",
                "target_risk",
                "target_rt",
                "expected_higher_rt",
                "actual_higher_rt")

compared_counties = data.frame(matrix(nrow = 0,
                                      ncol=length(columnNames)))

for (date_index in 1:49){
  
  selected_counties = dplyr::filter(data,
                                    date == dates[date_index])
  
    
    
    
#  for(i in 1:nrow(selected_counties)){
    for(i in 1:2){    
    county = selected_counties[i,]
    target_county = county$stateFips
    target_risk = county$risk_level
    target_rt = county$Rt3NextWeeks
    
    
    other_counties = selected_counties %>%
      dplyr::filter(risk_level != target_risk)  %>%
      mutate(target_county = target_county,
             target_risk = target_risk,
             target_rt = target_rt,
             expected_higher_rt = if_else(risk_level > target_risk, 1, 0),
             actual_higher_rt = if_else(Rt3NextWeeks > target_rt, 1, 0))
    
    compared_counties = rbind(compared_counties, other_counties)
    
  }
save(compared_counties, 
     file=paste0("ProcessedData/comparedCounties/",
                 date_index,
                 target_county,
                 ".RDA"))
}




