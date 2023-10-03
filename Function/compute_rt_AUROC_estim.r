library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(emmeans)
library(car)
library(usdata)


# Load CDC, covidestimates and NCHS Data
estimates_data = fread("RawData/estimates.csv")
load("ProcessedData/county.NCHS.RDA")
CDC_data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")





#clean estimate data
estimates = estimates_data %>%
  dplyr::select(
    date,
    fips,
    #Estimate of the effective reproductive number (Rt)
    "Rt" = r_t
  ) %>%
  mutate(
    date = as.Date(date, "%y/%m/%d")
  )




# Clean CDC data
CDC = CDC_data %>%
  dplyr::select(
    date,
    "fips" = fips_code,
    "county" = county_name,
    "state" = state_name,
    community_transmission_level
  ) %>%
  filter(community_transmission_level != "")%>%
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    community_transmission_level = factor(community_transmission_level,
                                          level=c("low", "moderate", "substantial", "high"))
  )



# clean NCHS Data
NCHS = county.NCHS %>%
  dplyr::select(
    "fips" = fips_code,
    state,
    UR_code,
    UR_category
  ) %>%
  dplyr::mutate(
    state = abbr2state(state)
  )





data = estimates %>%
  inner_join(CDC, by=c("date", "fips")) %>%
  left_join(NCHS, by=c("state", "fips")) %>%
  group_by(county, state) %>%
  mutate(
    Rt_NextWeek = lead(Rt, 1),
    risk_level = case_when(community_transmission_level == "low"  ~ 1,
                           community_transmission_level == "moderate"  ~ 2,
                           community_transmission_level == "substantial"  ~ 3,
                           community_transmission_level == "high"  ~ 4),
    stateFips = paste0(state, fips)
  )

names(data)
summary(data)
dates = sort(unique(data$date))

columnNames = c("date",
                "fips",
                "Rt",
                "county",
                "state",
                "community_transmission_level",
                "UR_code",
                "UR_category",
                "Rt_NextWeek",
                "risk_level",
                "stateFips",
                "target_UR",
                "target_county",
                "target_risk",
                "target_rt",
                "target_rt_NextWeek",
                "expected_higher_rt",
                "actual_higher_rt",
                "actual_higher_rt_NextWeek")



# define a sample function 
# sampleCounties = function(data_day, URcode){
#   
#   counties = dplyr::filter(
#     data_day,
#     UR_code == URcode
#   )
#   
#   n = nrow(counties)
#   
#   if (n <= 20){
#     set.seed(n)
#     sample = counties
#   }else{
#     sampleIndex = sample(1:n, 
#                          ceiling(0.2 * n),
#                          replace = F)
#     sample = counties[sampleIndex, ]
#   }
#   return(sample)
# }


for(date_index in 1:length(dates)){
  
  # define and empty dataframe
  compared_counties = data.frame(matrix(nrow = 0,
                                        ncol=length(columnNames)))
  
  # all counties in "date_index" specific day
  data_day = dplyr::filter(data,
                           date == dates[date_index])
  
  # 
  
  
  
  for(i in 1:nrow(data_day)){
    
    county = data_day[i,]
    target_UR = county$UR_code
    target_county = county$stateFips
    target_risk = county$risk_level
    target_rt = county$Rt
    target_rt_NextWeek = county$Rt_NextWeek
    
    
    # filter counties with different Risk level but the same UR code
    other_counties = data_day[i:nrow(data_day),] %>%
      dplyr::filter(
        risk_level != target_risk,
        UR_code == target_UR
      )%>%
      dplyr::mutate(
        target_UR = target_UR,
        target_county = target_county,
        target_risk = target_risk,
        target_rt = target_rt,
        target_rt_NextWeek = target_rt_NextWeek,
        expected_higher_rt = if_else(risk_level > target_risk, 1, 0),
        actual_higher_rt = if_else(Rt > target_rt, 1, 0),
        actual_higher_rt_NextWeek = if_else(Rt_NextWeek > target_rt_NextWeek, 1, 0)
      )
    
    compared_counties = rbind(compared_counties, other_counties)
    
  }
  
  
  save(compared_counties, 
       file=paste0("ProcessedData/outputsample_covidEstim/",
                   date_index,
                   ".RDA"))
}





