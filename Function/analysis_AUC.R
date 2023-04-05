library(ggplot2)
library(dplyr)


# load ddata
load("ProcessedData/rt_counties_TranRisk.RDA")

# make a data frame for AUC 
names(AUC_data)
AUC_data = rt_counties_TranRisk %>%
  mutate(
    community_level = case_when(community_transmission_level == "low"  ~ 1,
                                community_transmission_level == "moderate"  ~ 2,
                                community_transmission_level == "substantial" ~ 3,
                                community_transmission_level == "high"  ~ 4)
    )%>%
  select(
    date,
    state,
    county_name,
    Rt_next3weeks,
    community_level,
    community_transmission_level
    ) 



compairAUC = function(first, second){
  if(
    ((first$community_level < second$community_level) & (first$Rt_next3weeks < second$Rt_next3weeks)) |
    ((first$community_level > second$community_level) & (first$Rt_next3weeks > second$Rt_next3weeks))
    ){return(1)
  }else{
      return(0)
    }
}

FirstRandomCounty = function(data){
  return(data %>% ungroup() %>% na.omit %>% sample_n(size = 1))
}

SecondRandomCounty = function(data, first_sample){
  data = data %>% dplyr::filter(community_level != first_sample$community_level)
  return(data %>% ungroup() %>% na.omit %>% sample_n(size = 1))
}

f = FirstRandomCounty(AUC_data)
f
s = SecondRandomCounty(AUC_data, f)
s
compairAUC(f,s)






