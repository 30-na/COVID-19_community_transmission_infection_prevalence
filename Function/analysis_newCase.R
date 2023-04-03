
load("ColabData/counties.transmission.newCase.RDA")

library(dplyr)
library(data.table)
library(usdata)


# time interval
startDate = "2021-08-06"
endDate = "2022-10-18"

unique(counties.transmission.newCase$UR_category)
# NEW CASE CDC
names(counties.transmission.newCase)
newCase_cdc = counties.transmission.newCase %>%
  dplyr::filter(date < endDate,
                date > startDate,
                UR_category == "Large central metro")
  

fig = ggplot(data=newCase_cdc,
       aes(x=date,y=confirm, group=county_name)) +
  geom_point(aes(col=county_name))+
  labs(title = "New case based of CDC (cases_per_100K/100000*pop_2020) Formula")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")

ggsave("Figures/newCase_MCL_counties.jpg",
       fig, 
       height=6,
       width=8,
       scale=1.65)




fig = ggplot(data=newCase_cdc,
             aes(x=date,y=cases_per_100K_numeric, group=county_name)) +
  geom_point(aes(col=county_name))+
  labs(title = "New case based of CDC cases_per_100K")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")

ggsave("Figures/newCase_MCL_counties_perop.jpg",
       fig, 
       height=6,
       width=8,
       scale=1.65)





# NEWYORK NEW CASE
# load county case from New York Times
counties.case.NewYork2021 = fread("RawData/us-counties-2021.csv")
counties.case.NewYork2022 = fread("RawData/us-counties-2022.csv")
counties.case.NewYork2023 = fread("RawData/us-counties-2023.csv")

counties.case.NewYork = rbind(
  counties.case.NewYork2021,
  counties.case.NewYork2022,
  counties.case.NewYork2023
) %>%
  dplyr::mutate(
    date = as.Date(date, "%y/%m/%d"),
    state = state2abbr(state))


counties.case.NewYork = counties.case.NewYork %>%
  dplyr::arrange(
    state,
    county,
    date
  ) %>%
  dplyr::mutate(
    previousDayCase = lag(cases, n=1),
    confirm = cases - previousDayCase
  )




  

# load NCHS Data
load("ProcessedData/county.NCHS.RDA")

# merged datas
newCase_newYork = counties.case.NewYork %>%
  select(date,
         "fips_code" = fips,
         state,
         confirm)%>%
  dplyr::filter(date < endDate,
                date > startDate) %>%
  left_join(
    county.NCHS, by=c("state", "fips_code")
  ) %>%
  filter(UR_category == "Large central metro") %>%
  mutate(fips_code = as.character(fips_code))


names(newCase_newYork)

fig = ggplot(data=newCase_newYork,
             aes(x=date,y=confirm, group=fips_code)) +
  geom_point(aes(col=fips_code))+
  labs(title = "New case based of The New York Times Coronavirus Case Data")+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")

ggsave("Figures/newCase_NEWYORK_MCL_counties.jpg",
       fig, 
       height=6,
       width=8,
       scale=1.65)



