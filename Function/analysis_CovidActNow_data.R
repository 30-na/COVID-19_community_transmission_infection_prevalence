library(dplyr)
library(ggplot2)
library(car)

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
    date = as.Date(date, format = "%y-%m-%d"),
    cdcTransmissionLevel = factor(
      cdcTransmissionLevel,
      levels = c("Low", "Moderate", "Substantial", "High")
    )
  ) %>%
  left_join(county.NCHS,
            by = c("fips_code", "state")) %>%
  na.omit(cdcTransmissionLevel,
          UR_category)%>%
  group_by(county, state) %>%
  mutate(mean_last_7_days = zoo::rollmean(infection_rate, k = 7, fill = NA, align = "right"))

data %>%
  group_by(cdcTransmissionLevel) %>%
  count()



# Box Plot
fig = ggplot(data %>% dplyr::filter(!is.na(cdcTransmissionLevel))) +
  geom_boxplot(
    aes(
      y = infection_rate,
      x = cdcTransmissionLevel,
      fill = cdcTransmissionLevel,
      group = cdcTransmissionLevel
    ),
    alpha = 0.7
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    strip.text = element_text(size = 12)
  ) +
  labs(title = "Box Plot of Rt Number for Transmission Risk Level groups") +
  xlab("Transmision Risk Level") +
  scale_fill_manual(values = c('#2c7bb6', '#abd9e9', '#fdae61', '#d7191c')) 
 

ggsave(
  "Figures/rt_transmissionRisk_counties_BoxPlot.jpg",
  fig,
  height = 6,
  width = 8,
  scale = 1.3
)


# Box Plot facet
fig = ggplot(data %>% dplyr::filter(!is.na(cdcTransmissionLevel),
                                    !is.na(UR_category)))+
  geom_boxplot(aes(y=infection_rate,
                   x=cdcTransmissionLevel,
                   fill=cdcTransmissionLevel,
                   group=cdcTransmissionLevel),
               alpha=0.7)+
  theme_bw()+
  theme(legend.position = "top",
        panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  labs(title = "Box Plot of Rt Number for Transmission Risk Level groups")+
  xlab("Transmision Risk Level")+
  scale_fill_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))+
  facet_wrap(. ~ UR_category)

ggsave("Figures/rt_transmissionRisk_counties_BoxPlot_facet.jpg",
       fig,
       height=6,
       width=8,
       scale=1.3)



# line
fig = data %>%
  group_by(cdcTransmissionLevel, date) %>%
  summarise(meanRT = mean(infection_rate, na.rm = TRUE))%>%
  ggplot()+
  geom_line(aes(x=date,
                y=meanRT,
                color=cdcTransmissionLevel,
                group=cdcTransmissionLevel),
            alpha=0.7)+
  theme_bw()+
  labs(title = "The Mean Rt Number for the counties in four Transmission Risk Level groups")+
  xlab("date")+
  scale_colour_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))
ggsave("Figures/rt_transmissionRisk_counties_line.jpg",
       fig,
       height=6,
       width=8,
       scale=1.3)


# line for last 7 days
fig = data %>%
  group_by(cdcTransmissionLevel, date) %>%
  summarise(meanRT = mean(mean_last_7_days, na.rm = TRUE))%>%
  ggplot()+
  geom_line(aes(x=date,
                y=meanRT,
                color=cdcTransmissionLevel,
                group=cdcTransmissionLevel),
            alpha=0.7)+
  theme_bw()+
  labs(title = "The Mean Rt Number for the counties in four Transmission Risk Level groups (last 7 days )")+
  xlab("date")+
  scale_colour_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))
ggsave("Figures/rt_transmissionRisk_counties_line_last7days.jpg",
       fig,
       height=6,
       width=8,
       scale=1.3)


# Conduct ANOVA test if assumptions are met
## Fit anova model
##overall F test

oneway.test(data$infection_rate ~ data$cdcTransmissionLevel,
              var.equal=TRUE)

myfit = aov(result ~ brand, data=rust)
summary(myfit) # display Type I ANOVA table


# point
fig = data %>%
dplyr::filter(!is.na(cdcTransmissionLevel))%>%
ggplot()+
geom_point(aes(x=date,
y=infection_rate,
color=cdcTransmissionLevel,
group=cdcTransmissionLevel),
alpha=0.5)+
theme_bw()+
labs(title = "The Rt Number for the counties in four Transmission Risk Level groups")+
xlab("date")+
scale_colour_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))
ggsave("Figures/rt_transmissionRisk_counties_point.jpg",
fig,
height=6,
width=8,
scale=1.3)





# Box Plot facet
fig = ggplot(data %>% dplyr::filter(!is.na(cdcTransmissionLevel),
!is.na(UR_category)))+
geom_boxplot(aes(y=infection_rate,
x=cdcTransmissionLevel,
fill=cdcTransmissionLevel,
group=cdcTransmissionLevel),
alpha=0.7)+
theme_bw()+
theme(legend.position = "top",
panel.grid = element_blank(),
strip.text = element_text(size = 12))+
labs(title = "Box Plot of Rt Number for Transmission Risk Level groups")+
xlab("Transmision Risk Level")+
scale_fill_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))+
facet_wrap(. ~ UR_category)
ggsave("Figures/rt_transmissionRisk_counties_BoxPlot_facet.jpg",
fig,
height=6,
width=8,
scale=1.3)



#Density
geom_density(aes(x=infection_rate,
fill=cdcTransmissionLevel,
group=cdcTransmissionLevel),
color="grey",
alpha=0.7)+
#scale_x_continuous(trans = "log10")+
scale_linetype("")+
theme_bw()+
theme(legend.position = "top",
panel.grid = element_blank(),
strip.text = element_text(size = 12))+
labs(title = "Density of Rt Number for Transmission Risk Level groups")+
xlab("Transmision Risk Level")+
scale_fill_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))+
facet_wrap(. ~ UR_category)
# Density Plot
fig = ggplot(data %>% dplyr::filter(!is.na(cdcTransmissionLevel),
!is.na(UR_category)))+
geom_density(aes(x=infection_rate,
fill=cdcTransmissionLevel,
group=cdcTransmissionLevel),
color="grey",
alpha=0.7)+
#scale_x_continuous(trans = "log10")+
scale_linetype("")+
theme_bw()+
theme(legend.position = "top",
panel.grid = element_blank(),
strip.text = element_text(size = 12))+
labs(title = "Density of Rt Number for Transmission Risk Level groups")+
xlab("Transmision Risk Level")+
scale_fill_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))+
facet_wrap(. ~ UR_category)
ggsave("Figures/rt_transmissionRisk_counties_density_facet.jpg",
fig,
height=6,
width=8,
scale=1.65)



# Box Plot
fig = ggplot(data %>% dplyr::filter(!is.na(cdcTransmissionLevel)))+
geom_boxplot(aes(y=infection_rate,
x=cdcTransmissionLevel,
fill=cdcTransmissionLevel,
group=cdcTransmissionLevel),
alpha=0.7)+
theme_bw()+
theme(legend.position = "top",
axis.text.y = element_blank(),
panel.grid = element_blank(),
strip.text = element_text(size = 12))+
labs(title = "Box Plot of Rt Number for Transmission Risk Level groups")+
xlab("Transmision Risk Level")+
scale_colour_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))
ggsave("Figures/rt_transmissionRisk_counties_BoxPlot.jpg",
fig,
height=6,
width=8,
scale=1.3)
# Box Plot
fig = ggplot(data %>% dplyr::filter(!is.na(cdcTransmissionLevel)))+
geom_boxplot(aes(y=infection_rate,
x=cdcTransmissionLevel,
fill=cdcTransmissionLevel,
group=cdcTransmissionLevel),
alpha=0.7)+
theme_bw()+
theme(legend.position = "top",
axis.text.y = element_blank(),
panel.grid = element_blank(),
strip.text = element_text(size = 12))+
labs(title = "Box Plot of Rt Number for Transmission Risk Level groups")+
xlab("Transmision Risk Level")+
scale_fill_manual(values = c('#2c7bb6','#abd9e9','#fdae61','#d7191c'))
ggsave("Figures/rt_transmissionRisk_counties_BoxPlot.jpg",
fig,
height=6,
width=8,
scale=1.3)
# Density Plot

