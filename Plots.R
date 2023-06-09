
# Library # boxplot Anova   #### 
library(dplyr)
library(ggplot2)
library(FSA)
library(knitr)
library(kableExtra)
library(ggpubr)
library(emmeans)
library(pROC)
library(caret)
library(ROCR)

# Load data #### 
load("ProcessedData/CovidActNow.rda")
load("ProcessedData/county.NCHS.RDA")
load("ProcessedData/AUROC_merged.RDA")


# Clean and Merged Data #### 
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
  mutate(mean_last_7_days = ifelse(is.na(infection_rate),
                                   NA,
                                   zoo::rollmean(infection_rate, k = 7, fill = NA, align = "right")),
         Rt3NextWeeks = lead(mean_last_7_days, 21),
         risk_level = case_when(cdcTransmissionLevel == "Low"  ~ 1,
                                cdcTransmissionLevel == "Moderate"  ~ 2,
                                cdcTransmissionLevel == "Substantial"  ~ 3,
                                cdcTransmissionLevel == "High"  ~ 4),
         stateFips = paste(state, fips_code, sep = ",")
  ) %>%
  filter(!is.na(Rt3NextWeeks),
         !is.na(risk_level),
         !is.na(UR_category)
  )


# boxplot Anova   #### 
myfit = aov(mean_last_7_days ~ cdcTransmissionLevel,
            data=data)
anova(myfit)


gAnova = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)", 
       title = "Distribution of Rt by CDC transmission level") +
  theme_bw()+
  stat_compare_means(method = "anova")+
  ylim(c(0,3.3))

ggsave("Fig/gAnova.jpg",
       gAnova, 
       height=6,
       width=8,
       scale=1.65)



# Emmeans plot same day  #### 
cont <-
  emmeans::emmeans(
    myfit
    , specs = "cdcTransmissionLevel"
  )

gEmmeans1 = plot(cont, comparisons = F)+
  theme_bw()+
  labs(title = "Rt estimated marginal means for each CDC risk level")

ggsave("Fig/gEmmeans1.jpg",
       gEmmeans1, 
       height=6,
       width=8,
       scale=1.65)



# Emmeans plot 3 weeks later #### 

myfit3weeks = aov(Rt3NextWeeks ~ cdcTransmissionLevel,
                  data=data)

# Contrasts to perform pairwise comparisons
cont3weeks <-
  emmeans::emmeans(
    myfit3weeks
    , specs = "cdcTransmissionLevel"
  )

gEmmeans2 = plot(cont3weeks, comparisons = F)+
  theme_bw()+
  labs(title = "Rt estimated marginal means for each CDC risk level 3 weeks later")
cont3weeks

ggsave("Fig/gEmmeans2.jpg",
       gEmmeans2, 
       height=6,
       width=8,
       scale=1.65)

kruskal.test(Rt3NextWeeks ~ cdcTransmissionLevel,
             data=data)




# boxplot kruskal same day ####

gbox1 = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7, outlier.shape = NA) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)" 
       ,title = "Distribution of Rt by CDC transmission level"
  ) +
  theme_bw()+
  stat_compare_means()+
  ylim(c(0.5,1.5))


ggsave("Fig/gbox1.jpg",
       gbox1, 
       height=6,
       width=8,
       scale=1.65)




# boxplot kruskal 3 weeks later ####

gbox2 = ggplot(data, aes(x = cdcTransmissionLevel, y = Rt3NextWeeks, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)" 
       ,title = "Distribution of Rt by CDC transmission level at 21 days later"
  ) +
  theme_bw()+
  stat_compare_means()+
  ylim(c(0,3.3))

ggsave("Fig/gbox2.jpg",
       gbox2, 
       height=6,
       width=8,
       scale=1.65)




# Facet box same day #### 

gFacet = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)" 
       ,title = "Distribution of Rt by CDC transmission level"
  ) +
  theme_bw()+
  ylim(c(0,3.8))+
  stat_compare_means()+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="wilcox.test"
  )+
  facet_wrap(. ~ UR_category,
             ncol=2)

ggsave("Fig/gFacet.jpg",
       gFacet, 
       height=18,
       width=6,
       scale=1.65)


# facet box 3 weeks later  ####

gFacet3 = ggplot(data, aes(x = cdcTransmissionLevel, y = Rt3NextWeeks, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)" 
       ,title = "Distribution of Rt by CDC transmission level at 21 days later"
  ) +
  theme_bw()+
  ylim(c(0,3.8))+
  stat_compare_means()+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="wilcox.test"
  )+
  facet_wrap(. ~ UR_category,
             ncol=2)

ggsave("Fig/gFacet3.jpg",
       gFacet3, 
       height=18,
       width=6,
       scale=1.65)


# table output ####
sampleSize = data %>%
  group_by(cdcTransmissionLevel,
           UR_category) %>%
  count() %>%
  rename("Counties" = n)

table_data <- data %>% 
  group_by(cdcTransmissionLevel,
           UR_category)%>%
  summarize(
    Rtmean = mean(mean_last_7_days, na.rm = T),
    Rtmedian = median(mean_last_7_days, na.rm = T),
    RtVariance= var(mean_last_7_days, na.rm = T),
    Rtmean3NextWeeks = mean(Rt3NextWeeks, na.rm = T),
    Rtmedian3NextWeeks = median(Rt3NextWeeks, na.rm = T),
    RtVariance3NextWeeks= var(Rt3NextWeeks, na.rm = T)
  )%>%
  left_join(sampleSize, by = c("cdcTransmissionLevel",
                               "UR_category"))%>%
  arrange(UR_category) %>%
  select(UR_category,
         "Risk Level" = cdcTransmissionLevel,
         "Observation" = Counties,
         "Mean Rt" = Rtmean,
         "Mean Rt (3 Weeks)" = Rtmean3NextWeeks,
         "Median Rt" = Rtmedian,
         "Median Rt (3 Weeks)" = Rtmedian3NextWeeks
  )

#kable(table_data,
#      caption = "Summary statistics by CDC transmission level",
#      align = "l")%>%
#  kableExtra::kable_styling(latex_options = "hold_position")


UR_category_list = unique(sampleSize$UR_category)

kbl(table_data,
    caption = "Summary statistics by CDC transmission level",
    booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  pack_rows(UR_category_list[1], 1, 4) %>%
  pack_rows(UR_category_list[2], 5, 8) %>%
  pack_rows(UR_category_list[3], 9, 12) %>%
  pack_rows(UR_category_list[4], 13, 16) %>%
  pack_rows(UR_category_list[5], 17, 20) %>%
  pack_rows(UR_category_list[6], 21, 24)


pdf("table1.pdf", width = 8.5, height = 11)
print(kbl_result)  # Replace `kbl_result` with the name of the variable containing the table
dev.off()


# AUC ####
county_list = county.NCHS %>%
  select(state,
         fips_code,
         UR_code,
         UR_category) %>%
  distinct() %>%
  mutate(
    fips_state = paste(state, fips_code, sep=",")
  ) %>% 
  select(fips_state,
         UR_code,
         UR_category)

UR1 = filter(county_list, UR_code == 1)$fips_state
UR2 = filter(county_list, UR_code == 2)$fips_state
UR3 = filter(county_list, UR_code == 3)$fips_state
UR4 = filter(county_list, UR_code == 4)$fips_state
UR5 = filter(county_list, UR_code == 5)$fips_state
UR6 = filter(county_list, UR_code == 6)$fips_state




merged_counties = merged_counties2 %>%
  mutate(
    truePositive = ifelse(expected_higher_rt == 1 & actual_higher_rt == 1, 1, 0),
    falsePositive = ifelse(expected_higher_rt == 1 & actual_higher_rt == 0, 1, 0),
    trueNegative = ifelse(expected_higher_rt == 0 & actual_higher_rt == 0, 1, 0),
    falseNegative = ifelse(expected_higher_rt == 0 & actual_higher_rt == 1, 1, 0)
  )%>%
  na.omit(c(truePositive, falsePositive, trueNegative, falseNegative)) %>%
  mutate(
    target_UR = case_when(
      stateFips %in% UR1 ~ 1,
      stateFips %in% UR2 ~ 2,
      stateFips %in% UR3 ~ 3,
      stateFips %in% UR4 ~ 4,
      stateFips %in% UR5 ~ 5,
      stateFips %in% UR6 ~ 6
    )
  )

apply(merged_counties[23:26], 2, sum) 
county1 = filter(merged_counties, UR_code == 1 & target_UR == 1)
county2 = filter(merged_counties, UR_code == 2 & target_UR == 2)
county3 = filter(merged_counties, UR_code == 3 & target_UR == 3)
county4 = filter(merged_counties, UR_code == 4 & target_UR == 4)
county5 = filter(merged_counties, UR_code == 5 & target_UR == 5)
county6 = filter(merged_counties, UR_code == 6 & target_UR == 6)

# Create the prediction object
pred_obj <- prediction(merged_counties$expected_higher_rt,
                       merged_counties$actual_higher_rt)

# Create the performance object
perf_obj <- performance(pred_obj, "tpr", "fpr")

# Calculate the AUROC
auroc <- performance(pred_obj, "auc")@y.values[[1]]

# Create the ROC curve data frame
roc_data <- data.frame(fpr = unlist(perf_obj@x.values),
                       tpr = unlist(perf_obj@y.values))

# Create the ROC curve plot with AUROC
gRoc <- ggplot(data = roc_data, aes(x = fpr, y = tpr)) +
  geom_path(color = "#377eb8", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "False Positive Rate", y = "True Positive Rate",
    title = "ROC Curve for Evaluating CDC Transmission Risk Level",
    subtitle = paste("AUROC:", round(auroc, 2))) +
  theme_bw() +
  annotate("text", x = 0.8,
           y = 0.2,
           label = paste("AUROC =", round(auroc, 2)), size = 4)

# Print the ROC curve plot
ggsave("Fig/gRoc.jpg",
       gRoc, 
       height=6,
       width=8,
       scale=1.65)


# AUC facet ####
# Create the prediction object 
pred_obj1 <- prediction(county1$expected_higher_rt,
                        county1$actual_higher_rt)

pred_obj2 <- prediction(county2$expected_higher_rt,
                        county2$actual_higher_rt)

pred_obj3 <- prediction(county3$expected_higher_rt,
                        county3$actual_higher_rt)

pred_obj4 <- prediction(county4$expected_higher_rt,
                        county4$actual_higher_rt)

pred_obj5 <- prediction(county5$expected_higher_rt,
                        county5$actual_higher_rt)

pred_obj6 <- prediction(county6$expected_higher_rt,
                        county6$actual_higher_rt)



# Create the performance object
perf_obj1 <- performance(pred_obj1, "tpr", "fpr")
perf_obj2 <- performance(pred_obj2, "tpr", "fpr")
perf_obj3 <- performance(pred_obj3, "tpr", "fpr")
perf_obj4 <- performance(pred_obj4, "tpr", "fpr")
perf_obj5 <- performance(pred_obj5, "tpr", "fpr")
perf_obj6 <- performance(pred_obj6, "tpr", "fpr")


# Calculate the AUROC
auroc1 <- performance(pred_obj1, "auc")@y.values[[1]]
auroc2 <- performance(pred_obj2, "auc")@y.values[[1]]
auroc3 <- performance(pred_obj3, "auc")@y.values[[1]]
auroc4 <- performance(pred_obj4, "auc")@y.values[[1]]
auroc5 <- performance(pred_obj5, "auc")@y.values[[1]]
auroc6 <- performance(pred_obj6, "auc")@y.values[[1]]



# Create the ROC curve data frame
roc_data1 <- data.frame(fpr = unlist(perf_obj1@x.values),
                        tpr = unlist(perf_obj1@y.values))%>%
  mutate(
    UR_Category = "Large central metro",
    auroc = auroc1
  )

roc_data2 <- data.frame(fpr = unlist(perf_obj2@x.values),
                        tpr = unlist(perf_obj2@y.values))%>%
  mutate(
    UR_Category = "Large fringe metro",
    auroc = auroc2
  )

roc_data3 <- data.frame(fpr = unlist(perf_obj3@x.values),
                        tpr = unlist(perf_obj3@y.values))%>%
  mutate(
    UR_Category = "Medium metro",
    auroc = auroc3
  )

roc_data4 <- data.frame(fpr = unlist(perf_obj4@x.values),
                        tpr = unlist(perf_obj4@y.values))%>%
  mutate(
    UR_Category = "Small metro",
    auroc = auroc4
  )

roc_data5 <- data.frame(fpr = unlist(perf_obj5@x.values),
                        tpr = unlist(perf_obj5@y.values))%>%
  mutate(
    UR_Category = "Micropolitan",
    auroc = auroc5
  )

roc_data6 <- data.frame(fpr = unlist(perf_obj6@x.values),
                        tpr = unlist(perf_obj6@y.values))%>%
  mutate(
    UR_Category = "Noncore",
    auroc = auroc6
  )
roc_data = rbind(
  roc_data1,
  roc_data2,
  roc_data3,
  roc_data4,
  roc_data5,
  roc_data6
)

# Create the ROC curve plot with AUROC
gRocF <- ggplot(data = roc_data, aes(x = fpr, y = tpr)) +
  geom_path(color = "#377eb8", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "False Positive Rate", y = "True Positive Rate",
    title = "ROC Curve for Evaluating CDC Transmission Risk Level") +
  theme_bw() +
  facet_wrap(. ~ UR_Category,
             ncol=2)+
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUROC =", round(auroc, 2))),
            size = 4, show.legend = FALSE, data = subset(roc_data, !is.na(auroc)))


# Print the ROC curve plot
ggsave("Fig/gRocF.jpg",
       gRocF, 
       height=6,
       width=8,
       scale=1.65)


# Confusion Matrix ####


cm = confusionMatrix(factor(merged_counties$expected_higher_rt),
                     factor(merged_counties$actual_higher_rt))

# Create the confusion matrix
confusion_matrix <- cm$table

# Convert the confusion matrix to a data frame
confusion_df <- as.data.frame(confusion_matrix)


# Calculate percentage for labels
confusion_df$Percent <- sprintf("%.1f%%", confusion_df$Freq / sum(confusion_df$Freq) * 100)


# Plot the confusion matrix using ggplot2
gCM = ggplot(confusion_df,
           aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  #geom_text(aes(label = Count), color = "black") +
  geom_text(aes(label = Percent), color = "black", size = 3) +
  scale_fill_gradient(low = "#fff7ec", high = "#ef6548") +
  scale_x_discrete(limits = rev(levels(confusion_df$Reference)),
                   position = "top") +  # Reverse x-axis
  
  labs(title = paste0("Confusion Matrix for all counties") ,
       x = "Actual",
       y = "Predicted") +
  theme_minimal()+
  guides(fill = FALSE)

ggsave("Fig/gCM.jpg",
       gCM, 
       height=6,
       width=8,
       scale=1.65)





# Confusion Matrix facet ####
cm1 = confusionMatrix(factor(county1$expected_higher_rt),
                      factor(county1$actual_higher_rt))

cm2 = confusionMatrix(factor(county2$expected_higher_rt),
                      factor(county2$actual_higher_rt))

cm3 = confusionMatrix(factor(county3$expected_higher_rt),
                      factor(county3$actual_higher_rt))

cm4 = confusionMatrix(factor(county4$expected_higher_rt),
                      factor(county4$actual_higher_rt))

cm5 = confusionMatrix(factor(county5$expected_higher_rt),
                      factor(county5$actual_higher_rt))

cm6 = confusionMatrix(factor(county6$expected_higher_rt),
                      factor(county6$actual_higher_rt))



# Create the confusion matrix
confusion_df1 <- as.data.frame(cm1$table) %>%
  mutate(
    UR_Category = "Large central metro",
    Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
    per_num = Freq / sum(Freq) * 100
  )

confusion_df2 <- as.data.frame(cm2$table) %>%
  mutate(
    UR_Category = "Large fringe metro",
    Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
    per_num = Freq / sum(Freq) * 100
  )

confusion_df3 <- as.data.frame(cm3$table) %>%
  mutate(
    UR_Category = "Medium metro",
    Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
    per_num = Freq / sum(Freq) * 100
  )

confusion_df4 <- as.data.frame(cm4$table) %>%
  mutate(
    UR_Category = "Small metro",
    Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
    per_num = Freq / sum(Freq) * 100
  )

confusion_df5 <- as.data.frame(cm5$table) %>%
  mutate(
    UR_Category = "Micropolitan",
    Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
    per_num = Freq / sum(Freq) * 100
  )

confusion_df6 <- as.data.frame(cm6$table) %>%
  mutate(
    UR_Category = "Noncore",
    Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
    per_num = Freq / sum(Freq) * 100
  )


confusion_df = rbind(confusion_df1,
                     confusion_df2,
                     confusion_df3,
                     confusion_df4,
                     confusion_df5,
                     confusion_df6)



# Plot the confusion matrix using ggplot2
gCMF = ggplot(confusion_df,
       aes(x = Reference, y = Prediction, fill = per_num)) +
  geom_tile() +
  #geom_text(aes(label = Count), color = "black") +
  geom_text(aes(label = Percent), color = "black", size = 3) +
  scale_fill_gradient(low = "#fff7ec",
                      high = "#ef6548") +
  scale_x_discrete(limits = rev(levels(confusion_df$Reference)),
                   position = "top") +  # Reverse x-axis
  
  labs(
    #title = paste0("Confusion Matrix for", UR Category, " counties") ,
    x = "Actual",
    y = "Predicted") +
  theme_minimal()+
  guides(fill = FALSE)+
  facet_wrap(. ~ UR_Category,
             ncol=2)


ggsave("Fig/gCMF.jpg",
       gCMF, 
       height=6,
       width=8,
       scale=1.65)