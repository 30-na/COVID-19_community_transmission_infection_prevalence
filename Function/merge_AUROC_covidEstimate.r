folder_path <- "ProcessedData/outputsample_covidEstim"

# get a list of all the RDA files in the folder
file_list <- list.files(path = folder_path,
                        pattern = "\\.RDA$")

# initialize an empty data frame to store the merged data
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

merged_counties = data.frame(matrix(nrow = 0,
                                     ncol=length(columnNames)))

colnames(merged_counties) = columnNames


# loop through each file, load the data, and merge it with the existing data
for (file in file_list) {
  load(file.path(folder_path, file))
  # merge the new data with the existing data
  print(file)
  merged_counties <- rbind(merged_counties,
                           compared_counties
                           )
}

save(merged_counties
     ,file = "ProcessedData/AUROC_merged_Estim.RDA")
