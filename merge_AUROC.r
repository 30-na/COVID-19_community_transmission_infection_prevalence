folder_path <- "ProcessedData/outputsample"

# get a list of all the RDA files in the folder
file_list <- list.files(path = folder_path,
                        pattern = "\\.RDA$")

# initialize an empty data frame to store the merged data
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

merged_counties = data.frame(matrix(nrow = 0,
                                      ncol=length(columnNames)))
colnames(merged_counties) = columnNames


# loop through each file, load the data, and merge it with the existing data
for (file in file_list[1:1000]) {
  load(file.path(folder_path, file))
  # merge the new data with the existing data
  print(file)
  merged_counties <- rbind(merged_counties,
                           compared_counties)
}


save(merged_counties
     ,file = "ProcessedData/AUROC_merged.RDA")
