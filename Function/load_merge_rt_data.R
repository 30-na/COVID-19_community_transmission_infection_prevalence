library(dplyr)


folder_path <- "ProcessedData/RtCounties"

# get a list of all the RDA files in the folder
file_list <- list.files(path = folder_path,
                        pattern = "\\.rda$")

# initialize an empty data frame to store the merged data
merged_data <- data.frame(id = character(), date = character(), state = character(),
                          fips_code = numeric(), county = character(), Rt = numeric())

# loop through each file, load the data, and merge it with the existing data
for (file in file_list) {
  # load the data from the file
  load(file.path(folder_path, file))
  
  # extract the information from the file name
  file_info <- strsplit(file, "_")
  id <- file_info[[1]][1]
  state <- file_info[[1]][2]
  fipscode <- as.numeric(file_info[[1]][3])
  county <- gsub("\\.rda", "", file_info[[1]][4])
  
  # create a data frame with the extracted information and the rt_mean data
  nrow <- nrow(rt.df)
  new_data <- data.frame(date = as.Date(rt.df$date,
                                        format="%y-%m-%d"),
                         id = rep(id, nrow),
                         state = rep(state, nrow),
                         fips_code = rep(as.numeric(fipscode), nrow),
                         county_name = rep(county, nrow),
                         Rt = rt.df$rt.mean)
  
  # merge the new data with the existing data
  merged_data <- rbind(merged_data, new_data)
}


# load data
load("ProcessedData/counties.transmission.newCase.RDA")


## merge data
rt_counties_TranRisk = merged_data %>%
  dplyr::left_join(counties.transmission.newCase,
                   by=c("date", "state", "fips_code", "county_name")) %>%
  # add the Rt number for three weeks later
  # Note we assume the we have data for all of the day
  dplyr::group_by(state, county_name)%>%
  dplyr::mutate(
    Rt_next3weeks = lead(Rt, n = 21L)
    )



save(rt_counties_TranRisk
     ,file = "ProcessedData/rt_counties_TranRisk.RDA")














