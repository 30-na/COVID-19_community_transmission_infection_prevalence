if (!require(EpiNow2)) {
  remotes::install_github("epiforecasts/EpiNow2", dependencies = TRUE)
}else {
  print("EpiNow2 is allready installed")
}

# load library
library(dplyr)
library(EpiNow2)
library(data.table)


#load file
load("ProcessedData/counties.transmission.newCase.RDA")
head(counties.transmission.newCase)
# 1. define Targets
target.NCHS = "Large central metro"
names(counties.transmission.newCase)
# set number of cores to use fitting the model
options(mc.cores = 4)
# no benefit on runtime if cores > chains which is set to 4 by default

# literature distributions 
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani", fixed = TRUE)
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer", fixed = TRUE)

# define reporting delay as lognormal with mean of 4 days and sd of 1 day in absence of
# evidence. If data on onset -> report then can use estimate_delay to estimate the delay
reporting_delay <- list(mean = convert_to_logmean(4, 1),
                        mean_sd = 0.1,
                        sd = convert_to_logsd(4, 1),
                        sd_sd = 0.1,
                        max = 15)

# filter list of Large Centeral Metro Counties
LCM_counties = counties.transmission.newCase %>%
  filter(
    UR_category == target.NCHS
  ) %>%
  mutate(
    state_fips = paste(state, fips_code, county_name, sep="_")
  )
names(LCM_counties)




target.counties.list = unique(LCM_counties$state_fips)

# initial dataframe
#original.columns = c("date", "rt.mean", "county")
#LCM_counties_rt = data.frame(matrix(nrow = 0, ncol = length(original.columns)))   
#colnames(LCM_counties_rt) = LCM_counties_rt


for(i in 1:length(target.counties.list)){
  # set target county
  target.county = target.counties.list[i]
  
  # set confirm case for the target county
  reported_cases = LCM_counties %>%
    dplyr::filter(
      state_fips == target.county
    ) %>%
    dplyr::select(
      date,
      confirm
    )%>%
    dplyr::arrange(
      date
    )
  
  # run epinow
  out <- epinow(reported_cases = reported_cases, 
                generation_time = generation_time,
                delays = delay_opts(incubation_period, reporting_delay),
                rt = rt_opts(prior = list(mean = 1.5, sd = 0.5)),
                # here we define the quality of the gaussian process approximation
                # if the fit to data appears poor try increasing basis_prop and
                # potentially the boundary_scale (see ?gp_opts for details)
                # though this will likely increase runtimes.
                gp = gp_opts(basis_prop = 0.2),
                # in some instances stan chains can get stuck when estimating in 
                # these instances consider using the future fitting mode by passing 
                # `future = TRUE, max_execution_time = 60 * 30` to stan_opts and calling 
                # `future::plan("multiprocess")` prior to running epinow this will time out
                # chains after 30 minutes but still return results from completed chains
                stan = stan_opts(),
                horizon = 14, 
                #target_folder = "results",
                #logs = file.path("logs", Sys.Date()),
                #return_output = TRUE, 
                verbose = TRUE)
  
  # make a data frame
  r = (out[["estimates"]][["summarised"]]$variable) == "R"
  rt.mean = out[["estimates"]][["summarised"]]$mean[r]
  date = out[["estimates"]][["summarised"]]$date[r]
  
  rt.df = data.frame(
    date = as.Date(date, "%Y-%m-%d"),
    rt.mean = rt.mean,
    county = target.counties.list[i]
  )
  
  # merged all data
  #LCM_counties_rt = rbind(
  #    LCM_counties_rt,
  #    rt.df
  #  )
  
  # save file to github
  save(rt.df, file = paste0("ProcessedData/RtCountiesNCHS/",i, "_",  target.counties.list[i], "_rt.rda" ))
}
