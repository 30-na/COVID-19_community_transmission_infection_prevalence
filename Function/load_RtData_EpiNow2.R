# packages
library(data.table)
library(EpiNow2)
library(covidregionaldata)

country <- "United States"

generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani", max = 10, fixed = TRUE
)

incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")




reporting_delay <- list(
  mean = convert_to_logmean(4, 1)
  ,mean_sd = 0.1
  ,sd = convert_to_logsd(4, 1)
  ,sd_sd = 0.1
  ,max = 15)
reporting_delay <- list(
  mean = convert_to_logmean(2, 1)
  , sd = convert_to_logsd(2, 1)
  , max = 10,
  dist = "lognormal"
)



reported_cases <- covidregionaldata::get_national_data(
  country
  , source = "who"
  )

reported_cases1 <- covidregionaldata::get_national_data(
  "United States"
  , source = "who"
)


reported_cases <- data.table::setDT(reported_cases)
reported_cases <- reported_cases[, .(date, confirm = cases_new)]

reported_cases <- reported_cases[date >= (max(date) - 12*7)]



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
              target_folder = "results",
              logs = file.path("logs", Sys.Date()),
              return_output = TRUE, 
              verbose = TRUE)


plot(out)













