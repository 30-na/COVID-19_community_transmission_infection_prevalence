library(EpiEstim)
library(data.table)
library(dplyr)
library(usdata)


fileName = "United_States_COVID-19_Cases_and_Deaths_by_State_over_Time_-_ARCHIVED.csv"
dest = "RawData"
data.cdc = fread(paste(dest, fileName, sep="/"))
data.case = data.cdc %>%
  dplyr::select(
    "date" = submission_date
    , state
    # Number of new cases
    , "case" = new_case
  ) %>%
  mutate(
    state = abbr2state(state)
    , date = as.Date(date, format="%m/%d/%Y")
  ) %>%
  dplyr::filter(
    case >= 0
    ) %>%
  na.omit()
summary(data.case)

# list of states
state.list = unique(data.case$state)

columns = c("date", "state", "t_start", "t_end", "Mean(R)", "Std(R)",
    "Quantile.0.025(R)", "Quantile.0.05(R)", "Quantile.0.25(R)",
    "Median(R)", "Quantile.0.75(R)", "Quantile.0.95(R)", "Quantile.0.975(R)")

rt.data = data.frame(matrix(nrow = 0, ncol = length(columns)))   
colnames(rt.data) = columns
  
for (s in state.list) {
  state_data <- subset(data.case
                       , state == s)
  
  state_data = state_data[order(state_data$date), ] # sort by date
  
  est = estimate_R(
    state_data$case
    , method = "parametric_si"
    , config = make_config(list(mean_si=4.7, std_si=2.9))
    )
  
  state.rt = cbind(
    date = state_data$date[-(1:7)]
    ,state = s
    ,est$R
  )
  
  
  rt.data <- rbind(
    rt.data
    , state.rt
    )
}

columns = c("date", "state", "t_start", "t_end", "Mean", "Std",
            "Quantile.0.025", "Quantile.0.05", "Quantile.0.25",
            "Median", "Quantile.0.75", "Quantile.0.95", "Quantile.0.975")

colnames(rt.data) = columns

save(rt.data
     ,file = "ProcessedData/rt.data.RDA")
# Visualize the estimated R_t values

# window_size parameter controls the length of the sliding windows, 
# while the window_step parameter controls the amount by which the windows
# are shifted over time. The optimal values for these parameters depend on
# the specific characteristics of the data and the objectives of the analysis.

