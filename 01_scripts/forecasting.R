# Forecast Cases cumulative -----

forec_cases_cumsum <- function(data, n_ahead) {
  
  data_ts <- ts(data[Cases_cumsum != 0, Cases_cumsum])
  
  pred <- es(data_ts,
             model = c("MMN", "MMdN"),
             ic = "AICc",
             h = n_ahead,
             loss = "MSE",
             interval = "parametric",
             level = 0.90,
             silent = "all")
  
  return(pred)
  
}

# Forecast Deaths cumulative -----

forec_deaths_cumsum <- function(data, n_ahead) {
  
  data_ts <- ts(data[, Deaths_cumsum])
  
  pred <- es(data_ts,
             model = c("AAN", "AAdN", "MMN", "MMdN"),
             ic = "AICc",
             h = n_ahead,
             loss = "MSE",
             interval = "parametric",
             level = 0.90,
             silent = "all")
  
  return(pred)
  
}
