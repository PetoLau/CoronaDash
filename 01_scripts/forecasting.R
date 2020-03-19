# Forecast Cases cumulative -----

forec_cases_cumsum <- function(data, n_ahead) {
  
  data_ts <- ts(data[Cases_cumsum != 0, Cases_cumsum])
  
  
  if (length(data_ts) < 6) {
    
    data_ts <- ts(c(rep(data[Cases_cumsum != 0, Cases_cumsum][1], 6 - length(data_ts)),
                    data_ts))
    
  }
  
  pred <- es(data_ts,
             model = c("MMN", "MMdN"),
             ic = "AICc",
             h = n_ahead,
             loss = "MSE",
             interval = "parametric",
             level = 0.90,
             silent = "all"
             )
  
  return(pred)
  
}

# Forecast Deaths cumulative -----

forec_deaths_cumsum <- function(data, n_ahead) {
  
  data_ts <- ts(data[Deaths_cumsum != 0, Deaths_cumsum])
  
  if (length(data_ts) < 6) {
    
    data_ts <- ts(c(rep(data[Deaths_cumsum != 0, Deaths_cumsum][1], 6 - length(data_ts)),
                    data_ts))
    
  }
  
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
