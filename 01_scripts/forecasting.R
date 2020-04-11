# Forecast Cases cumulative -----

forec_cases_cumsum <- function(data, n_ahead) {
  
  data_ts <- ts(data[Cases_cumsum != 0, Cases_cumsum])
  
  # If length of ts is too low, then replicate first value
  if (length(data_ts) < 6) {
    
    data_ts <- ts(c(rep(data[Cases_cumsum != 0, Cases_cumsum][1], 6 - length(data_ts)),
                    data_ts))
    
  }
  
  pred <- es(data_ts,
             model = c("MMN", "MMdN", "MAdN", "AMdN"), # multiple choices of ETS model
             ic = "BICc", # Information Criterion
             h = n_ahead,
             loss = "MSE",
             interval = "parametric",
             level = 0.90,
             silent = "all"
             )
  
  # Handle cumulative ts, have to be increasing all the time
  if (pred$forecast[1] < data_ts[length(data_ts)]) {
    
    print("using only cases!")
    
    data_ts <- ts(data[Cases != 0, Cases])
    
    
    if (length(data_ts) < 6) {
      
      data_ts <- ts(c(rep(data[Cases != 0, Cases][1], 6 - length(data_ts)),
                      data_ts))
      
    }
    
    pred <- es(data_ts,
               model = c("MMN", "MMdN", "MAdN", "AMN", "AMdN"), # multiple choices of ETS model
               ic = "BICc",
               h = n_ahead,
               loss = "MSE",
               interval = "parametric",
               level = 0.90,
               silent = "all"
               )
    
    # Heuristic for Mean + PI forecasts
    pred$forecast <- cumsum(pred$forecast) + data[.N, Cases_cumsum]
    pred$upper <- cumsum(pred$forecast) + data[.N, Cases_cumsum]
    pred$upper <- pred$upper + pred$upper*0.1
    
  }
  
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
