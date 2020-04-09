# Read data from JHU source CSSE ARCGIS -----
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

source("01_scripts/read_data_worldometer.R")

read_data_cases <- function() {
  
  # data_confirmed <- fread("02_data/time_series_19-covid-Confirmed.csv")
  data_confirmed <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  setnames(data_confirmed, colnames(data_confirmed)[2], "Country")

  data_confirmed_melt <- melt(data_confirmed,
                              id.vars = c(2),
                              measure.vars = 5:length(colnames(data_confirmed)),
                              variable.factor = FALSE,
                              variable.name = "DateRep",
                              value.name = "Cases_cumsum"
                              )
  
  data_confirmed_melt[, DateRep := lubridate::mdy(DateRep)]
  
  data_confirmed_melt_agg <- copy(data_confirmed_melt[,
                                                      .(Cases_cumsum = sum(Cases_cumsum, na.rm = TRUE)),
                                                      by = .(Country, DateRep)])
  
  # New Cases per day 
  setorder(data_confirmed_melt_agg, Country, DateRep)
  data_confirmed_melt_agg[, Cases := c(.SD[1, Cases_cumsum],
                                       diff(Cases_cumsum,
                                            lag = 1,
                                            differences = 1)
                                       ),
                          by = .(Country)]
  
  data_confirmed_melt_agg[Cases < 0, Cases := 0]
  
  # data_confirmed_melt_agg[.("Slovakia"), on = .(Country)]
  # data_confirmed_melt_agg[.("Switzerland"), on = .(Country)]
  
  return(data_confirmed_melt_agg)
  
}

read_data_deaths <- function() {
  
  # data_deaths <- fread("02_data/time_series_19-covid-Deaths.csv")
  data_deaths <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  
  setnames(data_deaths, colnames(data_deaths)[2], "Country")
  
  data_deaths_melt <- melt(data_deaths,
                           id.vars = c(2),
                           measure.vars = 5:length(colnames(data_deaths)),
                           variable.factor = FALSE,
                           variable.name = "DateRep",
                           value.name = "Deaths_cumsum"
                           )
  
  data_deaths_melt[, DateRep := lubridate::mdy(DateRep)]
  
  data_deaths_melt_agg <- copy(data_deaths_melt[,
                                                .(Deaths_cumsum = sum(Deaths_cumsum, na.rm = TRUE)),
                                                by = .(Country, DateRep)])
  
  # New Cases per day 
  setorder(data_deaths_melt_agg, Country, DateRep)
  data_deaths_melt_agg[, Deaths := c(.SD[1, Deaths_cumsum],
                                      diff(Deaths_cumsum,
                                      lag = 1,
                                      differences = 1)
                                     ),
                       by = .(Country)]
  
  data_deaths_melt_agg[Deaths < 0, Deaths := 0]
  
  # data_deaths_melt_agg[.("Slovakia"), on = .(Country)]
  # data_deaths_melt_agg[.("Switzerland"), on = .(Country)]
  
  return(data_deaths_melt_agg)
  
}

read_data_recovered <- function() {
  
  # data_recovered <- fread("02_data/time_series_19-covid-Recovered.csv")
  # data_recovered <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
  data_recovered <- fread("https://github.com/ulklc/covid19-timeseries/raw/master/countryReport/raw/rawReport.csv")
  
  setnames(data_recovered, colnames(data_recovered)[3], "Country")
  data_recovered[, (c("confirmed", "death", "region", "countryCode")) := NULL]
  
  setnames(data_recovered, colnames(data_recovered)[c(1,5)], c("DateRep", "Recovered_cumsum"))
  data_recovered[, DateRep := lubridate::ymd(DateRep)]
  
  data_recovered[.("United States"), on = .(Country), Country := "US"]
  data_recovered[.("South Korea"), on = .(Country), Country := "Korea, South"]
  data_recovered[.("Republic of the Congo"), on = .(Country), Country := "Congo (Brazzaville)"]
  data_recovered[.("DR Congo"), on = .(Country), Country := "Congo (Kinshasa)"]
  data_recovered[.("Taiwan"), on = .(Country), Country := "Taiwan*"]
  data_recovered[.("Cape Verde"), on = .(Country), Country := "Cabo Verde"]
  data_recovered[.("Vatican City"), on = .(Country), Country := "Holy See"]
  data_recovered[.("Ivory Coast"), on = .(Country), Country := "Cote d'Ivoire"]
  
  # data_recovered[, unique(Country)]
  
  # New Cases per day
  setorder(data_recovered, Country, DateRep)
  data_recovered[, Recovered := c(.SD[1, Recovered_cumsum],
                                  diff(Recovered_cumsum,
                                       lag = 1,
                                       differences = 1)
                                  ),
                 by = .(Country)]

  data_recovered[Recovered < 0, Recovered := 0]
  
  # data_recovered_melt <- melt(data_recovered,
  #                             id.vars = c(2),
  #                             measure.vars = 5:length(colnames(data_recovered)),
  #                             variable.factor = FALSE,
  #                             variable.name = "DateRep",
  #                             value.name = "Recovered_cumsum",
  #                             na.rm = TRUE
  #                             )
  # 
  # data_recovered_melt[, DateRep := lubridate::mdy(DateRep)]
  # 
  # data_recovered_melt_agg <- copy(data_recovered_melt[,
  #                                                     .(Recovered_cumsum = sum(Recovered_cumsum, na.rm = TRUE)),
  #                                                     by = .(Country, DateRep)])
  # 
  # # New Cases per day 
  # setorder(data_recovered_melt_agg, Country, DateRep)
  # data_recovered_melt_agg[, Recovered := c(.SD[1, Recovered_cumsum],
  #                                    diff(Recovered_cumsum,
  #                                         lag = 1,
  #                                         differences = 1)
  #                                    ),
  #                         by = .(Country)]
  # 
  # data_recovered_melt_agg[Recovered < 0, Recovered := 0]
  
  # data_recovered_melt_agg[.("Slovakia"), on = .(Country)]
  # data_recovered_melt_agg[.("Switzerland"), on = .(Country)]
  
  return(data_recovered)
  
}

join_all_corona_data <- function() {
  
  data_all <- copy(read_data_cases())
  data_deaths <- copy(read_data_deaths())
  data_recov <- copy(read_data_recovered())
  data_tests <- copy(read_data_worldometer())
  
  # join deaths
  data_all[data_deaths,
           on = .(Country, DateRep),
           (c("Deaths_cumsum", "Deaths")) := .(i.Deaths_cumsum, i.Deaths)]
  
  
  data_all[.("Burma"), on = .(Country), Country := "Myanmar"]
  data_all[.("West Bank and Gaza"), on = .(Country), Country := "Palestine"]
  
  # join recovered
  data_all[data_recov,
           on = .(Country, DateRep),
           (c("Recovered_cumsum", "Recovered",
              "lat", "lon")) := .(i.Recovered_cumsum, i.Recovered,
                                  i.lat, i.lon)]
  
  # join tests
  data_all[data_tests,
           on = .(Country),
           (c("TotalTests", "Tests_1M_Pop")) := 
             .(i.TotalTests, i.Tests_1M_Pop
               )]
  
  # compute active cases cumsum
  data_all[, Active_cases_cumsum := Cases_cumsum - Deaths_cumsum - Recovered_cumsum]
  
  data_all[is.na(Recovered_cumsum), Recovered_cumsum := 0]
  data_all[is.na(Recovered), Recovered := 0]
  data_all[is.na(Active_cases_cumsum), Active_cases_cumsum := 0]
  
  # data_all[.("Slovakia"), on = .(Country)]
  # data_all[.("Italy"), on = .(Country)]
  
  return(data_all)
  
}
