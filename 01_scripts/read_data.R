# Read corona virus data from the source:
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

read_data <- function() {
  
  # read xls data
  data_res <- as.data.table(read_xlsx("02_data/COVID-19-geographic-disbtribution-worldwide-2020-03-19.xlsx"))
  # str(data_res)
  
  setnames(data_res, "Countries and territories", "Country")
  
  # data_res[, unique(Country)]
  
  # Interpolate missing dates between min-max date of a Country = add 0s
  data_intrapolated <- rbindlist(lapply(data_res[, unique(Country)],
                                        function(i) 
                                          padr::pad(data_res[.(i), on = .(Country)],
                                                    by = "DateRep",
                                                    interval = "day",
                                                    end_val = data_res[.(i), on = .(Country),
                                                                       max(DateRep)]
                                                    )[is.na(Cases),
                                                      (c("Cases", "Deaths", "Country", "GeoId")) :=
                                                        .(0, 0, i, data_res[.(i),
                                                                            on = .(Country),
                                                                            unique(GeoId)[1]]
                                                          )
                                                      ]
                                        )
                                )
  
  data_intrapolated[, DateRep := lubridate::date(DateRep)]
  
  # Compute cumulative sum of Cases and Deaths by Country
  setorder(data_intrapolated, Country, DateRep)
  data_intrapolated[, Cases_cumsum := cumsum(Cases),
                    by = .(Country)]
  data_intrapolated[, Deaths_cumsum := cumsum(Deaths),
                    by = .(Country)]

  # data_intrapolated[.("United_States_of_America"), on = .(Country)]
  
  return(data_intrapolated)
  
}
