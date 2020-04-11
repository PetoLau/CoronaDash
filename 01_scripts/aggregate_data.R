# Aggregate data for the "whole" World ----
# "whole" - all available countries

aggregate_data <- function(data) {
  
  
  data_agg <- copy(data[,
                        .(
                          Country = "World",
                          Cases = sum(Cases, na.rm = T),
                          Deaths = sum(Deaths, na.rm = T),
                          Cases_cumsum = sum(Cases_cumsum, na.rm = T),
                          Deaths_cumsum = sum(Deaths_cumsum, na.rm = T),
                          Recovered_cumsum = sum(Recovered_cumsum, na.rm = T),
                          Active_cases_cumsum = sum(Active_cases_cumsum, na.rm = T),
                          Population = sum(Population, na.rm = T)
                          ),
                        by = .(DateRep)])
  
  return(data_agg)
  
}
