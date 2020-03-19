# Aggregate data ----

aggregate_data <- function(data) {
  
  
  data_agg <- copy(data[,
                        .(Cases = sum(Cases, na.rm = T),
                          Deaths = sum(Deaths, na.rm = T),
                          Cases_cumsum = sum(Cases_cumsum, na.rm = T),
                          Deaths_cumsum = sum(Deaths_cumsum, na.rm = T),
                          Country = "World"
                          ),
                        by = .(DateRep)])
  
  return(data_agg)
  
}
