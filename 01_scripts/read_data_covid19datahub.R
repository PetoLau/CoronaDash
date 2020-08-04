# install.packages("COVID19")

read_data_covid19datahub <- function() {
  
  x <- covid19(verbose = FALSE)
  
  x <- as.data.table(x)
  
  x[.("United States"), on = .(administrative_area_level_1), administrative_area_level_1 := "US"]
  
  setnames(x, "administrative_area_level_1", "Country")
  
  x_max <- copy(x[, .SD[date == max(date)], by = .(Country)])
  
  setnames(x_max, "tests", "TotalTests")
  x_max[, Tests_1M_Pop := ceiling((TotalTests / population) * 1e6)]
  
  return(x_max[!is.na(TotalTests),
               .(Country, TotalTests, Tests_1M_Pop)])
  
}
