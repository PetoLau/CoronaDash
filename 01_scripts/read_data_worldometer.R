read_data_worldometer <- function() {
  
  # read and preprocess data from API: https://github.com/ChrisMichaelPerezSantiago/covid19
  data_res <- httr::GET(url = "https://covid19-server.chrismichael.now.sh/api/v1/AllReports")
  
  data_res_j <- jsonlite::fromJSON(rawToChar(data_res$content))
  
  data_res_dt <- as.data.table(data_res_j$reports$table[[1]][[2]])
  
  # string to integer
  data_res_dt[, TotalTests := as.integer(gsub(pattern = ",", replacement = "", TotalTests))]
  data_res_dt[, Tests_1M_Pop := as.integer(gsub(pattern = ",", replacement = "", Tests_1M_Pop))]
  
  # Countries labels
  data_res_dt[.("USA"), on = .(Country), Country := "US"]
  data_res_dt[.("UK"), on = .(Country), Country := "United Kingdom"]
  data_res_dt[.("S. Korea"), on = .(Country), Country := "Korea, South"]
  data_res_dt[.("UAE"), on = .(Country), Country := "United Arab Emirates"]
  data_res_dt[.("Taiwan"), on = .(Country), Country := "Taiwan*"]
  
  return(data_res_dt[!is.na(TotalTests),
                     .(Country, TotalTests, Tests_1M_Pop)])

}
