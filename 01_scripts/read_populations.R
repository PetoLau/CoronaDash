read_populations <- function() {
  
  data_pop <- fread("02_data/country_population.csv")
  
  return(data_pop)
  
}
