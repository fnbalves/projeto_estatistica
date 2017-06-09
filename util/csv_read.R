load_data <- function(file_name){
  data <- read.csv(file_name, sep=",", row.names=1)
  data_matrix <- as.matrix(data)
  return(data_matrix)
}