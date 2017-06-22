load_data <- function(file_name){
  data <- read.csv(file_name, sep=";", header = TRUE)
  data_matrix <- as.matrix(data)
  return(data_matrix)
}

make_hist <- function(data, column_name){
  barplot(table(data[, column_name]))
}