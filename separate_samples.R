source('util/csv_read.R')

print_summary <- FALSE

accidents <- load_data('data/acidentes-2015_reordenado.csv')
accident_types <- accidents[, 11]
ascii_accident_types <- iconv(accident_types, from="UTF-8", to="ASCII", "")

size_accidents <- dim(accidents)[1]

set_types <- unique(ascii_accident_types)

motorcycle_data <- c()
car_data <- c()

for(i in 1:size_accidents){
  current_type <- ascii_accident_types[i]
  
  new_data <- c(accidents[i, 1], accidents[i, 2], accidents[i, 11])
  
  if(grepl("Moto", current_type) || grepl("Ciclo", current_type)){
    motorcycle_data <- c(motorcycle_data, new_data)
  }else if(grepl("Auto", current_type)){
    car_data <- c(car_data, new_data)
  }
}

motorcycle_data <- matrix(motorcycle_data, ncol=3, byrow=TRUE)
colnames(motorcycle_data) <- c("longitude", "latitude", "type")

car_data <- matrix(car_data, ncol=3, byrow=TRUE)
colnames(car_data) <- c("longitude", "latitude", "type")

num_motorcycles = dim(motorcycle_data)[1]
num_cars = dim(car_data)[1]

if(print_summary){
  print(paste("Todos os tipos:", toString(set_types)))
  print(paste("Numero de acidentes c/ motos", toString(num_motorcycles)))
  print(paste("Numero de acidentes c/ carros", toString(num_cars)))
}