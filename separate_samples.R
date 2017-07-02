source('util/csv_read.R')

print_summary <- FALSE

accidents <- load_data('data/acidentes-2016.csv')
accident_types <- accidents[, 11]
accident_descriptions <- accidents[, 10]

ascii_accident_types <- iconv(accident_types, from="UTF-8", to="ASCII", "")
ascii_accident_descriptions <- iconv(accident_descriptions, from="UTF-8", to="ASCII", "")

size_accidents <- dim(accidents)[1]

set_types <- unique(ascii_accident_types)

motorcycle_data <- c()
others_data <- c()

for(i in 1:size_accidents){
  current_type <- ascii_accident_types[i]
  current_description <- ascii_accident_descriptions[i]
  
  new_data <- c(accidents[i, 1], accidents[i, 2], accidents[i, 11])
  
  if(grepl("Moto", current_type) || 
     grepl("Ciclo", current_type) || 
     grepl("moto", current_description, ignore.case = TRUE) || 
     grepl("CINQUENT", current_description, ignore.case = TRUE) || 
     grepl("50", current_description, ignore.case = TRUE)){
    
    motorcycle_data <- c(motorcycle_data, new_data)
  } else {
    others_data <- c(others_data, new_data)
  }
}

motorcycle_data <- matrix(motorcycle_data, ncol=3, byrow=TRUE)
colnames(motorcycle_data) <- c("longitude", "latitude", "type")

others_data <- matrix(others_data, ncol=3, byrow=TRUE)
colnames(others_data) <- c("longitude", "latitude", "type")

num_motorcycles = dim(motorcycle_data)[1]
num_others = dim(others_data)[1]

if(print_summary){
  print(paste("Todos os tipos:", toString(set_types)))
  print(paste("Numero de acidentes c/ motos", toString(num_motorcycles)))
  print(paste("Numero de acidentes s/ motos", toString(num_others)))
}