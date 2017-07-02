source('util/csv_read.R')
source('util/distance_calculation.R')

#config
test_to_run <- 'num_equipements' #'num_equipements' #'hist_dist' #'worst_equipement'

accidents <- load_data('data/acidentes-2015_reordenado.csv')    

equipements <- load_data('data/semaforos.csv')

print_progress <- function(i, size, title){
  str_to_print <- title
  str_to_print <- paste(str_to_print, toString(i))
  str_to_print = paste(str_to_print, "de")
  str_to_print = paste(str_to_print, toString(size))
  str_to_print = paste(str_to_print, "-------")
  cat("\r", str_to_print)
}

size_equipements <- dim(equipements)[1]
size_accidents <- dim(accidents)[1]
distances <- 1:size_accidents

make_dist_statistics <- function(){
  accidents_long <- as.numeric(accidents[, 1])
  accidents_lat <- as.numeric(accidents[, 2])
  
  for(i in 1:size_accidents){
    print_progress(i, size_accidents, "[histograma de distancias]")
    
    current_long <- accidents_long[i]
    current_lat <- accidents_lat[i]
    closest_distance <- get_closest_equipement(current_lat, current_long, equipements)[1]
    distances[i] <- closest_distance
  }
  
  return(distances)
}

make_equipements_nearby_statistics <- function(threshold){
  source('separate_samples.R')
  
  size_motos <- dim(motorcycle_data)[1]
  size_cars <- dim(car_data)[1]
  
  num_less_motos <- 1:size_motos
  num_less_cars <- 1:size_cars
  
  all_data <- 
  for(i in 1:size_motos){
    print_progress(i, size_motos, "[sinais proximos de acidentes c/ moto]")
    
    current_long <- as.numeric(motorcycle_data[i,1])
    current_lat <- as.numeric(motorcycle_data[i,2])
    num_less <- get_num_less_than(current_lat, current_long, threshold, equipements)
    num_less_motos[i] <- num_less
  }
  
  for(i in 1:size_cars){
    print_progress(i, size_cars, "[sinais proximos de acidentes c/ carro]")
    
    current_long <- as.numeric(car_data[i,1])
    current_lat <- as.numeric(car_data[i,2])
    num_less <- get_num_less_than(current_lat, current_long, threshold, equipements)
    num_less_cars[i] <- num_less
  }
  
  return(list(cars=num_less_cars, motos=num_less_motos))
}

discover_worst_equipement <- function(accident_data){
  num_accidents <- matrix(, nrow=size_equipements, ncol = 3)
  
  equipements_latitudes <- as.numeric(equipements[, 8])
  equipements_longitudes <- as.numeric(equipements[, 9])
  
  best_max <- -1
  best_longitude <- -1
  best_latitude <- -1
  
  for(i in 1:size_equipements){
    print_progress(i, size_equipements, '[pior sinal]')
    
    current_lat <- equipements_latitudes[i]
    current_long <- equipements_longitudes[i]
    
    num_less <- get_num_accidents_nearby(current_lat, current_long, accident_data, 0.1)
    num_accidents[i, ] <- c(num_less, current_lat, current_long)
    
    if(num_less > best_max){
      best_max <- num_less
      best_latitude <- current_lat
      best_longitude <- current_long
    }
  }
  return(num_accidents[order(num_accidents[,1], decreasing = TRUE),])
}

if(test_to_run == 'hist_dist'){
  distances <- make_dist_statistics()
  hist(distances, breaks = seq(0, 10, 0.05), main='histograma - Distancia do acidente para o sinal mais proximo')
  mean_distances <- mean(distances)
  std_distances <- sd(distances)
  max_distance <- max(distances)
  min_distance <- min(distances)
  
  cat('\n', paste('Media distancia (km)', toString(mean_distances)))
  cat('\n', paste('Desvio distancia (km)', toString(std_distances)))
  cat('\n', paste('Distancia maxima (km)', toString(max_distance)))
  cat('\n', paste('Distancia minima (km)', toString(min_distance)))
  
  total <- length(distances)
  num_less_than <- sum(distances < 0.5)
  perc_less_than <- num_less_than*100.0/total
  cat('\n', paste('Porcentagem de acidentes a menos que 500 metros de um sinal', toString(perc_less_than)))
}

if(test_to_run == 'worst_equipement'){
  cat('\n', 'Acidentes geral------------')
  results_all <- discover_worst_equipement(accidents);
  cat('\n', 'Pior sinal no geral')
  cat('\n', results_all[1,])
  dev.new()
  hist(results_all[,1], main="histograma - Numero de acidentes com vitimas nas redondezas do sinal de transito")
  
  source('separate_samples.R')
  
  cat('\n', 'Carros apenas----------')
  results_cars <- discover_worst_equipement(car_data)
  cat('\n', 'Pior para carros')
  cat('\n', results_cars[1,])
  dev.new()
  hist(results_cars[,1], breaks=c(9,10,11,12), main="histograma - numero de acidentes com vitimas (carros apenas) nas redondezas do sinal de transito")
  
  cat('\n', 'Motos apenas---------')
  results_motos <- discover_worst_equipement(motorcycle_data)
  cat('\n', 'Pior para motos')
  cat('\n', results_motos[1,])
  dev.new()
  hist(results_motos[,1], main="histograma - numero de acidentes com vitimas (motos apenas) nas redondezas do sinal de transito")
}

if(test_to_run == 'num_equipements'){
  results <- make_equipements_nearby_statistics(0.5)
  num_less_cars <- results$cars
  num_less_motos <- results$motos
  dev.new()
  hist(num_less_cars, main='histograma - numero de sinais a menos de 500 metros de distancia (carros)')
  dev.new()
  hist(num_less_motos, main='histograma - numero de sinais a menos de 500 metros de distancia (motos)')
  cat('\n', 'wilcoxon test')
  result_test <- wilcox.test(num_less_motos, num_less_cars, alternative = "greater", paired=FALSE, conf.level=0.95)
  print(result_test)
}