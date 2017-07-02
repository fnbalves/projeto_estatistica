source('util/csv_read.R')
source('util/distance_calculation.R')

equipements_latitude_index <- 8
equipements_longitude_index <- 9

#config
test_to_run <- 'worst_equipement' #'num_equipements' #'hist_dist' #'worst_equipement'

accidents <- load_data('data/acidentes-2016.csv')

equipements <- load_data('data/semaforos.csv')#load_data('data/equipamentos-de-monitoramento-e-ficalizacao.csv')

motorcycle_data <- matrix(motorcycle_data, ncol=3, byrow=TRUE)

print_progress <- function(i, size, title){
  str_to_print <- title
  str_to_print <- paste(str_to_print, toString(i))
  str_to_print <- paste(str_to_print, "de")
  str_to_print <- paste(str_to_print, toString(size))
  str_to_print <- paste(str_to_print, "-------")
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
  size_others <- dim(others_data)[1]
  
  num_less_motos <- 1:size_motos
  num_less_others <- 1:size_others
  
  for(i in 1:size_motos){
    print_progress(i, size_motos, "[sinais proximos de acidentes c/ moto]")
    
    current_long <- as.numeric(motorcycle_data[i,1])
    current_lat <- as.numeric(motorcycle_data[i,2])
    num_less <- get_num_less_than(current_lat, current_long, threshold, equipements)
    num_less_motos[i] <- num_less
  }
  
  for(i in 1:size_others){
    print_progress(i, size_others, "[sinais proximos de acidentes s/ moto]")
    
    current_long <- as.numeric(others_data[i,1])
    current_lat <- as.numeric(others_data[i,2])
    num_less <- get_num_less_than(current_lat, current_long, threshold, equipements)
    num_less_others[i] <- num_less
  }
  
  return(list(others=num_less_others, motos=num_less_motos))
}

discover_worst_equipement <- function(accident_data, with_order){
  if(missing(with_order)){
    with_order <- TRUE
  }
  
  num_accidents <- matrix(, nrow=size_equipements, ncol = 3)
  
  equipements_latitudes <- as.numeric(equipements[, equipements_latitude_index])
  equipements_longitudes <- as.numeric(equipements[, equipements_longitude_index])
  
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
  
  if(with_order){
    return(num_accidents[order(num_accidents[,1], decreasing = TRUE),])
  }else{
    return(num_accidents)
  }
}

if(test_to_run == 'hist_dist'){
  distances <- make_dist_statistics()
  hist(distances, breaks = seq(0, 10, 0.5), main='histograma - Distancia do acidente para o sinal mais proximo')
  mean_distances <- mean(distances)
  std_distances <- sd(distances)
  max_distance <- max(distances)
  min_distance <- min(distances)
  
  cat('\n', paste('Media amostral distancia (km)', toString(mean_distances)))
  cat('\n', paste('Desvio amostral distancia (km)', toString(std_distances)))
  cat('\n', paste('Distancia maxima (km)', toString(max_distance)))
  cat('\n', paste('Distancia minima (km)', toString(min_distance)))
  
  total <- length(distances)
  num_less_than <- sum(distances < 0.1)
  perc_less_than <- num_less_than/total
  vari <- sqrt(perc_less_than*(1 - perc_less_than)/total)
  lower_val <- perc_less_than - 1.96*vari
  upper_val <- perc_less_than + 1.96*vari
  
  str_to_print <- 'IC - Porcentagem de acidentes a menos que 500 metros de um sinal ['
  str_to_print <- paste(str_to_print, toString(lower_val))
  str_to_print <- paste(str_to_print, ',')
  str_to_print <- paste(str_to_print, toString(upper_val))
  str_to_print <- paste(str_to_print, ']')
  cat('\n', str_to_print)
  
}

if(test_to_run == 'worst_equipement'){
  cat('\n', 'Acidentes geral------------')
  results_all <- discover_worst_equipement(accidents);
  cat('\n', 'Pior sinal no geral')
  cat('\n', results_all[1,])
  dev.new()
  hist(results_all[,1], main="histograma - Numero de acidentes com vitimas nas redondezas do sinal de transito")
  
  source('separate_samples.R')
  
  cat('\n', 'Sem motos apenas----------')
  results_others_raw <- discover_worst_equipement(others_data, with_order = FALSE)
  results_others <- results_others_raw[order(results_others_raw[,1], decreasing = TRUE),]
  cat('\n', 'Pior sem motos')
  cat('\n', results_others[1,])
  dev.new()
  hist(results_others[,1], breaks=c(0,1,2,3,4), main="histograma - Numero de acidentes com vitimas (carros apenas) nas redondezas do sinal de transito")
  
  cat('\n', 'Motos apenas---------')
  results_motos_raw <- discover_worst_equipement(motorcycle_data, with_order = FALSE)
  results_motos <- results_motos_raw[order(results_motos_raw[,1], decreasing = TRUE),]
  cat('\n', 'Pior para motos')
  cat('\n', results_motos[1,])
  dev.new()
  hist(results_motos[,1], breaks=c(0,1,2,3,4,5,6), main="histograma - Numero de acidentes com vitimas (motos apenas) nas redondezas do sinal de transito")
  num_equipements <- dim(equipements)[1]
  joint_results <- matrix(, nrow=num_equipements, ncol=2)
  for(i in 1:num_equipements){
    joint_results[i, ] <- c(results_cars_raw[i][1], results_motos_raw[i][1])
  }
  
  correlation <- cor(joint_results[, 1], joint_results[, 2], method='pearson')
  cat('\n', paste('Correlacao (pearson) acidentes com carro e com motos', toString(correlation)))
  #join categories
  for(i in 1:num_equipements){
    if(joint_results[i , 1] > 0){
      joint_results[i, 1] = 1
    }
    
    if(joint_results[i , 2] > 0){
      joint_results[i, 2] = 1
    }
  }
  
  res <- chisq.test(joint_results[, 1], joint_results[, 2])
  print(res)
}

if(test_to_run == 'num_equipements'){
  results <- make_equipements_nearby_statistics(0.5)
  num_less_others <- results$others
  num_less_motos <- results$motos
  dev.new()
  hist(num_less_others, main='histograma - Numero de sinais a menos de 500 metros de distancia (sem motos)')
  dev.new()
  hist(num_less_motos, main='histograma - Numero de sinais a menos de 500 metros de distancia (motos)')
  cat('\n', 'wilcoxon test')
  result_test <- wilcox.test(num_less_others, num_less_motos, paired=FALSE, alternative = "greater")
  print(result_test)
}