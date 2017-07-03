source('util/csv_read.R')
source('util/distance_calculation.R')

print_progress <- function(i, size, title){
  str_to_print <- title
  str_to_print <- paste(str_to_print, toString(i))
  str_to_print <- paste(str_to_print, "de")
  str_to_print <- paste(str_to_print, toString(size))
  str_to_print <- paste(str_to_print, "-------")
  cat("\r", str_to_print)
}

get_distances <- function(accidents, equipments, equipments_latitude_index, equipments_longitude_index){
  
  size_accidents <- dim(accidents)[1]
  accidents_long <- as.numeric(accidents[, 1])
  accidents_lat <- as.numeric(accidents[, 2])
  
  distances <- 1:size_accidents
  
  for(i in 1:size_accidents){
    print_progress(i, size_accidents, "[histograma de distancias]")
    
    current_long <- accidents_long[i]
    current_lat <- accidents_lat[i]
    closest_distance <- get_closest_equipment(current_lat, current_long, 
                                              equipments, equipments_latitude_index, equipments_longitude_index)[1]
    distances[i] <- closest_distance
  }
  
  return(distances)
}

get_equipments_nearby <- function(threshold, motorcycle_data, others_data, 
                                  equipments, equipments_latitude_index, equipments_longitude_index){
  
  size_motos <- dim(motorcycle_data)[1]
  size_others <- dim(others_data)[1]
  
  num_less_motos <- 1:size_motos
  num_less_others <- 1:size_others
  
  for(i in 1:size_motos){
    print_progress(i, size_motos, "[sinais proximos de acidentes c/ moto]")
    
    current_long <- as.numeric(motorcycle_data[i,1])
    current_lat <- as.numeric(motorcycle_data[i,2])
    num_less <- get_num_less_than(current_lat, current_long, threshold, 
                                  equipments, equipments_latitude_index, equipments_longitude_index)
    num_less_motos[i] <- num_less
  }
  
  for(i in 1:size_others){
    print_progress(i, size_others, "[sinais proximos de acidentes s/ moto]")
    
    current_long <- as.numeric(others_data[i,1])
    current_lat <- as.numeric(others_data[i,2])
    num_less <- get_num_less_than(current_lat, current_long, threshold, 
                                  equipments, equipments_latitude_index, equipments_longitude_index)
    num_less_others[i] <- num_less
  }
  
  return(list(others=num_less_others, motos=num_less_motos))
}

get_accidents_nearby_equipments <- function(accident_data, equipments, 
                                     equipments_latitude_index, equipments_longitude_index, dist_thresh, with_order){
  if(missing(with_order)){
    with_order <- TRUE
  }
  
  size_equipments <- dim(equipments)[1]
  
  num_accidents <- matrix(, nrow=size_equipments, ncol = 3)
  
  equipments_latitudes <- as.numeric(equipments[, equipments_latitude_index])
  equipments_longitudes <- as.numeric(equipments[, equipments_longitude_index])
  
  best_max <- -1
  best_longitude <- -1
  best_latitude <- -1
  
  for(i in 1:size_equipments){
    print_progress(i, size_equipments, '[pior sinal]')
    
    current_lat <- equipments_latitudes[i]
    current_long <- equipments_longitudes[i]
    
    num_less <- get_num_accidents_nearby(current_lat, current_long, accident_data, dist_thresh)
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

make_distance_statistics <- function(accidents, equipments, dist_thresh,
                                     equipments_latitude_index, equipments_longitude_index){
  
  distances <- get_distances(accidents, equipments, 
                             equipments_latitude_index, equipments_longitude_index)
  
  mean_distances <- mean(distances)
  std_distances <- sd(distances)
  max_distance <- max(distances)
  min_distance <- min(distances)
  
  cat("\n", paste("Media amostral distancia (km)", toString(mean_distances)))
  cat("\n", paste("Desvio amostral distancia (km)", toString(std_distances)))
  cat("\n", paste("Distancia maxima (km)", toString(max_distance)))
  cat("\n", paste("Distancia minima (km)", toString(min_distance)))
  
  dev.new()
  
  hist(distances, breaks = seq(0, max_distance + 1, 0.1), main="Histograma - Distancia do acidente para o equipamento mais proximo",
       xlab = "Distancia (km)", ylab="Frequencia amostral")
  
  total <- length(distances)
  num_less_than <- sum(distances < dist_thresh)
  perc_less_than <- num_less_than/total
  vari <- sqrt(perc_less_than*(1 - perc_less_than)/total)
  lower_val <- perc_less_than - 1.96*vari
  upper_val <- perc_less_than + 1.96*vari
  
  in_metters <- dist_thresh*1000
  
  str_to_print <- paste("IC 95 - Porcentagem de acidentes a menos que", toString(in_metters))
  str_to_print<- paste(str_to_print, "metros de um equipamento [")
  str_to_print <- paste(str_to_print, toString(lower_val))
  str_to_print <- paste(str_to_print, ",")
  str_to_print <- paste(str_to_print, toString(upper_val))
  str_to_print <- paste(str_to_print, "]")
  cat('\n', str_to_print)
}

make_chisquare_independence_analysis <- function(accidents, motorcycle_data, others_data, equipments, 
                                                 equipments_latitude_index, equipments_longitude_index, dist_thresh, use_3=TRUE){
  cat('\n', 'Acidentes geral------------')
  results_all <- get_accidents_nearby_equipments(accidents, equipments, 
                                                 equipments_latitude_index, equipments_longitude_index, dist_thresh, with_order=TRUE)
  
  cat('\n', 'Pior sinal no geral')
  cat('\n', results_all[1,])
  
  cat('\n', 'Sem motos apenas----------')
  results_others_raw <- get_accidents_nearby_equipments(others_data, equipments, 
                                                        equipments_latitude_index, equipments_longitude_index, dist_thresh, with_order=FALSE)
  
  results_others <- results_others_raw[order(results_others_raw[,1], decreasing = TRUE),]
  cat('\n', 'Pior sem motos')
  cat('\n', results_others[1,])
  dev.new()
  hist(results_others[,1], breaks = seq(0, max(results_others[,1]) + 1, 1), 
       main="Histograma - Numero de acidentes com vitimas (sem moto) nas redondezas do equipamento",
       xlab="Numero de acidentes (sem moto)", ylab="Frequencia amostral")
  
  cat('\n', 'Motos apenas---------')
  results_motos_raw <- get_accidents_nearby_equipments(motorcycle_data, equipments, 
                                                       equipments_latitude_index, equipments_longitude_index, dist_thresh, with_order=FALSE)
  
  results_motos <- results_motos_raw[order(results_motos_raw[,1], decreasing = TRUE),]
  cat('\n', 'Pior para motos')
  cat('\n', results_motos[1,])
  dev.new()
  hist(results_motos[,1], breaks = seq(0, max(results_motos[,1]) + 1, 1),
       main="Histograma - Numero de acidentes com vitimas (motos apenas) nas redondezas do equipamento",
       xlab="Numero de acidentes (com moto)", ylab="Frequencia amostral")
  
  num_equipments <- dim(equipments)[1]
  joint_results <- matrix(, nrow=num_equipments, ncol=2)
  for(i in 1:num_equipments){
    joint_results[i, ] <- c(results_others_raw[i][1], results_motos_raw[i][1])
  }
  
  correlation <- cor(joint_results[, 1], joint_results[, 2], method='pearson')
  cat('\n', paste('Correlacao (pearson) acidentes com carro e com motos', toString(correlation)))
  #join categories > 2 => 3 or > 1 => 2 depending on use_3
  
  for(i in 1:num_equipments){
    if(use_3){
      if(joint_results[i , 1] > 2){
        joint_results[i, 1] = 3
      }
      
      if(joint_results[i , 2] > 2){
        joint_results[i, 2] = 3
      }
    }else{
      if(joint_results[i , 1] > 1){
        joint_results[i, 1] = 2
      }
      
      if(joint_results[i , 2] > 2){
        joint_results[i, 2] = 2
      }
    }
  }
  
  res <- chisq.test(joint_results[, 1], joint_results[, 2])
  print(res)
}

make_wilcoxon_analysis <- function(others_data, motorcycle_data, equipments,
                                   equipments_latitude_index, equipments_longitude_index){
  
  results <- get_equipments_nearby(dist_thresh, motorcycle_data, others_data, 
                                   equipments, equipments_latitude_index, equipments_longitude_index)
  
  num_less_others <- results$others
  num_less_motos <- results$motos
  
  in_metters <- dist_thresh*1000
  
  basic_str <- paste("Histograma - Numero de equipamentos a menos de", toString(in_metters))
  dev.new()
  hist(num_less_others, breaks=seq(0,max(num_less_others) + 1,1), main=paste(basic_str, "metros de distancia (sem motos)"),
       xlab="Numero de sinais", ylab="Frequencia amostral")
  
  dev.new()
  hist(num_less_motos, breaks=seq(0,max(num_less_motos) + 1,1), main=paste(basic_str, "metros de distancia (com motos)"),
       xlab="Numero de sinais", ylab="Frequencia amostral")
  
  cat('\n', 'Teste de wilcoxon')
  cat('\n', 'Hipotese nula: a media do numero de equipamentos eh igual para as duas amostras')
  result_test <- wilcox.test(num_less_others, num_less_motos, paired=FALSE, alternative = "greater")
  print(result_test)
}
