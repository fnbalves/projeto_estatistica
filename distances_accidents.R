source('util/csv_read.R')
source('util/distance_calculation.R')

accidents <- load_data('data/acidentes-2016.csv')
equipements <- load_data('data/semaforos.csv')

size_accidents <- dim(accidents)[1]
distances <- 1:size_accidents

accidents_long <- as.numeric(accidents[, 1])
accidents_lat <- as.numeric(accidents[, 2])

for(i in 1:size_accidents){
  str_to_print = toString(i)
  str_to_print = paste(str_to_print, "of")
  str_to_print = paste(str_to_print, toString(size_accidents))
  
  print(str_to_print)
  
  current_long <- accidents_long[i]
  current_lat <- accidents_lat[i]
  closest_distance <- get_closest_equipement(current_lat, current_long, equipements)[1]
  distances[i] <- closest_distance
}

hist(distances, breaks = seq(0, 10, 0.05))
mean_distances <- mean(distances)
std_distances <- sd(distances)
max_distance <- max(distances)
min_distance <- min(distances)

print(paste('Media distancia (km)', toString(mean_distances)))
print(paste('Desvio distancia (km)', toString(std_distances)))
print(paste('Distancia maxima (km)', toString(max_distance)))
print(paste('Distancia minima (km)', toString(min_distance)))

source('separate_samples.R')

size_motos <- dim(motorcycle_data)[1]
size_cars <- dim(car_data)[1]

num_less_100_motos <- 1:size_motos
num_less_100_cars <- 1:size_cars

for(i in 1:size_motos){
  current_long <- as.numeric(motorcycle_data[i,1])
  current_lat <- as.numeric(motorcycle_data[i,2])
  num_less <- get_num_less_than(current_lat, current_long, 0.300, equipements)
  num_less_100_motos[i] <- num_less
}

for(i in 1:size_cars){
  current_long <- as.numeric(car_data[i,1])
  current_lat <- as.numeric(car_data[i,2])
  num_less <- get_num_less_than(current_lat, current_long, 0.300, equipements)
  num_less_100_cars[i] <- num_less
}