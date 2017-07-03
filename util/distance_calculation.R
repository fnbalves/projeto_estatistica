#From https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula

get_distance <- function(lat1, lon1, lat2, lon2){
  R <- 6371; #Radius of the earth in km
  dLat <- deg2rad(lat2-lat1);
  dLon <- deg2rad(lon2-lon1); 
  a <- sin(dLat/2)*sin(dLat/2) +
      cos(deg2rad(lat1))*cos(deg2rad(lat2))* 
      sin(dLon/2)*sin(dLon/2);
  
  c <- 2*atan2(sqrt(a),sqrt(1-a)); 
  d <- R * c; #Distance in km
  return(d);
}

deg2rad <- function(deg){
  return(deg*pi/180.0)
}

get_closest_equipment <- function(lat, long, equipments, equipments_latitude_index, equipments_longitude_index){
  equipments_latitudes <- as.numeric(equipments[, equipments_latitude_index])
  equipments_longitudes <- as.numeric(equipments[, equipments_longitude_index])
  
  size_vec <- length(equipments_latitudes)
  min_distance <- 10000000000000000
  lat_for_min <- 0
  long_for_min <- 0
  
  for(i in 1:size_vec){
    current_latitude <- equipments_latitudes[i]
    current_longitude <- equipments_longitudes[i]
    new_distance <- get_distance(lat, long, current_latitude, current_longitude)
    
    if(!is.na(new_distance)){
      if(new_distance < min_distance){
        min_distance <- new_distance
        lat_for_min <- current_latitude
        long_for_min <- current_longitude
      }
    }else{
      print("error")
      print(paste("current_latitude", toString(current_latitude)))
      print(paste("current_longitude", toString(current_longitude)))
      print(paste("lat", toString(lat)))
      print(paste("long", toString(long)))
    }
  }
  
  return(c(min_distance, lat_for_min, long_for_min))
}

get_num_less_than <- function(lat, long, threshold, equipments, equipments_latitude_index, equipments_longitude_index){
  size_vec <- dim(equipments)[1]
  num_less <- 0
  
  equipments_latitudes <- as.numeric(equipments[, equipments_latitude_index])
  equipments_longitudes <- as.numeric(equipments[, equipments_longitude_index])
  
  for(i in 1:size_vec){
    current_latitude <- equipments_latitudes[i]
    current_longitude <- equipments_longitudes[i]
    
    new_distance <- get_distance(lat, long, current_latitude, current_longitude)
    
    if(is.na(new_distance)){
      next
    }
    
    if(new_distance < threshold){
      num_less <- num_less + 1
    }
  }
  return(num_less)
}

get_num_accidents_nearby <- function(lat, long, accidents, threshold){
  size_vec <- dim(accidents)[1]
  num_less <- 0
  
  accidents_longitudes <- as.numeric(accidents[, 1])
  accidents_latitudes <- as.numeric(accidents[, 2])
  
  for(i in 1:size_vec){
    current_latitude <- accidents_latitudes[i]
    current_longitude <- accidents_longitudes[i]
    
    new_distance <- get_distance(lat, long, current_latitude, current_longitude)
    
    if(is.na(new_distance)){
      next
    }
    
    if(new_distance < threshold){
      num_less <- num_less + 1
    }
  }
  return(num_less)
}


getLatLongData <- function(original_table, latColumn, longColumn){
  
  size <- dim(original_table)[1]
  
  lat_list <- original_table[,latColumn]
  long_list <- original_table[,longColumn]
  
  mat <- matrix(c(lat_list,long_list),nrow=length(lat_list))
  
  return(mat)
}
