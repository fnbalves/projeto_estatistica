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

get_closest_equipement <- function(lat, long, equipements){
  equipements_latitudes <- as.numeric(equipements[, 8])
  equipements_longitudes <- as.numeric(equipements[, 9])
  
  size_vec = length(equipements_latitudes)
  min_distance = 10000000000000000000
  lat_for_min = 0
  long_for_min = 0
  
  for(i in 1:size_vec){
    current_latitude <- equipements_latitudes[i]
    current_longitude <- equipements_longitudes[i]
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
  
  #retorna min_dist, lat, long
  return(c(min_distance, lat_for_min, long_for_min))
}