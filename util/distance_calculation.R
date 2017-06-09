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