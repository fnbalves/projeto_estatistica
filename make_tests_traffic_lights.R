#limpa todos os dados (seguranca)
rm(list=ls())

source('util/csv_read.R')
source('util/distance_calculation.R')
source('make_distance_statistics_tests.R')

latitude_index <- 8
longitude_index <- 9

#config
test_to_run <- 'hist_dist' #'wilcoxon' #'hist_dist' #'chisquare'

accidents <- load_data('data/acidentes-2016.csv')
traffic_lights <- load_data('data/semaforos.csv')

dist_thresh <- 0.2

if(test_to_run == 'hist_dist'){
  make_distance_statistics(accidents, traffic_lights, dist_thresh, latitude_index, longitude_index)
}

if(test_to_run == 'chisquare'){
  source('separate_samples.R')
  
  make_chisquare_independence_analysis(accidents, motorcycle_data, others_data, traffic_lights, 
                                       latitude_index, longitude_index, dist_thresh)
}

if(test_to_run == 'wilcoxon'){
  source('separate_samples.R')
  
  make_wilcoxon_analysis(others_data, motorcycle_data, traffic_lights,
                         latitude_index, longitude_index)
}