#limpa todos os dados (seguranca)
rm(list=ls())

source('util/csv_read.R')
source('util/distance_calculation.R')
source('make_distance_statistics_tests.R')

latitude_index <- 1
longitude_index <- 2

#config
test_to_run <- 'hist_dist' #'wilcoxon' #'hist_dist' #'chisquare'

accidents <- load_data('data/acidentes-2016.csv')    

fisc_equipements <- load_data('data/fiscalizacao-eletronica.csv')
cttu_equipements <- load_data('data/monitoramentocttu.csv')
misc_equipements <- load_data('data/equipamentos-de-monitoramento-e-ficalizacao.csv')

size_fisc_equipements <- dim(fisc_equipements)[1]
size_cttu_equipements <- dim(cttu_equipements)[1]
size_misc_equipements <- dim(misc_equipements)[1]

all_fisc <- getLatLongData(fisc_equipements,8,7)
all_cttu <- getLatLongData(cttu_equipements,4,3)
all_misc <- getLatLongData(misc_equipements,3,4)

all_equipments <- c()

for(i in 1:size_fisc_equipements){
  all_equipments <- c(all_equipments,all_fisc[i,])
}
for(i in 1:size_cttu_equipements){
  all_equipments <- c(all_equipments,all_cttu[i,])
}
for(i in 1:size_misc_equipements){
  all_equipments <- c(all_equipments,all_misc[i,])
}

all_equipments <- matrix(all_equipments,ncol = 2, byrow = TRUE)

dist_thresh <- 0.2

if(test_to_run == 'hist_dist'){
  make_distance_statistics(accidents, all_equipments, dist_thresh, latitude_index, longitude_index)
}

if(test_to_run == 'chisquare'){
  source('separate_samples.R')
  
  make_chisquare_independence_analysis(accidents, motorcycle_data, others_data, all_equipments, 
                                       latitude_index, longitude_index, dist_thresh, use_3=FALSE)
}

if(test_to_run == 'wilcoxon'){
  source('separate_samples.R')
  
  make_wilcoxon_analysis(others_data, motorcycle_data, all_equipments,
                         latitude_index, longitude_index)
}