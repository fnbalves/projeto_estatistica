#limpa todos os dados (seguranca)
rm(list=ls())

read_accident_data <- function(file_name, column_type, column_description){
  
  data <- read.csv(file_name, sep=";", header = TRUE)
  
  accidents <- as.matrix(data)
  
  size_accidents <- dim(accidents)[1]
  
  motorcycle_data <- c()
  others_data <- c()
  
  for(i in 1:size_accidents){
    
    if(grepl("Moto", accidents[i, column_type]) || 
       grepl("Ciclo", accidents[i, column_type]) || 
       grepl("moto", accidents[i, column_description], ignore.case = TRUE) || 
       grepl("CINQUENT", accidents[i, column_description], ignore.case = TRUE) || 
       grepl("50", accidents[i, column_description], ignore.case = TRUE)){
      
      motorcycle_data <- c(motorcycle_data, accidents[i,])
    } else {
      others_data <- c(others_data, accidents[i,])
    }
    
  }
  
  motorcycle_data <- matrix(motorcycle_data, ncol=11, byrow=TRUE)
  colnames(motorcycle_data) <- c("data_abertura", "hora_abertura", "bairro", "endereco", "complemento", "tipo_ocorrencia", "quantidade_vitimas", "descricao", "tipo", "latitude", "longitude")
  num_motorcycles = dim(motorcycle_data)[1]
  
  others_data <- matrix(others_data, ncol=11, byrow=TRUE)
  colnames(others_data) <- c("data_abertura", "hora_abertura", "bairro", "endereco", "complemento", "tipo_ocorrencia", "quantidade_vitimas", "descricao", "tipo", "latitude", "longitude")
  num_others = dim(others_data)[1]
  
  motorcycle_percent = num_motorcycles/size_accidents;
  vari <- sqrt(motorcycle_percent*(1 - motorcycle_percent)/size_accidents)
  lower_b <- motorcycle_percent - 1.96*vari
  upper_b <- motorcycle_percent + 1.96*vari
  
  return(list(total=size_accidents, motos=num_motorcycles))
}

print_ic <- function(percent, size, title){
  vari <- sqrt(percent*(1 - percent)/size)
  lower_b <- percent - 1.96*vari
  upper_b <- percent + 1.96*vari
  
  str_to_print <- paste("IC 95 - ", title)
  str_to_print <- paste(str_to_print, " [")
  str_to_print <- paste(str_to_print, toString(lower_b))
  str_to_print <- paste(str_to_print, ",")
  str_to_print <- paste(str_to_print, upper_b)
  str_to_print <- paste(str_to_print, "]")
  
  cat("\n", str_to_print)
}

results_2015 <- read_accident_data("data/acidentes-2015.csv", 9, 8)
results_2016 <- read_accident_data("data/acidentes-2016.csv", 11, 10)

total <- results_2015$total + results_2016$total
total_motos <- results_2015$motos + results_2016$motos

P <- total_motos/total
P1 <- results_2015$motos/results_2015$total
P2 <- results_2016$motos/results_2016$total

print_ic(P1, results_2015$total, "procentagem motos 2015")
print_ic(P2, results_2016$total, "procentagem motos 2016")

Z <- (P1 - P2)/sqrt(P*(1-P)*((1/results_2015$total) + (1/results_2016$total)))

cat("\n", "Estatistica de teste Z")
cat("\n", Z)

if(Z > 1.96){
  cat("\n", "A hipotese nula (P1 < P2) pois Z > 1.96")
}else{
  cat("\n", "A hipotese nula nao pode ser rejeitada")
}
