
read2015 <- function(){
  data <- read.csv('data/acidentes-2015.csv', sep=";", header = TRUE)
  
  accidents <- as.matrix(data)
  
  size_accidents <- dim(accidents)[1]
  
  column_type = 9;
  column_description = 8;
  
  motorcycle_data <- c()
  others_data <- c()
  
  for(i in 1:size_accidents){
    
  #  str_to_print = toString(i)
  #  str_to_print = paste(str_to_print, "of")
  #  str_to_print = paste(str_to_print, toString(size_accidents))
  #  print(str_to_print)
    
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
  
  print(paste("Num accidents - ", toString(size_accidents)))
  print(paste("Num motorcycles - ", toString(num_motorcycles)))
  print(paste("Num other accidents - ", toString(num_others)))
  
  motorcycle_percent = num_motorcycles/size_accidents;
  vari <- sqrt(motorcycle_percent*(1 - motorcycle_percent)/size_accidents)
  lower_b <- motorcycle_percent - 1.96*vari
  upper_b <- motorcycle_percent + 1.96*vari
  
  str_to_print <- "IC - porcentagem de motorcycles ["
  str_to_print <- paste(str_to_print, toString(lower_b))
  str_to_print <- paste(str_to_print, ',')
  str_to_print <- paste(str_to_print, upper_b)
  str_to_print <- paste(str_to_print, ']')
  
  print(str_to_print)
  print(paste("Percent motorcycles - ", toString(motorcycle_percent)))
  
  others_percent = num_others/size_accidents;
  print(paste("Percent other accidents - ", toString(others_percent)))
  
  print("---------------------- END -------------------------")
  
  return(list(total=size_accidents, motos=num_motorcycles))
}

read2016 <- function(){
  data <- read.csv('data/acidentes-2016.csv', sep=";", header = TRUE)
  
  accidents <- as.matrix(data)
  
  size_accidents <- dim(accidents)[1]
  
  column_type = 11;
  column_description = 10;
  
  motorcycle_data <- c()
  others_data <- c()
  
  for(i in 1:size_accidents){
    
    #  str_to_print = toString(i)
    #  str_to_print = paste(str_to_print, "of")
    #  str_to_print = paste(str_to_print, toString(size_accidents))
    #  print(str_to_print)
    
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
  
  print(paste("Num accidents - ", toString(size_accidents)))
  print(paste("Num motorcycles - ", toString(num_motorcycles)))
  print(paste("Num other accidents - ", toString(num_others)))
  
  motorcycle_percent = num_motorcycles/size_accidents;
  vari <- sqrt(motorcycle_percent*(1 - motorcycle_percent)/size_accidents)
  lower_b <- motorcycle_percent - 1.96*vari
  upper_b <- motorcycle_percent + 1.96*vari
  
  str_to_print <- "IC - porcentagem de motorcycles ["
  str_to_print <- paste(str_to_print, toString(lower_b))
  str_to_print <- paste(str_to_print, ',')
  str_to_print <- paste(str_to_print, upper_b)
  str_to_print <- paste(str_to_print, ']')
  
  print(str_to_print)
  print(paste("Percent motorcycles - ", toString(motorcycle_percent)))
  
  others_percent = num_others/size_accidents;
  print(paste("Percent other accidents - ", toString(others_percent)))
  
  print("---------------------- END -------------------------")
  
  return(list(total=size_accidents, motos=num_motorcycles))
}

results_2015 <- read2015()
results_2016 <- read2016()

total <- results_2015$total + results_2016$total
total_motos <- results_2015$motos + results_2016$motos

P <- total_motos/total
P1 <- results_2015$motos/results_2015$total
P2 <- results_2016$motos/results_2016$total

Z <- (P2 - P1)/sqrt(P*(1-P)*((1/results_2015$total) + (1/results_2016$total)))

print('Estatistica de teste Z')
print(Z)
