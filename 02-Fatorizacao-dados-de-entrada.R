##################################################################
######### Projeto 2 - Previsão de demanda - Grupo Bimbo ##########
############### Fatorização dos dados de entrada #################
##################################################################

Azure <- FALSE

if(!Azure){
  
  setwd("C:/DataScience/FCD/BigDataAnalytics-R-Azure/Projeto-2/previsao_demanda_grupo_bimbo/")
  getwd()
  
}


############################################################
#### Carregamento de dados e bibliotecas ###################
############################################################

if(Azure){
  
  data <- maml.mapInputPort(1) # class: data.frame
#  test <- maml.mapInputPort(2) # class: data.frame
  
}else{
  
  library(readr)
  data <- read_csv("dados/train_sample_v2.csv")
#  test <- read_csv("dados/test.csv")
  
  data$Venta_hoy <- NULL
  data$Venta_uni_hoy <- NULL
  data$Dev_proxima <- NULL
  data$Dev_uni_proxima <- NULL
  data$Semana <- NULL
  data$Agencia_ID <- NULL
}

############## Fatorização dos dados de entrada ##########

data[, -5] <- as.data.frame(lapply(data[, -5], cut, ordered_result = TRUE, 1000))

head(data)
summary(data)


############### Saída do bloco ########################

if(Azure){
  maml.mapOutputPort("data");
}

