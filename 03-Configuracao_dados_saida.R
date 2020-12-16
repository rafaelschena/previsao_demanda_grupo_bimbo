##################################################################
######### Projeto 2 - Previsão de demanda - Grupo Bimbo ##########
############### Formatação dados de saída ########################
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
  
  pred <- maml.mapInputPort(1) # class: data.frame
  test <- maml.mapInputPort(2) # class: data.frame
  
}else{
  
  library(readr)
 
  test <- read_csv("dados/test.csv")
  pred <- read_csv("dados/test.csv") # apenas para teste da saída formatada
  test$Semana <- NULL
  test$Agencia_ID <- NULL
}

# Aqui presume-se que, caso este código seja rodado fora do AzureML, o dataset pred
# já tenha sido gerado com as predições do modelo com os labels na sua última coluna

saida <- as.data.frame(cbind(test$id, pred[[ncol(pred)]]))
colnames(saida) <- c("id", "Demanda_uni_equil")

saida$Demanda_uni_equil[saida$Demanda_uni_equil < 0] <- 0


############### Saída do bloco ########################

if(Azure){
  maml.mapOutputPort("saida");
}