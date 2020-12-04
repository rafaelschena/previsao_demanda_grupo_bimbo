##################################################################
######### Projeto 2 - Previsão de demanda - Grupo Bimbo ##########
################ Normalização dos dados de treino ################
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
  test <- maml.mapInputPort(2) # class: data.frame
  
}else{
  
  library(readr)
  data <- read_csv("dados/train_sample_v2.csv")
  test <- read_csv("dados/test.csv")
  
  #cliente_tabla <- read_csv("dados/cliente_tabla.csv")
  #producto_tabla <- read_csv("dados/producto_tabla.csv")
  #town_state <- read_csv("dados/town_state.csv")
  #sample_submission <- read_csv("dados/sample_submission.csv")
}


##############################################
#### Análise Exploratória dos Dados ##########
##############################################

if(!Azure){
  
  head(data)
  head(test)
  
  # Únicas variáveis preditoras no test set:
  # Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID
  
  #str(data)
  summary(data)
  summary(test)
  
  
}

# As variáveis Venta_uni_hoy Venta_hoy Dev_uni_proxima Dev_proxima não se encontram
# presentes no dataset de treinamento, portanto serão excluídas.

data$Venta_hoy <- NULL
data$Venta_uni_hoy <- NULL
data$Dev_proxima <- NULL
data$Dev_uni_proxima <- NULL

# Normalização dos dados do dataset test

min_data <- lapply(data, min)
max_data <- lapply(data, max)

min_test <- lapply(test, min)
max_test <- lapply(test, max)

normalizar <- function(x, min, max){
  return((x-min)/(max-min))
}

# Não será feita a normalização do index, porque o index não é variável preditora.
for(v in names(test)[-1]){
  
  if(v %in% names(data)){
    min <- min(min_data[[v]], min_test[[v]])
    max <- max(max_data[[v]], max_test[[v]])
  }else{
    min <- min_test[[v]]
    max <- max_test[[v]]
  }
  
  test[[v]] <- normalizar(test[[v]], min, max)
  
}


if(Azure){
  maml.mapOutputPort("test");
}
