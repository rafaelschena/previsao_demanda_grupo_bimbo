##################################################################
######### Projeto 2 - Previsão de demanda - Grupo Bimbo ##########
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

head(data)
head(test)

# Únicas variáveis preditoras no test set:
# Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID

#str(data)
summary(data)
summary(test)

# Verificando dados faltantes nos datasets

conta_NA <- function(X){
  return(sum(is.na(X)))
}

lapply(data, conta_NA)
lapply(test, conta_NA) 

# As variáveis Venta_uni_hoy Venta_hoy Dev_uni_proxima Dev_proxima não se encontram
# presentes no dataset de treinamento, portanto serão excluídas.

if(!Azure){
  data$Venta_hoy <- NULL
  data$Venta_uni_hoy <- NULL
  data$Dev_proxima <- NULL
  data$Dev_uni_proxima <- NULL
}


# Normalização dos dados do dataset data

min_data <- lapply(data, min)
max_data <- lapply(data, max)

min_test <- lapply(test, min)
max_test <- lapply(test, max)

normalizar <- function(x, min, max){
  return((x-min)/(max-min))
}

for(v in names(data)){
  
  if(v %in% names(test)){
    min <- min(min_data[[v]], min_test[[v]])
    max <- max(max_data[[v]], max_test[[v]])
  }else{
    min <- min_data[[v]]
    max <- max_data[[v]]
  }
  
  data[[v]] <- normalizar(data[[v]], min, max)
  
}

# Normalização do dataset test
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

################ Correlação ##########################

# Métodos de Correlação
# Pearson - coeficiente usado para medir o grau de relacionamento entre duas variáveis com relação linear
# Spearman - teste não paramétrico, para medir o grau de relacionamento entre duas variaveis
# Kendall - teste não paramétrico, para medir a força de dependência entre duas variaveis

# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")


# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(data, method = method)))

head(cors)

# Preprando o plot
require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Metodo", labs,"com outliers"),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)


################# Boxplots #############################


boxplot(data, Demanda_uni_equil ~ ., col = "blue", main = "Boxplot com os outliers")


################ Histogramas ###########################

library(ggplot2)

lapply(names(data), function(x){
  ggplot(data, aes_string(x)) +
    geom_histogram(bins = 30L, fill = "blue", alpha = 0.5) +
    ggtitle(paste("Histograma de", x,"com outliers"))
})


############### Limpeza de outliers #####################

#data_copia <- data

#data <- data_copia


for(v in names(data)){
  
  upper_bound <- median(data[[v]]) + 3*mad(data[[v]])
  lower_bound <- median(data[[v]]) - 3*mad(data[[v]])
  data <- subset(data, data[[v]] <= upper_bound & data[[v]] >= lower_bound)
  print(v)
  print(nrow(data)) 
}

summary(data)


################ Histogramas ###########################

lapply(names(data), function(x){
  ggplot(data, aes_string(x)) +
    geom_histogram(bins = 100L, fill = "green", alpha = 0.5) +
    ggtitle(paste("Histograma de", x,"sem outliers"))
})

################# Boxplots #############################


boxplot(data[, -1], Demanda_uni_equil ~ ., col = "green", main = "Boxplot sem os outliers")

################ Nova Correlação ##########################

# Métodos de Correlação
# Pearson - coeficiente usado para medir o grau de relacionamento entre duas variáveis com relação linear
# Spearman - teste não paramétrico, para medir o grau de relacionamento entre duas variaveis
# Kendall - teste não paramétrico, para medir a força de dependência entre duas variaveis

# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")


# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(data, method = method)))

head(cors)

# Preprando o plot
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando metodo", labs, "sem outliers"),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)

############### Saída do bloco ########################

if(Azure){
  maml.mapOutputPort("data");
}

