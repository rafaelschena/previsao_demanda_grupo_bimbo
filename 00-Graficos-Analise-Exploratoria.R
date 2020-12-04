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
  
  # As variáveis Venta_uni_hoy Venta_hoy Dev_uni_proxima Dev_proxima não se encontram
  # presentes no dataset de treinamento.
  
  
  # Limpeza e manipulação
  
  # Verificando a existência de NAs no arquivo
  variaveis <- names(data)
  variaveis
  
  for(v in variaveis){
    print('-------------')
    print(paste("Número de NAs na variável", v))
    print(sum(is.na(data[v])))
  }
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
                  main = paste("Plot de Correlação usando Metodo", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)



################ Density plot ##########################
library(ggplot2)

# Visualizando o relacionamento entre as variáveis preditoras e demanda de produtos
#
# Variáveis preditoras Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID

labels <- c("Demanda vs Semana",
            "Demanda vs Agencia_ID",
            "Demanda vs Canal_ID",
            "Demanda vs Ruta_SAK",
            "Demanda vs Cliente_ID",
            "Demanda vs Producto_ID")

xAxis <- c("Semana", "Agencia_ID", "Canal_ID", "Ruta_SAK", "Cliente_ID", "Producto_ID")

# Função para os Density Plots
plot.scatter <- function(X, label){ 
  ggplot(data, aes_string(x = X, y = "Demanda_uni_equil")) + 
    geom_point(aes_string(colour = "Demanda_uni_equil"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) 
}

Map(plot.scatter, xAxis, labels)

if(Azure){
  maml.mapOutputPort("data");
}
