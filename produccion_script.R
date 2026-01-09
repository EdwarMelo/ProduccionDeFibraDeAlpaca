################################################################################
# Produccion de Fibra de Alpaca y sus Factores Productivos en un Escenario de 
# Cambio Climatico, periodo 1992-2022
################################################################################

#### Configuraciones Iniciales ####
# Se limpia memoria
rm(list = ls())
# Situar el directorio de trabajo 
setwd("D:/TESIS/git/backup_git") # flexible a modificacion
# Verificacion de ubicacion del directorio de trabajo
getwd()
# Verificacion del contenido de directorio de trabajo
dir()

#### Se carga el dataset desde el repositorio de GitHub ####
dataset 


# Transformacion de Datos de formato .xlsx  a .csv 



library(readxl)
data <- read_excel("data_ts.xlsx")
write.csv(data, file="data_ts.csv")
dataset <- read.csv("data_ts.csv")


colnames(data)
dataset <- keep(dataset(colnames(data)))

dataset$X <- NULL
write.csv(dataset$X <- NULL, file="data_ts.csv")
dataset <- read.csv("data_ts.csv")





# Se carga los datos de series temporales





