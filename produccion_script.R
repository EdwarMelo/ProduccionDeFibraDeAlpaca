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
dataset <- read.csv("https://raw.githubusercontent.com/EdwarMelo/ProduccionDeFibraDeAlpaca/refs/heads/main/data_ts.csv",
                    colClasses =  c("NULL", "character", rep("numeric",6)),
                    row.names = 2)
                   







