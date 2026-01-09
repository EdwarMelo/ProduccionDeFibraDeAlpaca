################################################################################
# Produccion de Fibra de Alpaca y sus Factores Productivos en un Escenario de 
# Cambio Climatico, periodo 1992-2022

################################################################################

#####
# Transformacion de Datos de formato .xlsx  a .csv 
getwd()
setwd("D:/TESIS/git/backup_git")

library(readxl)
data <- read_excel("data_ts.xlsx")
write.csv(data, file="data_ts.csv")
dataset <- read.csv("data_ts.csv")

# Se carga los datos de series temporales





