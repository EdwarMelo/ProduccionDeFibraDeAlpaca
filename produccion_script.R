################################################################################
# Produccion de Fibra de Alpaca y sus Factores Productivos en un Escenario de 
# Cambio Climatico, periodo 1992-2022
################################################################################

#### Configuraciones Iniciales ####
# Se limpia memoria
rm(list = ls())
# dev.off()
# Situar el directorio de trabajo 
setwd("D:/TESIS/git/backup_git") # flexible a modificacion
# Verificacion de ubicacion del directorio de trabajo
getwd()
# Verificacion del contenido de directorio de trabajo
dir()

#### Carga de librerias ####
library(dplyr)
library(wooldridge)
library(zoo)
library(quantmod)
library(dynlm)
library(lmtest)
library(car)
library(stargazer)
library(sandwich)
library(tseries)
library(urca)
library(vars)

#### Se carga el dataset desde el repositorio de GitHub ####
data <- read.csv("https://raw.githubusercontent.com/EdwarMelo/ProduccionDeFibraDeAlpaca/refs/heads/main/data_ts.csv",
                  colClasses =  c("NULL", "character", rep("numeric",6)),
                  row.names = 2)

#### Se generan y transforman las variables a Logaritmos ####
# Dependiente
data$y <- log(data$produccion) # Produccion de Fibra de Alpaca - Produccion
# Independientes y Exogenas
data$x1 <- log(data$pastizales) # Praderas o pastizales permamentes - Naturaleza
data$x2 <- log(data$poblacion) # Poblacion de alpacas - Capital
data$x3 <- log(data$empleo) # Empleo agricola - Trabajo
data$c1 <- log(data$precipitacion) # Precipitacion Acumulada Agregada
data$c2 <- log(data$temperatura) # Temperatura Media

#### Estadisticos Descriptivos ####
summary(data)

#### Transformacion del dataset en formato de series temporales ####
tsdata <- ts(data, start = 1992, end = 2022, frequency = 1)

#### Prueba de Dickey-Fuller Aumentada ####
### Variable Dependiente - Produccion
## Con 1 rezago
# yregL1 <- dynlm(d(y) ~ L(y) + L(d(y)) + trend(tsdata), data = tsdata)
# adf.test(data$y, k=1)
## Con 2 rezagos
# yregL2 <- dynlm(d(y) ~ L(y) + L(d(y)) + L(d(y),2) + trend(tsdata), data = tsdata)
# adf.test(data$y, k=2)
# stargazer(yregL1, yregL2, type = "text") # Solo para la primera version
ur.df(data$y, type=c("trend"), selectlags = c("AIC")) %>% summary()
ur.df(data$y, type=c("trend"), selectlags = c("BIC")) %>% summary()
#---
### Variable Independiente - Naturaleza
## Con 1 rezago
# x1regL1 <- dynlm(d(x1) ~ L(x1) + L(d(x1)) + trend(tsdata), data = tsdata)
# adf.test(data$x1, k=1)
## Con 2 rezagos
# x1regL2 <- dynlm(d(x1) ~ L(x1) + L(d(x1)) + L(d(x1),2) + trend(tsdata), data = tsdata)
# adf.test(data$x1, k=2)
# stargazer(x1regL1, x1regL2, type = "text") # Solo para la primera version
ur.df(data$x1, type=c("trend"), selectlags = c("AIC")) %>% summary()
ur.df(data$x1, type=c("trend"), selectlags = c("BIC")) %>% summary()
#---
### Variable Independiente - Capital
## Con 1 rezago
# x2regL1 <- dynlm(d(x2) ~ L(x2) + L(d(x2)) + trend(tsdata), data = tsdata)
# adf.test(data$x2, k=1)
## Con 2 rezagos
# x2regL2 <- dynlm(d(x2) ~ L(x2) + L(d(x2)) + L(d(x2),2) + trend(tsdata), data = tsdata)
# adf.test(data$x2, k=2)
# stargazer(x2regL1, x2regL2, type = "text") # Solo para la primera version
ur.df(data$x2, type=c("trend"), selectlags = c("AIC")) %>% summary()
ur.df(data$x2, type=c("trend"), selectlags = c("BIC")) %>% summary()
#---
### Variable Independiente - Trabajo
## Con 1 rezago
# x3regL1 <- dynlm(d(x3) ~ L(x3) + L(d(x3)) + trend(tsdata), data = tsdata)
# adf.test(data$x3, k=1)
## Con 2 rezagos
# x3regL2 <- dynlm(d(x3) ~ L(x3) + L(d(x3)) + L(d(x3),2) + trend(tsdata), data = tsdata)
# adf.test(data$x3, k=2)
# stargazer(x3regL1, x3regL2, type = "text") # Solo para la primera version
ur.df(data$x3, type=c("trend"), selectlags = c("AIC")) %>% summary()
ur.df(data$x3, type=c("trend"), selectlags = c("BIC")) %>% summary()
#---
### Variable Exogena - Precipitacion
## Con 1 rezago
# c1regL1 <- dynlm(d(c1) ~ L(c1) + L(d(c1)) + trend(tsdata), data = tsdata)
# adf.test(data$c1, k=1)
## Con 2 rezagos
# c1regL2 <- dynlm(d(c1) ~ L(c1) + L(d(c1)) + L(d(c1),2) + trend(tsdata), data = tsdata)
# adf.test(data$c1, k=2)
# stargazer(c1regL1, c1regL2, type = "text") # Solo para la primera version
ur.df(data$c1, type=c("trend"), selectlags = c("AIC")) %>% summary()
ur.df(data$c1, type=c("trend"), selectlags = c("BIC")) %>% summary()
#---
### Variable Exogena - Temperatura
## Con 1 rezago
# c2regL1 <- dynlm(d(c2) ~ L(c2) + L(d(c2)) + trend(tsdata), data = tsdata)
# adf.test(data$c2, k=1)
## Con 2 rezagos
# c2regL2 <- dynlm(d(c2) ~ L(c2) + L(d(c2)) + L(d(c2),2) + trend(tsdata), data = tsdata)
# adf.test(data$c2, k=2)
# stargazer(c2regL1, c2regL2, type = "text") # Solo para la primera version
ur.df(data$c2, type=c("trend"), selectlags = c("AIC")) %>% summary()
ur.df(data$c2, type=c("trend"), selectlags = c("BIC")) %>% summary()

#### Prueba de COintegracion de Engel-Granger Aumentada ####
# Se define el modelo
model1 <- dynlm(y ~ x1 + x2 + x3 + c1 + c2 + trend(tsdata), data = tsdata)
model1 %>% summary()
# Se aplica el Test de Engle-Granger Aumentada aplicando a los residuos el 
# Test de Dickey-Fuller Aumentada
res1 <- resid(model1)
ur.df(res1, type = c("trend"), selectlags = c("AIC")) %>% summary()

#### Modelo de Correccion de Errores ####
# Se calculan los modelos y los residuos del largo plazo
modelo2 <- dynlm(d(y) ~ L(d(y)) + d(x1) + d(x2) + d(x3) + d(c1) + d(c2) + 
                   trend(tsdata), data = tsdata)
modelo2 %>% summary()
res2 <- resid(modelo2)
# ---
modelo2 <- dynlm(d(y) ~ d(x1) + d(x2) + d(x3) + d(c1) + d(c2) + 
                   trend(tsdata), data = tsdata)
modelo2 %>% summary()
res2 <- resid(modelo2)
# Se genera el modelo de correcion de error (corto plazo)
mce1 <- dynlm(d(y) ~ d(x1) + d(x2) + d(x3) + d(c1) + d(c2) + 
                trend(tsdata) + L(res2), data = tsdata)
summary(mce1)
# ---
mce2 <- dynlm(d(y) ~ L(d(y)) + d(x1) + d(x2) + d(x3) + d(c1) + d(c2) + 
               trend(tsdata) + L(res2), data = tsdata)
summary(mce2)
