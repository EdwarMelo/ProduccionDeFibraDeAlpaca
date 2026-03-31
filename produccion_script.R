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
library(ggplot2)
library(lubridate)
library(tidyverse)
library(PerformanceAnalytics)
library(Ryacas)
library(caracas)
library(devtools)
library(reticulate)
library(AICcmodavg)
library(slider)
library(TTR)
library(knitr)

#### Se carga el dataset desde el repositorio de GitHub ####
datos <- read.csv("https://raw.githubusercontent.com/EdwarMelo/ProduccionDeFibraDeAlpaca/refs/heads/main/data_ts.csv",
                 colClasses = c("character", rep("numeric",6)))

#### Estadisticos Descriptivos ####
summary(datos)
# Varianza
var(datos$produccion)
var(datos$pastizales)
var(datos$alpacas)
var(datos$empleo)
var(datos$precipitacion)
var(datos$temperatura)
# Desviacion Estandar
sd(datos$produccion)
sd(datos$pastizales)
sd(datos$alpacas)
sd(datos$empleo)
sd(datos$precipitacion)
sd(datos$temperatura)

#### Graficos Descriptivos ####
df <- datos[, -1]
df <- df %>% rename("Produccion (t)"=1, "Naturaleza (Mha)"=2, "Capital (M)"=3, 
                    "Trabajo (M)"=4, "Precipitacion (mm)"=5, 
                    "Temperatura (°C)"=6)
# Comportamiento Temporal de las Variables ####
df1 <- ts(df, start=1992, end=2022, frequency=1)
plot.ts(df1, plot.type = "multiple", 
        main = "Comportamiento Temporal de las Variables/Dimensiones",
        xlab = "Periodo Anual 1992-2022")
# Suavizado por Medias Moviles
fecha <- as.Date(datos$tiempo, format = "%Y")
prod <- datos$produccion
amprod <- SMA(datos$produccion, n=3) # Media Movil de la Produccion
nat <- datos$pastizales
amnat <- SMA(datos$pastizales, n=3) # Media Movil de la Naturaleza
cap <- datos$alpacas
amcap <- SMA(datos$alpacas, n=3) # Media Movil del capital
trab <- datos$empleo
amtrab <- SMA(datos$empleo, n=3) # Media Movil del trabajo
prec <- datos$precipitacion
amprec <- SMA(datos$precipitacion, n=3) # Media Movil de la Precipitacion
temp <- datos$temperatura
amtemp <- SMA(datos$temperatura, n=3) # Media Movil de la Temperatura

# Correlacion entre variables
chart.Correlation(df, main="Correlacion de las Variables/Dimensiones")

#### Estimacion del Modelo Cobb-Douglas ####
# Considerando que la funcion de produccion Cobb-Douglas es exponencial, 
# se aplicaran logaritmos naturales para flexibilizar su analisis.
lndata <- datos[,-1]
lndata$produccion <- log(lndata$produccion) # Produccion de Fibra de Alpaca - Produccion
lndata$pastizales <- log(lndata$pastizales) # Praderas o pastizales permamentes - Naturaleza
lndata$alpacas <- log(lndata$alpacas) # Poblacion de alpacas - Capital
lndata$empleo <- log(lndata$empleo) # Empleo agricola - Trabajo
lndata$precipitacion <- log(lndata$precipitacion) # Precipitacion Acumulada Agregada
lndata$temperatura <- log(lndata$temperatura) # Temperatura Media
# Se renombra a las variables
lndata <- lndata %>% rename("lnProduccion"=1, "lnNaturaleza"=2, "lnCapital"=3, 
                            "lnTrabajo"=4, "lnPrecipitacion"=5, 
                            "lnTemperatura"=6)
# Se aplica el formato de series de tiempo
tsdata <- ts(lndata, start = 1992, end = 2022, frequency = 1)
# Se observa el comportamiento temporal de las variables en Logaritmos
plot.ts(tsdata, plot.type = "multiple", 
        main = "Comportamiento Temporal de las Variables/Dimensiones en Logaritmos",
        xlab = "Periodo 1992-2022 (Anual)")
# Correlaciones previas
chart.Correlation(lndata, main="Correlacion de las Variables en Logaritmos") # En Logaritmos
chart.Correlation(tsdata, main="Correlacion de las Variables en Series Temporales") # En series de tiempo
# Se estima el Modelo de Produccion Cobb-Douglas sin Tendencia
CDmodel1 <- dynlm(lnProduccion ~ lnNaturaleza + lnCapital + lnTrabajo + 
                   lnPrecipitacion + lnTemperatura, 
                 data = tsdata)
CDmodel1 %>% summary()
# Se estima el Modelo de Produccion Cobb-Douglas con Tendencia
CDmodel2 <- dynlm(lnProduccion ~ lnNaturaleza + lnCapital + lnTrabajo + 
                    lnPrecipitacion + lnTemperatura + trend(tsdata), 
                  data = tsdata)
CDmodel2 %>% summary()
# Criterios de Informacion
AIC(CDmodel1)
AIC(CDmodel2)
BIC(CDmodel1)
BIC(CDmodel2)
# Comparacion
stargazer(CDmodel1, CDmodel2, type = "text")

#### Analisis del Modelo Estimado ####
# Definicion de simbolos
def_sym(N, K, L, Prec, Temp, t)
# Definicion de los Parametros
b1 <- CDmodel$coefficients[2]
b2 <- CDmodel$coefficients[3]
b3 <- CDmodel$coefficients[4]
b4 <- CDmodel$coefficients[5]
b5 <- CDmodel$coefficients[6]
b6 <- CDmodel$coefficients[7]
# Definicion del Modelo Cobb-Douglas
Y <- (N^b1)*(K^b2)*(L^b3)*(Prec^b4)*(Temp^b5)
# Rendimientos de Escala
re <- b1+b2+b3+b4+b5 # Existen Rendimientos Crecientes a Escala
# Productividad Marginal
PMgN <- der(Y, N) # Productividad Marginal del Factor Naturaleza
PMgK <- der(Y, K) # Productividad Marginal del Factor Capital
PMgL <- der(Y, L) # Productividad Marginal del Factor Trabajo
PMgPrec <- der(Y, Prec) # Productividad Marginal del Factor Precipitacion
PMgTemp <- der(Y, Temp) # Productividad Marginal del Factor Temperatura
# Tasa Marginal de Sustitucion Tecnica
TMST <- PMgL/PMgK

#### Prueba de Raiz Unitaria : Dickey Fuller Aumentada ####
# Se realiza el test de raiz unitaria a cada variable con el proposito de 
# comprobar la estacionariedad
### Variable Dependiente - Produccion
ur.df(lndata$lnProduccion, type=c("trend"), selectlags = c("AIC")) %>% summary() # No estacionaria
#---
### Variable Independiente - Naturaleza
ur.df(lndata$lnNaturaleza, type=c("trend"), selectlags = c("AIC")) %>% summary() # No Estacionaria
### Variable Independiente - Capital
ur.df(lndata$lnCapital, type=c("trend"), selectlags = c("AIC")) %>% summary() # No estacionaria
### Variable Independiente - Trabajo
ur.df(lndata$lnTrabajo, type=c("trend"), selectlags = c("AIC")) %>% summary() # No estacionaria
### Variable Exogena - Precipitacion
ur.df(lndata$lnPrecipitacion, type=c("trend"), selectlags = c("AIC")) %>% summary() # No estacionaria
### Variable Exogena - Temperatura
ur.df(lndata$lnTemperatura, type=c("trend"), selectlags = c("AIC")) %>% summary() # Estacionaria

#### Prueba de COintegracion de Engel-Granger Aumentada ####
# Se considera el modelo
CDmodel %>% summary()
# Se aplica el Test de Engle-Granger Aumentada aplicando a los residuos el 
# Test de Dickey-Fuller Aumentada
res <- resid(CDmodel)
ur.df(res, type = c("trend"), selectlags = c("AIC")) %>% summary() # Si existe cointegracion

#### Modelo de Correccion de Errores ####
# Al existir cointegracion entre las variables, el modelo original se considera 
# como modelo de Largo Plazo y sus residuos se rezagan para completar el 
# Modelo de Correccion de Errores.
# Se genera el Modelo de Correcion de Error (corto plazo)
mce <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo) 
             + d(lnPrecipitacion) + d(lnTemperatura) + L(res),
             data = tsdata)
summary(mce)


# --------------------------------------------------------

#### Modelo de Correccion de Errores II ####
# Se calculan los modelos y los residuos del largo plazo
lpmodel <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo) 
                 + d(lnPrecipitacion) + d(lnTemperatura), 
                 data = tsdata)
lpmodel %>% summary()
lpres <- resid(lpmodel)
# Se genera el Modelo de Correcion de Error (corto plazo)
mce <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo) 
             + d(lnPrecipitacion) + d(lnTemperatura) + L(lpres),
             data = tsdata)
summary(mce)





