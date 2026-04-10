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
library(fUnitRoots)
library(ggpubr)
library(patchwork)
library(forecast)
library(tidyr)
library(GGally)

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
# Se genera el dataframne para observar el comportamiento de las 
# variables/dimensiones
df1 <- datos
df1 <- df1 %>% rename("Periodo (A)"=1,"Produccion (t)"=2, 
                      "Naturaleza (Mha)"=3,"Capital (M)"=4, 
                      "Trabajo (M)"=5, "Precipitacion (mm)"=6,
                      "Temperatura (°C)"=7)

#### Graficos Descriptivos ####
# Se genera un dataframe en formato largo con el proposito de observar el 
# comportamiento de las variables/dimensiones
df1$`Periodo (A)` <- as.numeric(df1$`Periodo (A)`)
datos_largo <- df1 %>%
  pivot_longer(
    cols = -`Periodo (A)`,
    names_to = "variable",
    values_to = "valor") %>%
  mutate(variable = factor(variable, levels = c(
    "Produccion (t)", "Naturaleza (Mha)", "Capital (M)",
    "Trabajo (M)", "Precipitacion (mm)", "Temperatura (°C)")))
# 
ggplot(datos_largo, aes(x = `Periodo (A)`, y = valor)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_point(color = "red", size = 1.5) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1992, 2022, by = 6)) +
  labs(
    title = "Comportamiento Temporal de la Produccion y sus Factores Productivos",
    y = NULL, x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "lightgreen", color = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))

# Generamos otro dataframe en proposito de generar los graficos descriptivos
fecha <- as.Date(datos$tiempo, format = "%Y")
prod <- datos$produccion
nat <- datos$pastizales
cap <- datos$alpacas
trab <- datos$empleo
prec <- datos$precipitacion
temp <- datos$temperatura
# 
df2 <- data.frame(fecha,prod,nat,cap,trab,prec,temp)
# 
# Graficos del Comportamiento de las Variables/Dimensiones, sus Medias Moviles
# y su Volatilidad en una ventana de tres años, y la Variacion Porcentual con 
# respecto a su Media Movil
# Produccion
p1 <- ggplot(df2, aes(x = fecha)) +
  geom_bar(aes(y = prod), stat = "identity", fill = "lightgreen", 
           alpha = 0.5, width = 250) +
  geom_line(aes(y = prod, color = "Producción"), size = 1) +
  geom_line(aes(y = SMA(prod, n = 3), color = "Media Movil"), size = 1) +
  geom_line(aes(y = runSD(prod, 3), color = "Volatilidad"), size = 1) +
  geom_text(aes(y = prod, label = round(prod)), vjust = -1, 
            size = 3.5, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Producción" = "darkblue",
                                "Media Movil" = "red", 
                                "Volatilidad" = "purple" )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(x = "Periodo", y = "Producción (t)", color = "Leyenda") + 
  coord_cartesian(ylim = (c(0,5500)))+
  theme_minimal(base_size = 12)+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
p2 <- ggplot(df2, aes(x = fecha)) +
  geom_line(aes(y = abs((prod - SMA(prod, n = 3))/(SMA(prod, n = 3)))*100, 
                color = "Variacion Porcentual\nde la Produccion\ncon respecto a la\nMedia Movil"), 
            size=1) +
  geom_line(aes(y = mean(abs((prod - SMA(prod, n = 3))/(SMA(prod, n = 3)))*100, 
                         na.rm = T), 
                color = "Promedio de la\nMedia Movil\n "), 
            size = 0.7) +
  geom_text(aes(y = abs((prod - SMA(prod, n = 3))/(SMA(prod, n = 3)))*100, 
                label = round(abs((prod - SMA(prod, n = 3))/(SMA(prod, n=3)))*100,1)), 
            vjust = -0.5, size = 3.5, 
            check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Variacion Porcentual\nde la Produccion\ncon respecto a la\nMedia Movil" = "orange2",
    "Promedio de la\nMedia Movil\n " = "green3")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(y = "Variacion de la Producción (%)", color = "Leyenda") + 
  coord_cartesian(ylim=(c(0,16))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
p1 / p2 + 
  plot_annotation(title = "Comportamiento de la Produccion (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14))) + 
  plot_layout(heights = c(3, 1))
# Naturaleza
n1 <- ggplot(df2, aes(x = fecha)) +
  geom_bar(aes(y = nat), stat = "identity", fill = "lightgreen", 
           alpha = 0.5, width = 250) +
  geom_line(aes(y = nat, color = "Naturaleza"), size = 1) +
  geom_line(aes(y = SMA(nat, n = 3), color = "Media Movil"), size = 1) +
  geom_line(aes(y = runSD(nat, 3), color = "Volatilidad"), size = 1) +
  geom_text(aes(y = nat, label = round(nat,1)), vjust = -1, 
            size = 3.5, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Naturaleza" = "darkblue",
                                "Media Movil" = "red", 
                                "Volatilidad" = "purple" )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(x = "Periodo", y = "Naturaleza (Mha)", color = "Leyenda") + 
  coord_cartesian(ylim = (c(0,27)))+
  theme_minimal(base_size = 12)+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
n2 <- ggplot(df2, aes(x = fecha)) +
  geom_line(aes(y = abs((nat - SMA(nat, n = 3))/(SMA(nat, n = 3)))*100, 
                color = "Variacion Porcentual\nde la Naturaleza\ncon respecto a la\nMedia Movil"), 
            size=1) +
  geom_line(aes(y = mean(abs((nat - SMA(nat, n = 3))/(SMA(nat, n = 3)))*100, 
                         na.rm = T), 
                color = "Promedio de la\nMedia Movil\n "), 
            size = 0.7) +
  geom_text(aes(y = abs((nat - SMA(nat, n = 3))/(SMA(nat, n = 3)))*100, 
                label = round(abs((nat - SMA(nat, n = 3))/(SMA(nat, n=3)))*100,1)), 
            vjust = -0.5, size = 3.5, 
            check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Variacion Porcentual\nde la Naturaleza\ncon respecto a la\nMedia Movil" = "orange2",
    "Promedio de la\nMedia Movil\n " = "green3")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(y = "Variacion de la Naturaleza (%)", color = "Leyenda") + 
  coord_cartesian(ylim=(c(0,2))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
n1 + n2 + 
  plot_annotation(title = "Comportamiento de la Naturaleza (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14))) + 
  plot_layout(heights = c(3, 1))
# Capital
c1 <- ggplot(df2, aes(x = fecha)) +
  geom_bar(aes(y = cap), stat = "identity", fill = "lightgreen", 
           alpha = 0.5, width = 250) +
  geom_line(aes(y = cap, color = "Capital"), size = 1) +
  geom_line(aes(y = SMA(cap, n = 3), color = "Media Movil"), size = 1) +
  geom_line(aes(y = runSD(cap, 3), color = "Volatilidad"), size = 1) +
  geom_text(aes(y = cap, label = round(cap,1)), vjust = -1, 
            size = 3.5, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Capital" = "darkblue",
                                "Media Movil" = "red", 
                                "Volatilidad" = "purple" )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(x = "Periodo", y = "Capital (M)", color = "Leyenda") + 
  coord_cartesian(ylim = (c(0,6.5)))+
  theme_minimal(base_size = 12)+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
c2 <- ggplot(df2, aes(x = fecha)) +
  geom_line(aes(y = abs((cap - SMA(cap, n = 3))/(SMA(cap, n = 3)))*100, 
                color = "Variacion Porcentual\ndel Capital\ncon respecto a la\nMedia Movil"), 
            size=1) +
  geom_line(aes(y = mean(abs((cap - SMA(cap, n = 3))/(SMA(cap, n = 3)))*100, 
                         na.rm = T), 
                color = "Promedio de la\nMedia Movil\n "), 
            size = 0.7) +
  geom_text(aes(y = abs((cap - SMA(cap, n = 3))/(SMA(cap, n = 3)))*100, 
                label = round(abs((cap - SMA(cap, n = 3))/(SMA(cap, n=3)))*100,1)), 
            vjust = -0.5, size = 3.5, 
            check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Variacion Porcentual\ndel Capital\ncon respecto a la\nMedia Movil" = "orange2",
    "Promedio de la\nMedia Movil\n " = "green3")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(y = "Variacion del Capital (%)", color = "Leyenda") + 
  coord_cartesian(ylim=(c(0,7))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
c1 + c2 + 
  plot_annotation(title = "Comportamiento del Capital (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14))) + 
  plot_layout(heights = c(3, 1))
# Trabajo
t1 <- ggplot(df2, aes(x = fecha)) +
  geom_bar(aes(y = trab), stat = "identity", fill = "lightgreen", 
           alpha = 0.5, width = 250) +
  geom_line(aes(y = trab, color = "Trabajo"), size = 1) +
  geom_line(aes(y = SMA(trab, n = 3), color = "Media Movil"), size = 1) +
  geom_line(aes(y = runSD(trab, 3), color = "Volatilidad"), size = 1) +
  geom_text(aes(y = trab, label = round(trab,1)), vjust = -1, 
            size = 3.5, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Trabajo" = "darkblue",
                                "Media Movil" = "red", 
                                "Volatilidad" = "purple" )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(x = "Periodo", y = "Trabajo (M)", color = "Leyenda") + 
  coord_cartesian(ylim = (c(0,7)))+
  theme_minimal(base_size = 12)+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
t2 <- ggplot(df2, aes(x = fecha)) +
  geom_line(aes(y = abs((trab - SMA(trab, n = 3))/(SMA(trab, n = 3)))*100, 
                color = "Variacion Porcentual\ndel Trabajo\ncon respecto a la\nMedia Movil"), 
            size=1) +
  geom_line(aes(y = mean(abs((trab - SMA(trab, n = 3))/(SMA(trab, n = 3)))*100, 
                         na.rm = T), 
                color = "Promedio de la\nMedia Movil\n "), 
            size = 0.7) +
  geom_text(aes(y = abs((trab - SMA(trab, n = 3))/(SMA(trab, n = 3)))*100, 
                label = round(abs((trab - SMA(trab, n = 3))/(SMA(trab, n=3)))*100,1)), 
            vjust = -0.5, size = 3.5, 
            check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Variacion Porcentual\ndel Trabajo\ncon respecto a la\nMedia Movil" = "orange2",
    "Promedio de la\nMedia Movil\n " = "green3")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(y = "Variacion del Trabajo (%)", color = "Leyenda") + 
  coord_cartesian(ylim=(c(0,12))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
t1 + t2 + 
  plot_annotation(title = "Comportamiento del Trabajo (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14))) + 
  plot_layout(heights = c(3, 1))
# Precipitacion
pr1 <- ggplot(df2, aes(x = fecha)) +
  geom_bar(aes(y = prec), stat = "identity", fill = "lightgreen", 
           alpha = 0.5, width = 250) +
  geom_line(aes(y = prec, color = "Precipitacion"), size = 1) +
  geom_line(aes(y = SMA(prec, n = 3), color = "Media Movil"), size = 1) +
  geom_line(aes(y = runSD(prec, 3), color = "Volatilidad"), size = 1) +
  geom_text(aes(y = prec, label = round(prec)), vjust = -1, 
            size = 3.5, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Precipitacion" = "darkblue",
                                "Media Movil" = "red", 
                                "Volatilidad" = "purple" )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(x = "Periodo", y = "Precipitacion (mm)", color = "Leyenda") + 
  coord_cartesian(ylim = (c(0,2500)))+
  theme_minimal(base_size = 12)+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
pr2 <- ggplot(df2, aes(x = fecha)) +
  geom_line(aes(y = abs((prec - SMA(prec, n = 3))/(SMA(prec, n = 3)))*100, 
                color = "Variacion Porcentual\nde la Precipitacion\ncon respecto a la\nMedia Movil"), 
            size=1) +
  geom_line(aes(y = mean(abs((prec - SMA(prec, n = 3))/(SMA(prec, n = 3)))*100, 
                         na.rm = T), 
                color = "Promedio de la\nMedia Movil\n "), 
            size = 0.7) +
  geom_text(aes(y = abs((prec - SMA(prec, n = 3))/(SMA(prec, n = 3)))*100, 
                label = round(abs((prec - SMA(prec, n = 3))/(SMA(prec, n=3)))*100,1)), 
            vjust = -0.5, size = 3.5, 
            check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Variacion Porcentual\nde la Precipitacion\ncon respecto a la\nMedia Movil" = "orange2",
    "Promedio de la\nMedia Movil\n " = "green3")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(y = "Variacion de la Precipitacion (%)", color = "Leyenda") + 
  coord_cartesian(ylim=(c(0,20))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
pr1 + pr2 + 
  plot_annotation(title = "Comportamiento de la Precipitacion (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14))) + 
  plot_layout(heights = c(3, 1))
# Temperatura
tm1 <- ggplot(df2, aes(x = fecha)) +
  geom_bar(aes(y = temp), stat = "identity", fill = "lightgreen", 
           alpha = 0.5, width = 250) +
  geom_line(aes(y = temp, color = "Temperatura"), size = 1) +
  geom_line(aes(y = SMA(temp, n = 3), color = "Media Movil"), size = 1) +
  geom_line(aes(y = runSD(temp, 3), color = "Volatilidad"), size = 1) +
  geom_text(aes(y = temp, label = round(temp,1)), vjust = -1, 
            size = 3.5, check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Temperatura" = "darkblue",
                                "Media Movil" = "red", 
                                "Volatilidad" = "purple" )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(x = "Periodo", y = "Temperatura (°C)", color = "Leyenda") + 
  coord_cartesian(ylim = (c(0,27)))+
  theme_minimal(base_size = 12)+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
tm2 <- ggplot(df2, aes(x = fecha)) +
  geom_line(aes(y = abs((temp - SMA(temp, n = 3))/(SMA(temp, n = 3)))*100, 
                color = "Variacion Porcentual\nde la Temperatura\ncon respecto a la\nMedia Movil"), 
            size=1) +
  geom_line(aes(y = mean(abs((temp - SMA(temp, n = 3))/(SMA(temp, n = 3)))*100, 
                         na.rm = T), 
                color = "Promedio de la\nMedia Movil\n "), 
            size = 0.7) +
  geom_text(aes(y = abs((temp - SMA(temp, n = 3))/(SMA(temp, n = 3)))*100, 
                label = round(abs((temp - SMA(temp, n = 3))/(SMA(temp, n=3)))*100,1)), 
            vjust = -0.5, size = 3.5, 
            check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Variacion Porcentual\nde la Temperatura\ncon respecto a la\nMedia Movil" = "orange2",
    "Promedio de la\nMedia Movil\n " = "green3")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y",
               limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
               expand = expansion(mult = 0.01)) +
  labs(y = "Variacion de la Temperatura (%)", color = "Leyenda") + 
  coord_cartesian(ylim=(c(0,5))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.line.y = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
tm1 + tm2 + 
  plot_annotation(title = "Comportamiento de la Temperatura (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          face = "bold",
                                                          size = 14))) + 
  plot_layout(heights = c(3, 1))
# Graficos de Violin y de Cajas para cada una de las Variables/Dimensiones de 
# esta investigacion
# Produccion
p3 <- ggplot(df2, aes(x = prod)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 31, fill = "lightgreen",
                 color = "white", alpha = 0.7) +
  geom_density(color = "purple",
               linewidth = 0.9) +
  geom_vline(xintercept = mean(df2$prod, na.rm = T),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  annotate("text", x = mean(df2$prod, na.rm = TRUE), y = Inf,
           label = paste0("Media: ", round(mean(df2$prod, na.rm = TRUE), 2)),
           vjust = 2, hjust = -0.1, size  = 4, color = "darkblue") +
  labs(x = "Producción (t)",y=NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
p4 <- ggplot(df2, aes(y = prod, x = " ")) +
  geom_violin(fill = "purple", color = "purple", alpha = 0.1, 
              adjust = 1, size = 0.75) +
  geom_boxplot(width = 0.2, fill = "lightgreen", color = "limegreen",
               size = 1, outlier.shape = 21, alpha =0.5,
               outlier.fill = "darkblue", outlier.color = "white",
               outlier.size = 2.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "red", color = "white") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"),
    legend.position  = "none")+
  coord_flip()
# 
p3 / p4 + 
  plot_annotation(title = "Distribución de la Producción (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))

# Naturaleza
n3 <- ggplot(df2, aes(x = nat)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 31, fill = "lightgreen",
                 color = "white", alpha = 0.7) +
  geom_density(color = "purple",
               linewidth = 0.9) +
  geom_vline(xintercept = mean(df2$nat, na.rm = T),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  annotate("text", x = mean(df2$nat, na.rm = TRUE), y = Inf,
           label = paste0("Media: ", round(mean(df2$nat, na.rm = TRUE), 2)),
           vjust = 2, hjust = -0.1, size  = 4, color = "darkblue") +
  labs(x = "Naturaleza (Mha)",y=NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
n4 <- ggplot(df2, aes(y = nat, x = " ")) +
  geom_violin(fill = "purple", color = "purple", alpha = 0.1, 
              adjust = 1, size = 0.75) +
  geom_boxplot(width = 0.2, fill = "lightgreen", color = "limegreen",
               size = 1, outlier.shape = 21, alpha =0.5,
               outlier.fill = "darkblue", outlier.color = "white",
               outlier.size = 2.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "red", color = "white") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"),
    legend.position  = "none")+
  coord_flip()
# 
n3 / n4 + 
  plot_annotation(title = "Distribución de la Naturaleza (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Capital
c3 <- ggplot(df2, aes(x = cap)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 31, fill = "lightgreen",
                 color = "white", alpha = 0.7) +
  geom_density(color = "purple",
               linewidth = 0.9) +
  geom_vline(xintercept = mean(df2$cap, na.rm = T),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  annotate("text", x = mean(df2$cap, na.rm = TRUE), y = Inf,
           label = paste0("Media: ", round(mean(df2$cap, na.rm = TRUE), 2)),
           vjust = 2, hjust = -0.1, size  = 4, color = "darkblue") +
  labs(x = "Capital (M)",y=NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
c4 <- ggplot(df2, aes(y = cap, x = " ")) +
  geom_violin(fill = "purple", color = "purple", alpha = 0.1, 
              adjust = 1, size = 0.75) +
  geom_boxplot(width = 0.2, fill = "lightgreen", color = "limegreen",
               size = 1, outlier.shape = 21, alpha =0.5,
               outlier.fill = "darkblue", outlier.color = "white",
               outlier.size = 2.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "red", color = "white") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"),
    legend.position  = "none")+
  coord_flip()
# 
c3 / c4 + 
  plot_annotation(title = "Distribución del Capital (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Trabajo
t3 <- ggplot(df2, aes(x = trab)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 31, fill = "lightgreen",
                 color = "white", alpha = 0.7) +
  geom_density(color = "purple",
               linewidth = 0.9) +
  geom_vline(xintercept = mean(df2$trab, na.rm = T),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  annotate("text", x = mean(df2$trab, na.rm = TRUE), y = Inf,
           label = paste0("Media: ", round(mean(df2$trab, na.rm = TRUE), 2)),
           vjust = 2, hjust = -0.1, size  = 4, color = "darkblue") +
  labs(x = "Trabajo (M)",y=NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
t4 <- ggplot(df2, aes(y = trab, x = " ")) +
  geom_violin(fill = "purple", color = "purple", alpha = 0.1, 
              adjust = 1, size = 0.75) +
  geom_boxplot(width = 0.2, fill = "lightgreen", color = "limegreen",
               size = 1, outlier.shape = 21, alpha =0.5,
               outlier.fill = "blue", outlier.color = "white",
               outlier.size = 2.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "red", color = "white") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"),
    legend.position  = "none")+
  coord_flip()
# 
t3 / t4 + 
  plot_annotation(title = "Distribución del Trabajo (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Precipitacion
pr3 <- ggplot(df2, aes(x = prec)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 31, fill = "lightgreen",
                 color = "white", alpha = 0.7) +
  geom_density(color = "purple",
               linewidth = 0.9) +
  geom_vline(xintercept = mean(df2$prec, na.rm = T),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  annotate("text", x = mean(df2$prec, na.rm = TRUE), y = Inf,
           label = paste0("Media: ", round(mean(df2$prec, na.rm = TRUE), 2)),
           vjust = 2, hjust = -0.1, size  = 4, color = "darkblue") +
  labs(x = "Precipitacion (mm)",y=NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
pr4 <- ggplot(df2, aes(y = prec, x = " ")) +
  geom_violin(fill = "purple", color = "purple", alpha = 0.1, 
              adjust = 1, size = 0.75) +
  geom_boxplot(width = 0.2, fill = "lightgreen", color = "limegreen",
               size = 1, outlier.shape = 21, alpha =0.5,
               outlier.fill = "blue", outlier.color = "white",
               outlier.size = 2.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "red", color = "white") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"),
    legend.position  = "none")+
  coord_flip()
# 
pr3 / pr4 + 
  plot_annotation(title = "Distribución de la Precipitacion (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Temperatura
tm3 <- ggplot(df2, aes(x = temp)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 31, fill = "lightgreen",
                 color = "white", alpha = 0.7) +
  geom_density(color = "purple",
               linewidth = 0.9) +
  geom_vline(xintercept = mean(df2$temp, na.rm = T),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  annotate("text", x = mean(df2$temp, na.rm = TRUE), y = Inf,
           label = paste0("Media: ", round(mean(df2$temp, na.rm = TRUE), 2)),
           vjust = 2, hjust = -0.1, size  = 4, color = "darkblue") +
  labs(x = "Temperatura (°C)",y=NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"))
# 
tm4 <- ggplot(df2, aes(y = temp, x = " ")) +
  geom_violin(fill = "purple", color = "purple", alpha = 0.1, 
              adjust = 1, size = 0.75) +
  geom_boxplot(width = 0.2, fill = "lightgreen", color = "limegreen",
               size = 1, outlier.shape = 21, alpha =0.5,
               outlier.fill = "blue", outlier.color = "white",
               outlier.size = 2.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "red", color = "white") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray50", linewidth = 0.5),
    axis.text = element_text(color = "gray40"),
    axis.title = element_text(color = "gray30"),
    legend.position  = "none")+
  coord_flip()
# 
tm3 / tm4 + 
  plot_annotation(title = "Distribución de la Temperatura (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
#### Funciones de Autocorrelacion y Autocorrelacion Parcial ####
tema <- list(theme_minimal(base_size = 12),
             theme(plot.title = element_text(hjust = 0.5, face = "bold", 
                                             size = 12),
                   axis.line = element_line(color = "gray70", 
                                            linewidth = 0.4),
                   axis.text = element_text(color = "gray40"),
                   axis.title = element_text(color = "gray30", size = 11)))
# Produccion
p_df <- ts(df1["Produccion (t)"], start = 1992, end = 2022, frequency = 1)
# 
p_intervalo <- 1.96 / sqrt(length(p_df))
# 
p_acf <- ggAcf(p_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  p_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -p_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -p_intervalo, ymax = p_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación (ACF)", x = "Rezagos",y = "ACF") +
  tema
# 
p_pacf <- ggPacf(p_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  p_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -p_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -p_intervalo, ymax = p_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", y = "PACF") +
  tema
# 
p_acf / p_pacf + 
  plot_annotation(title = "Funcion de Autocorrelacion de la Produccion (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Naturaleza
n_df <- ts(df1["Naturaleza (Mha)"], start=1992, end=2022, frequency=1)
# 
n_intervalo <- 1.96 / sqrt(length(n_df))
# 
n_acf <- ggAcf(n_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  n_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -n_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -n_intervalo, ymax = n_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación (ACF)", x = "Rezagos",y = "ACF") +
  tema
# 
n_pacf <- ggPacf(n_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  n_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -n_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -n_intervalo, ymax = n_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", y = "PACF") +
  tema
# 
n_acf / n_pacf + 
  plot_annotation(title = "Funcion de Autocorrelacion de la Naturaleza (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Capital
c_df <- ts(df1["Capital (M)"], start=1992, end=2022, frequency=1)
# 
c_intervalo <- 1.96 / sqrt(length(c_df))
# 
c_acf <- ggAcf(c_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  c_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -c_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -c_intervalo, ymax = c_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación (ACF)", x = "Rezagos",y = "ACF") +
  tema
# 
c_pacf <- ggPacf(c_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  c_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -c_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -c_intervalo, ymax = c_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", y = "PACF") +
  tema
# 
c_acf / c_pacf + 
  plot_annotation(title = "Funcion de Autocorrelacion del Capital (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Trabajo
t_df <- ts(df1["Trabajo (M)"], start=1992, end=2022, frequency=1)
# 
t_intervalo <- 1.96 / sqrt(length(t_df))
# 
t_acf <- ggAcf(t_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  t_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -t_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -t_intervalo, ymax = t_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación (ACF)", x = "Rezagos",y = "ACF") +
  tema
# 
t_pacf <- ggPacf(t_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  t_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -t_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -t_intervalo, ymax = t_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", y = "PACF") +
  tema
# 
t_acf / t_pacf + 
  plot_annotation(title = "Funcion de Autocorrelacion del Trabajo (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Precipitacion
pr_df <- ts(df1["Precipitacion (mm)"], start=1992, end=2022, frequency=1)
# 
pr_intervalo <- 1.96 / sqrt(length(pr_df))
# 
pr_acf <- ggAcf(pr_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  pr_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -pr_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -pr_intervalo, ymax = pr_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación (ACF)", x = "Rezagos",y = "ACF") +
  tema
# 
pr_pacf <- ggPacf(pr_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  pr_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -pr_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -pr_intervalo, ymax = pr_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", y = "PACF") +
  tema
# 
pr_acf / pr_pacf + 
  plot_annotation(title = "Funcion de Autocorrelacion de la Precipitacion (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Temperatura
tm_df <- ts(df1["Temperatura (°C)"], start=1992, end=2022, frequency=1)
# 
tm_intervalo <- 1.96 / sqrt(length(tm_df))
# 
tm_acf <- ggAcf(tm_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  tm_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -tm_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -tm_intervalo, ymax = tm_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación (ACF)", x = "Rezagos",y = "ACF") +
  tema
# 
tm_pacf <- ggPacf(tm_df, ci = 0) +
  geom_segment(lineend = "round", color = "#8B7355", linewidth = 1) +
  geom_hline(yintercept =  tm_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = -tm_intervalo, linetype = "dashed", 
             color = "#a84040", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -tm_intervalo, ymax = tm_intervalo,
           fill = "#a84040", alpha = 0.1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", y = "PACF") +
  tema
# 
tm_acf / tm_pacf + 
  plot_annotation(title = "Funcion de Autocorrelacion de la Temperatura (Periodo 1992-2022)",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold",
                                                          size = 14))) +
  plot_layout(widths = c(2, 2))
# Correlacion entre variables
df3 <- df1[,-1]
chart.Correlation(df3, method = "pearson",
                  main="Correlación de Variables · Pearson")
chart.Correlation(df3, method = "spearman",
                  main="Correlación de Variables · Spearman")

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
vif(CDmodel1)
vif(CDmodel2)
dwtest(CDmodel1)
dwtest(CDmodel2)
# Comparacion
stargazer(CDmodel1, CDmodel2, type = "text",
          title = "Modelos Cobb-Douglas — Producción de Fibra de Alpaca",
          column.labels = c("Sin tendencia", "Con tendencia"),
          dep.var.label = "ln(Producción)",
          covariate.labels = c("ln(Naturaleza)", "ln(Capital)",
                               "ln(Trabajo)", "ln(Precipitación)",
                               "ln(Temperatura)", "Tendencia"),
          digits = 3, star.cutoffs  = c(0.1, 0.05, 0.01),
          add.lines = list(
            c("AIC", round(AIC(CDmodel1),3), round(AIC(CDmodel2),3)),
            c("BIC", round(BIC(CDmodel1),3), round(BIC(CDmodel2),3)),
            c("Durbin-Watson", round(dwtest(CDmodel1)[[1]][[1]],3), 
              round(dwtest(CDmodel2)[[1]][[1]],3)),
            c("DW (p-value)", round(dwtest(CDmodel1)[[4]],3), 
              round(dwtest(CDmodel2)[[4]],3))))

#### Analisis del Modelo Estimado ####
# Definición de símbolos
def_sym(N, K, L, Prec, Temp, t)
# Coeficientes del Modelo 2
b0 <- CDmodel2$coefficients[1]   # Constante
b1 <- CDmodel2$coefficients[2]   # ln(Naturaleza)
b2 <- CDmodel2$coefficients[3]   # ln(Capital)
b3 <- CDmodel2$coefficients[4]   # ln(Trabajo)
b4 <- CDmodel2$coefficients[5]   # ln(Precipitación)
b5 <- CDmodel2$coefficients[6]   # ln(Temperatura)
b6 <- CDmodel2$coefficients[7]   # Tendencia
# Definicion del Modelo Cobb-Douglas
Y <- exp(b0) * (N^b1) * (K^b2) * (L^b3) * (Prec^b4) * (Temp^b5) * exp(b6 * t)
# Rendimientos de Escala
re <- b1 + b2 + b3
cat("Rendimientos a escala:", round(re, 3),
    ifelse(re > 1, "→ Crecientes", ifelse(re < 1, 
                                          "→ Decrecientes", "→ Constantes")))
# Productividad Marginal
PMgN <- der(Y, N) # Productividad Marginal del Factor Naturaleza
PMgK <- der(Y, K) # Productividad Marginal del Factor Capital
PMgL <- der(Y, L) # Productividad Marginal del Factor Trabajo
PMgPrec <- der(Y, Prec) # Productividad Marginal del Factor Precipitacion
PMgTemp <- der(Y, Temp) # Productividad Marginal del Factor Temperatura
# Tasa Marginal de Sustitucion Tecnica
TMST <- abs(PMgL / PMgK)

#### 1. ADF en niveles ####
ur.df(lndata$lnProduccion,    type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnNaturaleza,    type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnCapital,       type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnTrabajo,       type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnPrecipitacion, type = "drift", selectlags = "AIC") %>% summary()
ur.df(lndata$lnTemperatura,   type = "drift", selectlags = "AIC") %>% summary()
# Resultado: todas I(1) excepto lnTemperatura que es I(0)

#### 2. ADF en primeras diferencias — confirmar I(1) ####
ur.df(diff(lndata$lnProduccion),    type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnNaturaleza),    type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnCapital),       type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnTrabajo),       type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnPrecipitacion), type = "none",  selectlags = "AIC") %>% summary()
# Resultado: todas estacionarias en diferencias → I(1) confirmado

#### 3. Modelo de largo plazo — solo variables I(1) ####
CDmodel_LP <- dynlm(lnProduccion ~ lnNaturaleza + lnCapital + lnTrabajo +
                      lnPrecipitacion + trend(tsdata),
                    data = tsdata)
summary(CDmodel_LP)

#### 4. Test de Cointegración Engle-Granger ####
res <- resid(CDmodel_LP)
ur.df(res, type = "none", selectlags = "AIC") %>% summary()
# Si tau < -1.95 → existe cointegración → relación de largo plazo válida

#### 5. Modelo de Corrección de Errores ####
# MCE con tendencia
mce1 <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo)
             + d(lnPrecipitacion) + lnTemperatura
             + L(res) + trend(tsdata),
             data = tsdata)
summary(mce1)
# MCE sin tendencia (más parsimonioso)
mce2 <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo)
              + d(lnPrecipitacion) + lnTemperatura + L(res),
              data = tsdata)
summary(mce2)
# Comparar ambos
AIC(mce1, mce2)
BIC(mce1, mce2)

# Verificar coeficiente ECT
ect <- coef(summary(mce2))["L(res)", ]
cat("\n--- Mecanismo de Corrección de Errores ---\n")
cat("Coeficiente ECT:", round(ect["Estimate"], 4), "\n")
cat("p-valor:        ", round(ect["Pr(>|t|)"], 4), "\n")
cat("Velocidad de ajuste:", abs(round(ect["Estimate"] * 100, 1)), "% por año\n")

