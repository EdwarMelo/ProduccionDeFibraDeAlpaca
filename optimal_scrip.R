################################################################################
# Produccion de Fibra de Alpaca y sus Factores Productivos en un Escenario de 
# Cambio Climatico, periodo 1992-2022
################################################################################

#### Configuraciones Iniciales ####
rm(list = ls())
setwd("D:/TESIS/git/backup_git")
getwd()
dir()

#### Carga de librerias ####
library(dplyr); library(wooldridge); library(zoo); library(quantmod)
library(dynlm); library(lmtest); library(car); library(stargazer)
library(sandwich); library(tseries); library(urca); library(vars)
library(ggplot2); library(lubridate); library(tidyverse)
library(PerformanceAnalytics); library(Ryacas); library(caracas)
library(devtools); library(reticulate); library(AICcmodavg)
library(slider); library(TTR); library(knitr); library(fUnitRoots)
library(ggpubr); library(patchwork); library(forecast)
library(tidyr); library(GGally)

#### Paleta de colores institucional ####
col_primario   <- "#89371C"   # Terracota oscuro
col_secundario <- "#7B2E43"   # Burdeos
col_oscuro     <- "#1D1D1B"   # Negro suave
col_neutro     <- "#5F5248"   # Gris cálido
col_claro      <- "#FCEDD9"   # Crema

#### Se carga el dataset desde el repositorio de GitHub ####
datos <- read.csv(
  "https://raw.githubusercontent.com/EdwarMelo/ProduccionDeFibraDeAlpaca/refs/heads/main/data_ts.csv",
  colClasses = c("character", rep("numeric", 6)))

#### Estadisticos Descriptivos ####
estadisticos <- data.frame(
  Media  = sapply(datos[,-1], mean),
  Mediana= sapply(datos[,-1], median),
  SD     = sapply(datos[,-1], sd),
  Var    = sapply(datos[,-1], var),
  Min    = sapply(datos[,-1], min),
  Max    = sapply(datos[,-1], max),
  CV     = sapply(datos[,-1], function(x) (sd(x) / mean(x)) * 100)
) %>% round(3)

print(estadisticos)

# Generacion de dataframe
df1 <- datos %>%
  rename("Periodo (A)"      = 1, "Produccion (t)"    = 2,
         "Naturaleza (Mha)" = 3, "Capital (M)"       = 4,
         "Trabajo (M)"      = 5, "Precipitacion (mm)"= 6,
         "Temperatura (°C)" = 7)

#### Tema base ggplot2 ####
tema_base <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold",
                                     size = 14, color = col_oscuro),
    plot.subtitle     = element_text(hjust = 0.5, color = col_neutro, 
                                     size = 11),
    plot.caption      = element_text(color = col_neutro, size = 9),
    strip.text        = element_text(face = "bold", size = 11, 
                                     color = col_oscuro),
    strip.background  = element_rect(fill = col_claro, color = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(linetype = "dotted", color = "#E0D5C8"),
    axis.line.x       = element_line(color = col_neutro, linewidth = 0.4),
    axis.line.y       = element_line(color = col_neutro, linewidth = 0.4),
    axis.text         = element_text(color = col_neutro),
    axis.title        = element_text(color = col_oscuro),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(color = col_oscuro, size = 10)
  )

#### Graficos facet_wrap — comportamiento temporal ####
df1$`Periodo (A)` <- as.numeric(df1$`Periodo (A)`)

datos_largo <- df1 %>%
  pivot_longer(cols = -`Periodo (A)`,
               names_to = "variable", values_to = "valor") %>%
  mutate(variable = factor(variable, levels = c(
    "Produccion (t)", "Naturaleza (Mha)", "Capital (M)",
    "Trabajo (M)", "Precipitacion (mm)", "Temperatura (°C)")))

ggplot(datos_largo, aes(x = `Periodo (A)`, y = valor)) +
  geom_line(color = col_primario, linewidth = 1.1) +
  geom_point(color = col_secundario, size = 1.8, shape = 21,
             fill = col_claro, stroke = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1992, 2022, by = 6)) +
  labs(title = "Comportamiento Temporal de la Producción, sus Factores Productivos y los Factores Exogenos de Cambio Climatico",
       y = NULL, x = NULL) +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Función auxiliar — gráfico serie + media móvil + volatilidad ####
fecha <- as.Date(paste0(datos$tiempo, "-01-01"))
df2   <- data.frame(fecha,
                    prod = datos$produccion, nat = datos$pastizales,
                    cap  = datos$alpacas,    trab = datos$empleo,
                    prec = datos$precipitacion, temp = datos$temperatura)

grafico_serie <- function(data, var, etiqueta_y, titulo,
                          ylim_serie, ylim_var, decimales = 0) {
  g1 <- ggplot(data, aes(x = fecha)) +
    geom_bar(aes(y = .data[[var]]), stat = "identity",
             fill = col_claro, alpha = 0.6, width = 120) +
    geom_line(aes(y = .data[[var]], color = etiqueta_y),
              linewidth = 1) +
    geom_line(aes(y = SMA(.data[[var]], n = 3), color = "Media Móvil"),
              linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = runSD(.data[[var]], 3), color = "Volatilidad"),
              linewidth = 1) +
    geom_text(aes(y = .data[[var]],
                  label = round(.data[[var]], decimales)),
              vjust = -0.8, size = 3, color = col_oscuro,
              check_overlap = TRUE) +
    scale_color_manual(values = c(col_primario, col_secundario, 
                                  col_neutro) %>%
                         setNames(c(etiqueta_y, "Media Móvil", 
                                    "Volatilidad"))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
                 expand = expansion(mult = 0.01)) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.08))) +
    coord_cartesian(ylim = ylim_serie) +
    labs(x = "Periodo", y = etiqueta_y, color = NULL) +
    tema_base +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  
  g2 <- ggplot(data, aes(x = fecha)) +
    geom_line(aes(y = abs((.data[[var]] - SMA(.data[[var]], 3)) /
                            SMA(.data[[var]], 3)) * 100,
                  color = "Variación (%)"),
              linewidth = 1) +
    geom_line(aes(y = mean(abs((.data[[var]] - SMA(.data[[var]], 3)) /
                                 SMA(.data[[var]], 3)) * 100,
                           na.rm = TRUE),
                  color = "Promedio"),
              linewidth = 1, linetype = "dashed") +
    geom_text(aes(y = abs((.data[[var]] - SMA(.data[[var]], 3)) /
                            SMA(.data[[var]], 3)) * 100,
                  label = round(abs((.data[[var]] - SMA(.data[[var]], 3)) /
                                      SMA(.data[[var]], 3)) * 100, 1)),
              vjust = -0.5, size = 3, color = col_oscuro,
              check_overlap = TRUE) +
    scale_color_manual(values = c("Variación (%)" = col_primario,
                                  "Promedio"       = col_secundario)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 limits = c(as.Date("1991-10-31"), as.Date("2022-08-31")),
                 expand = expansion(mult = 0.01)) +
    coord_cartesian(ylim = ylim_var) +
    labs(y = paste0("Variación (%)"), x = NULL, color = NULL) +
    tema_base +
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  
  g1 / g2 +
    plot_annotation(
      title = titulo,
      subtitle = "Serie original · Media móvil (3 años) · Volatilidad",
      theme = theme(
        plot.title    = element_text(hjust = 0.5, face = "bold",
                                     size = 14, color = col_oscuro),
        plot.subtitle = element_text(hjust = 0.5, color = col_neutro, 
                                     size = 10)
      )) +
    plot_layout(heights = c(3, 1))
}

# Producción
grafico_serie(df2, "prod", "Producción (t)",
              "Comportamiento de la Producción (1992–2022)",
              c(0, 5500), c(0, 17))
# Naturaleza
grafico_serie(df2, "nat", "Naturaleza (Mha)",
              "Comportamiento de la Naturaleza (1992–2022)",
              c(0, 27), c(0, 1.5), decimales = 1)
# Capital
grafico_serie(df2, "cap", "Capital (M)",
              "Comportamiento del Capital (1992–2022)",
              c(0, 6.5), c(0, 7), decimales = 1)
# Trabajo
grafico_serie(df2, "trab", "Trabajo (M)",
              "Comportamiento del Trabajo (1992–2022)",
              c(0, 7), c(0, 12), decimales = 1)
# Precipitación
grafico_serie(df2, "prec", "Precipitación (mm)",
              "Comportamiento de la Precipitación (1992–2022)",
              c(0, 2500), c(0, 20))
# Temperatura
grafico_serie(df2, "temp", "Temperatura (°C)",
              "Comportamiento de la Temperatura (1992–2022)",
              c(0, 27), c(0, 5), decimales = 1)

#### Función auxiliar — histograma + violin ####
grafico_dist <- function(data, var, etiqueta_x, titulo) {
  media_val  <- mean(data[[var]], na.rm = TRUE)
  mediana_val <- median(data[[var]], na.rm = TRUE)
  
  g_hist <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 31, fill = col_primario,
                   color = col_claro, alpha = 0.8) +
    geom_density(color = col_secundario, linewidth = 1) +
    geom_vline(xintercept = media_val,
               linetype = "dashed", color = col_oscuro, linewidth = 0.8) +
    annotate("text", x = media_val, y = Inf,
             label = paste0("Media: ", round(media_val, 2)),
             vjust = 2, hjust = -0.1, size = 3.8, color = col_primario) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = etiqueta_x, y = NULL) +
    tema_base +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  g_viol <- ggplot(data, aes(y = .data[[var]], x = " ")) +
    geom_violin(fill = col_primario, color = NA,
                alpha = 0.25, adjust = 1.1) +
    geom_boxplot(width = 0.18, fill = col_claro,
                 color = col_primario, linewidth = 0.8,
                 outlier.shape = 21, outlier.fill = col_secundario,
                 outlier.color = col_claro, outlier.size = 2.5) +
    stat_summary(fun = mean, geom = "point",
                 shape = 23, size = 3,
                 fill = col_secundario, color = col_claro) +
    annotate("text", x = 1.35, y = media_val,
             label = paste0("Media: ", round(media_val, 2)),
             hjust = 1, size = 3.5, color = col_primario) +
    annotate("text", x = 1.35, y = mediana_val,
             label = paste0("Mediana: ", round(mediana_val, 2)),
             hjust = 0, size = 3.5, color = col_secundario) +
    labs(x = NULL, y = etiqueta_x) +
    tema_base +
    theme(legend.position = "none") +
    coord_flip()
  
  g_hist / g_viol +
    plot_annotation(
      title = titulo,
      subtitle = "Distribución empírica · Medidas de posición",
      theme = theme(
        plot.title    = element_text(hjust = 0.5, face = "bold",
                                     size = 14, color = col_oscuro),
        plot.subtitle = element_text(hjust = 0.5, color = col_neutro, 
                                     size = 10)
      )) +
    plot_layout(heights = c(2, 1))
}

# Produccion
grafico_dist(df2, "prod", "Producción (t)",
             "Distribución de la Producción (1992–2022)")
# Naturaleza
grafico_dist(df2, "nat",  "Naturaleza (Mha)",
             "Distribución de la Naturaleza (1992–2022)")
# Capital
grafico_dist(df2, "cap",  "Capital (M)",
             "Distribución del Capital (1992–2022)")
# Trabajo
grafico_dist(df2, "trab", "Trabajo (M)",
             "Distribución del Trabajo (1992–2022)")
# Precipitacion
grafico_dist(df2, "prec", "Precipitación (mm)",
             "Distribución de la Precipitación (1992–2022)")
# Temperatura
grafico_dist(df2, "temp", "Temperatura (°C)",
             "Distribución de la Temperatura (1992–2022)")

#### Funciones de Autocorrelación ACF / PACF ####
tema_acf <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold",
                                    size = 12, color = col_oscuro),
    axis.line        = element_line(color = col_neutro, linewidth = 0.4),
    axis.text        = element_text(color = col_neutro),
    axis.title       = element_text(color = col_oscuro, size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", color = "#E0D5C8")
  )

grafico_acf <- function(serie_ts, nombre, intervalo) {
  
  g_acf <- ggAcf(serie_ts, ci = 0) +
    geom_segment(lineend = "round", color = col_primario, linewidth = 1) +
    geom_hline(yintercept =  intervalo, linetype = "dashed",
               color = col_secundario, linewidth = 0.7) +
    geom_hline(yintercept = -intervalo, linetype = "dashed",
               color = col_secundario, linewidth = 0.7) +
    geom_hline(yintercept = 0, color = col_neutro, linewidth = 0.4) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -intervalo, ymax = intervalo,
             fill = col_secundario, alpha = 0.08) +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.25)) +
    labs(title = "Autocorrelación (ACF)", x = "Rezagos", y = "ACF") +
    tema_acf
  
  g_pacf <- ggPacf(serie_ts, ci = 0) +
    geom_segment(lineend = "round", color = col_primario, linewidth = 1) +
    geom_hline(yintercept =  intervalo, linetype = "dashed",
               color = col_secundario, linewidth = 0.7) +
    geom_hline(yintercept = -intervalo, linetype = "dashed",
               color = col_secundario, linewidth = 0.7) +
    geom_hline(yintercept = 0, color = col_neutro, linewidth = 0.4) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -intervalo, ymax = intervalo,
             fill = col_secundario, alpha = 0.08) +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.25)) +
    labs(title = "Autocorrelación Parcial (PACF)", x = "Rezagos", 
         y = "PACF") +
    tema_acf
  
  g_acf / g_pacf +
    plot_annotation(
      title   = paste0("Función de Autocorrelación y Autocorrelacion Parcial · ", nombre, " (1992–2022)"),
      caption = "Bandas de confianza al 95%  ·  Zona sombreada: no significancia",
      theme   = theme(
        plot.title   = element_text(hjust = 0.5, face = "bold",
                                    size = 14, color = col_oscuro),
        plot.caption = element_text(color = col_neutro, size = 9, hjust = 0.5)
      ))
}

vars_acf <- list(
  list(ts(df1[["Produccion (t)"]],     start = 1992, end = 2022, 
          frequency = 1), "Producción"),
  list(ts(df1[["Naturaleza (Mha)"]],   start = 1992, end = 2022, 
          frequency = 1), "Naturaleza"),
  list(ts(df1[["Capital (M)"]],        start = 1992, end = 2022, 
          frequency = 1), "Capital"),
  list(ts(df1[["Trabajo (M)"]],        start = 1992, end = 2022, 
          frequency = 1), "Trabajo"),
  list(ts(df1[["Precipitacion (mm)"]], start = 1992, end = 2022, 
          frequency = 1), "Precipitación"),
  list(ts(df1[["Temperatura (°C)"]],   start = 1992, end = 2022, 
          frequency = 1), "Temperatura")
)

for (v in vars_acf) {
  intervalo <- 1.96 / sqrt(length(v[[1]]))
  print(grafico_acf(v[[1]], v[[2]], intervalo))
}

#### Correlación entre variables ####
df3 <- df1[, -1]
chart.Correlation(df3, method = "pearson",  main = "Correlación · Pearson")
chart.Correlation(df3, method = "spearman", main = "Correlación · Spearman")

#### Estimación del Modelo Cobb-Douglas ####
lndata <- datos[, -1] %>%
  mutate(across(everything(), log)) %>%
  rename("lnProduccion" = 1, "lnNaturaleza" = 2, "lnCapital"      = 3,
         "lnTrabajo"    = 4, "lnPrecipitacion" = 5, "lnTemperatura" = 6)

tsdata <- ts(lndata, start = 1992, end = 2022, frequency = 1)

CDmodel1 <- dynlm(lnProduccion ~ lnNaturaleza + lnCapital + lnTrabajo +
                    lnPrecipitacion + lnTemperatura, data = tsdata)

CDmodel2 <- dynlm(lnProduccion ~ lnNaturaleza + lnCapital + lnTrabajo +
                    lnPrecipitacion + lnTemperatura + trend(tsdata), 
                  data = tsdata)

stargazer(CDmodel1, CDmodel2, type = "text",
          title         = "Modelos Cobb-Douglas — Producción de Fibra de Alpaca",
          column.labels = c("Sin tendencia", "Con tendencia"),
          dep.var.label = "ln(Producción)",
          covariate.labels = c("ln(Naturaleza)", "ln(Capital)", 
                               "ln(Trabajo)", "ln(Precipitación)", 
                               "ln(Temperatura)", "Tendencia"),
          digits       = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          add.lines    = list(
            c("AIC", round(AIC(CDmodel1), 3), round(AIC(CDmodel2), 3)),
            c("BIC", round(BIC(CDmodel1), 3), round(BIC(CDmodel2), 3)),
            c("Durbin-Watson",
              round(dwtest(CDmodel1)[[1]][[1]], 3),
              round(dwtest(CDmodel2)[[1]][[1]], 3)),
            c("DW (p-valor)",
              round(dwtest(CDmodel1)[[4]], 3),
              round(dwtest(CDmodel2)[[4]], 3))))

#### Análisis del Modelo Estimado ####
def_sym(N, K, L, Prec, Temp, t)

b0 <- CDmodel2$coefficients[1]; b1 <- CDmodel2$coefficients[2]
b2 <- CDmodel2$coefficients[3]; b3 <- CDmodel2$coefficients[4]
b4 <- CDmodel2$coefficients[5]; b5 <- CDmodel2$coefficients[6]
b6 <- CDmodel2$coefficients[7]

Y    <- exp(b0) * (N^b1) * (K^b2) * (L^b3) * (Prec^b4) * (Temp^b5) * exp(b6 * t)
re   <- b1 + b2 + b3

cat("Rendimientos a escala:", round(re, 3),
    ifelse(re > 1, "→ Crecientes",
           ifelse(re < 1, "→ Decrecientes", "→ Constantes")), "\n")

PMgN    <- der(Y, N)
PMgK    <- der(Y, K)
PMgL    <- der(Y, L)
PMgPrec <- der(Y, Prec)
PMgTemp <- der(Y, Temp)
TMST    <- abs(PMgL / PMgK)

#### Prueba de Raíz Unitaria — ADF en niveles ####
ur.df(lndata$lnProduccion,    type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnNaturaleza,    type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnCapital,       type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnTrabajo,       type = "trend", selectlags = "AIC") %>% summary()
ur.df(lndata$lnPrecipitacion, type = "drift", selectlags = "AIC") %>% summary()
ur.df(lndata$lnTemperatura,   type = "drift", selectlags = "AIC") %>% summary()
# Resultado: I(1) todas excepto lnTemperatura → I(0)

#### ADF en primeras diferencias — confirmar I(1) ####
ur.df(diff(lndata$lnProduccion),    type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnNaturaleza),    type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnCapital),       type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnTrabajo),       type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(lndata$lnPrecipitacion), type = "none",  selectlags = "AIC") %>% summary()
# Resultado: todas estacionarias en diferencias → I(1) confirmado

#### Modelo de largo plazo — variables I(1) ####
CDmodel_LP <- dynlm(lnProduccion ~ lnNaturaleza + lnCapital + lnTrabajo +
                      lnPrecipitacion + trend(tsdata), data = tsdata)
summary(CDmodel_LP)

#### Test de Cointegración Engle-Granger ####
res <- resid(CDmodel_LP)
ur.df(res, type = "none", selectlags = "AIC") %>% summary()
# tau < -1.95 → existe cointegración → relación de largo plazo válida

#### Modelo de Corrección de Errores ####
mce1 <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo)
              + d(lnPrecipitacion) + lnTemperatura + L(res) + trend(tsdata),
              data = tsdata)

mce2 <- dynlm(d(lnProduccion) ~ d(lnNaturaleza) + d(lnCapital) + d(lnTrabajo)
              + d(lnPrecipitacion) + lnTemperatura + L(res),
              data = tsdata)

stargazer(mce1, mce2, type = "text",
          title         = "Modelo de Corrección de Errores (MCE)",
          column.labels = c("MCE con tendencia", "MCE sin tendencia"),
          dep.var.label = "Δln(Producción)",
          covariate.labels = c("Δln(Naturaleza)", "Δln(Capital)", 
                               "Δln(Trabajo)", "Δln(Precipitación)", 
                               "ln(Temperatura)", "ECT (−1)", "Tendencia"),
          digits       = 4,
          star.cutoffs = c(0.1, 0.05, 0.01),
          add.lines    = list(
            c("AIC", round(AIC(mce1), 3), round(AIC(mce2), 3)),
            c("BIC", round(BIC(mce1), 3), round(BIC(mce2), 3))))

ect <- coef(summary(mce2))["L(res)", ]
cat("\n══════════════════════════════════════════\n")
cat(" Mecanismo de Corrección de Errores (ECT)\n")
cat("══════════════════════════════════════════\n")
cat(" Coeficiente ECT :", round(ect["Estimate"],   4), "\n")
cat(" p-valor         :", round(ect["Pr(>|t|)"],   4), "\n")
cat(" Velocidad ajuste:", abs(round(ect["Estimate"] * 100, 1)), 
    "% por año\n")
cat("══════════════════════════════════════════\n")
