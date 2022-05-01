################################################################################
#    Tortugas marinas vs terrestres primera parte estudio de la base de datos  #
################################################################################


################################################################################
############  Estdio de la base de datos desde el año 2000-2021   ##############
################################################################################

dt_0021 <- df_tortugas2 %>% group_by(especie) %>% 
  summarise(n = n());dt_0021

tabla <- dt_0021  %>% 
  summarise(Especies=especie,
            n_tortugas=n,
            Porcentaje=((n/sum(n))*100)) %>% 
  mutate(Porcentaje=round(Porcentaje,3),
         Marinas = c("Si","No","Si","No","No",
                     "Si","Si","Si")) %>% 
  group_by(Marinas) %>% 
  arrange(desc(n_tortugas));as.data.frame(tabla);view(tabla)


write.xlsx(as.data.frame(tabla), file = "C:/Users/jcge9/Desktop/TFG/TFG_R/tabla_excel.xlsx")

################################################################################
##########################   Modelo de ARIMA   #################################
################################################################################

library(quantmod)
library(tseries)
library(fImport)
library(parsedate)
require(tseries)
library(dplyr)
library(aTSA)
library(stats)
library(forecast)
library(fpp2)


# extracción de los datos temporales
tor_anio_mes <- expand.grid(anio=as.character(levels(df_tortugas_marinas$anio)),
                            mes=as.character(levels(df_tortugas_marinas$mes))) %>% left_join(
                              df_tortugas_marinas %>% group_by(anio,mes) %>% summarize(n=n()),
                              by=c("anio","mes")
                            ) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  arrange(anio)

# datos<-df_tortugas_marinas %>%
#    group_by(format(fecha,"%Y-%m")) %>%
#    summarize(n=n())
# plot(datos$n, type="line")

datos <- tor_anio_mes[,"n"]


# Transformación de los datos en series temporales
road_to_arima <- ts(datos, start = c(2000,1), frequency=12)
road_to_arima
plot(road_to_arima, type="l",
     main="Gráfico de series temporales",
     xlab="Años",
     ylab="Núm.tortugas")

# Visualización de los datos, test de Dickey-Fuller p<0.05 los datos son estacionarios
adf.test(road_to_arima)
ndiffs(road_to_arima)


# Autocorrelación y autocorrelación parcial
# Objetivo sacar los moving averages (MA)--> ACF y los autoregresivos (AR) --> PACF

# Modelo ARIMA, función autimática de R para sacar los coeficientes anteriores

# x_modelo <- auto.arima(road_to_arima, stepwise = F, approximation = F, trace = T)
# Mejor modelo: ARIMA(2,0,2)(0,0,1) with non-zero mean 
# estacionales estacional: (orden 0 ar, orden 0 diff, orden 1 ma))

x_modelo <- arima(road_to_arima, 
                  order = c(2,0,2),
                  seasonal = c(0,0,1))
summary(x_modelo)



### Comprobar si el modelo es bueno
### test de Ljung-Box p>0.05, el modelo es válido
### El error tiene una media entorno a 0
### En principio el modelo es bueno

tsdiag(x_modelo)
Box.test(residuals(x_modelo))
error.2=residuals(x_modelo)
plot(error.2)

# Como se ajusta el modelo a nuestros datos originales? --> Observados vs esperados
plot(road_to_arima, col="red", 
     main ="ARIMA, observados vs esperados",
     xlab=expression(bold(tiempo)),
     ylab=expression(bold(Núm.tortugas)))
lines(fitted(x_modelo),co="blue")

# Predicción
pronostico <- (forecast(x_modelo,h=12));pronostico
plot(pronostico, main="Pronóstico SARIMA (2,0,3) (0,0,1) [12]",
     xlab=expression(bold(Tiempo)),
     ylab=expression(bold(Núm.tortugas)))

write.xlsx(pronostico, 'pronostico.xlsx')

