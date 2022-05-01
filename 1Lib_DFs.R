################################################################################
###########    Librerías y data frames que se usan para el estudio    ##########
################################################################################

#---------------------#
#      Librerías      #
#---------------------#

### Librerías para la manipulación de tablas, y visualización de los datos.
library(tidyverse)
library(ggthemes)
library(ggtext)
library(tidytext)
library(cowplot)
library(coin)

citation("dplyr")

### Librerías para el análisis estadístico
library(DescTools)
library(FSA)
library(coin)

### Librerías para la interacción R-Excel
library(xlsx)
library(readxl)


#--------------------------------------------------------------------------------------------------------------#
#                                           Data de todas las tortugas                                         #
#--------------------------------------------------------------------------------------------------------------#


df_tortugas2 <-  read_excel("C:/Users/jcge9/Downloads/data_tortugas.xlsx", 
                                            col_types = c("numeric", "text", "text", 
                                                          "text", "date", "date", "text", "text", 
                                                          "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text")) %>%
  filter(anio %in% c("2000", "2001","2002", "2003", "2004", "2005",
                     "2006", "2007","2008", "2009", "2010", "2011",
                     "2012", "2013","2014", "2015", "2016", "2017",
                     "2018", "2019","2020","2021")) %>% 
  mutate(mes = factor(mes,
                      levels = c("Enero","Febrero","Marzo","Abril",
                                 "Mayo","Junio","Julio","Agosto",
                                 "Septiembre","Octubre","Noviembre","Diciembre")),
         estacion = factor(estacion,
                           levels = c("Primavera","Verano",
                                      "Otoño","Invierno")),
         anio = factor(anio,
                       levels = c("2000", "2001","2002", "2003", "2004", "2005",
                                  "2006", "2007","2008", "2009", "2010", "2011",
                                  "2012", "2013","2014", "2015", "2016", "2017",
                                  "2018", "2019","2020","2021")))


#--------------------------------------------------------------------------------------------------------------#
#                                           Data de las tortugas marinas                                       #
#--------------------------------------------------------------------------------------------------------------#


df_tortugas_marinas <- df_tortugas2 %>% 
  filter(especie %in% c("Caretta caretta","Chelonia mydas",
                        "Dermochelys coriacea","Eretmochelys imbricata",
                        "Lepidochelys olivacea")) %>% 
  select(especie, nombre.especie, anio, mes, 
         estacion, muni, causa,causa_orig, muerte,lesion,
         cuerpo,estado,fecha,ficha)#;view(df_tortugas_marinas)

nrow(df_tortugas_marinas)     # 1908 especímenes
colnames(df_tortugas_marinas);ncol(df_tortugas_marinas) # 



