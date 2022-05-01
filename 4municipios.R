#############################################################  
#### Estudio de la llegada de las tortugas a los municipios #
#############################################################

# Visualización previa de los municipios

municipios <- df_tortugas_marinas %>% group_by(muni) %>%  summarise(n = n())

#view(municipios)

#### Resumir los municipios --> los que se encuentren por debajo de  Guía de Isora(1.94%) pertenecerán a "Otros"

muni_no_otros <- municipios %>%  filter(muni %in% c("Adeje","Arico","Santa Cruz de Tenerife","Arona",
                                                    "Santiago del Teide","Granadilla de Abona","Candelaria",
                                                    "Güímar","Rosario, El","San Cristóbal de La Laguna",
                                                    "Guía de Isora"))


muni_otros <- municipios %>% filter(muni %in% c("NA","Buenavista del Norte","San Miguel de Abona",
                                                "Garachico","Puerto de la Cruz","Tacoronte","Arafo",
                                                "Realejos, Los", "Icod de los Vinos","Tegueste",
                                                "Santa Úrsula","San Juan de la Rambla","Matanza de Acentejo, La",
                                                "La Guancha","Fasnia","Silos, Los","Orotava, La"))

sumatorio_otros <- sum(muni_otros$n)
sumatorio_otros

municipios_bien <- data.frame(n = c(muni_no_otros$n,sumatorio_otros),
                              muni = c(muni_no_otros$muni,"Otros")) %>% 
  mutate(n = (n/sum(n))*100)

as.data.frame(municipios_bien)
por_ord <- tibble(muni_porcentaje = round(municipios_bien$n,2)) %>% arrange(desc(muni_porcentaje))
por_ord$muni_porcentaje

# Etiquetas---------------------------------------------------------#
x_muni <- c(12.1,11.1,10.1,9,8.1,7.1,6.1,5.1,4.1,3.1,2.1,1.1)
y_muni <- c(por_ord$muni_porcentaje)+10
label_muni <- c("35.13 %","11.45 %","9.51%","8.42 % ","7.79 %","7.27 %",
                "4.97 %","3.97 %","3.61 %","3.29 %","2.72 %","1.88 %")

eti_porcentajes <- tibble(x = x_muni,
                          y = y_muni,
                          label = label_muni)
eti_porcentajes
#-------------------------------------------------------------------#
graf_muni <- municipios_bien %>%
  ggplot(aes(reorder(muni,n),n, fill = muni)) +
  geom_bar(stat = "identity", show.legend = F, width = .9,
           col = "black") +
  geom_text(data = eti_porcentajes,
            aes(x=x,y=y,label=label), 
            inherit.aes = F,size=3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,55),
                     breaks = seq(0,50,10)) +
  labs(x = "Municipio",
       y = "Porcentaje",
       title = "Municipio de procedencia") +
  scale_x_discrete(breaks=c("Adeje","Arico","Santa Cruz de Tenerife","Arona",
                            "Santiago del Teide","Granadilla de Abona","Otros","Candelaria",
                            "Güímar","Rosario, El","San Cristóbal de La Laguna",
                            "Guía de Isora","Fasnia","Silos, Los","Orotava, La"),
                   labels=c("Adeje","Arico","S.C.Tenerife","Arona",
                            "Santiago del Teide","Granadilla de Abona","Otros","Candelaria",
                            "Güímar","El Rosario","S.C.La Laguna",
                            "Guía de Isora","Fasnia","Los Silos","La Orotava")) +
  scale_fill_manual(values = c("lightslategray","navajowhite1","salmon4","cadetblue2","grey28","goldenrod2",    
                               "gray93","olivedrab3","brown2","pink4","grey20","gray60")) +
  theme(panel.background = element_blank(),
        plot.title = element_markdown(margin = margin(b = 1, unit = "lines")),
        axis.line.x = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.caption =  element_markdown(hjust = 0, face = "italic"));graf_muni



#------------------------------------------------------------------------------#
#                     Estudio específico de adeje                              #
#------------------------------------------------------------------------------#

# Como Adeje fue el municipio más representativo por mucho, estudiaremos este más
# a fondo. tenemos que preparar primero los datos para sacar los lugares, trabajaremos
# primero con los lugares que preprocesó carlos "lugar", de nuevo, del año 2000-2021.

df_lugar <- df_tortugas2 %>% 
  select(lugar_orig,lugar,anio,especie,fmt_lugar,muni,causa,causa_orig) %>% 
  filter(especie %in% c("Caretta caretta","Chelonia mydas",
                        "Dermochelys coriacea","Eretmochelys imbricata",
                        "Lepidochelys olivacea")) %>% view()

df_tortugas_adeje <- df_lugar %>% 
  filter(muni %in% "Adeje")

 # df_tortugas_adeje %>% 
 #   group_by(lugar) %>% 
 #   summarise(n=n()) %>% view()

df_adeje.2 <- df_tortugas_adeje %>% 
  mutate(lugar_jc=case_when(grepl("Fañabe",lugar)|grepl("FAÑABE",lugar)~"Fañabe",
                            grepl("Colon",lugar)|grepl("Colòn",lugar)~"Puerto Colon",
                            grepl("Duque",lugar)~"Playa Duque",
                            grepl("Caleta",lugar)~"Playa de la Caleta",
                            grepl("Paraiso",lugar)~"Playa Paraíso",
                            grepl("Bobo",lugar)~"Playa del Bobo",
                            grepl("Puertito",lugar)~"El Puertito Adeje",
                            grepl("Duque",lugar)~"Playa Duque",
                            grepl("Salvaje",lugar)~"Callao Salvaje",
                            grepl("Galera",lugar)~"Puerto la Galera",
                            grepl("Armeñime",lugar)~"Armeñime",
                            grepl("AMERICAS",lugar)|grepl("Americas",lugar)~"Playa de las Américas",
                            grepl("Golf",lugar)~"Amarilla Golf",
                            grepl("Troya",lugar)~"Playa Troya",
                            grepl("Puerto",lugar)|grepl("Marina",lugar)|grepl("Troya",lugar)|
                              grepl("Costa",lugar)|grepl("NA",lugar)|grepl("Playa",lugar)|
                              grepl("Adeje",lugar)~"Indeterminado")) %>% 
  select(anio,lugar_jc)


unique(df_adeje.2$lugar_jc)

adeje_no_otros <- df_adeje.2 %>% filter(lugar_jc %in% c("Puerto Colon","Fañabe","Indeterminado",     
                                                           "Playa de la Caleta","Callao Salvaje",
                                                           "Playa Paraíso","El Puertito Adeje")) %>% view()
adeje_otros <- df_adeje.2 %>% filter(!(lugar_jc %in% c("Puerto Colon","Fañabe","Indeterminado",     
                                                     "Playa de la Caleta","Callao Salvaje",
                                                     "Playa Paraíso","El Puertito Adeje"))) %>%  
  mutate(lugar_jc=rep("Otros",13)) %>% view()

df_adeje_bien <- data.frame(anio=c(adeje_no_otros$anio,adeje_otros$anio),
                            lugar=c(adeje_no_otros$lugar_jc,adeje_otros$lugar_jc))


adeje_anio <- expand.grid(lugar=as.character(unique(as.character(df_adeje_bien$lugar))),
                          anio=as.character(unique(as.character(df_adeje_bien$anio)))) %>% left_join(
                          df_adeje_bien %>% group_by(lugar,anio) %>% summarize(n=n()),
                          by=c("lugar","anio")) %>% mutate(n=ifelse(is.na(n),0,n))

text_adeje <- adeje_anio %>% 
  filter(anio %in% c("2015","2016","2017","2018",
                     "2019","2020","2021")) %>% 
  mutate(anio=factor(anio,
                     levels = c("2015","2016","2017","2018",
                                "2019","2020","2021")),
         n=(n/sum(n)*100)) %>% 
  select(lugar,n) %>% 
  group_by(lugar) %>% 
  summarise(n=sum(n)) %>% 
  arrange(n) %>% 
  mutate(x=n+15,
         y=c(1,2,3,4,5,6,7,8),
         label=rev(c("74.80 %","17.90 %","2.12 %",
                     "1.82 %","1.21 %","0.91%",
                     "0.91%","0.30%")));text_adeje 
  

graf_adeje <- adeje_anio %>% 
  filter(anio %in% c("2015","2016","2017","2018",
                    "2019","2020","2021")) %>% 
  mutate(anio=factor(anio,
                     levels = c("2015","2016","2017","2018",
                                "2019","2020","2021")),
         n=(n/sum(n)*100)) %>% 
  select(lugar,n) %>% 
  group_by(lugar) %>% 
  summarise(n=sum(n)) %>% 
  ggplot(aes(reorder(lugar,n),n,fill=lugar)) +
  geom_bar(stat = "identity", show.legend = F, width = .9,
           col="black") +
  labs(title = "Lugares Adeje (2015-2021)",
       x = "Lugar",
       y = "Porcentaje") +
  coord_flip() +
  geom_text(data = text_adeje,
            aes(x=y,
                y=x,
                label=label),
            inherit.aes = F,size=3.5)+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,110),
                     breaks = seq(0,100,25)) +
  scale_fill_manual(values = c("lightslategray","navajowhite1","salmon4","cadetblue2","grey28","goldenrod2",    
                               "gray93","olivedrab3","brown2","pink4","grey20","gray60")) +
  scale_x_discrete(breaks=c("Puerto Colon","Fañabe","Playa de la Caleta",
                            "Indeterminado","El Puertito Adeje","Playa Paraíso",
                            "Otros","Callao Salvaje"),
                   labels=c("Puerto de Colón","Fáñabe","Playa de la Caleta",
                            "Indeterminado","El Puertito","Playa Paraíso",
                            "Otros","Callao Salvaje")) +
  theme(panel.background = element_blank(),
        plot.title = element_markdown(hjust = -.5),
        axis.line.x = element_line(),
        title = element_markdown(size = 11, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.caption =  element_markdown(hjust = 0, face = "italic"));graf_adeje


plot_grid(graf_muni,graf_adeje,
          ncol = 2,
          labels = c("A","B"),
          rel_heights = c(.6,.555))

#------------------------------------------------------------------------------#
#                           Causas en Puerto de Còlon                          #
#------------------------------------------------------------------------------#

df_lugar.3 <- df_tortugas2 %>% 
  select(lugar_orig,lugar,anio,especie,muni,causa,causa_orig) %>% 
  filter(especie %in% c("Caretta caretta","Chelonia mydas",
                        "Dermochelys coriacea","Eretmochelys imbricata",
                        "Lepidochelys olivacea"))

df_lugar.4 <- df_lugar.3 %>% 
  filter(muni %in% "Adeje") %>% 
  mutate(lugar_jc=case_when(grepl("Fañabe",lugar)|grepl("FAÑABE",lugar)~"Fáñabe",
                            grepl("Colon",lugar)|grepl("Colòn",lugar)~"Puerto Colon",
                            grepl("Duque",lugar)~"Playa Duque",
                            grepl("Caleta",lugar)~"Playa de la Caleta",
                            grepl("Paraiso",lugar)~"Playa Paraíso",
                            grepl("Bobo",lugar)~"Playa del Bobo",
                            grepl("Puertito",lugar)~"El Puertito Adeje",
                            grepl("Duque",lugar)~"Playa Duque",
                            grepl("Salvaje",lugar)~"Callao Salvaje",
                            grepl("Galera",lugar)~"Puerto la Galera",
                            grepl("Armeñime",lugar)~"Armeñime",
                            grepl("AMERICAS",lugar)|grepl("Americas",lugar)~"Playa de las Américas",
                            grepl("Golf",lugar)~"Amarilla Golf",
                            grepl("Troya",lugar)~"Playa Troya",
                            grepl("Puerto",lugar)|grepl("Marina",lugar)|grepl("Troya",lugar)|
                              grepl("Costa",lugar)|grepl("NA",lugar)|grepl("Playa",lugar)|
                              grepl("Adeje",lugar)~"Indeterminado")) %>% view()
df_pue_colon <- df_lugar.4 %>% 
  filter(lugar_jc %in% "Puerto Colon")

df_puerto_colon_causas <- expand.grid(causa=as.character(unique(as.character(df_pue_colon$causa))),
                                      anio=as.character(unique(as.character(df_pue_colon$anio))),
                                      lugar_jc=as.character(unique(as.character(df_pue_colon$lugar_jc)))) %>% left_join(
                                        df_pue_colon %>% group_by(causa,anio,lugar_jc) %>% summarize(n=(n())),
                                                     by=c("causa","anio","lugar_jc")
                                                   ) %>% mutate(n=ifelse(is.na(n),0,n)) %>% view()


df_puerto_colon_causas %>% 
  filter(anio %in% c("2015","2016","2017","2018","2019","2020","2021")) %>%
  mutate(anio = factor(anio,
                       levels = c("2015","2016","2017","2018","2019","2020","2021")),
         causa = factor(causa,
                        levels = c("Artes de pesca","Enmallada","Hidrocarburos",
                                   "Choque con embarcación","Choques","Enfermedad",
                                   "Debilidad agotamiento","Soleándose","Indeterminado",
                                   "Otros","NA"))) %>%
  group_by(anio) %>%
  mutate(causa_n=(n/sum(n))*100) %>% 
  ggplot(aes(anio,causa_n, fill=causa)) +
  geom_bar(stat = "identity",position="stack",col="black",
           width = .65) +
  scale_fill_manual(values = c("purple","violet","darkred","red","salmon",
                               "forestgreen","yellowgreen","yellow","blue4","blue",
                               "cyan")) +
  labs(title = "Causas actuales Puerto Colòn",
       x="Año",
       y="Porcentaje") +
  scale_y_continuous(expand = expansion(0)) +
  theme(panel.background = element_blank(),
        plot.title = element_markdown(margin = margin(b = 1, unit = "lines")),
        axis.line.x = element_line(),
        title = element_markdown(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.caption =  element_markdown(hjust = 0, face = "italic"),
        legend.key.size = unit(.35,"cm"))

########################
#### Tabla anterior ####
########################

tabla_pc.1 <- df_puerto_colon_causas %>% 
  filter(anio %in% c("2015","2016","2017","2018","2019","2020","2021")) %>%
  mutate(anio = factor(anio,
                       levels = c("2015","2016","2017","2018","2019","2020","2021")),
         causa = factor(causa,
                        levels = c("Artes de pesca","Enmallada","Hidrocarburos",
                                   "Choque con embarcación","Choques","Enfermedad",
                                   "Debilidad agotamiento","Soleándose","Indeterminado",
                                   "Otros","NA"))) %>% 
  group_by(anio) %>%
  mutate(n=(n/sum(n))*100)



pc_artes.pesca <- tabla_pc.1 %>% filter(causa %in% "Artes de pesca")
pc_enmallada <- tabla_pc.1 %>% filter(causa %in% "Enmallada")
pc_hidrocarburos <- tabla_pc.1 %>% filter(causa %in% "Hidrocarburos")
pc_choque_barcos <- tabla_pc.1 %>% filter(causa %in% "Choque con embarcación")
pc_choques <- tabla_pc.1 %>% filter(causa %in% "Choques")
pc_enfermedad <- tabla_pc.1 %>% filter(causa %in% "Enfermedad")
pc_deb_agot <- tabla_pc.1 %>% filter(causa %in% "Debilidad agotamiento")
pc_soleandose <- tabla_pc.1 %>% filter(causa %in% "Soleándose")
pc_indet<- tabla_pc.1 %>% filter(causa %in% "Indeterminado")
pc_otros<- tabla_pc.1 %>% filter(causa %in% "Otros")
pc_na<- tabla_pc.1 %>% filter(causa %in% "NA")


tabla_pc.2 <- data.frame(anio=c(2016,2015,2017,2018,2019,2020,2021),
                         Artes_pesca=round(pc_artes.pesca$n,2),
                         enmallada=round(pc_enmallada$n,2),
                         hidrocarburos=round(pc_hidrocarburos$n,2),
                         choque_barcos=round(pc_choque_barcos$n,2),
                         choques=round(pc_choques$n,2),
                         enfermedad=round(pc_enfermedad$n,2),
                         deb_agot=round(pc_deb_agot$n,2),
                         soleandose=round(pc_soleandose$n,2),
                         indeterminado=round(pc_indet$n,2),
                         otros=round(pc_otros$n,2),
                         `NA`=round(pc_na$n,2)) %>%
  arrange(anio) %>%  view()

