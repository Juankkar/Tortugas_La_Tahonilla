##########################
#### Causas y muertes ####
##########################

# abreviaciones: artes de pesca (AP), Choques con embarcaciones (CE), Choques (C),
#                Debilidad agotamiento (DA), Enfermedad (En), Enmallada (Em),
#                Hidrocarburos (H), Indeterminado (I), Otros (O), Soleándose (S)



df_tm_causas <- df_tortugas_marinas #%>% filter(anio != "2000")
causa_muerte <- expand.grid(causa=as.character(unique(as.character(df_tm_causas$causa))),
                            muerte=as.character(unique(as.character(df_tm_causas$muerte)))) %>% left_join(
                              df_tm_causas %>% group_by(causa,muerte) %>% summarize(n=(n())),
                              by=c("causa","muerte")
                            ) %>% mutate(n=ifelse(is.na(n),0,n))

sum(causa_muerte$n)
cm_no_indeterninado <- causa_muerte %>% filter(causa %in% c("Enfermedad","Artes de pesca","Otros",
                                                            "Debilidad agotamiento","Hidrocarburos",         
                                                            "Choques","Choque con embarcación",
                                                            "Enmallada","Soleándose"));cm_no_indeterninado
cm_no_indeterninado %>% summarise(x=sum(n))

cm_indeterminado <- filter(causa_muerte, causa %in% c("Indeterminado", "NA")) %>% 
  group_by(muerte) %>% summarise(n=sum(n)) %>% mutate(causa=c("Indeterminado","Indeterminado"),
                                                      muerte=c("No","Si"),
                                                      n=n);cm_indeterminado
cm_indeterminado %>% summarise(x=sum(n))


c = c(cm_no_indeterninado$causa, cm_indeterminado$causa);c
m = c(cm_no_indeterninado$muerte, cm_indeterminado$muerte);m
n = c(cm_no_indeterninado$n,cm_indeterminado$n);n


cm <- data.frame(causa=c,
                 muerte=m,
                 n=n);view(cm)    


# Tabla de datos de las causas, desde el año 2000-2021

causa_No <- cm %>% filter(muerte %in% "No") %>% 
  mutate(Supervivientes=n) %>% select(causa,Supervivientes);causa_No
causa_Si <- filter(cm, muerte %in% "Si") %>% 
  mutate(Fallecidas=n) %>% select(causa,Fallecidas);causa_Si

causa_no_si <- left_join(causa_No,causa_Si, by='causa');causa_no_si


tabla_causas <- cm %>% 
  group_by(causa) %>% 
  summarise(n=sum(n)) %>% 
  mutate(frecuencia=round(n/sum(n),4)) %>%   
  arrange(desc(n));tabla_causas

tabla_causas_completa <- left_join(tabla_causas, causa_no_si,
                                   by='causa');tabla_causas_completa

write.xlsx(as.data.frame(tabla_causas), file = "C:/Users/jcge9/Desktop/TFG/TFG_R/tabla_causas.xlsx")






######################################   Juntar   #########################################
# 
# Causas humanas: Artes de pesca, choques, hidrocarburos, enmallada, choque con embarcación.
# Causas naturales:Enfermedad, Debilidad y agotamiento, soleándose.
# Indeterminados-otros: Indeterminado, NA, Otros.

causa_anio <- expand.grid(causa_orig=as.character(unique(as.character(df_tortugas_marinas$causa_orig))),
                          anio=as.character(unique(as.character(df_tortugas_marinas$anio)))) %>% left_join(
                          df_tortugas_marinas %>% group_by(causa_orig,anio) %>% summarize(n=(n())),
                          by=c("causa_orig","anio")
                              ) %>% mutate(n=ifelse(is.na(n),0,n))   

causa_anio %>% group_by(causa_orig) %>% summarise(n=n())
  
df_causas_h <- causa_anio %>% filter(causa_orig %in% c("Anzuelo","Artes de pesca","Capturada",
                                                  "Choque con embarcación","Choques","Decomisos",
                                                  "Enmallada","Hidrocarburos","Rafia")) %>% 
  group_by(anio) %>% 
  summarise(n=sum(n)) %>% 
  mutate(causa=rep("Causas humanas",22))

sum(df_causas_h$n)

  
df_causas_n <- causa_anio %>% filter(causa_orig %in% c("Debilidad agotamiento","Enfermedad",
                                                  "Inanición","Muerte Natural","Soleándose")) %>% 
  group_by(anio) %>% 
  summarise(n=sum(n)) %>% 
  mutate(causa=rep("Causas naturales",22))
  
sum(df_causas_n$n)

df_causas_i <- causa_anio %>% filter(causa_orig %in% c("Indeterminado", "Otros", "NA")) %>% 
 group_by(anio) %>% 
  summarise(n=sum(n)) %>% 
  mutate(causa=rep("Indet./Otros",22))

sum(df_causas_i$n)

dfc_completo <- data.frame(anio=c(df_causas_h$anio,df_causas_i$anio,df_causas_n$anio),
                           causa=c(df_causas_h$causa,df_causas_i$causa,df_causas_n$causa),
                           n=c(df_causas_h$n,df_causas_i$n,df_causas_n$n)) %>% 
  mutate(anio=factor(anio,
                     levels = c("2000","2001","2002","2003","2004",
                                "2005","2006","2007","2008","2009",
                                "2010","2011",
                                "2012","2013","2014","2015","2016",
                                "2017","2018","2019","2020","2021")));view(dfc_completo)

#-------------------------------------------#
# Tabla de estaísticos descriptivos         #
# Causas humanas, naturales, indeterminadas #
#-------------------------------------------#

# 2001-2012
tabla.est.0112 <- dfc_completo %>% 
  filter(anio %in% c("2001","2002","2003","2004","2005","2006",
                     "2007","2008","2009","2010","2011","2012")) %>%   
  group_by(causa) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n));tabla.est.0112
# 2013-2021
tabla.est.1321 <- dfc_completo %>% 
  filter(!(anio %in% c("2001","2002","2003","2004","2005","2006",
                     "2007","2008","2009","2010","2011","2012"))) %>%   
  group_by(causa) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n));tabla.est.1321

# write.xlsx(tabla.est.0112,'tabla.est.0112.xlsx')
# write.xlsx(tabla.est.1321,'tabla.est.1321.xlsx')



#-----------------------------------------------------------#
# Gráficos de las causas humanas, indeterminados, naturales #
#-----------------------------------------------------------#

dfc_completo %>% 
  filter(anio != "2000") %>% 
  #group_by(causa) %>% 
  ggplot(aes(anio,n,col = causa,group=causa)) +
  geom_point(size = 2) +
  geom_line(size = .75) +
  geom_smooth(method = "lm", se =F, size=.75) +
  labs(title = "Evolución de las causas",
       x = "Año",
       y = "Núm.tortugas",
       col = "Causas") +
  scale_color_manual(
                     breaks = c("Causas humanas","Causas naturales",
                                "Indet./Otros"),
                     labels= c("Humanas","Naturales","Indet./Otras"),
                     values = c("pink","yellowgreen","skyblue2"),
                     ) + 
  theme_classic() +
  theme(plot.title = element_markdown(hjust = .5),
        title = element_markdown(size = 11, face = "bold"),
        panel.background = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(angle = 270, vjust  = 0.5), 
        plot.caption = element_markdown(hjust = 0, face = "italic"),
        legend.position = "top")




# Comparación de los grupos, Causas humanas, causas naturales, Indeterminados-otros
# de la última década.

dfc_ult.decada <- dfc_completo %>% 
  filter(anio %in% c("2013","2014","2015","2016",
                     "2017","2018","2019","2020","2021"))


library(DescTools)
tapply(dfc_ult.decada$n,dfc_ult.decada$causa,shapiro.test)                      # p>0.05
kruskal.test(n~causa, data = dfc_ult.decada)

gfc_hist.1 <- dfc_ult.decada %>%
  mutate(causa=factor(causa,
                      levels = c("Causas humanas","Causas naturales",
                                 "Indet./Otros"),
                      labels = c("Humanas","Naturales","Indet./Otros"))) %>% 
  ggplot(aes(n, fill = causa)) +
  geom_histogram(col = "black", bins = 20,
                 show.legend = F) +
  # geom_text(data=hist_text.c1,
  #           aes(x=x,y=y, label=lab,col=causas),
  #           inherit.aes = F) +
  facet_wrap(~causa, ncol = 2) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  labs(title = "Distribución de los datos",
       subtitle = "",
       x="Núm.tortugas",
       y="Frecuencia") +
  theme_test() +
  theme(plot.title = element_text(size = 11,
                                  #margin = margin(b=1, unit = "lines"),
                                  face = "bold",
                                  hjust = .5),
        plot.subtitle  =element_text(size = 9),
        axis.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)));gfc_hist.1

#### K-W 1

gfc_anova <- dfc_ult.decada %>% 
  ggplot(aes(causa,n,fill=causa)) +
  geom_jitter(pch=21, 
              position = position_jitterdodge(.5,seed = 20101997),
              show.legend = F) +
  geom_boxplot(alpha=.5, width=.5,
               show.legend = F) +
  # stat_summary(fun = mean, geom = "crossbar", 
  #              width = .5, show.legend = F) +
  labs(title = "Comparación de las causas 2013-21",
       subtitle = "X\u00B2 Kruskal-Wallis = 4.8; P = 0.09",
        x = "Causas",
        y = "Núm.tortugas") +
  scale_x_discrete(breaks=c("Causas humanas", "Causas naturales","Indet./Otros"),
                   labels=c("Humanas\n(n=9)", "Naturales\n(n=9)","Indet./Otros\n(n=9)")) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  theme_classic() +
  theme(plot.title = element_text(size = 11, ,
                                  face = "bold",
                                  hjust = .5,
                                  margin = margin(b=1,unit = "lines")),
        panel.background = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), size = 13,face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), size = 13,face = "bold"),
        axis.text.x = element_text(size = 11), 
        plot.caption = element_markdown(hjust = 0, face = "italic"),
        legend.position = c(.5,.85),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(.4,"cm"));gfc_anova

# Comparación de los grupos 

library(FSA)

dfc_2000_2011 <- dfc_completo %>% 
  filter(anio %in% c("2001","2002","2003","2004",
                     "2005","2006","2007","2008","2009",
                     "2010","2011","2012")) 

tapply(dfc_2000_2011$n,dfc_2000_2011$causa,shapiro.test)                        # p<0.05
LeveneTest(dfc_2000_2011$n,as.factor(dfc_2000_2011$causa), center = "median")   # p>0.05

kruskal.test(n~causa, data = dfc_2000_2011)                                     # p<0.05


dunnTest(n~as.factor(causa), data = dfc_2000_2011,
            method="bonferroni")
##### Histograma

gfc_hist2 <- dfc_2000_2011 %>% 
  mutate(causa=factor(causa,
                      levels = c("Causas humanas","Causas naturales",
                                 "Indet./Otros"),
                      labels = c("Humanas","Naturales","Indet./Otros"))) %>% 
  ggplot(aes(n, fill = causa)) +
  geom_histogram(col = "black", bins = 20,
                 show.legend = F) +
  facet_wrap(~causa, ncol = 2) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  labs(title = "Distribución de los datos",
       subtitle = "",
       x="Núm.tortugas",
       y="Frecuencia") +
  theme_test() +
  theme(plot.title = element_text(size = 11,
                                  #margin = margin(b=1, unit = "lines"),
                                  face = "bold",
                                  hjust = .5),
        plot.subtitle  =element_text(size = 9),
        axis.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)));gfc_hist2

##### K-W test

gfc_kw <- dfc_2000_2011 %>% 
  ggplot(aes(causa,n,fill=causa)) +
  geom_jitter(pch=21, 
              position = position_jitterdodge(.35,seed = 20101997),
              show.legend = F) +
  geom_boxplot(alpha=.5, width=.5,
               show.legend = F) +
  # stat_summary(fun = median, geom = "crossbar", 
  #              width = .35, show.legend = F) +
  scale_fill_manual(values = c("pink","yellowgreen","skyblue2")) +
  scale_x_discrete(breaks=c("Causas humanas", "Causas naturales","Indet./Otros"),
                   labels=c("Humanas\n(n=12)", "Naturales\n(n=12)","Indet./Otros\n(n=12)")) +
  labs(title = "Comparación de las causas 2001-12",
       subtitle = "X\u00B2 Kruskal-Wallis = 21.52; P = 2.12e-05\nPost-hoc: Test de Dunnet (corrección: Bonf.)",
       x = "Causas",
       y = "Núm.Tortugas") +
  theme_classic() +
  theme(plot.title = element_markdown(size = 11, face = "bold"),
        plot.subtitle  =element_text(size = 10),
        panel.background = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 13, face = "bold",margin = margin(t = 10)),
        axis.title.y = element_text(size = 13, face = "bold",margin = margin(r = 10)),
        axis.text.x = element_text(size = 11), 
        plot.caption = element_markdown(hjust = 0, face = "italic"),
        legend.position = c(.75,.7),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(.4,"cm"));gfc_kw

#########################################################################################
############################ Juntar los gráficos a analizar #############################
#########################################################################################


### K-W 1

plot_causas1 <- plot_grid(gfc_hist.1,gfc_anova,
          rel_widths = c(.4,.6),
          labels = c("C","D"))

### K-W 1

plot_causas2 <- plot_grid(gfc_hist2,gfc_kw,
          rel_widths = c(.4,.6),
          labels = c("","B"))

plot_grid(plot_causas2,
          plot_causas1,
          nrow = 2)


################################################################################
############### Muertes de las tortugas a lo largo de los años  ################
################################################################################

anio_muerte <- expand.grid(anio=as.character(unique(as.character(df_tortugas_marinas$anio))),
                            muerte=as.character(unique(as.character(df_tortugas_marinas$muerte)))) %>% left_join(
                              df_tortugas_marinas %>% group_by(anio,muerte) %>% summarize(n=(n())),
                              by=c("anio","muerte")
                            ) %>% mutate(n=ifelse(is.na(n),0,n))
anio_muerte %>% 
  group_by(muerte) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n))
 

tapply(anio_muerte$n, anio_muerte$muerte, shapiro.test)
LeveneTest(anio_muerte$n, as.factor(anio_muerte$muerte), center = mean)

wilcox_test(n~factor(muerte), data = anio_muerte)


# Histograma para ver la distribución de los datos
muerte_hist <- anio_muerte %>% 
  ggplot(aes(n, fill=muerte)) +
  geom_histogram(col="black",alpha=.75, 
                 show.legend = F) +
  labs(title = "Distribución de los datos",
       x="Núm.tortugas",
       y="Frecuencia")+
  scale_fill_manual(breaks=c("No","Si"),
                    values = c("white","black")) +
  facet_wrap(.~muerte, nrow = 2) +
  theme_test() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    #axis.text = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );muerte_hist
  

# Gráfico de cajas entre las tortugas que sobreviven y no 
muerte_cajas <- anio_muerte %>% 
  ggplot(aes(muerte,n,fill=muerte)) +
  geom_jitter(pch=21, show.legend = F, 
              position = position_jitterdodge(.5,seed = 20101997)) +
  geom_boxplot(alpha=.5, width=.5, show.legend = F) +
  labs(title = "Llegadas vivas/muertas",
       subtitle = "Z = 5.685; P = 1.308e-08",
       x="Muertas",
       y="Núm.tortugas") +
  scale_fill_manual(values = c("white","black")) +
  scale_x_discrete(breaks=c("Si","No")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    axis.text.x = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );muerte_cajas

# Ambos gráficos juntos

plot_grid(muerte_hist, muerte_cajas,
          cols = 2,rel_widths = c(.4,.6),
          labels = c("A","B"))

no <- anio_muerte %>% filter(muerte %in% "No")
si <- anio_muerte %>% filter(!(muerte %in% "No"))

#no$anio == si$anio

tabla_muerte <- data.frame(anio = no$anio,
                           Sobrevive = no$n,
                           Fallece = si$n) %>% 
  arrange(anio);tabla_muerte
sum(tabla_muerte$Sobrevive) + sum(tabla_muerte$Fallece)

write.xlsx(tabla_muerte, 'muerte.xlsx')
sum(anio_muerte$n)

################################################################################
############################  Tortugas rehabilitadas ###########################
################################################################################

df_rehabilitadas <-  read_excel("C:/Users/jcge9/Downloads/data_tortugas.xlsx",
                                sheet = "Hoja2") %>% 
  filter(Especie %in% c("Caretta caretta","Chelonia mydas",
                        "Dermochelys coriacea","Eretmochelys imbricata",
                        "Lepidochelys olivacea"))

tor_no_rehab <- df_tortugas_marinas %>% 
  filter(ficha %in% df_rehabilitadas$`Nº Ficha`) %>% 
  mutate(rehabilitadas = rep("No rehab.",352)) %>% 
  select(rehabilitadas,anio)
tor_rehab <- df_tortugas_marinas %>% 
  filter(!(ficha %in% df_rehabilitadas$`Nº Ficha`)) %>% 
  mutate(rehabilitadas = rep("Rehab.",1561)) %>% 
  select(rehabilitadas,anio)


r_nor <- data.frame(rehabilitadas=c(tor_no_rehab$rehabilitadas,
                                            tor_rehab$rehabilitadas),
                            anio=c(tor_no_rehab$anio,
                                   tor_rehab$anio))%>% 
  group_by(anio, rehabilitadas) %>% 
  summarise(n=n()) 

tapply(r_nor$n, r_nor$rehabilitadas, shapiro.test)
wilcox_test(n~as.factor(rehabilitadas), data = r_nor)

hist_rehab <- r_nor %>% 
  ggplot(aes(n, fill=rehabilitadas)) +
  geom_histogram(col="black",alpha=.75, 
                 show.legend = F) +
  labs(title = "Distribución de los datos",
       x="Núm.tortugas",
       y="Frecuencia")+
  # # scale_y_continuous(expand = expansion(0),
  # #                    limits = c(0,13)) +
   scale_fill_manual(breaks=c("No rehab.","Rehab."),
                     values = c("gray","orange")) +
  facet_wrap(.~rehabilitadas, nrow = 2) +
  theme_test() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    #axis.text = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );hist_rehab


# Gráfico de cajas entre las tortugas que sobreviven y no 
boxplot_rehab <-  r_nor %>% 
  ggplot(aes(rehabilitadas,n,fill=rehabilitadas)) +
  geom_jitter(pch=21, show.legend = F, 
              position = position_jitterdodge(.5,seed = 20101997)) +
  geom_boxplot(alpha=.5, width=.5, show.legend = F) +
  labs(title = "Rehabilitadas/No rehabilitadas",
       subtitle = "Z = -5.5291; P = 3.218e-08",
       x="Condición",
       y="Núm.tortugas") +
  scale_fill_manual(values = c("gray","orange")) +
  scale_x_discrete(breaks=c("No rehab.","Rehab.")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    axis.title = element_text(face = "bold", size=13),
    axis.text.x = element_text(size=12)
    # axis.title.x = element_text(margin = margin(r=10)),
    # axis.title.x = element_text(margin = margin(r=10))
  );boxplot_rehab

plot_grid(muerte_hist, muerte_cajas,
          hist_rehab,boxplot_rehab,
          cols = 2,rel_widths = c(.4,.6),
          labels = c("A","B",
                     "C","D"))


#-------------------------------------------------------------------------#
# Tabla de datos de estadísticos descriptivos... a ver como lo hacemos... #
#-------------------------------------------------------------------------#


rehab_nrehab %>% 
  group_by(rehabilitadas) %>% 
  summarise(num_tor=sum(n),
            minimo=min(n),
            q1=quantile(n,.25),
            mediana=median(n),
            q3=quantile(n,.75),
            maximo=max(n))

