################################################################################
#############################      Biometría     ###############################
################################################################################
colnames(biometria)
class(biometria$fecha_ok)
?read_excel()
biometria <- read_excel("C:/Users/jcge9/Downloads/data_tortugas.xlsx", 
                         sheet = "Biometria", col_types = c("text", 
                                                            "text", "text", "text", "text", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "date"))%>% 
  mutate(anio=as.character(format(fecha_ok, "%Y")),
         mes=as.character(format(fecha_ok,"%b")),
         estacion=case_when(mes == "dic."~"Invierno",
                            mes == "ene."~"Invierno",
                            mes == "feb."~"Invierno",
                            mes == "mar."~"Primavera",
                            mes == "abr."~"Primavera",
                            mes == "may."~"Primavera",
                            mes == "jun."~"Verano",
                            mes == "jul."~"Verano",
                            mes == "ago."~"Verano",
                            mes == "sep."~"Otoño",
                            mes == "oct."~"Otoño",
                            mes == "nov."~"Otoño")) %>% 
  filter(LRC > 0,LCC > 0,
         ACC > 0,ARC > 0,
         Peso > 0,Peso < 250) %>% 
  mutate(estadio=case_when(LCC<20 ~ "Cría",
                           LCC>=20 & LCC<40~ "Juv.pequeño",
                           LCC>=40 & LCC<60~"Juv.grande",
                           LCC>=60 & LCC<80 ~"Subadulto",
                           LCC>=80 ~"Adulto")) %>% view()

 # biometria %>%
 #   group_by(especie,estadio) %>%
 #   summarise(n=n())
 #   summarise(n=n(),
 #            media_LRC=mean(LRC),
 #            media_LCC=mean(LCC),
 #            media_ARC=mean(ARC),
 #            media_ACC=mean(ACC),
 #            media_Peso=mean(Peso))

#------------------------------------------------------------------------------#
#                               Caretta caretta                                #
#------------------------------------------------------------------------------#


biomet.cc <- biometria %>% filter(especie %in% "Caretta caretta") # n = 671


# #------------------------------------------------------------------------------#
# #                 Estudio de la longitud del recto del caparazón               #
# #------------------------------------------------------------------------------#


estudio_l_c <- data.frame(
  valores=c(27,380,227,33,4),
  tipo=c("Cría", "Juvenil pequeño",
         "Juvenil grande","subadulto",
         "Adulto")
) %>% mutate(tipo=factor(tipo,
                         levels = c("Cría", "Juvenil pequeño",
                                    "Juvenil grande", "subadulto",
                                    "Adulto")))

cc <- estudio_l_c %>%
  ggplot(aes(tipo, valores)) +
  geom_bar(stat = "identity", fill="orange",
           col="black", width = .65) +
  geom_text(data = tibble(x=c(1,2,3,4,5), y=estudio_l_c$valores+45),
            aes(x=x,y=y), label=c("27","380","227","33","4"),
            inherit.aes = F, size=4) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,550))+
  scale_x_discrete(breaks=c("Cría", "Juvenil pequeño",
                            "Juvenil grande", "subadulto",
                            "Adulto"),
                   labels=c("(<20cm)\nCría","(20-40cm)\nJuvenil\npequeño",
                            "(40-60cm)\nJuvenil\ngrande","(60-80cm)\nSubadulto",
                            "(>80cm)\nAdulto")) +
  labs(title = "Longitud Curva, *C.caretta*",
       subtitle="n = (671)",
       x="Longitud curva (cm)",
       y="Núm.tortugas") +
  theme_pander() +
  theme(
    plot.title = element_markdown(hjust = .5),
    plot.subtitle = element_markdown(hjust = .5),
    axis.line.x = element_line(),
    axis.title.x = element_text(face = "bold",
                                margin = margin(t=10)),
    axis.title.y = element_text(face = "bold",
                                margin = margin(r=10)));cc

# 
# #------------------------------------------------------------------------------#
# #                              Chelonia mydas                                  #
# #------------------------------------------------------------------------------#
# 
biometcm <- biometria %>% filter(especie %in% "Chelonia mydas")
        # n = 18


 
# #------------------------------------------------------------------------------#
# #                 Estudio de la longitud del recto del caparazón               #
# #------------------------------------------------------------------------------#


estudio_l_cm <- data.frame(
  valores=c(0,5,6,7,0),
  tipo=c("Cría", "Juvenil pequeño",
         "Juvenil grande","subadulto",
         "Adulto")
) %>% mutate(tipo=factor(tipo,
                         levels = c("Cría", "Juvenil pequeño",
                                    "Juvenil grande", "subadulto",
                                    "Adulto")))

cm <- estudio_l_cm %>%
  ggplot(aes(tipo, valores)) +
  geom_bar(stat = "identity", fill="forestgreen",
           col="black", width = .65) +
  geom_text(data = tibble(x=c(1,2,3,4,5), y=estudio_l_cm$valores+2),
            aes(x=x,y=y), label=c("","5","6","7",""), inherit.aes = F, size=4) +
   scale_y_continuous(expand = expansion(0),
                      limits = c(0,15))+
  scale_x_discrete(breaks=c("Cría", "Juvenil pequeño",
                            "Juvenil grande", "subadulto",
                            "Adulto"),
                   labels=c("(<20cm)\nCría","(20-40cm)\nJuvenil\npequeño",
                            "(40-60cm)\nJuvenil\ngrande","(60-80cm)\nSubadulto",
                            "(>80cm)\nAdulto")) +
  labs(title = "Longitud Curva, *C.mydas*",
       subtitle="n = (18)",
       x="Longitud curva (cm)",
       y="Núm.tortugas") +
  theme_pander() +
  theme(
    plot.title = element_markdown(hjust = .5),
    plot.subtitle = element_markdown(hjust = .5),
    axis.line.x = element_line(),
    axis.title.x = element_text(face = "bold",
                                margin = margin(t=10)),
    axis.title.y = element_text(face = "bold",
                                margin = margin(r=10))
  );cm

plot_grid(cc,cm)



#------------------------------------------------------------------------------#
#                                 PCA                                          #
#------------------------------------------------------------------------------#


variables <- biomet.cc[,c("LRC","LCC","ARC","ACC","Peso")] 

pca <- prcomp(as.matrix(variables),center = T, scale = T)
summary(pca)
plot(pca, main = "PCA",
     xlab="Componentes")

biplot(pca, scale = 0)

plot(pca, main = "PCA",
     xlab="Componentes")

biometria_es <- biomet.cc[,c("especie","estacion","estadio")] 

pca_biomet <- data.frame(biometria_es,
                         pca$x[,c("PC1","PC2","PC3")]) %>% view()
#############################
# Correlación PC, variables #
#############################

cor_pca <- cor(biomet.cc[,c("LRC","LCC","ARC","ACC","Peso")],
    pca_biomet[,c("PC1","PC2")]);cor_pca

write.xlsx(cor_pca, 'pca_correlacion.xlsx')
#### PCA ESPECIES
pca_biomet %>% 
  #filter(!(especie %in% "Caretta caretta")) %>% 
  mutate(estadio=factor(estadio,
                        levels = c("Cría","Juv.pequeño",
                                   "Juv.grande","Subadulto",
                                   "Adulto"))) %>% 
  ggplot(aes(PC1,PC2, 
             col = estadio, 
             fill=estadio,
             #alpha=especie
             )) +
  geom_point() +
  theme_classic() +
  labs(x="PC1 (94% Varianza explicada)",
       y="PC2 (4% Varianza explicada)",
       fill=NULL,
       color=NULL,
       title = "PCA Variables biométricas") +
  stat_ellipse(geom = "polygon", alpha=.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("red", "gray","forestgreen",
                                "blue","orange")) +
  scale_fill_manual(values = c("red", "gray","forestgreen",
                               "blue","orange")) +
  theme(
    plot.title = element_text(face="bold", hjust = .5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = c(.6,.3),
    legend.background = element_rect(color = "black", fill = NA),
    legend.margin = margin(t = -2, r = 0, b = 2, l = 2)
  )
##############
# Gráfico 3d #
##############
library(rgl)
biomet_3d <- pca_biomet %>% 
  mutate(color=case_when(estadio=="Cría"~"blue",
                         estadio=="Juv.pequeño"~"red",
                         estadio=="Juv.grande"~"gray",
                         estadio=="Subadulto"~"yellow",
                         estadio=="Adulto"~"forestgreen",
                         TRUE~NA_character_)) 

plot3d(x=pca_biomet$PC1,y=pca_biomet$PC2,z=pca_biomet$PC3,
       col = biomet_3d$color, type = "s", size = 1,
       xlab = "PC1 (94%)", ylab="PC2 (4%)", zlab="PC3 (1%)")

#################################
# Son significativos los grupo? #
#################################
library(nortest)
library(coin)
tapply(pca_biomet$PC1, pca_biomet$estadio, shapiro.test)
kruskal.test(PC1~as.factor(estadio), data = pca_biomet)               # PC1 P<0.05

d_t.1 <- dunnTest(PC1~as.factor(estadio), data = pca_biomet,
         method = "bonf");d_t.1$dtres
                                    # Adulto p>0.05 -> Juv.grande y Subadulto; p<0.05 Cría, Juv.pequeño
                                    # Cría   p>0.05 -> ninguno ;    p<0.05 Juv.grande, peq, subadulto
                                    # Juv.grande p>0.05 ninguno;    p<0.05 Juv.peq, subadulto
                                    # Juv.peq p>0.05 ninguno;       p<0.05 subadulto

write.xlsx(d_t.1$dtres, "tabla_pca1.xlsx")

tapply(pca_biomet$PC2, pca_biomet$estadio, shapiro.test)
kruskal.test(PC2~as.factor(estadio), data = pca_biomet)              # PC2 P<0.05

d_t.2 <- dunnTest(PC2~as.factor(estadio), data = pca_biomet,
                  method = "bonf");d_t.2$dtres
                                    
                                    # Adulto p>0.05 -> Cría, juv. peq, Subadulto; p<0.05 Juv.grande 
                                    # Cría p>0.05 subadulto; p<0.05 juv.peq y grande
                                    # Juv.grande p>0.05 ninguno; p<0.05 Juv.peq; 

##### PCA DE LAS ESTACIONES
pca_biomet %>% 
  filter(especie %in% "Caretta caretta") %>% 
  ggplot(aes(PC1,PC2, col = estacion, fill=estacion)) +
  geom_point() +
  theme_test() +
  labs(title = "PCA parámetros biométricos-estación",
       x="PC1 (94%)",
       y="PC2 (0.4%)",
       fill="Estación",
       color="Estación") +
  stat_ellipse(geom = "polygon", alpha=.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("cyan", "orange","yellowgreen","magenta")) +
  scale_fill_manual(values = c("cyan", "orange","yellowgreen","magenta")) +
  theme(
    plot.title = element_text(face="bold", hjust = .5),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = c(.5,.2)
  )
###############################
#       Gráfico 3D estaciones #
###############################

biomet_3d.2 <- pca_biomet %>% 
  mutate(color.2=case_when(estacion=="Primavera"~"yellowgreen",
                         estacion=="Verano"~"magenta",
                         estacion=="Otoño"~"orange",
                         estacion=="Invierno"~"cyan",
                         TRUE~NA_character_)) %>% view()

plot3d(x=pca_biomet$PC1,y=pca_biomet$PC2,z=pca_biomet$PC3,
       col = biomet_3d.2$color, type = "s", size = 1,
       xlab = "PC1 (94%)", ylab="PC2 (0.4%)", zlab="PC3 (0.01)")

#################################
# Son significativos los grupo? #
#################################

tapply(pca_biomet$PC1, pca_biomet$estacion, lillie.test)
kruskal.test(PC1~as.factor(estacion), data = pca_biomet)               
d_te1 <- dunnTest(PC1~as.factor(estacion), data = pca_biomet,
         method = "bonf");d_te1$dtres

tapply(pca_biomet$PC2, pca_biomet$estacion, lillie.test)
kruskal.test(PC2~as.factor(estacion), data = pca_biomet)               

cor()


library(psych)
pairs.panels(biomet.cc[,c("LRC","LCC","ARC","ACC","Peso")],
             main = "Correlacion lineal tortuga boba")


