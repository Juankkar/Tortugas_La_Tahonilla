# Análisis estadístico de tortugas marinas varadas en Tenerife, Islas Canarias.
# Statistical analysis of sea turtles stranding in Tenerife, Canary Islands.

## Trabajo de Fin de Grado de Juan Carlos García Estupiñán. Mis tutores fueron Carlos Pérez González y José Alberto Delgado Bello.

### Hola, este es mi TFG, en el he realizado un análisis estadístico de una base de datos proporcionada por el Centro de Recuperación de Fauna Silvestre, la Tahonilla.

Mi intención con este directorio es compartir tanto los resultados obtenidos en este trabajo, como los scripts que he programado para ello. Los datos obviamente, como no son míos, sino de la Tahonilla (y en principio no son públicos), he subido una Demo para que tengan una idea general. Si quieren los datos para replicar el experimento, pueden contactar con la Tahonilla.

Los resultados y el Abstract los subiré en otro Markdown en inglés, pero aquí explicaré las principales variables a estudiar, y los scripts utilizados.

## Variables Utilizadas para el análisis / Used Variables for the analysis.

### Variables generales / General variables:

* La especie / The specie (***"especie"***)

Script: 2Tor_de_data.R

* Variables tiempo: año, mes, estación / Time variables: year, month and season (***"anio"***, ***"mes"***, ***"estacion"***).

Script: / This were studied in the script: tor_ani_est.R

Además se realizó un modelo de Series Temporales, un SARIMA, en concreto para realizar un pronóstico de las tortugas que llegarían en 2022. Pero este estudio debido a diversas razones se realizó en el script: / In addition, a Time Series model was made, in specific a SARIMA model, to make a forecast of the turtles that would arrive in 2022. But this study, due to various reasons, was carried out in the script: 3Tor_de_data.R

* Municipio y lugares donde las tortugas son rescatadas / Muniicipality and places where the turtles are rescue. (***"muni"***, ***"lugar"***)

Script: 4municipios.R

* Causas del recate. tortugas que llegan vivas o muertas, y las que pueden ser rehabilitadas o no / Rescue causes. Turtles that arrive alive or dead, as well as the ones that can be or not rehabilitate. (***causa/causa_orig***, ***muerte***)

Script: 5causas2.R

* Las observaviones de los trabajadores a las tortugas cundo llegan al centro/ The observation that the workers of the center do to the turtles on the arrival (***"observa"***)

script: 6obs2.R

* Biometrías de las tortugas rescatadas: longitud recta del caparazón (***"LRC"***), Longitud Curva del Caparazón (***"LCC"***), anchura recta del caparazón (***"ARC"***), anchura curva del caparazón (***"ACC"***) y peso (***"Peso"***) / Biometry of the turtles straight carapace length (***"SRC"***), curved carapace length (***"CCL"***), straight carapace width (***"SCW"***), curved caparace width (***"CCW"***) and weight (***"Weigth"***)

Script: 7biometria.R


# Results of "Statistical analysisis of sea turtles strandings in Tenerife, Canary Islands". (Hightlights)

## Abstract:
<style>
body {
text-align: justify} </style>

The chelonian order is formed by two families and seven species of sea turtles, all of which are on the IUCN Red List and Appendix I of CITES. Because of that, it is important to study their strandings in the Canary Islands. The most representative was *Caretta caretta*, arriving mainly juveniles, especially in summer, followed by fall and spring equally and winter where less. When a forecast was made for 2022, it was seen that this pattern could occur in a similar way. Adeje was the municipality with the highest number of turtles arriving, mostly from Puerto Colòn, a tourist area. In fact, the most common causes were of anthropogenic origin, especially those related to fishing gear, but have been decreasing until now, with a slight increase of the natural ones and a large one of the undetermined causes. Most of the turtles are alive when they have been rescued, being rehabilitated most of them, where the lesions in the fins are the main affection, suffering necrosis and amputations.

**Keywords**: loggerhead, *Caretta caretta*, Canary Islands, strandings, sea turtles.

## 1) Species and biometric study of the turtles.

### In terms of numbers, *Caretta caretta* was the most common out of all of the turtles, representating 96.92% of the individuals. In total, there was five species, and over all, 1913 turtles identified.

* *Caretta caretta* (loggerhead turtle): 1854
* *Chelonya mydas* (green turtle) : 51
* *Dermochelys coriacea* (leatherback turtle): 6
* *Eretmochelys imbricata* (hawksbill turtle): 1
* *Lepidochelys olivacea* (Olive ridley turtle): 1

Then what we did was a biometric study of the turtles with this type of data taken. We used the caparace curved length (what is known by "CCL"). Some autors (*cite et al., 2018*) have used this parameter to determined the age state of their linving cicle (more likely for the loggerheads and green turtles). 

What we saw was that, approximatly 90% of the loggerhead turtles were juviniles (more were small size than big). I case of the green turtle, the sample was so small, that there was nothing solid that we could extract from the results.

The results of the loggerehead has sense because Canary Islands is a place for de juveniles to feed (*cite et al., 20..*), and after they migrate... ***leer mas para saber que poner***.

---
![](biometria1.png)

***Figrue 3.*** *Estimation of the living cile using the CCL for the loggerhead and green turtle*.

---

After that, we did a Principal Componen Analisis (PCA), to compare all the biometric variables. We choosed the first 2 components because they explained ~98% of the variance, where the PC1 explained ~94% and the PC2 ~4%.

We used the Pearson Correlation Test, to compare the PC, with the biometrics variables. All of them  were hightly correlated and negatively with the first component. Meanwhile, in the second component, the only variable with a respectable (but not hight at all)level of correlation and negative, was the weight. The size caparace for the second on the other hand were poorly correlated.

This could mean that the PC1 would explained the ones that are growing in size of the caparace and weight, are in the left of the graph of the **Figure 4**. In that case when we put the results of the linving cicle, it formed groups that were in orther of size: 
1. hatchlings 
2. Juveniles (small and bigs) 
3. Subadults and 
4. Adults. 

The other component, could probably mean that when they growth in caparace size, they would be found in the top fringe of the graph, on the other hand, the ones that are in the negative site, are growing in weight. Over were the Juveniles, unther, the hatchlings, subadults, adults

---
![](PCA.png)

***Figure 4.*** *PCA of the biometric variables (loggerhead turtle). Differents groups of the linving cicle ar formed*

---

## 2) Temporal Study and forcast for 2022.

The results showed tha there wasn't a clear pattern in terms of the year of arrival. That wasn't the case for the season. In this case, the majority of the turtles come in summer, it woudl seems that in the spring and fall season, come equally, and in winter it would be when less of them arrive.

---
![](temporal1.png)

***Figure 5.*** *Temporal evolution of the turtles arrivals*

---

So, to see if this diferences were significants, we did a test of Kruskal-Wallis (the data distribution wasn't normal), and because *p* < 0.05, we did the Dunnet Test as a Post-Hoc (with the Bonferroni correction). 
* The summer season was *p* < 0.05 with the rest.
* In case of Spring and fall *p* > 0.05. 
* Winter was *p* > 0.05 with the rest.

---
![](temporal2.png)

***Figure 6.*** ***A** Histograms that shows the data distribution, **B** Boxplot for comparing the seasons*

---

We did a time series analysis. We got a SARIMA model and the components, of the model where:

( p, d, q ) x ( P, D, Q )<sub>S</sub>

* No seasonal componentes: (p = 2, d = 0, q = 3 )
* Seasonal components: ( P = 1, D = 0, Q = 1 )
* seasonal period: S = 12
 
 In order to guarantee that the model was optimal. We first made sure that the data was stational by the Dickey-Fuller test (*p* < 0.05), we also did a Ljung-Box test, to corroborate the existence of white noise (*p* > 0.05). And finally before the forcast for 2022, we did atraining model one for 2021, to make sure that it didn't fail on the reality, and had good results.

 ---
![](modeloentrenamiento.png)

***Figure 8.*** **(I couldn't translete the plot text, but is basically the training model)** *Training Model to see how it adjust to the data in 2021 in red, in blue other whise, is the forcast for this year.*


 ---

