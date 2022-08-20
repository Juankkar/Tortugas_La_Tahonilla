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

Además se realizó un modelo de Series Temporales, un SARIMA en concreto para realizar un pronóstico de las tortugas que llegarían en 2022. Pero este estudio debido a diversas razones se realizó en el script: / In addition, a Time Series model was made, a specific SARIMA to make a forecast of the turtles that would arrive in 2022. But this study, due to various reasons, was carried out in the script: 3Tor_de_data.R

* Municipio y lugares donde las tortugas son rescatadas / Muniicipality and places where the turtles are rescue. (***"muni"***, ***"lugar"***)

Script: 4municipios.R

* Causas del recate. tortugas que llegan vivas o muertas, y las que pueden ser rehabilitadas o no / Rescue causes. Turtles that arrive alive or dead, as well as the ones that can be or not rehabilitate. (***causa/causa_orig***, ***muerte***)

Script: 5causas2.R

* Las observaviones de los trabajadores a las tortugas cundo llegan al centro/ The observation that the workers of the center do to the turtles on the arrival (***"observa"***)

script: 6obs2.R

* Biometrías de las tortugas rescatadas: longitud recta del caparazón (***"LRC"***), Longitud Curva del Caparazón (***"LCC"***), anchura recta del caparazón (***"ARC"***), anchura curva del caparazón (***"ACC"***) y peso (***"Peso"***) / Biometry of the turtles straight carapace length (***"SRC"***), curved carapace length (***"CCL"***), straight carapace width (***"SCW"***), curved caparace width (***"CCW"***) and weight (***"Weigth"***)

Script: 7biometria.R




