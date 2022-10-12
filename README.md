# Análisis estadístico de tortugas marinas varadas en Tenerife, Islas Canarias.
# Statistical analysis of sea turtles stranding in Tenerife, Canary Islands.

## Trabajo de Fin de Grado de *Juan Carlos García Estupiñán*. Mis tutores fueron Carlos Pérez González y José Alberto Delgado Bello.

### Hola, este es mi TFG, en el he realizado un análisis estadístico de una base de datos proporcionada por el *Centro de Recuperación de Fauna Silvestre, la Tahonilla*.

Mi intención con este directorio es compartir tanto los resultados obtenidos en este trabajo, como los scripts que he programado para ello. Los datos obviamente, como no son míos, sino de la Tahonilla (y en principio no son públicos), he subido una Demo para que tengan una idea general. Si quieren los datos para reproducir el experimento, pueden contactar con la Tahonilla.

Los resultados y el Abstract los subiré en otro Markdown en inglés, pero aquí explicaré las principales variables a estudiar, y los scripts utilizados.

## Librerías y puesta a punto de la base de datos / Libraries and making the dataset ready to use

* ***script:*** **[1Lib_Dfs.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/1Lib_DFs.R)**

## Variables Utilizadas para el análisis / Used Variables for the analysis.

* Las especies / The species (***"especie"***)

    * ***Script:*** **[2Tor_de_data.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/2Tor_de_data.R)**. Aquí también se encuentra el modelo de series temporales, un SARIMA / In here, there is also a time series model, a SARIMA.

* Variables de tiempo: año, mes, estación / Time variables: year, month and season (***"anio"***, ***"mes"***, ***"estacion"***).

    * ***Script:*** **[3tor_ani_est.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/3ani_est_mes.R)**

* Municipio y lugares donde las tortugas son rescatadas / Municipality and places where the turtles are rescue. (***"muni"***, ***"lugar"***)

    * ***Script:*** **[4municipios.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/4municipios.R)**

* Causas del recate. tortugas que llegan vivas o muertas, y las que pueden ser rehabilitadas o no / Rescue causes. Turtles that arrive alive or dead, as well as the ones that can be or not rehabilitate. (***causa/causa_orig***, ***muerte***)

    * ***Script:*** **[5causa_muer.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/5causa_muer.R)**

* Las observaviones de los trabajadores a las tortugas cundo llegan al centro/ The observation that the workers of the center do to the turtles on the arrival (***"observa"***)

    * ***script:*** **[6obs2.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/6obs2.R)**

* Biometrías de las tortugas rescatadas: longitud recta del caparazón (***"LRC"***), Longitud Curva del Caparazón (***"LCC"***), anchura recta del caparazón (***"ARC"***), anchura curva del caparazón (***"ACC"***) y peso (***"Peso"***) / Biometry of the turtles straight carapace length (***"SRC"***), curved carapace length (***"CCL"***), straight carapace width (***"SCW"***), curved caparace width (***"CCW"***) and weight (***"Weigth"***)

    * ***Script:*** **[7biometria.R](https://github.com/Juankkar/Tortugas_La_Tahonilla/blob/main/scripts/7biometria.R)**


