# <strong>Análisis estadístico de tortugas marinas varadas en Tenerife, Islas Canarias.
# Statistical analysis of sea turtles stranding in Tenerife, Canary Islands.</strong>

## <strong>Trabajo de Fin de Grado de *Juan Carlos García Estupiñán*. Mis tutores fueron Carlos Pérez González y José Alberto Delgado Bello.</strong>

### Hola, este es mi TFG, en el he realizado un análisis estadístico de una base de datos proporcionada por el <em>Centro de Recuperación de Fauna Silvestre, la Tahonilla</em>.

Mi intención con este directorio es compartir tanto los resultados obtenidos en este trabajo, como los scripts que he programado para ello. Los datos obviamente, como no son míos, sino de la Tahonilla (y en principio no son públicos), he subido una Demo para que tengan una idea general. Si quieren los datos para reproducir el experimento, pueden contactar con la Tahonilla.

Los resultados y el Abstract los subiré en otro Markdown en inglés, pero aquí explicaré las principales variables a estudiar, y los scripts utilizados.

## Librerías y puesta a punto de la base de datos / Libraries and making the dataset ready to used

* <strong><em>script:</strong></em> <strong><a href="scripts/1Lib_DFs.R">1Lib_Dfs.R</a></strong>

## Variables Utilizadas para el análisis / Used Variables for the analysis.

* Las especies / The species (<strong><em>"especie"</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/2Tor_de_data.R">2Tor_de_data.R</a></strong>. Aquí también se encuentra el modelo de series temporales, un SARIMA / In here, there is also a time series model, a SARIMA.

* Variables de tiempo: año, mes, estación / Time variables: year, month and season (<strong><em>"anio"</strong></em>, <strong><em>"mes"</strong></em>, <strong><em>"estacion"</strong></em>).

    * <strong><em>Script:</strong></em> <strong><a href="scripts/3ani_est_mes.R">3tor_ani_est.R</a></strong>

* xdMunicipio y lugares donde las tortugas son rescatadas / Municipality and places where the turtles are rescue. (<strong><em>"muni"</strong></em>, <strong><em>"lugar"</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/4municipios.R">4municipios.R</a></strong>

* Causas del recate. tortugas que llegan vivas o muertas, y las que pueden ser rehabilitadas o no / Rescue causes. Turtles that arrive alive or dead, as well as the ones that can be or not rehabilitate. (<strong><em>causa/causa_orig</strong></em>, <strong><em>muerte</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/5causa_muer.R">5causa_muer.R</a></strong>

* Las observaviones de los trabajadores a las tortugas cuando llegan al centro/ The observation that the workers of the center do to the turtles on the arrival (<strong><em>"observa"</strong></em>)

    * <strong><em>script:</strong></em><strong><a href="scripts/6obs2.R">6obs2.R</a></strong>

* Biometrías de las tortugas rescatadas: longitud recta del caparazón (<strong><em>"LRC"</strong></em>), Longitud Curva del Caparazón (<strong><em>"LCC"</strong></em>), anchura recta del caparazón (<strong><em>"ARC"</strong></em>), anchura curva del caparazón (<strong><em>"ACC"</strong></em>) y peso (<strong><em>"Peso"</strong></em>) / Biometry of the turtles straight carapace length (<strong><em>"SRC"</strong></em>), curved carapace length (<strong><em>"CCL"</strong></em>), straight carapace width (<strong><em>"SCW"</strong></em>), curved caparace width (<strong><em>"CCW"</strong></em>) and weight (<strong><em>"Weigth"</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/7biometria.R">6obs2.R</a></strong>


