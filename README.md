# Statistical analysis of sea turtles stranding in Tenerife, Canary Islands.</strong>

## <strong>Undergraduate thesis (Trabajo de Fin de Grado *TFG* in spanish) by *Juan Carlos García Estupiñán*. My supervisors were Carlos Pérez González and José Alberto Delgado Bello.</strong>

### <em> Data provided by the Centro de Recuperación de Fauna Silvestre la Tahonilla, Tenerife island</em>.

My intention with this repository is to share the results from this thesis, and the code in the R scripts that i have develop. 

## [Code used](code): 

### Libraries and making the dataset ready to used

* <strong><em>script:</strong></em> <strong><a href="scripts/1Lib_DFs.R">1Lib_Dfs.R</a></strong>

## Variables for the analysis.

* The species (<strong><em>"especie"</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/2Tor_de_data.R">2Tor_de_data.R</a></strong>. In here, there is also a time series model, a SARIMA, in order to predict the turtles coming in the year 2022 to the center.

* Time variables: year, month and season (<strong><em>"anio"</strong></em>, <strong><em>"mes"</strong></em>, <strong><em>"estacion"</strong></em>).

    * <strong><em>Script:</strong></em> <strong><a href="scripts/3ani_est_mes.R">3tor_ani_est.R</a></strong>

* Municipality and places where the turtles are rescue. (<strong><em>"muni"</strong></em>, <strong><em>"lugar"</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/4municipios.R">4municipios.R</a></strong>

* Rescue causes. Turtles that arrive alive or dead, as well as the ones that can be or not rehabilitate. (<strong><em>causa/causa_orig</strong></em>, <strong><em>muerte</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/5causa_muer.R">5causa_muer.R</a></strong>

* The observation that the workers of the center do to the turtles on the arrival (<strong><em>"observa"</strong></em>)

    * <strong><em>script:</strong></em><strong><a href="scripts/6obs2.R">6obs2.R</a></strong>

* Biometry of the turtles straight carapace length (<strong><em>"SRC"</strong></em>), curved carapace length (<strong><em>"CCL"</strong></em>), straight carapace width (<strong><em>"SCW"</strong></em>), curved caparace width (<strong><em>"CCW"</strong></em>) and weight (<strong><em>"Weigth"</strong></em>)

    * <strong><em>Script:</strong></em> <strong><a href="scripts/7biometria.R">7biometria.R</a></strong>


