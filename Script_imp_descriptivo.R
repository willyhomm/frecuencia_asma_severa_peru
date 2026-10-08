

#### 1. Exploración de missing #####

### 1.1 Exploración visual ####
library(dplyr)
data_final[,1:50] %>%  finalfit::missing_plot()
  # Se observa una elevada proporción de datos faltantes en variable exacerbaciones
data_final[,51:93] %>%  finalfit::missing_plot()
  # Se observa una elevada proporción de datos faltantes en variables habito_noc
  # habito, vef1, cvf y fmem_25/75


### 1.2 Exploración en lista ####
finalfit::missing_glimpse(data_final) # Variables que se deciden no imputar son:
  # exacerbaciones: n=179, 73.7 %
  # habito_noc_etiq: n=203, 83.5 %
  # habito: n=203, 83.5 %
  # vef1: n=116, 47.7 %
  # cvf: n=116, 47.7 %
  # fmem_25.75: n=242, 99.6 % 


### 1.3 Exploración visual dividiendo en grupos según variable respuesta ####
data_final %>% naniar::gg_miss_var(show_pct = T, facet = asma_severa_etiq)
  # Se podria dejar de imputar otras variables adicionales a las ya descritas
  # lineas arriba debido a que superan el 30 % de datos perdidos en los subgrupos
  # de asma severa; sin embargo, se decidió no hacerlo debido a que el trabajo es
  # netamente descriptivo.
data_final[,1:50] %>% naniar::gg_miss_var(show_pct = T, facet = asma_severa_etiq)
  # Ahora se observa mejor al desdoblar la información en primera mitad de variables
data_final %>% dplyr::select(asma_severa_etiq, 51:93) %>% naniar::gg_miss_var(show_pct = T, facet = asma_severa_etiq)
  # Ahora se observa mejor al desdoblar la información en segunda mitad de variables


### 1.4 Retirado de variables que generarían problemas de imputación ####

# NOTA: Para motivos de imputación, se crea la variable "data_final_imp" en la cual se
#       realizarán todos estos procesos.

data_final_imp <- data_final

# Retirado de variables que dependen de otra variables
data_final_imp <- subset(data_final_imp, select = c(-eosinofilos_new,
                                          -d_ptero_etiq,
                                          -d_fari_etiq,
                                          -blomia_etiq,
                                          -blatella_etiq,
                                          -perro_etiq,
                                          -gato_etiq,
                                          -medic,
                                          -alimen,
                                          -c(33:77),
                                          -animal,
                                          -habito,
                                          -intensidad_tto_etiq)
                   )


# Retirado de variables con exceso de datos perdidos
data_final_imp <- subset(data_final_imp, select = c(-exacerbaciones,
                                                   -habito_noc_etiq,
                                                   -vef1,
                                                   -cvf,
                                                   -`fmem_25/75`)
                         )


# Retirado de variables conflictivas para imputar
data_final_imp <- subset(data_final_imp, select = c(-calidad_activ,
                                                    -calidad_sint,
                                                    -calidad_emoc,
                                                    -calidad_expos,
                                                    -fecha_atencion,
                                                    -fecha_eos,
                                                    -fecha_ige,
                                                    -fecha_sens,
                                                    -ocupacion)
                         )

finalfit::missing_glimpse(data_final_imp)



#### 2. Imputación de datos #####


#library(devtools)
#install.packages("MissMech")
library(MissMech)

### 2.1. Evaluación que tipo de datos perdidos se trata ####
data_final_imp %>%
  select("ige", "tiempo_asma", "leucocitos", "eosinofilos_porc") %>%
  MissMech::TestMCARNormality() # p=0.4171543 --> No es rechaza la Ho, por lo tanto, 
  # no se evidenció evidencia estadística en contra del supuesto de MCAR para el
  # conjunto de variables evaluadas. Sin embargo, sale el aviso que hay 11 casos que
  # han sido removidos para el cálculo de este test. A parte, se observa que el número
  # de casos que analizó fue de 223, cuando se conoce que el total de dato de la base
  # es de 243. Por lo tanto, es necesario indagar cuál fue el motivo de no considerar
  # esos 11 casos y porque solo analizó 223 de 243.

datos_test <- data_final_imp %>%
  select(ige, tiempo_asma, leucocitos, eosinofilos_porc)

datos_test %>%
  mutate(
    n_faltantes = rowSums(is.na(.)),
    todas_na = n_faltantes == 4
  ) %>%
  count(n_faltantes) # Se observa que esos 11 casos desestimados por el Test fueron
  # porque se evidención NA para las 4 variables. Quiere decir que los 223 analizados
  # más 11 se suman 234. Restan saber que ocurrió con los otros 9 casos faltantes.

data_final_imp %>%
  select(ige, tiempo_asma, leucocitos, eosinofilos_porc) %>%
  mutate(
    fila = row_number(),
    patron = paste(
      ifelse(is.na(ige), "NA", "OK"),
      ifelse(is.na(tiempo_asma), "NA", "OK"),
      ifelse(is.na(leucocitos), "NA", "OK"),
      ifelse(is.na(eosinofilos_porc), "NA", "OK"),
      sep = " | "
    )
  ) %>%
  count(patron, sort = TRUE) # Se obtienen los patrones de las diferentes combinaciones
  # para cada variable. Se vuelve a confirmar que el patrón de todo NA correspondieron
  # a los 11 registros que el Test. También se observa que los patrones que el Test
  # omitió y no aviso´fueron aquellos que presentaron una frecuencia de 4, 3 y 2, los
  # cuales suman a 9.
  # La explicación a esa omisión es que TestMCARNormality() tiene por defecto
  # trabajar con los patrones que presenten al menos 6 casos. A manera de análisis de
  # sensibilidad, se vuelve a correr el Test pero incluyendo los patrones emitidos
  # previamente.

MissMech::TestMCARNormality(data_final_imp %>%
    select(ige, tiempo_asma, leucocitos, eosinofilos_porc),
    del.lesscases = 1
    ) # Vuelve a salir el aviso que 11 casos fueron removidos para el cálculo, lo
  # cual ya se sabe que se debe a que en todos los casos se obtuvo NA. Sin embargo,
  # ahora reporta que ya trabajó con 232 casos. El valor p obtenido fue 0.004905063
  # lo cual significa que los datos no responderían a un supuesto de MCAR, por lo
  # tanto se deberá evaluar si cumple con el supuesto MAR desde el punto de vista
  # teórico.


### 2.2. Imputación de datos numéricos (con pmm) y categóricos (con logreg) ####

# Paso 1: Cambio a factor las variables a utilizar para la imputación ----
str(data_final_imp) # Está como character, debe ser factor

data_final_imp[] <- lapply(data_final_imp, function(x) {
  if (is.character(x)) factor(x) else x
  })

str(data_final_imp) # Ahora sí es factor


# Paso 2: Especificar el método de imputación para variables numéricas y categóricas ----
library(mice)
metodo_imput <- sapply(data_final_imp,
                       function(x) ifelse(is.numeric(x),
                                          "pmm",
                                          ifelse(is.factor(x), "logreg", ""))) # Se indica
  # "pmm" para variables numéricas y "logreg" para variables dicotómicas
metodo_imput # Se observan 5 variables que no se tiene interés de imputar:
  # - id_num
  # - sexo_etiq
  # - edad
  # - procede_etiq
  # - asma_severa_etiq
metodo_imput[c("id_num", "sexo_etiq", "edad", "procede_etiq", "asma_severa_etiq")] <- ""
  # Se retiran las que no se tiene el interés de imputar
metodo_imput # Problema solucionado. Ahora solo están aquellas que se desean imputar


# Paso 3: Especificar las variables que se usarán como predictores ----

# NOTA: Dado que el mecanismo de pérdida de datos es MAR, se especifica al conjunto
# de variables que trabajarán como predictores

# a) Creación de la matriz de predictores
pred <- make.predictorMatrix(data_final_imp)

# b) Configurar todo en 0
pred[,] <- 0

# c) Especificación de predictores para cada variable
var_pred <- c("sexo_etiq",
              "edad",
              "asma_severa_etiq",
              "leucocitos",
              "eosinofilos_porc",
              "ige",
              "sensi_etiq",
              "tiempo_asma",
              "aler_medic_etiq",
              "aler_alim_etiq",
              "fam_aler_etiq",
              "cri_anim_etiq",
              "tto_gi_etiq",
              "tto_gs_etiq",
              "tto_antileu_etiq",
              "tto_b2_largo_etiq",
              "tto_b2_corto_etiq",
              "tto_anticol_etiq",
              "tto_asoc_etiq")

# d) Asignación de variables a utilizar para imputar
pred["leucocitos", var_pred] <- 1
pred["eosinofilos_porc", var_pred] <- 1
pred["ige", var_pred] <- 1
pred["sensi_etiq", var_pred] <- 1
pred["tiempo_asma", var_pred] <- 1

pred


# Paso 4: Imputación ----
imputacion <- mice(data_final_imp,
                   seed = 1,
                   predictorMatrix = pred,
                   method = metodo_imput,
                   m = 20) # Se detectó un evento que se procede a investigar
imputacion$loggedEvents # Se observa que no pudo imputar "tto_biol_etiq"debido a que
  # es constante
summary(imputacion) # Brinda como resumen:
  # - Total de imputaciones: 20
  # - Métodos de imputación: pmm y logreg
  # - Matriz de predictores: Con 1 figuran las variables que se usar para predecir la variable
    # de la izquierda
  # - Eventos: Se observa que no pudo imputar tto_biol_etiq porque es constante


### 2.3 Diagnóstico de la imputación ####

# a) Plausibilidad de datos categóricos imputados ----
stripplot(imputacion, ~ sensi_etiq +
                               aler_medic_etiq +
                               aler_alim_etiq +
                               fam_aler_etiq +
                               cri_anim_etiq +
                               tto_gi_etiq +
                               tto_gs_etiq +
                               tto_antileu_etiq +
                               tto_b2_largo_etiq +
                               tto_b2_corto_etiq +
                               tto_anticol_etiq +
                               tto_biol_etiq +
                               tto_asoc_etiq)
  # Se observa todos los datos categóricos plausibles dentro de su categorías plausibles


# b) Plausibilidad de datos numéricos imputados ----
library(lattice)
library(gridExtra)

p1 <- stripplot(
  imputacion,
  leucocitos,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "Leucocitos"
  )

p2 <- stripplot(
  imputacion,
  eosinofilos_porc,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "Eosinófilos (%)"
  )

p3 <- stripplot(
  imputacion,
  ige,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "IgE"
  )

p4 <- stripplot(
  imputacion,
  tiempo_asma,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "Tiempo de asma"
  )

p5 <- bwplot(
  imputacion,
  leucocitos,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "Leucocitos"
  )

p6 <- bwplot(
  imputacion,
  eosinofilos_porc,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "Eosinófilos (%)"
  )

p7 <- bwplot(
  imputacion,
  ige,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "IgE"
  )

p8 <- bwplot(
  imputacion,
  tiempo_asma,
  pch = c(21,20),
  cex = c(1,1.5),
  main = "Tiempo de asma"
  )

grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)
  # Se observa todos los datos numéricos plausibles dentro de sus respectivos rangos

densityplot(imputacion, layout = c(2,2))
  # Las cuatro variables numéricas muestran distribuciones imputadas que, en términos
    # generales, conservan la forma de las distribuciones observadas, aunque existe
    # variabilidad entre las 20 imputaciones, particularmente en las colas y en las
    # variables asimétricas.

# c) Convergencia para variables numéricas ----
plot(imputacion,
     y = c("leucocitos", "eosinofilos_porc", "ige", "tiempo_asma"),
     layout = c(2, 4)
     ) # Para las variables leucocitos, porcentaje de eosinófilos, IgE y tiempo de asma,
  # las cadenas correspondientes a las 20 imputaciones mostraron fluctuaciones alrededor
  # de valores relativamente estables (tanto para su media como para su DE), sin tendencias
  # sistemáticas o divergencia progresiva entre cadenas, lo que fue considerado compatible
  # con convergencia del procedimiento de imputación.”



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 3. ESTADISTICA DESCRIPTIVA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# NOTA: El análisis se realiza con "data_final". Las variables que fueron imputadas serán
#       analizadas a partir de la lista "imputación".

### 3.1. CARACTERISTICAS GENERALES DE LA POBLACION DE ESTUDIO ####

## Primer párrafo de características generales #####
dim(data_final) # Hay 243 pacientes

median(data_final$edad) # 37
min(data_final$edad) # 18
max(data_final$edad) # 92
IQR(data_final$edad) # 28

#tablita_sexo # El 58.02 % (n=141) fue femenino
table(data_final$sexo_etiq) # Fueron 141 mujeres y 102 hombres
prop.table(table(data_final$sexo_etiq))
round(prop.table(table(data_final$sexo_etiq))*100, 2) # El 58.02 % de pacientes
  # fueron mujeres

data_final$edad_cat <- ifelse(data_final$edad<60, "Adulto", "Adulto mayor")
table(data_final$edad_cat) # 203 personas fueron adultos < 60 años.
round(prop.table(table(data_final$edad_cat))*100, 2) # El 83.54 # fueron adultos < 60 años.

#tablita_procedencia # El 70.78 % (n=172) son de Lima
table(data_final$procede_etiq) # 172 fueron de Lima y 38 del Callao
prop.table(table(data_final$procede_etiq))
round(prop.table(table(data_final$procede_etiq))*100, 2) # El 80.78 % de pacientes
  # fueron de Lima.

#tablita_ocupacion # El 26.73 % (n=58) son estudiantes
table(data_final$ocupacion) # 58 pacientes fueron estudiantes
prop.table(table(data_final$ocupacion))
round(prop.table(table(data_final$ocupacion))*100, 2) # El 26.73 % de pacientes
  # fueron estudiantes



## Tabla 1 #####
#tablita_sexo
table(data_final$sexo_etiq) # 141 mujeres y 102 hombres
prop.table(table(data_final$sexo_etiq))
round(prop.table(table(data_final$sexo_etiq))*100, 2) # El 58.02 % son mujeres y el 
  # 41.98 % fueron hombres.

median(data_final$edad) # 37
min(data_final$edad) # 18
max(data_final$edad) # 92

table(data_final$edad_cat) # 203 personas fueron adultos < 60 años.
round(prop.table(table(data_final$edad_cat))*100, 2) # El 83.54 # fueron adultos < 60 años.

#tablita_procedencia
data_final$procede3 <- ifelse(data_final$procede_etiq=="Lima", "Lima",
                        ifelse(data_final$procede_etiq=="Callao", "Callao", "Otros"))
table(data_final$procede3)
round(prop.table(table(data_final$procede3))*100, 2)

#tablita_ocupacion
data_final$ocupa3 <- ifelse(data_final$ocupacion=="estudiante", "Estudiante",
                      ifelse(data_final$ocupacion=="ama de casa", "Ama de casa", "Otros"))
table(data_final$ocupa3)
round(prop.table(table(data_final$ocupa3))*100, 2)




### 3.2. PARÁMETROS BIOQUÍMICOS Y DE SENSIBILIZACIÓN ####

## Tabla 2 #####

# Variable "Leucocitos" sin imputar ----
median(data_final$leucocitos, na.rm = TRUE) # 8200
min(data_final$leucocitos, na.rm = TRUE) # 1048
max(data_final$leucocitos, na.rm = TRUE) # 20250
sum(is.na(data_final$leucocitos)) # Presenta 46 datos perdidos

# Variable "Leucocitos" imputado ----
leucocitos_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(mediana = median(datos$leucocitos),
    minimo  = min(datos$leucocitos),
    maximo  = max(datos$leucocitos))})
  ) # Se obtiene una tabla con la mediana, mínimo y máximo de cada una de las 20 
  # imputaciones calculadas
leucocitos_imput
class(leucocitos_imput) # "matrix" "array" 
leucocitos_imput <- as.data.frame(leucocitos_imput)
class(leucocitos_imput) # "data.frame"

median(leucocitos_imput$mediana) # 8200
min(leucocitos_imput$minimo) # 1048
max(leucocitos_imput$maximo) # 20250


# Variable "Eosinófilos porcentual" sin imputar ----
median(data_final$eosinofilos_porc, na.rm = TRUE) # 5
min(data_final$eosinofilos_porc, na.rm = TRUE) # 0
max(data_final$eosinofilos_porc, na.rm = TRUE) # 18.4
sum(is.na(data_final$eosinofilos_porc)) # Presenta 46 datos perdidos

# Variable "Eosinófilos porcentual" imputado ----
eosi_porc_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(mediana = median(datos$eosinofilos_porc),
    minimo  = min(datos$eosinofilos_porc),
    maximo  = max(datos$eosinofilos_porc))})
  ) # Se obtiene una tabla con la mediana, mínimo y máximo de cada una de las 20 imputaciones
      # calculadas
eosi_porc_imput
class(eosi_porc_imput) # "matrix" "array" 
eosi_porc_imput <- as.data.frame(eosi_porc_imput)
class(eosi_porc_imput) # "data.frame"

median(eosi_porc_imput$mediana) # 4.6
min(eosi_porc_imput$minimo) # 0
max(eosi_porc_imput$maximo) # 18.4


# Variable "Eosinófilos absoluto" sin imputar ----
median(data_final$eosinofilos_new, na.rm = TRUE) # 367.84
min(data_final$eosinofilos_new, na.rm = TRUE) # 0
max(data_final$eosinofilos_new, na.rm = TRUE) # 1586
sum(is.na(data_final$eosinofilos_new)) # Presenta 46 datos perdidos

# Variable "Eosinófilos absoluto" imputado ----

# NOTA: Esta variable le corresponde a "eosinofilos_new", la cual es una dependencia de la
#       variable "eosinofilos_porc". Quiere decir que "eosinofilos_new" no ha sido imputada
#       como sí lo fue "eosinofilos_porc". Por lo tanto, se procederá a recuperar los datos
#       perdidos de la variable "eosinofilos_new" a partir de la imputación de la variable
#       "eosinofilos_porc". Se hará los siguientes pasos:
  # - En cada una de las 20 bases imputadas, a partir de "eosinofilos_porc" se completará
    # "eosinofilos_new" solo donde falte.
  # - Luego se calculará la mediana, mínimo y máximo de eosinofilos_new en cada una de las
    # 20 imputaciones.

metodo_imput[c("leucocitos", "eosinofilos_porc")] # Se comprueba que las variables necesarias
  # para calcular "eosinofilos_new" han sido imputadas.
sapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(leucocitos = sum(is.na(datos$leucocitos)),
    eosinofilos_porc = sum(is.na(datos$eosinofilos_porc)))
  }) # Se comprueba que ya no hay NA para "leucocitos" y "eosinofilos_porc" en cada una de 
  # las 20 base de datos imputadas.

bases_completas <- lapply(1:20, function(i) {
  datos <- complete(imputacion, i) # Se crean 20 bases de datos imputadas agrupadas en "datos"
  datos$eosinofilos_new <- data_final$eosinofilos_new # Se agrega "datos" a "eosinofilos_new"
  faltantes <- is.na(datos$eosinofilos_new)
  datos$eosinofilos_new[faltantes] <-
    datos$leucocitos[faltantes] *
    datos$eosinofilos_porc[faltantes] / 100
  datos
  }) # Se completó "eosinofilos_new" solo donde faltaba
names(bases_completas[[1]]) # Se confirma que "eosinofilos_new" ya se encuentra añadida.

eosinofilos_new_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- bases_completas[[i]]
  c(imputacion = i,
    mediana = median(datos$eosinofilos_new),
    minimo  = min(datos$eosinofilos_new),
    maximo  = max(datos$eosinofilos_new))})
  ) # Se obtiene una tabla con la mediana, mínimo y máximo de cada una de las 20 imputaciones
# calculadas
eosinofilos_new_imput
class(eosinofilos_new_imput) # "matrix" "array" 
eosinofilos_new_imput <- as.data.frame(eosinofilos_new_imput)
class(eosinofilos_new_imput) # "data.frame"

median(eosinofilos_new_imput$mediana) # 357
min(eosinofilos_new_imput$minimo) # 0
max(eosinofilos_new_imput$maximo) # 2430


# Variable "IgE" sin imputar ----
median(data_final$ige, na.rm = TRUE) # 403.8
min(data_final$ige, na.rm = TRUE) # 8.9
max(data_final$ige, na.rm = TRUE) # 6000
sum(is.na(data_final$ige)) # Presenta 39 datos perdidos

# Variable "IgE" sin imputado ----
ige_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(mediana = median(datos$ige),
    minimo  = min(datos$ige),
    maximo  = max(datos$ige))})
  ) # Se obtiene una tabla con la mediana, mínimo y máximo de cada una de las 20 imputaciones
      # calculadas
ige_imput
class(ige_imput) # "matrix" "array" 
ige_imput <- as.data.frame(ige_imput)
class(ige_imput) # "data.frame"

median(ige_imput$mediana) # 402.1
min(ige_imput$minimo) # 8.9
max(ige_imput$maximo) # 6000


# Variable "Sensibilización" sin imputar ----
#tablita_sensibilizacion # El 86.52 % (n=199) SÍ estuvieron sensibilizados
table(data_final$sensi_etiq) # 199 pacientes informaron sensibilización positiva
round(prop.table(table(data_final$sensi_etiq))*100, 2) # El 86.52 % de pacientes
  # reportó sensibilización positiva.
sum(is.na(data_final$sensi_etiq)) # Presenta 13 datos perdidos

# Variable "Sensibilización" imputado ----
sensi_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$sensi_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que sí fueron sensibles
sensi_imput # Se muestra el promedio de la proporción por cada imputación
p <- sensi_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_sensi_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_sensi_imput
pool_p_sensi_imput$qbar # 0.8613169 es la proporción combinada por Rubin
round(pool_p_sensi_imput$qbar*100, 2) #" El 86.13 % de pacientes reportó
  # sensibiliación positiva
round(pool_p_sensi_imput$qbar * 243) # Aproximadamente 209 participantes


# Variable "Agente que generó sensibilización" ----
# - Dermatophagoides pteronyssinus ----
table(data_final$d_ptero_etiq) # Se deben retirar del conteo a los "No aplica"
table(data_final$d_ptero_etiq[data_final$d_ptero_etiq != "No aplica"]) # 190 pacientes
  # fueron sensibilizados a d_ptero_etiq
sum(table(data_final$d_ptero_etiq[data_final$d_ptero_etiq != "No aplica"])) # 199
prop.table(table(data_final$d_ptero_etiq[data_final$d_ptero_etiq != "No aplica"]))*100
  # 95.477387 %

# - Dermatophagoides farinae ----
table(data_final$d_fari_etiq) # Se deben retirar del conteo a los "No aplica"
table(data_final$d_fari_etiq[data_final$d_fari_etiq != "No aplica"]) # 183 pacientes
  # fueron sensibilizados a d_fari_etiq
sum(table(data_final$d_fari_etiq[data_final$d_fari_etiq != "No aplica"])) # 199
prop.table(table(data_final$d_fari_etiq[data_final$d_fari_etiq != "No aplica"]))*100
  # 91.959799 %

# - Blomia tropicalis ----
table(data_final$blomia_etiq) # Se deben retirar del conteo a los "No aplica"
table(data_final$blomia_etiq[data_final$blomia_etiq != "No aplica"]) # 140 pacientes
  # fueron sensibilizados a blomia_etiq
sum(table(data_final$blomia_etiq[data_final$blomia_etiq != "No aplica"])) # 158
prop.table(table(data_final$blomia_etiq[data_final$blomia_etiq != "No aplica"]))*100
  # 88.60759 %

# - Blattella germanica ----
table(data_final$blatella_etiq) # Se deben retirar del conteo a los "No aplica"
table(data_final$blatella_etiq[data_final$blatella_etiq != "No aplica"]) # 60 pacientes
  # fueron sensibilizados a blatella_etiq
sum(table(data_final$blatella_etiq[data_final$blatella_etiq != "No aplica"])) # 197
prop.table(table(data_final$blatella_etiq[data_final$blatella_etiq != "No aplica"]))*100
  # 30.45685 %

# - Epitelio de perro ----
table(data_final$perro_etiq) # Se deben retirar del conteo a los "No aplica"
table(data_final$perro_etiq[data_final$perro_etiq != "No aplica"]) # 37 pacientes
  # fueron sensibilizados a perro_etiq
sum(table(data_final$perro_etiq[data_final$perro_etiq != "No aplica"])) # 199
prop.table(table(data_final$perro_etiq[data_final$perro_etiq != "No aplica"]))*100
  # 18.59296 %

# - Epitelio de gato ----
table(data_final$gato_etiq) # Se deben retirar del conteo a los "No aplica"
table(data_final$gato_etiq[data_final$gato_etiq != "No aplica"]) # 31 pacientes
  # fueron sensibilizados a gato_etiq
sum(table(data_final$gato_etiq[data_final$gato_etiq != "No aplica"])) # 199
prop.table(table(data_final$gato_etiq[data_final$gato_etiq != "No aplica"]))*100
  # 15.57789 %


## Párrafo de tabla 2 #####
table(data_final$asma_severa_etiq) # 16 asmáticos severos

colnames(imputacion$data) # dentro de imputación está la variable "eosinofilos_porc",
  # por lo tanto se extraerá lo requerido usando "imputacion"

sub_eosi_porc_1 <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  subgrupo <- datos$asma_severa_etiq == "Sí"
  c(imputacion = i,
    mediana = median(datos$eosinofilos_porc[subgrupo]),
    minimo  = min(datos$eosinofilos_porc[subgrupo]),
    maximo  = max(datos$eosinofilos_porc[subgrupo]))})
  )
sub_eosi_porc_1
class(sub_eosi_porc_1) # "matrix" "array" 
sub_eosi_porc_1 <- as.data.frame(sub_eosi_porc_1)
class(sub_eosi_porc_1) # "data.frame"

median(sub_eosi_porc_1$mediana) # Mediana de eosinófios de 3 % en el grupo con
  # asma severa
min(sub_eosi_porc_1$minimo) # 0.1 %
max(sub_eosi_porc_1$maximo) # 17 %

table(data_final$asma_severa_etiq) # 227 sin asma severa

sub_eosi_porc_2 <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  subgrupo <- datos$asma_severa_etiq == "No"
  c(imputacion = i,
    mediana = median(datos$eosinofilos_porc[subgrupo]),
    minimo  = min(datos$eosinofilos_porc[subgrupo]),
    maximo  = max(datos$eosinofilos_porc[subgrupo]))})
  )
sub_eosi_porc_2
class(sub_eosi_porc_2) # "matrix" "array" 
sub_eosi_porc_2 <- as.data.frame(sub_eosi_porc_2)
class(sub_eosi_porc_2) # "data.frame"

median(sub_eosi_porc_2$mediana) # Mediana de eosinófios de 4.8 % en el grupo
  # SIN asma severa
min(sub_eosi_porc_2$minimo) # 0 %
max(sub_eosi_porc_2$maximo) # 18.4 %

data_final$acaro <- apply(
  data_final[, c("d_ptero_etiq", "d_fari_etiq", "blomia_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")} # al menos uno es "Sí"
    if (any(is.na(x))) {return(NA)} # hay algún NA y no hay "Sí"
    if (all(x == "No aplica", na.rm = FALSE)) {return("No aplica")} # los tres son "No aplica"
    if (all(x %in% c("No", "No aplica"))) {return("No")} # todos son "No" o combinación de "No" y "No aplica"
    if (all(x == "No")) {return("No")}
    return(NA) }
  )
table(data_final$acaro) # Se deben retirar del conteo a los "No aplica"
table(data_final$acaro, useNA = "ifany") # Se observa que hay 13 datos perdidos
table(data_final$acaro[data_final$acaro != "No aplica"]) # Se retiró los "No aplica"
  # y se observó que la alergia los ácaros estuvo presente en n=198 pacientes
round(prop.table(table(data_final$acaro[data_final$acaro != "No aplica"]))*100, 2)
  # Alergia a los ácaros en el 99.5 % de pacientes.
sum(table(data_final$acaro[data_final$acaro != "No aplica"])) # 199 pacientes con
  # datos disponibles para ácaro
data_final$acaro2 <- ifelse(is.na(data_final$acaro), "perdido", data_final$acaro)
  # Ahora se tiene el interés de saber cuántos en total debieron responder a la
    # pregunta de ácaro, para ello se debe considerar dentro del conteo al la datos
    # perdidos.
table(data_final$acaro2) # Se deben retirar del conteo a los "No aplica"
table(data_final$acaro2[data_final$acaro2 != "No aplica"]) # Son 13 datos perdidos
sum(table(data_final$acaro2[data_final$acaro2 != "No aplica"])) # Son 212 los que 
  # debieron responder a la pregunta de ácaro
round(prop.table(table(data_final$acaro2[data_final$acaro2 != "No aplica"]))*100, 2)
  # Los 13 datos perdidos representan el 6.13 % de todos los que debía de haber
  # respondido la pregunta de ácaro.

table(data_final$asma_severa_etiq) # 16 asmáticos severos
data_final_asma_seve <- subset(data_final, asma_severa_etiq == "Sí") # Se crea el
  # subgrupo de solo asmáticos severos
table(data_final_asma_seve$acaro) # Se debe retirar los "No aplica"
table(data_final_asma_seve$acaro[data_final_asma_seve$acaro != "No aplica"]) # La
  # totalidad de pacientes (n=11) presentó alergia a los ácaros.
round(prop.table(table(data_final_asma_seve$acaro[data_final_asma_seve$acaro != "No aplica"]))*100, 2)
  # El 100 % de pacientes del grupo de asma severa presentó alergia a los ácaros.
table(data_final_asma_seve$acaro2)
table(data_final_asma_seve$acaro2[data_final_asma_seve$acaro2 != "No aplica"]) # 1
  # paciente presenta dato perdido.
round(prop.table(table(data_final_asma_seve$acaro2[data_final_asma_seve$acaro2 != "No aplica"]))*100, 2)
  # En el grupo de pacientes con asma severa, el 8.33 % presentó dato perdido para
  # alergia a los acaros.

table(data_final$asma_severa_etiq) # 227 sin asma severa
data_final_asma_no_seve <- subset(data_final, asma_severa_etiq == "No") # Se crea el
  # subgrupo de solo asmáticos severos
table(data_final_asma_no_seve$acaro) # Se debe retirar los "No aplica"
table(data_final_asma_no_seve$acaro[data_final_asma_no_seve$acaro != "No aplica"])
  # 187 pacientes presentó alergia a los ácaros.
round(prop.table(table(data_final_asma_no_seve$acaro[data_final_asma_no_seve$acaro != "No aplica"]))*100, 2)
  # El 99.47 % de pacientes del grupo de asma NO severa presentó alergia a los ácaros.
table(data_final_asma_no_seve$acaro2)
table(data_final_asma_no_seve$acaro2[data_final_asma_no_seve$acaro2 != "No aplica"])
  # 12 pacientes presentaron datos perdidos.
round(prop.table(table(data_final_asma_no_seve$acaro2[data_final_asma_no_seve$acaro2 != "No aplica"]))*100, 2)
  # En el grupo de pacientes con asma NO severa, el 6.00 % presentó dato perdido para
  # alergia a los acaros



### 3.3. CARACTERIZACIÓN CLÍNICA DE LOS PACIENTES CON ASMA ####

## Tabla 3 #####

# Variable "Asma severa" ----
table(data_final$asma_severa_etiq) # 16
prop.table(table(data_final$asma_severa_etiq))*100 # 6.58 %


# Variable "Tiempo de asma (años)" sin imputar ----
shapiro.test(data_final$tiempo_asma) # p=3.607e-09 --> No Normal 
median(data_final$tiempo_asma, na.rm = T) # 15
min(data_final$tiempo_asma, na.rm = T) # 0
max(data_final$tiempo_asma, na.rm = T) # 75
sum(is.na(data_final$tiempo_asma)) # 65 datos perdidos

# Variable "Tiempo de asma (años)" imputada ----
tiempo_asma_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(mediana = median(datos$tiempo_asma),
    minimo  = min(datos$tiempo_asma),
    maximo  = max(datos$tiempo_asma))})
  ) # Se obtiene una tabla con la mediana, mínimo y máximo de ada una de las 20 imputaciones
      # calculadas
tiempo_asma_imput
class(tiempo_asma_imput) # "matrix" "array" 
tiempo_asma_imput <- as.data.frame(tiempo_asma_imput)
class(tiempo_asma_imput) # "data.frame"

median(tiempo_asma_imput$mediana) # 15
min(tiempo_asma_imput$minimo) # 0
max(tiempo_asma_imput$maximo) # 75


# Variable "Antecedente alergia a los medicamentos" sin imputar ----
table(data_final$aler_medic_etiq) # El 27.31 % (n=65) SÍ reportaron antecedente de
  # alergia a medicamento
sum(table(data_final$aler_medic_etiq)) # 238
round(prop.table(table(data_final$aler_medic_etiq))*100, 2) # 27.31 %
sum(is.na(data_final$aler_medic_etiq)) # 5 datos perdidos

# Variable "Antecedente alergia a los medicamentos" imputada ----
aler_med_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$aler_medic_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que sí fueron sensibles
aler_med_imput # Se muestra el promedio de la proporción por cada imputación
p <- aler_med_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_aler_med_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_aler_med_imput
pool_p_aler_med_imput$qbar # 0.272428 es la proporción combinada por Rubin
round(pool_p_aler_med_imput$qbar*100, 2)
round(pool_p_aler_med_imput$qbar * 243) # Aproximadamente 66 participantes


# Variable "Antecedente alergia a los alimentos" sin imputar ----
table(data_final$aler_alim_etiq) # Hay 14 pacientes con antecedente de alergia a alimentos
sum(table(data_final$aler_alim_etiq)) # Hay 237 pacientes con información disponible.
round(prop.table(table(data_final$aler_alim_etiq))*100, 2) # El 5.91 % de pacientes
  # presenta reporte de antecedente de alergia a alimentos.
sum(is.na(data_final$aler_alim_etiq)) # 6 datos perdidos

# Variable "Antecedente alergia a los alimentos" imputada ----
aler_alim_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$aler_alim_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que sí fueron alérgicos
aler_alim_imput # Se muestra el promedio de la proporción por cada imputación
p <- aler_alim_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_aler_alim_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_aler_alim_imput
pool_p_aler_alim_imput$qbar # 0.05864198 es la proporción combinada por Rubin
round(pool_p_aler_alim_imput$qbar*100, 2) # 5.86 % 
round(pool_p_aler_alim_imput$qbar * 243) # Aproximadamente 14 participantes


# Variable "Antecedente familiares con alergia" sin imputar ----
table(data_final$fam_aler_etiq) # Hay 65 pacientes con antecedente de alergia a alimentos
sum(table(data_final$fam_aler_etiq)) # # Hay 227 pacientes con información disponible.
round(prop.table(table(data_final$fam_aler_etiq))*100, 2) # El 28.63 % de pacientes presentó
  # antecedente de alergia a alimentos.
sum(is.na(data_final$fam_aler_etiq)) # 16 datos perdidos

# Variable "Antecedente familiares con alergia" imputada ----
fam_aler_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$fam_aler_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ tuvieron familiares alérgicos
fam_aler_imput # Se muestra el promedio de la proporción por cada imputación
p <- fam_aler_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_fam_aler_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_fam_aler_imput
pool_p_fam_aler_imput$qbar # 0.2866255 es la proporción combinada por Rubin
round(pool_p_fam_aler_imput$qbar*100, 2) # 28.66 %
round(pool_p_fam_aler_imput$qbar * 243) # Aproximadamente 70 participantes


# Variable "Familiar con alergia" ----
# - Madre ----
data_final$madre_alergia <- apply(
  data_final[, c("madre_asma_etiq", "madre_rin_etiq", "madre_ecz_etiq", "madre_urti_etiq",
                 "madre_aler_alim_etiq", "madre_aler_med_etiq", "madre_pic_inse_etiq",
                 "madre_dermat_etiq", "madre_otro_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")} # al menos uno es "Sí"
    if (any(is.na(x))) {return(NA)} # hay algún NA y no hay "Sí"
    if (all(x == "No aplica", na.rm = FALSE)) {return("No aplica")} # los tres son "No aplica"
    if (all(x %in% c("No", "No aplica"))) {return("No")} # todos son "No" o combinación de "No" y "No aplica"
    if (all(x == "No")) {return("No")}
    return(NA) }
  )
table(data_final$madre_alergia) # Madre con alergia en n=37 pacientes
inclu_1 <- data_final$madre_alergia != "No aplica" 
sum(table(data_final$madre_alergia[inclu_1])) # Hay 65 pacientes
round(prop.table(table(data_final$madre_alergia[inclu_1]))*100, 2) # Madre con alergia en
  # el 56.92 % de pacientes.


# - Padre ----
data_final$padre_alergia <- apply(
  data_final[, c("padre_asma_etiq", "padre_rin_etiq", "padre_ecz_etiq", "padre_urti_etiq",
                 "padre_aler_alim_etiq", "padre_aler_med_etiq", "padre_pic_inse_etiq",
                 "padre_dermat_etiq", "padre_otro_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")} # al menos uno es "Sí"
    if (any(is.na(x))) {return(NA)} # hay algún NA y no hay "Sí"
    if (all(x == "No aplica", na.rm = FALSE)) {return("No aplica")} # los tres son "No aplica"
    if (all(x %in% c("No", "No aplica"))) {return("No")} # todos son "No" o combinación de "No" y "No aplica"
    if (all(x == "No")) {return("No")}
    return(NA) }
  )
table(data_final$padre_alergia) # Padre con alergia en n=32 pacientes
inclu_2 <- data_final$padre_alergia != "No aplica" 
sum(table(data_final$padre_alergia[inclu_2])) # Hay 65 pacientes.
round(prop.table(table(data_final$padre_alergia[inclu_2]))*100, 2) # Padre con alergia
  # en el 49.23 % de pacientes.


# - Hermanos ----
data_final$hermanos_alergia <- apply(
  data_final[, c("herm_asma_etiq", "herm_rin_etiq", "herm_ecz_etiq", "herm_urti_etiq",
                 "herm_aler_alim_etiq", "herm_aler_med_etiq", "herm_pic_inse_etiq",
                 "herm_dermat_etiq", "herm_otro_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")} # al menos uno es "Sí"
    if (any(is.na(x))) {return(NA)} # hay algún NA y no hay "Sí"
    if (all(x == "No aplica", na.rm = FALSE)) {return("No aplica")} # los tres son "No aplica"
    if (all(x %in% c("No", "No aplica"))) {return("No")} # todos son "No" o combinación de "No" y "No aplica"
    if (all(x == "No")) {return("No")}
    return(NA) }
  )
table(data_final$hermanos_alergia) # Hermanos con alergia en n=17 pacientes
inclu_3 <- data_final$hermanos_alergia != "No aplica"
sum(table(data_final$hermanos_alergia[inclu_3])) # Hay 65 pacientes
round(prop.table(table(data_final$hermanos_alergia[inclu_3]))*100, 2) # Hermanos con
  # alergia en el 26.15 % de pacientes.


# - Abuelos maternos ----
data_final$abu_mama_alergia <- apply(
  data_final[, c("abu_mama_asma_etiq", "abu_mama_rin_etiq", "abu_mama_ecz_etiq", "abu_mama_urti_etiq",
                 "abu_mama_aler_alim_etiq", "abu_mama_aler_med_etiq", "abu_mama_pic_inse_etiq",
                 "abu_mama_dermat_etiq", "abu_mama_otro_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")} # al menos uno es "Sí"
    if (any(is.na(x))) {return(NA)} # hay algún NA y no hay "Sí"
    if (all(x == "No aplica", na.rm = FALSE)) {return("No aplica")} # los tres son "No aplica"
    if (all(x %in% c("No", "No aplica"))) {return("No")} # todos son "No" o combinación de "No" y "No aplica"
    if (all(x == "No")) {return("No")}
    return(NA) }
  )
table(data_final$abu_mama_alergia) # Abuelos maternos con alergia en n=4 pacientes
inclu_4 <- data_final$abu_mama_alergia != "No aplica"
sum(table(data_final$abu_mama_alergia[inclu_4])) # Hay 65 pacientes.
round(prop.table(table(data_final$abu_mama_alergia[inclu_4]))*100, 2) # Abuelos maternos con alergia
  # en el 6.15 % de pacientes.


# - Abuelos paternos ----
data_final$abu_papa_alergia <- apply(
  data_final[, c("abu_papa_asma_etiq", "abu_papa_rin_etiq", "abu_papa_ecz_etiq", "abu_papa_urti_etiq",
                 "abu_papa_aler_alim_etiq", "abu_papa_aler_med_etiq", "abu_papa_pic_inse_etiq",
                 "abu_papa_dermat_etiq", "abu_papa_otro_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")} # al menos uno es "Sí"
    if (any(is.na(x))) {return(NA)} # hay algún NA y no hay "Sí"
    if (all(x == "No aplica", na.rm = FALSE)) {return("No aplica")} # los tres son "No aplica"
    if (all(x %in% c("No", "No aplica"))) {return("No")} # todos son "No" o combinación de "No" y "No aplica"
    if (all(x == "No")) {return("No")}
    return(NA) }
  )
table(data_final$abu_papa_alergia) # Abuelos paternos con alergia en n=1 pacientes
inclu_5 <- data_final$abu_papa_alergia != "No aplica"
sum(table(data_final$abu_papa_alergia[inclu_5])) # Hay 65 pacientes.
round(prop.table(table(data_final$abu_papa_alergia[inclu_5]))*100, 2) # Abuelos paternos con alergia en
  # el 1.54 % de pacientes.


# Variable "Crianza de animales" sin imputar ----
table(data_final$cri_anim_etiq) # # Hay 120 pacientes que crian animales
sum(table(data_final$cri_anim_etiq)) # # Hay 226 pacientes con información disponible.
round(prop.table(table(data_final$cri_anim_etiq))*100, 2) # # El 53.1 % de pacientes presenta
  # reporte criar animales.
sum(is.na(data_final$cri_anim_etiq)) # 17 datos perdidos

# Variable "Crianza de animales" imputado ----
cri_anim_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$cri_anim_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ tuvieron familiares alérgicos
cri_anim_imput # Se muestra el promedio de la proporción por cada imputación
p <- cri_anim_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_cri_anim_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_cri_anim_imput
pool_p_cri_anim_imput$qbar # 0.5302469 es la proporción combinada por Rubin
round(pool_p_cri_anim_imput$qbar*100, 2) # 53.02 %
round(pool_p_cri_anim_imput$qbar * 243) # Aproximadamente 129 participantes


# Variable "Animales que cría" ----
table(data_final$animal) # 120 personas crían animales y 148 animales criados
sum(table(data_final$animal)) # 226


# - Crianza de perros ####
table(data_final$animal)

data_final$animal_perro <- apply(
  data_final[, c("animal")],
  1,
  function(x) {
    if (any(x %in% c("perro", "perro gato"))) {return("Sí")}
    if (any(is.na(x))) {return(NA)}
    if (all(x == "999")) {return("No aplica")}
    return("No") }
  )
data_final <- data_final %>% relocate(animal_perro, .after = animal) # Movilización
  # de la variable etiquetada a la derecha de la variable no etiquetada
table(data_final$animal_perro)
table(data_final$animal_perro[data_final$animal_perro != "No aplica"]) # 96 personas
  # crían perros
sum(table(data_final$animal_perro[data_final$animal_perro != "No aplica"])) # 120
  # respondieron a la pregunta de crianza de perros
round(prop.table(table(data_final$animal_perro[data_final$animal_perro != "No aplica"]))*100, 2)
  # 80 %


# - Crianza de gatos ####
table(data_final$animal)

data_final$animal_gato <- apply(
  data_final[, c("animal")],
  1,
  function(x) {
    if (any(x %in% c("gato", "perro gato"))) {return("Sí")}
    if (any(is.na(x))) {return(NA)}
    if (all(x == "999")) {return("No aplica")}
    return("No") }
  )
data_final <- data_final %>% relocate(animal_gato, .after = animal_perro) # Movilización
  # de la variable etiquetada a la derecha de la variable no etiquetada
table(data_final$animal_gato)
table(data_final$animal_gato[data_final$animal_gato != "No aplica"]) # 47 personas
  # crían gatos
sum(table(data_final$animal_gato[data_final$animal_gato != "No aplica"])) # 120
  # respondieron a la pregunta de crianza de gatos
round(prop.table(table(data_final$animal_gato[data_final$animal_gato != "No aplica"]))*100, 2)
  # 39.17 %


# - Crianza de otros animales ####
table(data_final$animal)

data_final$animal_otros <- apply(
  data_final[, c("animal")],
  1,
  function(x) {
    if (any(x %in% c("canarios", "conejo", "loros"))) {return("Sí")}
    if (any(is.na(x))) {return(NA)}
    if (all(x == "999")) {return("No aplica")}
    return("No") }
  )
data_final <- data_final %>% relocate(animal_otros, .after = animal_gato) # Movilización
  # de la variable etiquedada a la derecha de la variable no etiquetada
table(data_final$animal_otros) # Hay que retirar los "No aplica"
table(data_final$animal_otros[data_final$animal_otros != "No aplica"]) # 4 personas
  # crían otros tipos de animales
sum(table(data_final$animal_otros[data_final$animal_otros != "No aplica"])) # 120
  # respondieron a la pregunta de crianza de otros tipos de animales
round(prop.table(table(data_final$animal_otros[data_final$animal_otros != "No aplica"]))*100, 2)
  # 3.33 %



# Variable "Hábitos nocivos" ####
table(data_final$habito_noc_etiq) # 2
sum(table(data_final$habito_noc_etiq)) # 40
round(prop.table(table(data_final$habito_noc_etiq))*100, 2) # 5.00 %

table(data_final$habito) # Los únicos hábitos nocivos son consumo de tabaco en 2 personas


# Variable "Tratamiento" ####
# - Glucocorticoide inhalado sin imputar ----
table(data_final$tto_gi_etiq) # 158
sum(table(data_final$tto_gi_etiq)) # 239
round(prop.table(table(data_final$tto_gi_etiq))*100, 2) # 66.11 %
sum(is.na(data_final$tto_gi_etiq)) # 4 datos perdidos

# - Glucocorticoide inhalado imputado ----
tto_gi_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_gi_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron gi
tto_gi_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_gi_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_gi_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_gi_imput
pool_p_tto_gi_imput$qbar # 0.6596708 es la proporción combinada por Rubin
round(pool_p_tto_gi_imput$qbar*100, 2) # 65.97 %
round(pool_p_tto_gi_imput$qbar * 243) # Aproximadamente 160 participantes


# - Glucocorticoide sistémico sin imputar ----
table(data_final$tto_gs_etiq) # 1
sum(table(data_final$tto_gs_etiq)) # 239
round(prop.table(table(data_final$tto_gs_etiq))*100, 2) # 0.42 %
sum(is.na(data_final$tto_gs_etiq)) # 4 datos perdidos

# - Glucocorticoide sistémico imputado ----
tto_gs_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_gs_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron gi
tto_gs_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_gs_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_gs_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_gs_imput
pool_p_tto_gs_imput$qbar # 0.004115226 es la proporción combinada por Rubin
round(pool_p_tto_gs_imput$qbar*100, 2) # 0.41 %
round(pool_p_tto_gs_imput$qbar * 243) # Aproximadamente 1 participantes


# - Antileucotrienos sin imputar ----
table(data_final$tto_antileu_etiq) # 46
sum(table(data_final$tto_antileu_etiq)) # 239
round(prop.table(table(data_final$tto_antileu_etiq))*100, 2) # 19.25 %
sum(is.na(data_final$tto_antileu_etiq)) # 4 datos perdidos

# - Antileucotrienos imputado ----
tto_antileu_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_antileu_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron antileu
tto_antileu_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_antileu_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_antileu_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_antileu_imput
pool_p_tto_antileu_imput$qbar # 0.1927984 es la proporción combinada por Rubin
round(pool_p_tto_antileu_imput$qbar*100, 2) # 19.28 %
round(pool_p_tto_antileu_imput$qbar * 243) # Aproximadamente 47 participantes


# - LABA sin imputar ----
table(data_final$tto_b2_largo_etiq) # 149
sum(table(data_final$tto_b2_largo_etiq)) # 239
round(prop.table(table(data_final$tto_b2_largo_etiq))*100, 2) # 62.34
sum(is.na(data_final$tto_b2_largo_etiq)) # 4 datos perdidos

# - LABA imputado ----
tto_b2_largo_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_b2_largo_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron LABA
tto_b2_largo_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_b2_largo_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_b2_largo_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_b2_largo_imput
pool_p_tto_b2_largo_imput$qbar # 0.6230453 es la proporción combinada por Rubin
round(pool_p_tto_b2_largo_imput$qbar*100, 2) # 62.30 %
round(pool_p_tto_b2_largo_imput$qbar * 243) # Aproximadamente 151 participantes


# - SABA sin imputar ----
table(data_final$tto_b2_corto_etiq) # 22
sum(table(data_final$tto_b2_corto_etiq)) # 239
round(prop.table(table(data_final$tto_b2_corto_etiq))*100, 2) # 9.21 %
sum(is.na(data_final$tto_b2_corto_etiq)) # 4 datos perdidos

# - SABA imputado ----
tto_b2_corto_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_b2_corto_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron SABA
tto_b2_corto_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_b2_corto_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_b2_corto_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_b2_corto_imput
pool_p_tto_b2_corto_imput$qbar # 0.09218107 es la proporción combinada por Rubin
round(pool_p_tto_b2_corto_imput$qbar*100, 2) # 9.22 %
round(pool_p_tto_b2_corto_imput$qbar * 243) # Aproximadamente 22 participantes


# - Anticolinérgico sin imputar ----
table(data_final$tto_anticol_etiq) # 3
sum(table(data_final$tto_anticol_etiq)) # 239
round(prop.table(table(data_final$tto_anticol_etiq))*100, 2) # 1.26 %
sum(is.na(data_final$tto_anticol_etiq)) # 4 datos perdidos

# - Anticolinérgico imputado ----
tto_anticol_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_anticol_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron anticolinérgicos
tto_anticol_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_anticol_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_anticol_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_anticol_imput
pool_p_tto_anticol_imput$qbar # 0.0127572 es la proporción combinada por Rubin
round(pool_p_tto_anticol_imput$qbar*100, 2) # 1.28 %
round(pool_p_tto_anticol_imput$qbar * 243) # Aproximadamente 3 participantes


# - Biológicos ----
table(data_final$tto_biol_etiq) # 0
sum(table(data_final$tto_biol_etiq)) # 239
round(prop.table(table(data_final$tto_biol_etiq))*100, 2) # 0.00


# - Tratamiento asociado sin imputar ----
table(data_final$tto_asoc_etiq) # 1
sum(table(data_final$tto_asoc_etiq)) # 238
round(prop.table(table(data_final$tto_asoc_etiq))*100, 2) # 0.42
sum(is.na(data_final$tto_asoc_etiq)) # 5 datos perdidos

# - Tratamiento asociado imputado ----
tto_asoc_imput <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  c(imputacion = i,
    proporcion = mean(datos$tto_asoc_etiq == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron tto. asociado
tto_asoc_imput # Se muestra el promedio de la proporción por cada imputación
p <- tto_asoc_imput[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_tto_asoc_imput <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_tto_asoc_imput
pool_p_tto_asoc_imput$qbar # 0.004115226 es la proporción combinada por Rubin
round(pool_p_tto_asoc_imput$qbar*100, 2) # 0.41 %
round(pool_p_tto_asoc_imput$qbar * 243) # Aproximadamente  participante


# Variable "Intensidad del tto" ####
table(data_final$intensidad_tto_etiq) # Se deben retirar los "No aplica"
table(data_final$intensidad_tto_etiq[data_final$intensidad_tto_etiq != "No aplica"])
sum(table(data_final$intensidad_tto_etiq[data_final$intensidad_tto_etiq != "No aplica"]))
round(prop.table(table(data_final$intensidad_tto_etiq[data_final$intensidad_tto_etiq != "No aplica"]))*100, 2)
  # 89.31 % de dosis baja y 10.69 de dosis alta


## Párrafo de tabla 3 #####
table(data_final$asma_severa_etiq) # 16 pacientes con asma severa
round(prop.table(table(data_final$asma_severa_etiq))*100, 2) # 6.58

round(pool_p_aler_med_imput$qbar*100, 2) # 27.24 %
round(pool_p_aler_med_imput$qbar * 243) # Hay 66 pacientes con reporte de alergia a medicamentos

round(pool_p_aler_alim_imput$qbar*100, 2) # 5.86 %
round(pool_p_aler_alim_imput$qbar * 243) # Hay 14 pacientes con reporte de alergia a alimentos

table(data_final$madre_alergia[data_final$madre_alergia != "No aplica"]) # Hay 37
  # pacientes cuya madre tenía alergia
round(prop.table(table(data_final$madre_alergia[data_final$madre_alergia != "No aplica"]))* 100, 2)
  # El 56.92 % de los pacientes presenta madre con alergia.

# Mamá ----
table(data_final$madre_asma_etiq[data_final$madre_asma_etiq != "No aplica"]) # 15
sum(table(data_final$madre_asma_etiq[data_final$madre_asma_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_asma_etiq[data_final$madre_asma_etiq != "No aplica"]))*100, 2)
  # 15/65 pacientes tienen madre con asma, lo cual representa el 23.08 %

table(data_final$madre_rin_etiq[data_final$madre_rin_etiq != "No aplica"]) # 15
sum(table(data_final$madre_rin_etiq[data_final$madre_rin_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_rin_etiq[data_final$madre_rin_etiq != "No aplica"]))*100, 2)
  # 15/65 pacientes tienen madre con rinitis, lo cual representa el 23.08 #

table(data_final$madre_ecz_etiq[data_final$madre_ecz_etiq != "No aplica"]) # 1
sum(table(data_final$madre_ecz_etiq[data_final$madre_ecz_etiq != "No aplica"])) #65
round(prop.table(table(data_final$madre_ecz_etiq[data_final$madre_ecz_etiq != "No aplica"]))*100, 2)
  # 1/65 paciente tiene madre con eczema, lo cual representa el 1.54 %

table(data_final$madre_urti_etiq[data_final$madre_urti_etiq != "No aplica"]) # 2
sum(table(data_final$madre_urti_etiq[data_final$madre_urti_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_urti_etiq[data_final$madre_urti_etiq != "No aplica"]))*100, 2)
  # 2/65 pacientes tienen madre con urticaria, lo cual representa un 3.08 %

table(data_final$madre_aler_alim_etiq[data_final$madre_aler_alim_etiq != "No aplica"]) # 1
sum(table(data_final$madre_aler_alim_etiq[data_final$madre_aler_alim_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_aler_alim_etiq[data_final$madre_aler_alim_etiq != "No aplica"]))*100, 2)
  # 1/65 paciente tiene madre con alergia alimentaria, lo cual representa el 1.54 %

table(data_final$madre_aler_med_etiq[data_final$madre_aler_med_etiq != "No aplica"]) # 2
sum(table(data_final$madre_aler_med_etiq[data_final$madre_aler_med_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_aler_med_etiq[data_final$madre_aler_med_etiq != "No aplica"]))*100, 2)
  # 2/65 pacientes tienen madre con alergia medicamentosa, lo cual representa el 3.08 %

table(data_final$madre_pic_inse_etiq[data_final$madre_pic_inse_etiq != "No aplica"]) # 1
sum(table(data_final$madre_pic_inse_etiq[data_final$madre_pic_inse_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_pic_inse_etiq[data_final$madre_pic_inse_etiq != "No aplica"]))*100, 2)
  # 1/65 paciente tiene madre con alergia a la picadura de insectos, lo cua representa el 1.54 %

table(data_final$madre_dermat_etiq[data_final$madre_dermat_etiq != "No aplica"]) # 1
sum(table(data_final$madre_dermat_etiq[data_final$madre_dermat_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_dermat_etiq[data_final$madre_dermat_etiq != "No aplica"]))*100, 2)
  # 1/65 paciente tiene madre con dermatitis, lo cual representa el 1.54 %

table(data_final$madre_otro_etiq[data_final$madre_otro_etiq != "No aplica"]) # 0
sum(table(data_final$madre_otro_etiq[data_final$madre_otro_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$madre_otro_etiq[data_final$madre_otro_etiq != "No aplica"]))*100, 2)
  # No hay pacientes con madres que presenten otros tipos de alergias


# Papá ----
table(data_final$padre_asma_etiq[data_final$padre_asma_etiq != "No aplica"]) # 12
sum(table(data_final$padre_asma_etiq[data_final$padre_asma_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_asma_etiq[data_final$padre_asma_etiq != "No aplica"]))*100, 2)
  # 12/65 pacientes tienen padre con asma, lo cual representa el 18.46 %

table(data_final$padre_rin_etiq[data_final$padre_rin_etiq != "No aplica"]) # 20
sum(table(data_final$padre_rin_etiq[data_final$padre_rin_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_rin_etiq[data_final$padre_rin_etiq != "No aplica"]))*100, 2)
  # 20/65 pacientes tienen padre con rinitis, lo cual representa el 30.77

table(data_final$padre_ecz_etiq[data_final$padre_ecz_etiq != "No aplica"]) # 2
sum(table(data_final$padre_ecz_etiq[data_final$padre_ecz_etiq != "No aplica"])) #65
round(prop.table(table(data_final$padre_ecz_etiq[data_final$padre_ecz_etiq != "No aplica"]))*100, 2)
  # 2/65 paciente tiene padre con eczema, lo cual representa el 3.08 %

table(data_final$padre_urti_etiq[data_final$padre_urti_etiq != "No aplica"]) # 5
sum(table(data_final$padre_urti_etiq[data_final$padre_urti_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_urti_etiq[data_final$padre_urti_etiq != "No aplica"]))*100, 2)
  # 5/65 pacientes tienen padre con urticaria, lo cual representa un 7.69 %

table(data_final$padre_aler_alim_etiq[data_final$padre_aler_alim_etiq != "No aplica"]) # 2
sum(table(data_final$padre_aler_alim_etiq[data_final$padre_aler_alim_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_aler_alim_etiq[data_final$padre_aler_alim_etiq != "No aplica"]))*100, 2)
  # 2/65 pacientes tienen padre con alergia alimentaria, lo cual representa el 3.08 %

table(data_final$padre_aler_med_etiq[data_final$padre_aler_med_etiq != "No aplica"]) # 0
sum(table(data_final$padre_aler_med_etiq[data_final$padre_aler_med_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_aler_med_etiq[data_final$padre_aler_med_etiq != "No aplica"]))*100, 2)
  # No hay pacientes con padres que presenten alergia a medicamentos

table(data_final$padre_pic_inse_etiq[data_final$padre_pic_inse_etiq != "No aplica"]) # 0
sum(table(data_final$padre_pic_inse_etiq[data_final$padre_pic_inse_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_pic_inse_etiq[data_final$padre_pic_inse_etiq != "No aplica"]))*100, 2)
  # No hay pacientes con padres que presenten alergia a la picadura de insectos

table(data_final$padre_dermat_etiq[data_final$padre_dermat_etiq != "No aplica"]) # 0
sum(table(data_final$padre_dermat_etiq[data_final$padre_dermat_etiq != "No aplica"])) # 65
round(prop.table(table(data_final$padre_dermat_etiq[data_final$padre_dermat_etiq != "No aplica"]))*100, 2)
  # No hay pacientes con padres que presenten dermatitis

table(data_final$padre_otro_etiq[data_final$padre_otro_etiq != "No aplica"]) # 0
sum(table(data_final$padre_otro_etiq[data_final$padre_otro_etiq != "No aplica"])) # 67
round(prop.table(table(data_final$padre_otro_etiq[data_final$padre_otro_etiq != "No aplica"]))*100, 2)
  # No hay pacientes con padres que presenten otros tipos de alergias


# Tratamiento ----
round(pool_p_tto_gi_imput$qbar*100, 2) # 65.97 %
round(pool_p_tto_gi_imput$qbar * 243) # Aproximadamente 160 participantes

round(pool_p_tto_b2_largo_imput$qbar*100, 2) # 62.30 %
round(pool_p_tto_b2_largo_imput$qbar * 243) # Aproximadamente 151 participantes

table(data_final$asma_severa_etiq) # 16 pacientes con asma severa
table(data_final_asma_seve$tto_b2_corto_etiq) # 2
round(prop.table(table(data_final_asma_seve$tto_b2_corto_etiq))*100, 2) # 12.5 %
sum(is.na(data_final_asma_seve$tto_b2_corto_etiq)) # No hay datos perdidos

table(data_final$asma_severa_etiq) # 227 pacientes SIN asma severa
table(data_final_asma_no_seve$tto_b2_corto_etiq) # 20
round(prop.table(table(data_final_asma_no_seve$tto_b2_corto_etiq))*100, 2) # 8.97 %
sum(is.na(data_final_asma_no_seve$tto_b2_corto_etiq)) # Peo hay 4 datos perdidos, es
  # necesario dar el resulado imputado.
sub_tto_b2_corto <- do.call(rbind, lapply(1:20, function(i) {
  datos <- complete(imputacion, i)
  subgrupo <- datos$asma_severa_etiq == "No"
  c(imputacion = i,
    proporcion = mean(datos$tto_b2_corto_etiq[subgrupo] == "Sí"))})
  ) # Se calcula el promedio de las proporciones que SÍ usaron SABA en el grupo de
  # asmáticos NO severos
sub_tto_b2_corto # Se muestra el promedio de la proporción por cada imputación
p <- sub_tto_b2_corto[, "proporcion"] # Se extrae la proporción
n <- nrow(data_final)
u <- p * (1 - p) / n # Se calcula la varianza dentro de cada imputación
pool_p_sub_tto_b2_corto <- pool.scalar(
  Q = p,
  U = u
  ) # Combina las 20 estimaciones de proporción mediante las reglas de Rubin
pool_p_sub_tto_b2_corto
pool_p_sub_tto_b2_corto$qbar # 0.08986784 es la proporción combinada por Rubin
round(pool_p_sub_tto_b2_corto$qbar*100, 2) # 8.99 %
round(pool_p_sub_tto_b2_corto$qbar * 227) # Aproximadamente 20 participantes

data_final$corticoide <- apply(
  data_final[, c("tto_gi_etiq", "tto_gs_etiq")],
  1,
  function(x) {
    if (any(x == "Sí", na.rm = TRUE)) {return("Sí")}
    if (all(is.na(x))) {return(NA)}
    return("No") }
  )
table(data_final$corticoide) # 159 pacientes recibió corticoides
sum(table(data_final$corticoide)) # 239 pacientes estuvieron disponibles a la pregunta
  # de corticoide.
sum(is.na(data_final$corticoide)) # Hubo 4 datos perdidos
table(data_final$intensidad_tto_etiq[data_final$corticoide == "Sí"]) # 17
sum(table(data_final$intensidad_tto_etiq[data_final$corticoide == "Sí"]))# 159
round(prop.table(table(data_final$intensidad_tto_etiq[data_final$corticoide == "Sí"]))*100, 2)
  # 17/159 pacientes recibieron corticoides en dosis altas, lo cual representa el 10.69 %

table(data_final$asma_severa_etiq) # Hubo 227 pacientes SIN asma severa
table(data_final_asma_no_seve$intensidad_tto_etiq) # 1 paciente recibió dosis alta


### 3.4. CARACTERIZACIÓN DE LOS ANTECEDENTES REPORTADOS DE ALERGIA A MEDICAMENTOS ####

## Tabla 4 #####

# Variable "Medicamentos a los que reportó alergia" ----
# - Alergia a los AINE  ----
table(data_final$medic)

data_final$medic_aine <- apply(
  data_final[, c("medic")],
  1,
  function(x) {
    if (any(x %in% c("aines", "aines penicilina", "diclofenaco", "ibuprofeno",
                     "ketorolaco", "naproxeno", "naproxeno diclofenaco penicilina",
                     "penicilina aines", "acido salicilico"), na.rm = TRUE)) {return("Sí")}
    if (any(x == "999", na.rm = TRUE)) {return("No aplica")}
    if (any(is.na(x))) {return(NA)}
    return("No") }
  )
table(data_final$medic_aine) # No se retira los "No aplica" porque se quiere calcular
  # la proporción en todos los pacientes asmáticos
sum(table(data_final$medic_aine)) # Son 237 asmáticos disponibles
round(prop.table(table(data_final$medic_aine))*100, 2)
  # 34/237 pacientes reportaron alergia a los AINE, lo cual representa el 14.35 % de
    # todos los asmáticos

table(data_final$medic_aine) # Ahora sí se retira los "No aplica" porque interesa solo
  # los que reportaron alergia a algún medicamento.
table(data_final$medic_aine[data_final$medic_aine != "No aplica"]) # Hay 34 alérgicos
  # a los AINE
sum(table(data_final$medic_aine[data_final$medic_aine != "No aplica"])) # Son 64 datos
  # disponibles para los paceientes que respondieron de la alergia a los AINE
round(prop.table(table(data_final$medic_aine[data_final$medic_aine != "No aplica"]))*100, 2)
  # 34/64 pacientes reportaron alergia a los AINE, lo cual representa el 53.12 % de
    # todo los asmáticos que reportaron alergia a algún medicamento.


# - Alergia a los betalactámicos ----
table(data_final$medic)

data_final$medic_betalac <- apply(
  data_final[, c("medic")],
  1,
  function(x) {
    if (any(x %in% c("aines penicilina", "amoxicilina", "cefalosporinas",
                     "doxicilina cefalexina", "naproxeno diclofenaco penicilina",
                     "penicilina", "penicilina aines"), na.rm = TRUE)) {return("Sí")}
    if (any(x == "999", na.rm = TRUE)) {return("No aplica")}
    if (any(is.na(x))) {return(NA)}
    return("No") }
  )
table(data_final$medic_betalac) # No se retira los "No aplica" porque se quiere
  # calcular la proporción en todos los pacientes asmáticos
sum(table(data_final$medic_betalac)) # Son 237 asmáticos disponibles
round(prop.table(table(data_final$medic_betalac))*100, 2)
  # 21/237 pacientes reportaron alergia a los betalactámicos, lo cual representa el
    # 8.86 % de todos los asmáticos

table(data_final$medic_betalac) # Ahora sí se retira los "No aplica" porque interesa
  # solo los que reportaron alergia a algún medicamento.
table(data_final$medic_betalac[data_final$medic_betalac != "No aplica"]) # Hay 21
  # alérgicos a los betalactámicos
sum(table(data_final$medic_betalac[data_final$medic_betalac != "No aplica"])) # Son 64
  # datos disponibles para los pacientes que respondieron de la alergia a los betalac
round(prop.table(table(data_final$medic_betalac[data_final$medic_betalac != "No aplica"]))*100, 2)
  # 21/64 pacientes reportaron alergia a los betalactámicos, lo cual representa el
    # 32.81 % de todos los asmáticos que reportaron alergia a algún medicamento.



# - Alergia a los NO betalactámicos ----
table(data_final$medic)

data_final$medic_nobetalac <- apply(
  data_final[, c("medic")],
  1,
  function(x) {
    if (any(x %in% c("azitromicina", "ciprofloxacino", "eritromicin", "sulfa",
                     "doxicilina cefalexina"), na.rm = TRUE)) {return("Sí")}
    if (any(x == "999", na.rm = TRUE)) {return("No aplica")}
    if (any(is.na(x))) {return(NA)}
    return("No") }
  )
table(data_final$medic_nobetalac) # No se retira los "No aplica" porque se quiere
  # calcular la proporción en todos los pacientes asmáticos. Hay 6
sum(table(data_final$medic_nobetalac)) # Son 237 asmáticos disponibles
round(prop.table(table(data_final$medic_nobetalac))*100, 2)
  # 6/237 pacientes reportaron alergia a los no betalactámicos, lo cual representa el
    # 2.53 % de todos los asmáticos.

table(data_final$medic_nobetalac) # Ahora sí se retira los "No aplica" porque interesa
  # solo los que reportaron alergia a algún medicamento.
table(data_final$medic_nobetalac[data_final$medic_nobetalac != "No aplica"]) # Hay 6
  # alérgicos a los no betalactámicos.
sum(table(data_final$medic_nobetalac[data_final$medic_nobetalac != "No aplica"])) # Son
  # 64 datos disponibles para los pacientes que respondieron de la alergia a los no 
    # betalactámicos.
round(prop.table(table(data_final$medic_nobetalac[data_final$medic_nobetalac != "No aplica"]))*100, 2)
  # 6/64 pacientes reportaron alergia a los no betalactámicos, lo cual representa el
    # 9.38 % de todos los asmáticos que reportaron alergia a algún medicamento.


# - Alergia al metamizol ----
table(data_final$medic)

data_final$medic_metam <- apply(
  data_final[, c("medic")],
  1,
  function(x) {
    if (any(x %in% c("antalgina", "metamizol"), na.rm = TRUE)) {return("Sí")}
    if (any(x == "999", na.rm = TRUE)) {return("No aplica")}
    if (any(is.na(x))) {return(NA)}
    return("No") }
  )
table(data_final$medic_metam) # No se retira los "No aplica" porque se quiere
  # calcular la proporción en todos los pacientes asmáticos. Hay 3
sum(table(data_final$medic_metam)) # Son 237 asmáticos disponibles
round(prop.table(table(data_final$medic_metam))*100, 2)
  # 3/237 pacientes reportaron alergia al metamizol, lo cual representa el 1.27 %
    # de todos los asmáticos.

table(data_final$medic_metam) # Ahora sí se retira los "No aplica" porque interesa
  # solo los que reportaron alergia a algún medicamento.
table(data_final$medic_metam[data_final$medic_metam != "No aplica"]) # Hay 3
  # alérgicos al metamizol.
sum(table(data_final$medic_metam[data_final$medic_metam != "No aplica"])) # Son
  # 64 datos disponibles para los pacientes que respondieron de la alergia al 
    # metamizol
round(prop.table(table(data_final$medic_metam[data_final$medic_metam != "No aplica"]))*100, 2)
  # 3/64 pacientes reportaron alergia al metamizol, lo cual representa el 4.69 %
    # de todos los asmáticos que reportaron alergia a algún medicamento.


# - Alergia a otros medicamentos ----
table(data_final$medic)

data_final$medic_otros <- apply(
  data_final[, c("medic")],
  1,
  function(x) {
    if (any(x %in% c("anti tbc", "codeina", "contraste iodados",
                     "hidroxicloroquina"), na.rm = TRUE)) {return("Sí")}
    if (any(x == "999", na.rm = TRUE)) {return("No aplica")}
    if (any(is.na(x))) {return(NA)}
    return("No") }
  )
table(data_final$medic_otros) # No se retira los "No aplica" porque se quiere
  # calcular la proporción en todos los pacientes asmáticos. Hay 4
sum(table(data_final$medic_otros)) # Son 237 asmáticos disponibles
round(prop.table(table(data_final$medic_otros))*100, 2)
  # 4/237 pacientes reportaron alergia a otros medicamentos, lo cual representa el
    # 1.69 % de todos los asmáticos.

table(data_final$medic_otros) # Ahora sí se retira los "No aplica" porque interesa
  # solo los que reportaron alergia a algún medicamento.
table(data_final$medic_otros[data_final$medic_otros != "No aplica"]) # Hay 4
  # alérgicos a otros medicamentos.
sum(table(data_final$medic_otros[data_final$medic_otros != "No aplica"])) # Son
  # 64 datos disponibles para los pacientes que respondieron de la alergia a 
    # otros medicamentos
round(prop.table(table(data_final$medic_otros[data_final$medic_otros != "No aplica"]))*100, 2)
  # 4/64 pacientes reportaron alergia a otros medicamentos, lo cual representa el
    # 6.25 % de todos los asmáticos que reportaron alergia a algún medicamento.



## Párrafo de tabla 4 #####

table(data_final$medic_aine) # 34
sum(table(data_final$medic_aine)) # 237
round(prop.table(table(data_final$medic_aine))*100, 2) # 14.35 %

table(data_final$medic_aine[data_final$medic_aine != "No aplica"]) # 34
sum(table(data_final$medic_aine[data_final$medic_aine != "No aplica"])) # 64
round(prop.table(table(data_final$medic_aine[data_final$medic_aine != "No aplica"]))*100, 2)
  # 53.12 %

table(data_final$medic[data_final$medic_otros == "Sí"])

table(data_final$asma_severa_etiq) # Hay 16 asmáticos severos
data_final_asma_seve <- subset(data_final, asma_severa_etiq == "Sí")
table(data_final_asma_seve$medic_aine) # Hay que quitar los "No aplica"
table(data_final_asma_seve$medic_aine[data_final_asma_seve$medic_aine != "No aplica"])
  # 3
sum(table(data_final_asma_seve$medic_aine[data_final_asma_seve$medic_aine != "No aplica"]))
  # 4
round(prop.table(table(data_final_asma_seve$medic_aine[data_final_asma_seve$medic_aine != "No aplica"]))*100, 2)
  # 3/4 pacientes reportaron alergia a los AINE, lo cual representa el
    # 75.0 % de todos los asmáticos severos disponibles
sum(is.na(data_final_asma_seve$medic_aine)) # No hubo datos perdidos

table(data_final$asma_severa_etiq) # Hay 227 SIN asmá severa
data_final_asma_no_seve <- subset(data_final, asma_severa_etiq == "No")
table(data_final_asma_no_seve$medic_aine) # Hay que quitar los "No aplica"
table(data_final_asma_no_seve$medic_aine[data_final_asma_no_seve$medic_aine != "No aplica"])
  # 31
sum(table(data_final_asma_no_seve$medic_aine[data_final_asma_no_seve$medic_aine != "No aplica"]))
  # 60
round(prop.table(table(data_final_asma_no_seve$medic_aine[data_final_asma_no_seve$medic_aine != "No aplica"]))*100, 2)
  # 31/60 pacientes reportaron alergia a los AINE, lo cual representa el
    # 51.67 % de todos los NO asmáticos severos disponibles
sum(is.na(data_final_asma_no_seve$medic_aine)) # Hubo 6 datos perdidos

data_final$medic_aine2 <- ifelse(is.na(data_final$medic_aine), "perdido", data_final$medic_aine)
  # Ahora se tiene el interés de saber cuántos en total debieron responder a la
    # pregunta de alergia a los AINE, para ello se debe considera dentro del conteo a
    # lo datos perdidos.
data_final_asma_no_seve <- subset(data_final, asma_severa_etiq == "No")
table(data_final_asma_no_seve$medic_aine2) # Se deben retirar del conteo a los 
  # "No aplica"
table(data_final_asma_no_seve$medic_aine2[data_final_asma_no_seve$medic_aine2 != "No aplica"])
  # Son 6 datos perdidos
sum(table(data_final_asma_no_seve$medic_aine2[data_final_asma_no_seve$medic_aine2 != "No aplica"]))
  # Son 66 los que debieron responder a la pregunta de alergia a los AINE.
round(prop.table(table(data_final_asma_no_seve$medic_aine2[data_final_asma_no_seve$medic_aine2 != "No aplica"]))*100, 2)
  # Los 6 datos perdidos representan el 9.09 % de todos los que debía de haber
    # respondido la pregunta de alergia a los AINE.


## Párrafo final #####

# Variable "Alimentos a los que es alérgico" ----

# - Alergia a los mariscos ----
table(data_final$alimen)

data_final$alimen_maris <- apply(
  data_final[, c("alimen")],
  1,
  function(x) {
    if (any(x %in% c("langostinos", "mariscos", "pescado mariscos"), na.rm = TRUE)) {return("Sí")}
    if (any(x == "999", na.rm = TRUE)) {return("No aplica")}
    if (any(is.na(x))) {return(NA)}
    return("No") }
)
table(data_final$alimen_maris) # No se retira los "No aplica" porque se quiere calcular
  # la proporción en todos los pacientes asmáticos. Hay 5.
sum(table(data_final$alimen_maris)) # Son 238 asmáticos disponibles
round(prop.table(table(data_final$alimen_maris))*100, 2)
  # 5/238 pacientes reportaron alergia a los mariscos, lo cual representa el 2.10 %
    # de todos los asmáticos

table(data_final$alimen_maris) # Ahora sí se retira los "No aplica" porque interesa
  #solo los que reportaron alergia a algún alimento.
table(data_final$alimen_maris[data_final$alimen_maris != "No aplica"]) # Hay 5 alérgicos
  # a los mariscos
sum(table(data_final$alimen_maris[data_final$alimen_maris != "No aplica"])) # Son 14 datos
  # disponibles para los paceientes que respondieron de la alergia a los mariscos.
round(prop.table(table(data_final$alimen_maris[data_final$alimen_maris != "No aplica"]))*100, 2)
  # 5/14 pacientes reportaron alergia a los mariscos, lo cual representa el 35.71 % de
    # todo los asmáticos que reportaron alergia a algún alimento.

table(data_final$asma_severa_etiq) # 16 pacientes con asma severa
data_final_asma_seve <- subset(data_final, asma_severa_etiq == "Sí")
table(data_final_asma_seve$alimen_maris) # No hubo reporte de alergia a lo mariscos
  # en los pacientes con asma severa
data_final$alimen_maris2 <- ifelse(is.na(data_final$alimen_maris), "perdido", data_final$alimen_maris)
  # Ahora se tiene el interés de saber cuántos en total debieron responder a la
    # pregunta de alergia a los AINE, para ello se debe considera dentro del conteo a
    # lo datos perdidos.
data_final_asma_seve <- subset(data_final, asma_severa_etiq == "Sí")
table(data_final_asma_seve$alimen_maris2) # No hubo reporte de alergia a lo mariscos
  # en los pacientes con asma severa


table(data_final$asma_severa_etiq) # 227 pacientes SIN asma severa
data_final_asma_no_seve <- subset(data_final, asma_severa_etiq == "No")
table(data_final_asma_no_seve$alimen_maris)
table(data_final_asma_no_seve$alimen_maris[data_final_asma_no_seve$alimen_maris != "No aplica"])
  # 5
sum(table(data_final_asma_no_seve$alimen_maris[data_final_asma_no_seve$alimen_maris != "No aplica"]))
  # 14
round(prop.table(table(data_final_asma_no_seve$alimen_maris[data_final_asma_no_seve$alimen_maris != "No aplica"]))*100, 2)
  # 5/14 pacientes reportaron alergia a los mariscos, lo cual representa el 35.71 % de
    # todo los asmáticos que reportaron alergia a algún alimento.
data_final$alimen_maris2 <- ifelse(is.na(data_final$alimen_maris), "perdido", data_final$alimen_maris)
  # Ahora se tiene el interés de saber cuántos en total debieron responder a la
  # pregunta de alergia a los AINE, para ello se debe considera dentro del conteo a
  # lo datos perdidos.
data_final_asma_no_seve <- subset(data_final, asma_severa_etiq == "No")
table(data_final_asma_no_seve$alimen_maris2) # Se deben retirar del conteo a los 
  # "No aplica"
table(data_final_asma_no_seve$alimen_maris2[data_final_asma_no_seve$alimen_maris2 != "No aplica"])
  # Son 5 datos perdidos
sum(table(data_final_asma_no_seve$alimen_maris2[data_final_asma_no_seve$alimen_maris2 != "No aplica"]))
  # Son 19 los que debieron responder a la pregunta de alergia a los mariscos.
round(prop.table(table(data_final_asma_no_seve$alimen_maris2[data_final_asma_no_seve$alimen_maris2 != "No aplica"]))*100, 2)
  # Los 5 datos perdidos representan el 26.32 % de todos los que debía de haber
    # respondido la pregunta de alergia a los mariscos.



