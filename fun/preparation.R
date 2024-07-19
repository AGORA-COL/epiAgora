library(arrow)
library(dplyr)
library(tidyr)
library(tidyverse)
library(epitrix)
library(readxl)
library(stringr)
library(comorbidity)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)



#=====================
#LIMPIEZA DE DATOS
#=====================

clean_data_fun <- function(database_path) {
  # Cargar base de RIPS
  subsample_df_bogota <- readRDS(database_path) %>%
    mutate(ID_uniq = row_number())

  # Crear variables de fecha (mes, año)
  data_clean <- subsample_df_bogota %>%
    mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"),
           anio = format(fechaid, "%Y"),
           mes_anio = floor_date(fechaid, "month"))

  # Verificar la variable "edad" (PRIMERO SE DEBE VERIFICAR SI TODAS ESTAN EN LAS MISMAS UNIDADES)
  # data_clean <- data_clean %>%
  #   mutate(edad = as.numeric(edad)) %>%
  #   filter(edad >= 18)

  # Unificar categorías de la variable "sexo"
  data_clean <- data_clean %>%
    mutate(sexodesc = case_when(
      sexodesc %in% c("F", "FEMENINO") ~ "Mujeres",
      sexodesc %in% c("M", "MASCULINO") ~ "Hombres",
      sexodesc %in% c("N", "NO DEFINIDO") ~ "Sin dato"
    ))

  # Unificar categorías de la variable "tipo de evento"
  data_clean <- data_clean %>%
    mutate(tipoeventoripsdesc = case_when(
      tipoeventoripsdesc %in% c("C", "CONSULTAS") ~ "CONSULTAS",
      tipoeventoripsdesc %in% c("H", "HOSPITALIZACIONES") ~ "HOSPITALIZACIONES",
      tipoeventoripsdesc %in% c("P", "PROCEDIMIENTOS DE SALUD") ~ "PROCEDIMIENTOS DE SALUD",
      tipoeventoripsdesc %in% c("U", "URGENCIAS") ~ "URGENCIAS"
    ))

  return(data_clean)
}

# Uso de la función para la limpieza de datos
data_cleanbta <- clean_data_fun("dat/subsample_parquet")



#=====================================
#Unión con la clasificación de grupos
#=====================================

Clasificacion_fun <- function(database, cie10_lista) {

  # Cargar listado de códigos CIE10 clasificados
    cie10 <- read_excel(cie10_lista) %>%
             rename_with(epitrix::clean_labels)

  # Si "tipoeventoripsdesc" es igual a "HOSPITALIZACIONES", entonces une por "dxegreso", de lo contrario une por "dxprincipal"
    database <- database %>%
                mutate(cie10_key = case_when(
                tipoeventoripsdesc == "HOSPITALIZACIONES" ~ dxegreso,
                TRUE ~ dxprincipal))

    database_clas <- left_join(database, cie10, by = c("cie10_key" = "codigo"))

return(database_clas)
}


# Uso de la función para la clasificacion de codigos CIE10
data_btaclas <- Clasificacion_fun(data_cleanbta, "dat/Lista grupos morbilidad.xlsx")


#write_rds(data_btaclas, "dat/data_btaclas.rds")
#data_btaclas <- readRDS("dat/data_btaclas.rds")

tabla_clasagora <- data_btaclas %>%
                #filter(!is.na(grupos_agora)) %>%  # Se eliminan aquellos con Dx=1,0
                count(grupos_agora, name = "frecuencia")

tabla_clascharlson <- data_btaclas %>%
                      #filter(!is.na(charlson_clasificacion)) %>%  # Se eliminan aquellos con Dx=1,0
                      count(charlson_clasificacion, name = "frecuencia")




#=====================
# TABLAS
#=====================
#####
#Tabla de grupos de enfermedades por fechas
tabla_grupos_por_dias <- data_cupsbta %>%
  group_by(fechaid, grupos_AGORA) %>%
  summarise(frecuencia = n()) %>%
  pivot_wider(names_from = grupos_AGORA, values_from = frecuencia, values_fill = list(frecuencia = 0))


#####
#write_csv(tabla_grupos_por_dias, "dat/tabla_grupos_por_dias.csv")

tabla_frecuencias_diaria <- data_cupsbta %>%
  group_by(fechaid, grupos_AGORA) %>%
  summarise(frecuencia = n(), .groups = 'drop')

tabla_frecuencias_mensual <- data_cupsbta %>%
  group_by(mes_anio, grupos_AGORA) %>%
  summarise(frecuencia = n(), .groups = 'drop')


# PROPORCIONES (Se espera aclarar para luego ser prevalencias)
#####


#---------------------------------
#EXPLORACION INICIAL DE LOS DATOS
#---------------------------------

#Visualización de las series de tiempo para cada grupo de enfermedades.

#####
#Análisis Temporal:
#Identificación de patrones estacionales y tendencias.

#####
