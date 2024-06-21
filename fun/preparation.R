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



#-----------------
#LIMPIEZA DE DATOS
#-----------------

clean_data_fun <- function(subsample_path, cie10_path, cie10_sheet) {

  # Cargar base de registros CUPS
  subsample_df_bogota <- readRDS(subsample_path) %>%
    mutate(ID_uniq = row_number())

  # Cargar listado de códigos CIE10
  cie10 <- read_excel(cie10_path, sheet = cie10_sheet) %>%
    rename_with(clean_labels)

  # Identificar la descripción y grupo según codigo de CIE10 de registros CUPS
  data_clean <- left_join(subsample_df_bogota, cie10, by = c("dxprincipal" = "codigo"))
  data_clean <- rename(data_clean, grupos_AGORA = grandes_grupos_morbilidad_proyecto)

  # Crear variables de fecha (mes, año)
  data_clean <- data_clean %>%
    mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"),
           anio = format(fechaid, "%Y"),
           mes_anio = floor_date(fechaid, "month"))

  # Verificar la variable "edad" (PRIMERO SE DEBE VERIFICAR SI TODAS ESTAN EN LAS MISMAS UNIDADES)
  #   data_clean <- data_clean %>%
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

data_cupsbta <- clean_data_fun("dat/subsample_parquet", "dat/cupscodes.xlsx", "cie10-cons")

#write_rds(data_cupsbta, "dat/data_cupsbta.rds")
#data_cupsbta <- readRDS("dat/data_cupsbta.rds")



######################
# TABLAS
######################

data_cupsbta <- data_cupsbta %>% filter(!is.na(grupos_AGORA)) # Se eliminan aquellos con Dx=1,0

#Tabla de grupos de enfermedades por fechas
tabla_grupos_por_dias <- data_cupsbta %>%
  group_by(fechaid, grupos_AGORA) %>%
  summarise(frecuencia = n()) %>%
  pivot_wider(names_from = grupos_AGORA, values_from = frecuencia, values_fill = list(frecuencia = 0))

#write_csv(tabla_grupos_por_dias, "dat/tabla_grupos_por_dias.csv")

tabla_frecuencias_diaria <- data_cupsbta %>%
  group_by(fechaid, grupos_AGORA) %>%
  summarise(frecuencia = n(), .groups = 'drop')

tabla_frecuencias_mensual <- data_cupsbta %>%
  group_by(mes_anio, grupos_AGORA) %>%
  summarise(frecuencia = n(), .groups = 'drop')


###############################################################
# PROPORCIONES (Se espera aclarar para luego ser prevalencias)
###############################################################

# por dias
total_personas_dias <- data_cupsbta %>%
  group_by(fechaid) %>%
  summarise(total_personas = n_distinct(ID_uniq), .groups = 'drop')

tabla_frecuencias_diaria <- tabla_frecuencias_diaria %>%
  left_join(total_personas_dias, by = "fechaid") %>%
  mutate(proporcion = frecuencia / total_personas)


# por mes
total_personas_mes <- data_cupsbta %>%
  group_by(mes_anio) %>%
  summarise(total_personas = n_distinct(ID_uniq), .groups = 'drop')

tabla_frecuencias_mensual <- tabla_frecuencias_mensual %>%
  left_join(total_personas_mes, by = "mes_anio") %>%
  mutate(proporcion = frecuencia / total_personas)




#---------------------------------
#EXPLORACION INICIAL DE LOS DATOS
#---------------------------------

#Visualización de las series de tiempo para cada grupo de enfermedades.


#Análisis Temporal:
#Identificación de patrones estacionales y tendencias.


