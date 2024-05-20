library(arrow)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(stringr)
library(comorbidity)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)


#--------
#DATOS:
#--------
subsample_df_bogota <- readRDS("dat/subsample_parquet") %>% mutate(ID_uniq = row_number())



#-----------------
#LIMPIEZA DE DATOS
#-----------------

#Año de consulta
subsample_df_bogota <- subsample_df_bogota %>%
                       mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"),
                              anio = format(fechaid, "%Y"))

#Edad
subsample_df_bogota <- mutate(subsample_df_bogota, edad_num = as.numeric(edad))

subsample_df_bogota<- subsample_df_bogota %>% filter(edad_num >= 18)

#Sexo
subsample_df_bogota$sexo <- case_when(
  subsample_df_bogota$sexodesc %in% c("F", "FEMENINO") ~ "Mujeres",
  subsample_df_bogota$sexodesc %in% c("M", "MASCULINO") ~ "Hombres",
  subsample_df_bogota$sexodesc %in% c("N", "NO DEFINIDO") ~ "Sin dato"
)

#Tipo de procedimiento
subsample_df_bogota$tipoeventoripsdesc <-case_when(
  subsample_df_bogota$tipoeventoripsdesc %in% c("C", "CONSULTAS") ~ "CONSULTAS",
  subsample_df_bogota$tipoeventoripsdesc %in% c("H", "HOSPITALIZACIONES") ~ "HOSPITALIZACIONES",
  subsample_df_bogota$tipoeventoripsdesc %in% c("P", "PROCEDIMIENTOS DE SALUD") ~ "PROCEDIMIENTOS DE SALUD",
  subsample_df_bogota$tipoeventoripsdesc %in% c("U", "URGENCIAS") ~ "URGENCIAS"
)



#--------------------------
#GRUPOS DE EVENTOS DE SALUD
#--------------------------


#arhivo lista de códigos CIE10 (12,648 códigos)
cie10 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie10-cons")

#Grupos de AGORA
names(cie10)<- epitrix::clean_labels(names(cie10))

# Realizar la fusión de los datos por la columna "diagnosticocd"
merged_data_agr <- merge(subsample_bogotaf, cie10, by.x = "diagnosticocd", by.y = "codigo", all.x = TRUE)

# Crear una tabla donde las filas son los personaid únicos y las columnas son las categorías de grandes_grupos_morbilidad_proyecto
table_agora <- merged_data_agr %>%
  select(personaid, grandes_grupos_morbilidad_proyecto) %>%
  distinct() %>%
  pivot_wider(names_from = grandes_grupos_morbilidad_proyecto, values_from = grandes_grupos_morbilidad_proyecto, values_fn = length, values_fill = 0)

table1(~.,table_agora)

table_agora1<-table_agora[,-c(1,2)]
table_agora1[] <- lapply(table_agora1, as.factor)
table1(~.,table_agora1)




#---------------------------------
#EXPLORACION INICIAL DE LOS DATOS
#---------------------------------

#Visualización de las series de tiempo para cada grupo de enfermedades.


#Análisis Temporal:
#Identificación de patrones estacionales y tendencias.


