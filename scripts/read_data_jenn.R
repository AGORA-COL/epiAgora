###################################
#Proyecto AGORA
#Fecha: 07-marzo-2023
#Versión: Jennifer Murillo
###################################


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


#=============
#DATOS
#=============
#Muestra Bogotá: 35'351.724 (2008 a 2022)
#sample_df_bogota_parquet <- read_parquet("dat/sample_df_bogota_parquet") %>% mutate(ID_uniq = row_number())

#Subuestra Bogotá: 1,767,586 (2018 a 2022)
subsample_df_bogota <- readRDS("dat/subsample_parquet") %>% mutate(ID_uniq = row_number())



#=============
#Data cleaning
#=============

#Año de consulta
subsample_df_bogota <- subsample_df_bogota %>%
                       mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"),
                       anio = format(fechaid, "%Y"))

#Edad
subsample_df_bogota <- mutate(subsample_df_bogota, edad_num = as.numeric(edad))

#Sexo
subsample_df_bogota$sexo <- ifelse(subsample_df_bogota$sexodesc %in% c("F", "FEMENINO"), "Mujeres",
                                   ifelse(subsample_df_bogota$sexodesc %in% c("M", "MASCULINO"), "Hombres",
                                          ifelse(subsample_df_bogota$sexodesc %in% c("N", "NO DEFINIDO"), "Sin dato",
                                                 as.character(subsample_df_bogota$sexodesc))))

subsample_df_bta_pir <-  subsample_df_bogota %>% filter(sexo %in% c("Mujeres","Hombres"))




#-----------------------
#Pirámide poblacional (pendiente a que entreguen variable de unidades de medida de la edad)
#-----------------------
#####
#Grupos de edad de 5 años hasta los 80 años
breaks <- c(seq(0, 80, by = 5), Inf)  #"Inf" para el último grupo "más de 80"

etiquetas <- paste0(breaks[-length(breaks)], " a ", breaks[-1])
subsample_df_bta_pir$edad_quinquenal <- cut(subsample_df_bta_pir$edad_num, breaks = breaks,
                                            labels = etiquetas, include.lowest = TRUE)

#Total por sexo
total_hombres <- sum(subsample_df_bta_pir$sexo == "Hombres")
total_mujeres <- sum(subsample_df_bta_pir$sexo == "Mujeres")


tabla_edad <- subsample_df_bta_pir %>%
  group_by(edad_quinquenal, sexo) %>%
  summarise(total_personas = n()) %>%
  group_by(edad_quinquenal) %>%
  mutate(porcentaje = ifelse(sexo == "Hombres", total_personas / total_hombres * 100,
                             total_personas / total_mujeres * 100))


#Gráfico de barras para la pirámide poblacional
piramide <- ggplot(data = as.data.frame(tabla_edad), aes(x = edad_quinquenal, y = ifelse(sexo == "Hombres", porcentaje, -porcentaje), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
  labs(x = "Grupo de Edad", y = "% Población", title = "Pirámide Poblacional") +
  scale_fill_manual(values = c("Mujeres" = "darkred", "Hombres" = "midnightblue")) +
  theme_minimal() +
  coord_flip()

piramide


#Para año 2018
subsample_df_bta_pir_2018 <- subset(subsample_df_bta_pir, anio == "2018")
#Total por sexo
total_hombres_2018 <- sum(subsample_df_bta_pir_2018$sexo == "Hombres")
total_mujeres_2018 <- sum(subsample_df_bta_pir_2018$sexo == "Mujeres")

tabla_edad_2018 <- subsample_df_bta_pir_2018 %>%
  group_by(edad_quinquenal, sexo) %>%
  summarise(total_personas = n()) %>%
  group_by(edad_quinquenal) %>%
  mutate(porcentaje = ifelse(sexo == "Hombres", total_personas / total_hombres_2018 * 100,
                             total_personas / total_mujeres_2018 * 100))




piramide_2018 <- ggplot(data = as.data.frame(tabla_edad_2018), aes(x = edad_quinquenal, y = ifelse(sexo == "Hombres", porcentaje, -porcentaje), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
  labs(x = "Grupo de Edad", y = "% Población", title = "Pirámide Poblacional - 2018") +
  scale_fill_manual(values = c("Mujeres" = "darkred", "Hombres" = "midnightblue")) +
  theme_minimal() +
  coord_flip()

piramide_2018

grid.arrange(piramide, piramide_2018, ncol = 2)

#Se compara con el artículo: "El uso de pirámides poblacionales como representación gráfica del sistema de salud colombiano";  https://www.redalyc.org/journal/2738/273856494013/html/

#####

subsample_df_bogota<- subsample_df_bogota %>% filter(edad_num >= 18)




##############################################################
# CÁLCULO DE PREVALENCIA DE DIABETES MELLITUS (DM) POR 3 VÍAS
##############################################################


#Función de tablas de resultados

#por casos
tabla_resultados <- function(data, variables) {

  casos <- numeric(length(variables))
  n <- numeric(length(variables))
  prevalencias <- numeric(length(variables))

  for (i in seq_along(variables)) {
    casos[i] <- sum(data[[variables[i]]] == 1, na.rm = TRUE)
    n[i] <- sum(!is.na(data[[variables[i]]]))
    prevalencias[i] <- (casos[i] / n[i] )*100  }

  return(data.frame(Patología = variables, Casos = casos, n = n, Prevalencia=prevalencias))
}


#por año
tabla_anioresultados <- function(data, variables) {
  resultados <- data.frame(Variable = variables)

  #Prevalencia por año
  for (anio in sort(unique(data$anio))) {
    prevalencia_anio <- numeric(length(variables))

    for (i in seq_along(variables)) {
      prevalencia_anio[i] <- (sum(data[[variables[i]]] == 1 & data$anio == anio, na.rm = TRUE) / sum(data$anio == anio, na.rm = TRUE)) * 100
    }
    resultados$prevalencia_anio <- prevalencia_anio
    colnames(resultados)[ncol(resultados)] <- paste(anio)
  }
  #Prevalencia total
  prevalencia <- numeric(length(variables))
  for (i in seq_along(variables)) {
    prevalencia[i] <- (sum(data[[variables[i]]] == 1, na.rm = TRUE) / sum(!is.na(data[[variables[i]]]))) * 100
  }

  resultados <- cbind(resultados, prevalencia)
  return(resultados)
}



#-----------------------------------
# VIA 1: USANDO PAQUETE COMORBIDITY
#-----------------------------------
#####
#Se crea dataset con pacientes identificados según sus comorbilidades:
comorb_pack<-comorbidity(x = subsample_df_bogota, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
#View(comorb_pack) #1'142.343 sin duplicados

#Tabla de resultados mediante la función
comorb_pack$diabTotal <- ifelse(comorb_pack$diab == 1 | comorb_pack$diabwc == 1, 1,0 )
variables_comorbidity <-c("diab", "diabwc", "diabTotal", "rend")
resultados_comorbidity <- tabla_resultados(data = comorb_pack, variables = variables_comorbidity)


#como la función de comorbidity no tiene la opción de sacar grupos por año, se crea una función para procesar los datos de un año específico
procesar_anio <- function(anio) {
  subsample_df_bogota_anio <- subsample_df_bogota %>% filter(anio == anio)
  comorb_pack_anio <- comorbidity(
    x = subsample_df_bogota_anio,
    id = "personaid",
    code = "dxprincipal",
    map = "charlson_icd10_quan",
    assign0 = TRUE
  )
  comorb_pack_anio[[paste0("diabTotal_", anio)]] <- ifelse(comorb_pack_anio$diab == 1 | comorb_pack_anio$diabwc == 1, 1, 0)
  variables_comorbidity <- c("diab", "diabwc", paste0("diabTotal_", anio), "rend")
  resultados_comorbidity <- tabla_resultados(data = comorb_pack_anio, variables = variables_comorbidity)
  return(resultados_comorbidity)
}

# Procesar los datos para los años 2018 a 2022
resultados_comorbidity_2018 <- procesar_anio("2018")
resultados_comorbidity_2019 <- procesar_anio("2019")
resultados_comorbidity_2020 <- procesar_anio("2020")
resultados_comorbidity_2021 <- procesar_anio("2021")
resultados_comorbidity_2022 <- procesar_anio("2022")

#####


#----------------------------------------
# VIA 2: USANDO LISTA DE CÓDIGOS CIE-10
#----------------------------------------
#####
#Lista CIE-10 descargada de la página de SISPRO (https://web.sispro.gov.co/WebPublico/Consultas/ConsultarDetalleReferenciaBasica.aspx?Code=CIE10)
cie10_dm <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie-dm")

subsample_bta <- subsample_df_bogota %>%
                 select(ID_uniq, personaid, dxprincipal, codigoprocedimiento, fechaid, anio) %>%
                 mutate(dm = ifelse(dxprincipal %in% cie10_dm$codigo, 1, 0),
                        dm_t1 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t1"], 1, 0),
                        dm_t2 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t2"], 1, 0))


#Tabla de resultados
variables_cie10_dm <-c("dm_t1", "dm_t2", "dm")
resultados_cie10 <- tabla_resultados(data = subsample_bta, variables = variables_cie10_dm)
resultados_cie10_anios <- tabla_anioresultados(data = subsample_bta, variables = variables_cie10_dm)
#####


#--------------------------------------------------
# VIA 3: USANDO CÓDIGOS CUPS Y FUNCIÓN DE DANIELA
#--------------------------------------------------
#####
#FUNCION

#clasificación
classification <- function(cods, bd) {
  # Limpia los nombres de los códigos
  names(cods) <- epitrix::clean_labels(names(cods))
  cods <- cods %>% mutate(name_cods = epitrix::clean_labels(codigo_significado))

  cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
  cods_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo)
  names_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo_significado)
  names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)

  # Agrega columnas para indicar si hay coincidencias con los códigos CIE-10 y CUPS
  bd$enf_cie10 <- ifelse(bd$dxprincipal %in% cods_cie10$codigo, 1, 0)
  bd$enf_cups <- ifelse(bd$codigoprocedimiento %in% cods_cups$codigo, 1, 0)

  # Crea una columna para indicar si hay algún caso detectado
  bd$casos <- ifelse(bd$enf_cie10 == 1 | bd$enf_cups == 1, 1, 0)

  return(bd)
}

#PARA DM (DIABETES MELLITUS)
cods_DM <- read_excel("fun_daniela/data/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM") #Hoja que contine los códigos cups y cie10 clasificados
subsample_bogota_dm <- classification(cods_DM, subsample_df_bogota) #Clasificación de los registros de la base de datos de Bogotá con la función
evidencia <- read_excel("dat/validacion.xlsx")  #Evidencia
#Tabla de resultados
variables_cie10_cups <-c("enf_cie10", "enf_cups", "casos")
resultados_cie10_cups <- tabla_resultados(data = subsample_bogota_dm, variables = variables_cie10_cups)
resultados_cie10_cups_anios <- tabla_anioresultados(data = subsample_bogota_dm, variables = variables_cie10_cups)


#PARA ERC (ENFERMEDAD RENAL CRONICA)
cods_ERC <- read_excel("fun_daniela/data/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "ERC") #Hoja que contine los códigos cups y cie10 clasificados
subsample_bogota_erc <- classification(cods_ERC, subsample_bogotaf) #Clasificación de los registros de la base de datos de Bogotá con la función
evidencia <- read_excel("dat/validacion.xlsx")  #Evidencia
#Tabla de resultados
variables_cie10_cups <-c("enf_cie10", "enf_cups", "casos")
resultados_cie10_cups <- tabla_resultados(data = subsample_bogota_erc, variables = variables_cie10_cups)
resultados_cie10_cups_anios <- tabla_anioresultados(data = subsample_bogota_erc, variables = variables_cie10_cups)

#####



#----------------------------------------------------------------------------------------------------

##########################################
# Tablas drive codigos cie10 consolidados
###########################################


#TABLA DE DX (CIE10) REGISTRADOS EN LA BASE DE DATOS DE TODO BOGOTÁ, POR AÑO
df_pivot_dxprincipal <- read_excel("dat/df_pivot_dxprincipalfinal.xlsx")

cie10 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie10-cons") #arhivo lista de códigos CIE10 (12,648 códigos)
df_pivot_con_descripcion <- df_pivot_dxprincipal %>% left_join(cie10, by = c("dxprincipal" = "codigo")) #identificación de las descripciones de cada código cie10
cie10_noidentificados <- df_pivot_con_descripcion %>% filter(is.na(descripcion)) #4 no identificados

#TABLA DE CUPS REGISTRADOS EN LA BASE DE DATOS DE TODO BOGOTÁ, POR AÑO (12,062)
df_pivot_cups <- read_excel("dat/df_pivot_procedimientos.xlsx")

#Archivos de códigos CUPS:
#Fuente: https://www.minsalud.gov.co/salud/POS/paginas/plan-obligatorio-de-salud-pos.aspx

# Función para cargar y procesar los datos de un año específico
cargar_cups <- function(sheet_name) {
  read_excel("dat/cupscodes.xlsx", sheet = sheet_name, col_types = c("text", "text")) %>%
    mutate(descripcion = clean_labels(descripcion), aniocups = sheet_name)
}

# Lista de hojas a procesar
anios <- c("cups2009", "cups2015", "cups2016", "cups2017", "cups2018", "cups2019", "cups2020", "cups2021", "cups2022", "cups2023", "cups2024")
cups_todos <- bind_rows(lapply(anios, cargar_cups)) #unir todos los años disponibles

#identificación de las descripciones de cada cups
df_pivot_cups_descripcion <- df_pivot_cups %>% left_join(cups_todos, by = c("codigoprocedimiento" = "codigo"))
cups_noidentificado <- df_pivot_cups_descripcion %>% filter(is.na(descripcion)) #23  (%) no identificados

#-------------------------------------------------------------------------------

#Dx de ingreso y egreso
dx_ppal<-subsample_df_bogota %>%
         select(dxprincipal) %>% filter(!is.na(dxprincipal)) %>% distinct()
dx_egreso <- subsample_df_bogota %>%
             mutate(comprobacion = ifelse(dxegreso %in% dx_ppal$dxprincipal, "si", "no"))

dx_egreso_noidentificado <- dx_egreso %>%
                            select(dxprincipal, dxegreso, comprobacion) %>%
                            filter(comprobacion != "si") %>%
                            count(dxegreso, name = "count_dx")

subsample_df_bogota$tipoeventoripsdesc <-case_when(
  subsample_df_bogota$tipoeventoripsdesc %in% c("C", "CONSULTAS") ~ "CONSULTAS",
  subsample_df_bogota$tipoeventoripsdesc %in% c("H", "HOSPITALIZACIONES") ~ "HOSPITALIZACIONES",
  subsample_df_bogota$tipoeventoripsdesc %in% c("P", "PROCEDIMIENTOS DE SALUD") ~ "PROCEDIMIENTOS DE SALUD",
  subsample_df_bogota$tipoeventoripsdesc %in% c("U", "URGENCIAS") ~ "URGENCIAS" )

#¿Solo las hospitalizaciones tienen códigos en dxegreso?
#¿Cuantos registros hay que tienen Dx de ingreso diferente al de dxegreso?
subsample_bogota_dif <- subsample_df_bogota %>%
                            filter(dxegreso !=0) %>%
                            mutate(dxdif = ifelse(dxprincipal == dxegreso, 1, 0)) %>%
                            filter(dxdif == 0) #Hay 2265 dx diferentes entre ingreso y egreso y todos son de hospitalizaciones








#--------------------------------------------------------------------------------------------------------------------

#########################################################
#GRUPOS POR CHARLSON-RÉPLICA DE LA TABLA DE COMORBIDITY
#########################################################

#####
#Grupos de Charlons
cie10_charlson <- cie10 %>% filter(!is.na(charlson_clas))

# Realizar la fusión de los datos por la columna "dxprincipal"
merged_data_chrl <- merge(subsample_df_bogota, cie10_charlson, by.x = "dxprincipal", by.y = "codigo", all.x = TRUE)

# Tabla de comorbidity
table_charlson <- merged_data_chrl %>%
                  select(personaid, charlson_clas) %>%
                  distinct() %>%
                  pivot_wider(names_from = charlson_clas, values_from = charlson_clas, values_fn = length, values_fill = 0)
#####

#-------------------------------------------------------------------------------

#########################################################
#GRUPOS POR CHARLSON-RÉPLICA DE LA TABLA DE COMORBIDITY
#########################################################

names(cie10)<- epitrix::clean_labels(names(cie10))

# Realizar la fusión de los datos por la columna "dxprincipal"
merged_data_agr <- merge(subsample_df_bogota, cie10, by.x = "dxprincipal", by.y = "codigo", all.x = TRUE)

# Crear una tabla donde las filas son los personaid únicos y las columnas son las categorías de grandes_grupos_morbilidad_proyecto
table_agora <- merged_data_agr %>%
               select(personaid, grandes_grupos_morbilidad_proyecto) %>%
               distinct() %>%
               pivot_wider(names_from = grandes_grupos_morbilidad_proyecto, values_from = grandes_grupos_morbilidad_proyecto, values_fn = length, values_fill = 0)

table1(~.,table_agora)

table_agora1<-table_agora[,-c(1,2)]
table_agora1[] <- lapply(table_agora1, as.factor)
table1(~.,table_agora1)



#¿Que edad aparece en consultas con Dx propios de neonatos?
#ejemplo:
#P050= Bajo Peso Para La Edad Gestacional
#P073=Otros Recien Nacidos Pretermino
#P393=Infeccion Neonatal De Las Vias Urinarias
#P290=Insuficiencia Cardiaca Neonatal
#P292=Hipertension Neonatal
#P220=Sindrome De Dificultad Respiratoria Del Recien Nacido

#Descripción de dx diferentes
#####
merged_agr_hospit_ppal <- merged_data_agr %>%
  filter(tipoeventoripsdesc == "HOSPITALIZACIONES") %>%
  mutate(dxdif = ifelse(dxprincipal == dxegreso, 1, 0)) %>%
  filter(dxdif == 0)

table1(~grandes_grupos_morbilidad_proyecto,merged_agr_hospit_ppal)


merged_agr_hospit_egre <- subsample_df_bogota %>%
  merge(cie10, by.x = "dxegreso", by.y = "codigo", all.x = TRUE) %>%
  filter(tipoeventoripsdesc == "HOSPITALIZACIONES") %>%
  mutate(dxdif = ifelse(dxprincipal == dxegreso, 1, 0)) %>%
  filter(dxdif == 0)

table1(~grandes_grupos_morbilidad_proyecto,merged_agr_hospit_egre)
#####








