###################################
#Proyecto AGORA
#Fecha: 07-marzo-2023
#Versión: Jennifer Murillo
###################################


library(arrow)
library(dplyr)
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
#Muestra Bogotá: 35'351.724 (2008 a 2022)
#sample_df_bogota_parquet <- read_parquet("dat/sample_df_bogota_parquet") %>% mutate(ID_uniq = row_number())

#Subuestra Bogotá: 1,767,586 (2018 a 2022)
subsample_df_bogota <- readRDS("dat/subsample_parquet") %>% mutate(ID_uniq = row_number())



#---------------
#Data cleaning
#---------------

#AÑO DE CONSULTA
subsample_df_bogota <- subsample_df_bogota %>%
                  mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"),
                         anio = format(fechaid, "%Y"))

#EDAD
# Convertir la variable "edad" a formato numérico
subsample_df_bogota <- mutate(subsample_df_bogota, edad_num = as.numeric(edad))

#SEXO
# Unir las categorías de la variable "sexo"
subsample_df_bogota$sexo <- ifelse(subsample_df_bogota$sexodesc %in% c("F", "FEMENINO"), "Mujeres",
                                   ifelse(subsample_df_bogota$sexodesc %in% c("M", "MASCULINO"), "Hombres",
                                          ifelse(subsample_df_bogota$sexodesc %in% c("N", "NO DEFINIDO"), "Sin dato",
                                          as.character(subsample_df_bogota$sexodesc))))

subsample_df_bta_pir <-  subsample_df_bogota %>% filter(sexo %in% c("Mujeres","Hombres"))





# Pirámide poblacional
# Definir los grupos de edad de 5 años hasta los 80 años
breaks <- c(seq(0, 80, by = 5), Inf)  # Agregar "Inf" para el último grupo "más de 80"

# Asignar a cada individuo un grupo de edad quinquenal con etiquetas personalizadas
etiquetas <- paste0(breaks[-length(breaks)], " a ", breaks[-1])
subsample_df_bta_pir$edad_quinquenal <- cut(subsample_df_bta_pir$edad_num, breaks = breaks,
                                           labels = etiquetas, include.lowest = TRUE)

# Calcular el total de cada sexo
total_hombres <- sum(subsample_df_bta_pir$sexo == "Hombres")
total_mujeres <- sum(subsample_df_bta_pir$sexo == "Mujeres")

tabla_edad <- subsample_df_bta_pir %>%
  group_by(edad_quinquenal, sexo) %>%
  summarise(total_personas = n()) %>%
  group_by(edad_quinquenal) %>%
  mutate(porcentaje = ifelse(sexo == "Hombres", total_personas / total_hombres * 100,
                             total_personas / total_mujeres * 100))


# Gráfico de barras para la pirámide poblacional
piramide <- ggplot(data = as.data.frame(tabla_edad), aes(x = edad_quinquenal, y = ifelse(sexo == "Hombres", porcentaje, -porcentaje), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
  labs(x = "Grupo de Edad", y = "% Población", title = "Pirámide Poblacional") +
  scale_fill_manual(values = c("Mujeres" = "darkred", "Hombres" = "midnightblue")) +
  theme_minimal() +
  coord_flip()

piramide




#####

#Para año 2018
subsample_df_bta_pir_2018 <- subset(subsample_df_bta_pir, anio == "2018")
# Calcular el total de cada sexo
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
  labs(x = "Grupo de Edad", y = "% Población", title = "Pirámide Poblacional") +
  scale_fill_manual(values = c("Mujeres" = "darkred", "Hombres" = "midnightblue")) +
  theme_minimal() +
  coord_flip()

piramide_2018

grid.arrange(piramide, piramide_2018, ncol = 2)

#Se compara con el artículo: "El uso de pirámides poblacionales como representación gráfica del sistema de salud colombiano";  https://www.redalyc.org/journal/2738/273856494013/html/

#####

subsample_df_bogota<- subsample_df_bogota %>% filter(edad_num>=18)




##############################################################
# CÁLCULO DE PREVALENCIA DE DIABETES MELLITUS (DM) POR 3 VÍAS
##############################################################


#-----------------------------------
# VIA 1: USANDO PAQUETE COMORBIDITY
#-----------------------------------

#Se crea dataset con pacientes identificados según sus comorbilidades:
comorb_pack<-comorbidity(x = subsample_df_bogota, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
View(comorb_pack) #1,349,824 sin duplicados

# Se calculan las prevalencias:
p_diab <- mean(comorb_pack$diab == 1, na.rm = TRUE)
p_diabnocompli <- mean(comorb_pack$diabwc == 1, na.rm = TRUE)
p_diabt <- mean(comorb_pack$diab == 1 | comorb_pack$diabwc == 1, na.rm = TRUE)
p_renal <- mean(comorb_pack$rend == 1, na.rm = TRUE)


#Tabla resultado
casos_comorbidity <- comorb_pack %>%
  summarise(
    dm_sin_complicacion = sum(diab == 1),
    dm_con_complicacion = sum(diabwc == 1),
    dm_general = sum(diab == 1 | diabwc == 1),
    enfermedad_renal = sum(rend == 1)
  ) %>%
  melt(id.vars = NULL, value.name = "casos", variable.name = "Patología")%>%
  mutate(n = nrow(comorb_pack))

prevalencias_comorbidity <- comorb_pack %>%
  summarise(
    dm_sin_complicacion = sum(diab == 1) / n() * 100,
    dm_con_complicacion = sum(diabwc == 1) / n() * 100,
    dm_general = sum(diab == 1 | diabwc == 1) / n() * 100,
    enfermedad_renal = sum(rend == 1) / n() * 100
  ) %>%
  melt(id.vars = NULL, value.name = "Prevalencia", variable.name = "Patología")


tabla_comorbidity_sin18 <- casos_comorbidity %>%
  left_join(prevalencias_comorbidity %>% select(Patología, Prevalencia), by = "Patología")




#----------------------------------------
# VIA 2: USANDO LISTA DE CÓDIGOS CIE-10
#----------------------------------------

#lista CIE-10 descargada de la página de SISPRO (https://web.sispro.gov.co/WebPublico/Consultas/ConsultarDetalleReferenciaBasica.aspx?Code=CIE10)
cie10_dm <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie-dm")

#Se dejan solo las variables que se van a usar de la base total yse identifica aquellos registros que tienen códigos CIE10 de diabetes
subsample_bta <- subsample_df_bogota %>%
                 select(ID_uniq, personaid, dxprincipal, codigoprocedimiento, fechaid, anio) %>% #Se seleccionan solo las variables a usar para que quede menos pesada (mas el id único por si se requiere traer alguna otra variable después)
                 mutate(dm = ifelse(dxprincipal %in% cie10_dm$codigo, 1, 0),
                        dm_t1 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t1"], 1, 0),
                        dm_t2 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t2"], 1, 0))


# Cuántos dx hay de diabetes, contando una vez cada "personaid"
#Tabla resultado

#Por año
#Tabla resultado
prevalencias_cie10 <- subsample_bta %>%
  group_by(anio) %>%
  summarise(
    dm = sum(dm == 1) / n() * 100,
    dm_tipo1 = sum(dm_t1 == 1) / n() * 100,
    dm_tipo2 = sum(dm_t2 == 1) / n() * 100
  ) %>%
  melt(id.vars = "anio", value.name = "Prevalencia", variable.name =  "Tipo") %>%
  spread(key = anio, value = Prevalencia)


prevalencias_cie10t <- subsample_bta %>%
  summarise(
    dm = sum(dm == 1) / n() * 100,
    dm_tipo1 = sum(dm_t1 == 1) / n() * 100,
    dm_tipo2 = sum(dm_t2 == 1) / n() * 100
  ) %>%
  melt(id.vars = NULL, value.name = "P_gral", variable.name = "Tipo")

tabla_cie10 <- prevalencias_cie10 %>%
  left_join(prevalencias_cie10t %>% select(Tipo, P_gral), by = "Tipo")


##por casos:
casos_cie10 <- subsample_bta %>%
  summarise(
    dm = sum(dm == 1) ,
    dm_tipo1 = sum(dm_t1 == 1),
    dm_tipo2 = sum(dm_t2 == 1) ) %>%
  melt(id.vars = NULL, value.name = "Casos", variable.name =  "Tipo") %>%
  mutate(n = nrow(subsample_bta))

tabla_cie10cas <- casos_cie10 %>%
  left_join(prevalencias_cie10t %>% select(Tipo, P_gral), by = "Tipo")


#--------------------------------------------------
# VIA 3: USANDO CÓDIGOS CUPS Y FUNCIÓN DE DANIELA
#--------------------------------------------------


# FUNCION

#clasificación
classification <- function(cods, bd) {
  #Limpia los nombres de los códigos
  names(cods) <- epitrix::clean_labels(names(cods))
  cods <- cods %>% mutate(name_cods = epitrix::clean_labels(codigo_significado))
  cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
  cods_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo)
  names_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo_significado)
  names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)

  #Agrega columnas para indicar si hay coincidencias con los códigos CIE-10 y CUPS
  bd$enf_cie10 <- ifelse(bd$diagnosticocd %in% cods_cie10$codigo, 1, 0)
  bd$enf_cups <- ifelse(bd$procedimientocd %in% cods_cups$codigo, 1, 0)

  #Crea una columna para indicar si hay algún caso detectado
  bd$casos <- ifelse(bd$enf_cie10 == 1 | bd$enf_cups == 1, 1, 0)

  return(bd)
}




#Hoja que contine los cups clasificados
cods_DM <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM")

#Base de datos subsample de Bogotá
subsample_bogotaf <- subsample_df_bogota %>% rename(diagnosticocd = dxprincipal, procedimientocd = codigoprocedimiento)

#Clasificación de los registros de la base de datos de Bogotá con la función
subsample_bogota_c <- classification(cods_DM, subsample_bogotaf)

#Evidencia
evidencia <- read_excel("dat/validacion.xlsx")

#Tabla de resultados
#por año:
prevalencia_cupsdaniela <- subsample_bogota_c %>%
    group_by(anio) %>%
    summarise(
      dm_cie10 = sum(enf_cie10 == 1) / n() * 100,
      dm_cups = sum(enf_cups == 1) / n() * 100,
      dm_total = sum(casos == 1) / n() * 100 ) %>%
  melt(id.vars = "anio", value.name = "Prevalencia", variable.name =  "Tipo") %>%
  spread(key = anio, value = Prevalencia)

prevalencia_cupsdanielaT <- subsample_bogota_c %>%
  summarise(
    dm_cie10 = sum(enf_cie10 == 1) / n() * 100,
    dm_cups = sum(enf_cups == 1) / n() * 100,
    dm_total = sum(casos == 1) / n() * 100 ) %>%
  melt(id.vars = NULL, value.name = "P_gral", variable.name = "Tipo")

tabla_cupsd <- prevalencia_cupsdaniela %>%
  left_join(prevalencia_cupsdanielaT %>% select(Tipo, P_gral), by = "Tipo")



##por casos:
casos_cupsdaniela <- subsample_bogota_c %>%
  summarise(
    dm_cie10 = sum(enf_cie10 == 1) ,
    dm_cups = sum(enf_cups == 1) ,
    dm_total = sum(casos == 1) ) %>%
  melt(id.vars = NULL, value.name = "Casos", variable.name =  "Tipo") %>%
  mutate(n = nrow(subsample_bogota_c))

tabla_cupsdcas <- casos_cupsdaniela %>%
  left_join(prevalencia_cupsdanielaT %>% select(Tipo, P_gral), by = "Tipo")





#----------------------------------------------------------------------------------------------------

##########################################
# tablas drive codigos cie10 consolidados
###########################################


#TABLA DE DX (CIE10) REGISTRADOS EN LA BASE DE DATOS DE TODO BOGOTÁ, POR AÑO
df_pivot_dxprincipal <- read_excel("dat/df_pivot_dxprincipalfinal.xlsx")

#arhivo lista de códigos CIE10
cie10 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie10")

#identificación de las descripciones de cada código cie10
df_pivot_con_descripcion <- df_pivot_dxprincipal %>% left_join(cie10, by = c("dxprincipal" = "codigo"))

#write.xlsx(df_pivot_con_descripcion, "dat/df_pivot_con_descripcion.xlsx")
cie10_noidentificados <- df_pivot_con_descripcion %>% filter(is.na(descripcion)) #819 (6.6% no identificados)




#TABLA DE CUPS REGISTRADOS EN LA BASE DE DATOS DE TODO BOGOTÁ, POR AÑO
df_pivot_cups <- read_excel("dat/df_pivot_procedimientos.xlsx")

#Archivos de códigos CUPS:
cups2021 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2021") %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2022 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2022") %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2024 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2024") %>% mutate(descripcion = epitrix::clean_labels(descripcion)) %>% select(-nombre)

cups_todos <- dplyr::bind_rows(cups2021 %>% mutate(aniocups = "cups2021"),
                               cups2022 %>% mutate(aniocups = "cups2022"),
                               cups2024 %>% mutate(aniocups = "cups2024") )

#identificación de las descripciones de cada cups
df_pivot_cups_descripcion <- df_pivot_cups %>% left_join(cups_todos, by = c("codigoprocedimiento" = "codigo"))

#write.xlsx(df_pivot_con_descripcion, "dat/df_pivot_con_descripcion.xlsx")
cups_noidentificado <- df_pivot_cups_descripcion %>% filter(is.na(descripcion)) #2833 (9.4%) no identificados





#comparación cie10 con artículos






