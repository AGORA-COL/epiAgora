###################################
#Proyecto AGORA
#Fecha: 07-marzo-2023
#Versión: Jennifer Murillo
###################################


library(arrow)
library(dplyr)
library(tidyr)
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





#Pirámide poblacional
#Grupos de edad de 5 años hasta los 80 años
breaks <- c(seq(0, 80, by = 5), Inf)  #"Inf" para el último grupo "más de 80"

etiquetas <- paste0(breaks[-length(breaks)], " a ", breaks[-1])
subsample_df_bta_pir$edad_quinquenal <- cut(subsample_df_bta_pir$edad_num, breaks = breaks,
                                           labels = etiquetas, include.lowest = TRUE)

#Total por sexo
total_hombres <- sum(subsample_df_bta_pir$sexo == "Hombres")
total_mujeres <- sum(subsample_df_bta_pir$sexo == "Mujeres")

subsample_df_bogota<- subsample_df_bogota %>% filter(edad_num>=18)

tabla_edad <- subsample_df_bogota %>%
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


#####

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

#Se crea dataset con pacientes identificados según sus comorbilidades:
comorb_pack<-comorbidity(x = subsample_df_bogota, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
#View(comorb_pack) #1'142.343 sin duplicados

#Tabla de resultados mediante la función
comorb_pack$diabTotal <- ifelse(comorb_pack$diab == 1 | comorb_pack$diabwc == 1, 1,0 )
variables_comorbidity <-c("diab", "diabwc", "diabTotal", "rend")
resultados_comorbidity <- tabla_resultados(data = comorb_pack, variables = variables_comorbidity)


#como la función de comorbidity no tiene la opción de sacar grupos por año, se debe hacer manualmente
#Se crea dataset con pacientes identificados según sus comorbilidades:
#AÑO2018
subsample_df_bogota_2018<-subsample_df_bogota %>% filter(anio=="2018")
comorb_pack_2018<-comorbidity(x = subsample_df_bogota_2018, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
comorb_pack_2018$diabTotal_2018 <- ifelse(comorb_pack_2018$diab == 1 | comorb_pack_2018$diabwc == 1, 1,0 )
variables_comorbidity_2018 <-c("diab", "diabwc", "diabTotal_2018", "rend")
resultados_comorbidity_2018 <- tabla_resultados(data = comorb_pack_2018, variables = variables_comorbidity_2018)


#AÑO2019
subsample_df_bogota_2019<-subsample_df_bogota %>% filter(anio=="2019")
comorb_pack_2019<-comorbidity(x = subsample_df_bogota_2019, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
comorb_pack_2019$diabTotal_2019 <- ifelse(comorb_pack_2019$diab == 1 | comorb_pack_2019$diabwc == 1, 1,0 )
variables_comorbidity_2019 <-c("diab", "diabwc", "diabTotal_2019", "rend")
resultados_comorbidity_2019 <- tabla_resultados(data = comorb_pack_2019, variables = variables_comorbidity_2019)


#AÑO2020
subsample_df_bogota_2020<-subsample_df_bogota %>% filter(anio=="2020")
comorb_pack_2020<-comorbidity(x = subsample_df_bogota_2020, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
comorb_pack_2020$diabTotal_2020 <- ifelse(comorb_pack_2020$diab == 1 | comorb_pack_2020$diabwc == 1, 1,0 )
variables_comorbidity_2020 <-c("diab", "diabwc", "diabTotal_2020", "rend")
resultados_comorbidity_2020 <- tabla_resultados(data = comorb_pack_2020, variables = variables_comorbidity_2020)


#AÑO2021
subsample_df_bogota_2021<-subsample_df_bogota %>% filter(anio=="2021")
comorb_pack_2021<-comorbidity(x = subsample_df_bogota_2021, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
comorb_pack_2021$diabTotal_2021 <- ifelse(comorb_pack_2021$diab == 1 | comorb_pack_2021$diabwc == 1, 1,0 )
variables_comorbidity_2021 <-c("diab", "diabwc", "diabTotal_2021", "rend")
resultados_comorbidity_2021 <- tabla_resultados(data = comorb_pack_2021, variables = variables_comorbidity_2021)


#AÑO2022
subsample_df_bogota_2022<-subsample_df_bogota %>% filter(anio=="2022")
comorb_pack_2022<-comorbidity(x = subsample_df_bogota_2022, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
comorb_pack_2022$diabTotal_2022 <- ifelse(comorb_pack_2022$diab == 1 | comorb_pack_2022$diabwc == 1, 1,0 )
variables_comorbidity_2022 <-c("diab", "diabwc", "diabTotal_2022", "rend")
resultados_comorbidity_2022 <- tabla_resultados(data = comorb_pack_2022, variables = variables_comorbidity_2022)


#----------------------------------------
# VIA 2: USANDO LISTA DE CÓDIGOS CIE-10
#----------------------------------------

#Lista CIE-10 descargada de la página de SISPRO (https://web.sispro.gov.co/WebPublico/Consultas/ConsultarDetalleReferenciaBasica.aspx?Code=CIE10)
cie10_dm <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie-dm")

subsample_bta <- subsample_df_bogota %>%
                 select(ID_uniq, personaid, dxprincipal, codigoprocedimiento, fechaid, anio) %>%
                 mutate(dm = ifelse(dxprincipal %in% cie10_dm$codigo, 1, 0),
                        dm_t1 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t1"], 1, 0),
                        dm_t2 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t2"], 1, 0))


# Cuántos dx hay de diabetes, contando una vez cada "personaid"


#Tabla de resultados
variables_cie10_dm <-c("dm_t1", "dm_t2", "dm")
resultados_cie10 <- tabla_resultados(data = subsample_bta, variables = variables_cie10_dm)
resultados_cie10_anios <- tabla_anioresultados(data = subsample_bta, variables = variables_cie10_dm)





#--------------------------------------------------
# VIA 3: USANDO CÓDIGOS CUPS Y FUNCIÓN DE DANIELA
#--------------------------------------------------


#FUNCION

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
cods_DM <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "ERC")
#Base de datos subsample de Bogotá
subsample_bogotaf <- subsample_df_bogota %>% rename(diagnosticocd = dxprincipal, procedimientocd = codigoprocedimiento)

#PARA DM
#Hoja que contine los cups clasificados
cods_DM <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM")
#subsample_bogotaH <- subsample_bogotaf %>% filter(tipoeventoripsdesc %in% c("H", "HOSPITALIZACIONES"))
#subsample_bogotaH <- subsample_bogotaH %>% filter(edad>=18)

#Clasificación de los registros de la base de datos de Bogotá con la función
subsample_bogota_c <- classification(cods_DM, subsample_bogotaf)
#Evidencia
evidencia <- read_excel("dat/validacion.xlsx")

#Tabla de resultados
variables_cie10_cups <-c("enf_cie10", "enf_cups", "casos")
resultados_cie10_cups <- tabla_resultados(data = subsample_bogota_c, variables = variables_cie10_cups)
resultados_cie10_cups_anios <- tabla_anioresultados(data = subsample_bogota_c, variables = variables_cie10_cups)


#PARA ERC
#Hoja que contine los cups clasificados
cods_DM <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "ERC")
#subsample_bogotaH <- subsample_bogotaf %>% filter(tipoeventoripsdesc %in% c("H", "HOSPITALIZACIONES"))
#subsample_bogotaH <- subsample_bogotaH %>% filter(edad>=18)

#Clasificación de los registros de la base de datos de Bogotá con la función
subsample_bogota_c <- classification(cods_DM, subsample_bogotaf)
#Evidencia
evidencia <- read_excel("dat/validacion.xlsx")

#Tabla de resultados
variables_cie10_cups <-c("enf_cie10", "enf_cups", "casos")
resultados_cie10_cups <- tabla_resultados(data = subsample_bogota_c, variables = variables_cie10_cups)
resultados_cie10_cups_anios <- tabla_anioresultados(data = subsample_bogota_c, variables = variables_cie10_cups)





#----------------------------------------------------------------------------------------------------

##########################################
# tablas drive codigos cie10 consolidados
###########################################


#TABLA DE DX (CIE10) REGISTRADOS EN LA BASE DE DATOS DE TODO BOGOTÁ, POR AÑO
df_pivot_dxprincipal <- read_excel("dat/df_pivot_dxprincipalfinal.xlsx")

#arhivo lista de códigos CIE10 (12,648 códigos)
cie10 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie10")

#identificación de las descripciones de cada código cie10
df_pivot_con_descripcion <- df_pivot_dxprincipal %>% left_join(cie10, by = c("dxprincipal" = "codigo"))

#write.xlsx(df_pivot_con_descripcion, "dat/df_pivot_con_descripcion.xlsx")
cie10_noidentificados <- df_pivot_con_descripcion %>% filter(is.na(descripcion)) #789 (6.2% no identificados)

write.xlsx(cie10_noidentificados, "D:/Descargas/cie10_noidentificados.xlsx")


#TABLA DE CUPS REGISTRADOS EN LA BASE DE DATOS DE TODO BOGOTÁ, POR AÑO (12,062)
df_pivot_cups <- read_excel("dat/df_pivot_procedimientos.xlsx")

#Archivos de códigos CUPS:
#Fuente: https://www.minsalud.gov.co/salud/POS/paginas/plan-obligatorio-de-salud-pos.aspx
cups2009 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2009", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2015 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2015", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2016 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2016", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2017 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2017", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2018 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2018", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2019 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2019", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2020 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2020", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2021 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2021", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2022 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2022", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2023 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2023", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2024 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2024", col_types = c("text", "text")) %>% mutate(descripcion = epitrix::clean_labels(descripcion))

cups_todos <- dplyr::bind_rows(cups2009 %>% mutate(aniocups = "cups2009"),
                               cups2015 %>% mutate(aniocups = "cups2015"),
                               cups2016 %>% mutate(aniocups = "cups2016"),
                               cups2017 %>% mutate(aniocups = "cups2017"),
                               cups2018 %>% mutate(aniocups = "cups2018"),
                               cups2019 %>% mutate(aniocups = "cups2019"),
                               cups2020 %>% mutate(aniocups = "cups2020"),
                               cups2021 %>% mutate(aniocups = "cups2021"),
                               cups2022 %>% mutate(aniocups = "cups2022"),
                               cups2023 %>% mutate(aniocups = "cups2023"),
                               cups2024 %>% mutate(aniocups = "cups2024") )

#identificación de las descripciones de cada cups
df_pivot_cups_descripcion <- df_pivot_cups %>% left_join(cups_todos, by = c("codigoprocedimiento" = "codigo"))

#write.xlsx(df_pivot_con_descripcion, "dat/df_pivot_con_descripcion.xlsx")
cups_noidentificado <- df_pivot_cups_descripcion %>% filter(is.na(descripcion)) #23  (%) no identificados

write.xlsx(cups_noidentificado, "D:/Descargas/cups_noidentificado.xlsx")



