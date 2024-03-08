########################################
# COMPARACIÓN DE CÓDIGOS CUPS POR AÑO
#######################################



###################################
#Proyecto AGORA
#Fecha: 07-marzo-2023
#Versión: Jennifer Murillo
###################################

library(readxl)
library(dplyr)
library(stringr)
library(stringdist)



#Archivos de códigos CUPS:
cups2021 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2021") %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2022 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2022") %>% mutate(descripcion = epitrix::clean_labels(descripcion))
cups2024 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2024") %>% mutate(descripcion = epitrix::clean_labels(descripcion)) %>% select(-nombre)

cups_todos <- dplyr::bind_rows(cups2021 %>% mutate(aniocups = "cups2021"),
                               cups2022 %>% mutate(aniocups = "cups2022"),
                               cups2024 %>% mutate(aniocups = "cups2024") )


#¿Que cups tienen la misma descripción pero diferente código en cada año?
cups_diferentes_codigo <- cups_todos %>% group_by(descripcion) %>% filter(n_distinct(codigo) > 1) %>% ungroup() 
#¿Que cups tienen el mismo código pero diferente descripción en cada año?
cups_diferentes_descrip <- cups_todos %>% group_by(codigo) %>% filter(n_distinct(descripcion) > 1) %>% ungroup()


#Examinar que códigos CUPS hay en la submuestra de registros de Bogotá:
subsample_bta <- readRDS("dat/subsample_parquet") %>% mutate(ID_uniq = row_number()) %>%  select(ID_uniq, personaid, dxprincipal, codigoprocedimiento, fechaid) %>% #Se seleccionan solo las variables a usar para que quede menos pesada (mas el id único por si se requiere traer alguna otra variable después)
                                                      mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"))

codigos_cups_bta <- distinct(subsample_bta, codigoprocedimiento) %>% left_join(cups_todos, by = c("codigoprocedimiento" = "codigo")) %>%
                    filter(!is.na(codigoprocedimiento))

codigos_noidentificados <- codigos_cups_bta %>% filter(is.na(descripcion))


#----------------

# Se identifican los códigos del año 2022 que no están en la hoja del año 2021
codigos_nuevos_2022 <- anti_join(cups2022, cups2021, by = "codigo") #códigos adicionados
codigos_excluidos_2022 <- anti_join(cups2021, cups2022, by = "codigo")  #códigos excluidos

# Se identifican los códigos del año 2024 que no están en la hoja del año 2022
codigos_nuevos_2024 <- anti_join(cups2024, cups2022, by = "codigo")
codigos_excluidos_2024 <- anti_join(cups2022, cups2024, by = "codigo")


