########################################
# COMPARACIÓN DE CÓDIGOS CUPS POR AÑO
#######################################


library(readxl)
library(dplyr)
library(stringr)

#Archivo de códigos:
cups2021 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2021") %>% mutate(descripcion_1 = epitrix::clean_labels(descripcion))
cups2022 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2022") %>% mutate(descripcion_1 = epitrix::clean_labels(descripcion))
cups2024 <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cups2024") %>% mutate(descripcion_1 = epitrix::clean_labels(descripcion))


# Se identifican los códigos del año 2022 que no están en la hoja del año 2021
codigos_nuevos_2022 <- anti_join(cups2022, cups2021, by = "codigo")
View(codigos_nuevos_2022)

# Se identifican los códigos del año 2024 que no están en la hoja del año 2022
codigos_nuevos_2024 <- anti_join(cups2024, cups2022, by = "codigo")
View(codigos_nuevos_2024)

#Comparar entre año 2024 y 2022
codigos_dif <- cups2022 %>% anti_join(cups2021, by = c("codigo", "descripcion_1")) 
