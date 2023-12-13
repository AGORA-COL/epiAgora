# Rutina para clasificación de enfermedades
# Fecha: DIC-2023
# Proyecto AGORA

#cargar paquetes
library(tidyverse)
library(readxl)
library(purrr)
library(writexl)
library(dplyr)
library(openxlsx)
library(stringr)

source("fun/class_alg.R")

diseases <- c("DM", "ERC")

#Cargar datos
#CÓDIGOS DM
cods_DM <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = diseases[1])
#CÓDIGOS ERC
cods_ERC <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = diseases[2])

#BASE DE DATOS RIPS
#bd2011
bd2011 <- read_excel("dat/bd_2011.xlsx")
names(bd2011) <- epitrix::clean_labels(names(bd2011))
bd2011 <- bd2011 %>% mutate(name = epitrix::clean_labels(medicamentodesc))

#bd2012
bd2012 <- read_excel("dat/bd_2012.xlsx")
names(bd2012) <- epitrix::clean_labels(names(bd2012))
bd2012 <- bd2012 <- separate(bd2012, col= diagnosticoprincipal, into = c("diagnosticocd", "defcie10"), sep = "-")
bd2012 <- bd2012 %>% mutate (name = epitrix::clean_labels(defcie10))
bd2012$diagnosticocd <- gsub(" ", "", bd2012$diagnosticocd)

#bd2014
bd2014 <- read_excel("dat/bd_2014.xlsx")
names(bd2014) <- epitrix::clean_labels(names(bd2014))
bd2014 <- bd2014 %>% mutate (name = epitrix::clean_labels(nombre))

#bd2015
bd2015 <- read_excel("dat/bd_2015.xlsx")
names(bd2015) <- epitrix::clean_labels(names(bd2015))
bd2015 <- bd2015 %>% mutate (name = epitrix::clean_labels(medicamento))

#consolidado
consolidado <- data.frame(enf = character(),
                          bd = character(),
                          muestra = numeric(),
                          casos_CIE10 = numeric(),
                          prev_CIE10 = numeric(),
                          casos_CUPS = numeric(),
                          prev_CUPS = numeric(),
                          casos_dx_cups_atc = numeric(),
                          prev_dx_cups_atc = numeric(),
                          casos_total = numeric(),
                          prev_total = numeric(),
                          prev_literatura = numeric()
)

#EVIDENCIA
evidencia <- read_excel("dat/validacion.xlsx")


#Clasificación DM
bd2011 <- classification(cods_DM,bd2011)
bd2012 <- classification(cods_DM,bd2012)
bd2014 <- classification(cods_DM,bd2014)
bd2015 <- classification(cods_DM,bd2015)
#Creación tabla resultados
disease <- diseases[1]
evidencia_DM <- evidencia %>% filter(enfermedad == "DM")
resultados_tabla<-rbind(resultados(bd2011, disease), resultados(bd2012, disease),
                        resultados(bd2014, disease),resultados(bd2015, disease))
resultados_tabla <- resultados_tabla %>% mutate(prev_literatura = evidencia_DM$prevalencia)
consolidado <- rbind (consolidado, resultados_tabla)


#Clasificación ERC
bd2011 <- classification(cods_ERC,bd2011)
bd2012 <- classification(cods_ERC,bd2012)
bd2014 <- classification(cods_ERC,bd2014)
bd2015 <- classification(cods_ERC,bd2015)
#Creación tabla resultados
disease <- diseases[2]
evidencia_ERC <- evidencia %>% filter(enfermedad == "ERC")

resultados_tabla<-rbind(resultados(bd2011, disease), resultados(bd2012, disease),
                        resultados(bd2014, disease),resultados(bd2015, disease))
resultados_tabla <- resultados_tabla %>% mutate(prev_literatura = evidencia_ERC$prevalencia)

#Consolidado
consolidado <- rbind (consolidado, resultados_tabla)
