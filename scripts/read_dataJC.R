#cargar paquetes
library(tidyverse)
library(readxl)
library(purrr)
library(writexl)
library(dplyr)
library(openxlsx)
library(stringr)

source("fun/class_alg.R")

#Cargar datos
#CÓDIGOS DM
cods_DM <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM")
#CÓDIGOS ERC
cods_ERC <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "ERC")

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

#EVIDENCIA
evidencia <- read_excel("dat/validacion.xlsx")

#Clasificación DM
bd2011 <- classification(cods_DM,bd2011)
bd2012 <- classification(cods_DM,bd2012)
bd2014 <- classification(cods_DM,bd2014)
bd2015 <- classification(cods_DM,bd2015)

#Creación tabla resultados
evidencia_DM <- evidencia %>% filter(enfermedad == "DM")

resultadosdm <- data.frame("enf" = "DM",
                           "bd" = c("bd2011","bd2012","bd2014","bd2015"),
                           "muestra" = c(nrow(bd2011),nrow(bd2012),nrow(bd2014),nrow(bd2015)),
                           "casos_CIE10" = c(sum(bd2011$enf_cie10, na.rm = TRUE), sum(bd2012$enf_cie10, na.rm = TRUE),
                                             sum(bd2014$enf_cie10, na.rm = TRUE), sum(bd2015$enf_cie10, na.rm = TRUE)),
                           "prev_CIE10" = c(round(sum(bd2011$enf_cie10, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                            round(sum(bd2012$enf_cie10, na.rm = TRUE)/nrow(bd2012), digits = 4),
                                            round(sum(bd2014$enf_cie10, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                            round(sum(bd2015$enf_cie10, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                           "casos_CUPS" = c(sum(bd2011$enf_cups, na.rm = TRUE),"0",
                                            sum(bd2014$enf_cups, na.rm = TRUE),"0"),
                           "prev_CUPS" = c(round(sum(bd2011$enf_cups, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                           "0",
                                           round(sum(bd2014$enf_cups, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                           "0"),
                           "casos_dx_cups_atc" = c(sum(bd2011$target, na.rm = TRUE), sum(bd2012$target, na.rm = TRUE),
                                                   sum(bd2014$target, na.rm = TRUE), sum(bd2015$target, na.rm = TRUE)),
                           "prev_dx_cups_atc" = c(round(sum(bd2011$target, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                                  round(sum(bd2012$target, na.rm = TRUE)/nrow(bd2012), digits = 4),
                                                  round(sum(bd2014$target, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                                  round(sum(bd2015$target, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                           "casos_total" = c(sum(bd2011$casos, na.rm = TRUE), sum(bd2012$casos, na.rm = TRUE),
                                             sum(bd2014$casos, na.rm = TRUE), sum(bd2015$casos, na.rm = TRUE)),
                           "prev_total" = c(round(sum(bd2011$casos, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                            round(sum(bd2012$casos, na.rm = TRUE)/nrow(bd2012), digits = 4),
                                            round(sum(bd2014$casos, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                            round(sum(bd2015$casos, na.rm = TRUE)/nrow(bd2015), digits = 4)
                           ))

resultadosdm <- resultadosdm %>% mutate(prev_literatura = evidencia_DM$prevalencia)

#Clasificación ERC
bd2011 <- classification(cods_ERC,bd2011)
bd2012 <- classification(cods_ERC,bd2012)
bd2014 <- classification(cods_ERC,bd2014)
bd2015 <- classification(cods_ERC,bd2015)

#Creación tabla resultados
evidencia_ERC <- evidencia %>% filter(enfermedad == "ERC")

resultadoserc <- data.frame("enf" = "ERC",
                            "bd" = c("bd2011","bd2012","bd2014","bd2015"),
                            "muestra" = c(nrow(bd2011),nrow(bd2012),nrow(bd2014),nrow(bd2015)),
                            "casos_CIE10" = c(sum(bd2011$enf_cie10, na.rm = TRUE), sum(bd2012$enf_cie10, na.rm = TRUE),
                                              sum(bd2014$enf_cie10, na.rm = TRUE), sum(bd2015$enf_cie10, na.rm = TRUE)),
                            "prev_CIE10" = c(round(sum(bd2011$enf_cie10, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                             round(sum(bd2012$enf_cie10, na.rm = TRUE)/nrow(bd2012), digits = 4),
                                             round(sum(bd2014$enf_cie10, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                             round(sum(bd2015$enf_cie10, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                            "casos_CUPS" = c(sum(bd2011$enf_cups, na.rm = TRUE),"0",
                                             sum(bd2014$enf_cups, na.rm = TRUE),"0"),
                            "prev_CUPS" = c(round(sum(bd2011$enf_cups, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                            "0",
                                            round(sum(bd2014$enf_cups, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                            "0"),
                            "casos_dx_cups_atc" = c(sum(bd2011$target, na.rm = TRUE), sum(bd2012$target, na.rm = TRUE),
                                                    sum(bd2014$target, na.rm = TRUE), sum(bd2015$target, na.rm = TRUE)),
                            "prev_dx_cups_atc" = c(round(sum(bd2011$target, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                                   round(sum(bd2012$target, na.rm = TRUE)/nrow(bd2012), digits = 4),
                                                   round(sum(bd2014$target, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                                   round(sum(bd2015$target, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                            "casos_total" = c(sum(bd2011$casos, na.rm = TRUE), sum(bd2012$casos, na.rm = TRUE),
                                              sum(bd2014$casos, na.rm = TRUE), sum(bd2015$casos, na.rm = TRUE)),
                            "prev_total" = c(round(sum(bd2011$casos, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                             round(sum(bd2012$casos, na.rm = TRUE)/nrow(bd2012), digits = 4),
                                             round(sum(bd2014$casos, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                             round(sum(bd2015$casos, na.rm = TRUE)/nrow(bd2015), digits = 4)
                            ))

resultadoserc <- resultadoserc %>% mutate(prev_literatura = evidencia_ERC$prevalencia)

#Consolidado
consolidado <- rbind (resultadosdm, resultadoserc)
