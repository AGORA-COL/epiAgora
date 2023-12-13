library(tidyverse)
library(readxl)
library(purrr)
library(writexl)
library(dplyr)
library(openxlsx)
library(stringr)

#source("fun/class_alg.R")

#DM
#CÓDIGOS
cods <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM")
names(cods) <- epitrix::clean_labels(names(cods))
cods <- cods %>% mutate(name_cods = epitrix::clean_labels(codigo_significado))
cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
cods_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo)
names_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo_significado)
names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)

#BASE DE DATOS
#bd2011
bd2011 <- read_excel("dat/bd_2011.xlsx")
names(bd2011) <- epitrix::clean_labels(names(bd2011))
bd2011 <- bd2011 %>% mutate(name_drug = epitrix::clean_labels(medicamentodesc))
#bd2011 - cie10
bd2011$enf_cie10 <- NA
bd2011$enf_cie10[bd2011$diagnosticocd %in% cods_cie10$codigo] <- 1
#bd2011 - cups
bd2011$enf_cups <- NA
bd2011$enf_cups[bd2011$procedimientocd %in% cods_cups$codigo] <- 1
#bd2011 - meds
vector_raices_medic <- unique(cods$root)
bd2011$medicamento_target <- FALSE
for (i in seq_along (vector_raices_medic)) {
  bd2011$medicamento_target <- if_else (bd2011$medicamento_target == TRUE, bd2011$medicamento_target,
                                        str_detect(string = bd2011$name_drug, pattern = vector_raices_medic[i]))
}
#bd2011 - cie10, cups, meds
bd2011 <- mutate(bd2011,
                 casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | medicamento_target == "TRUE",1,0))


#bd2012
bd2012 <- read_excel("dat/bd_2012.xlsx")
names(bd2012) <- epitrix::clean_labels(names(bd2012))
bd20122_1 <- strsplit(bd2012$diagnosticoprincipal, "-")
bd20122_1 <- data.frame(bd2012, do.call(rbind, bd20122_1))
bd20122_1 <- bd20122_1 %>% rename(codcie10 = X1, defcie10 = X2)
bd20122_1 <- bd20122_1 %>% mutate (name_dx = epitrix::clean_labels(defcie10))
bd20122_1$codcie10 <- gsub(" ", "", bd20122_1$codcie10)

#bd2012 - cie10
bd20122_1$enf_cie10 <- NA
bd20122_1$enf_cie10[bd20122_1$codcie10 %in% cods_cie10$codigo] <- 1
#bd2012 - cie10, definición cie10
vector_raices_dx <- unique(cods$root)
bd20122_1$dx_target <- FALSE
for (i in seq_along(vector_raices_dx)) {
  bd20122_1$dx_target <- if_else (bd20122_1$dx_target == TRUE,  bd20122_1$dx_target,
                                  str_detect(string =   bd20122_1$name_dx, pattern = vector_raices_dx[i]))
}
bd20122_1<- mutate(bd20122_1,
                   casos = ifelse(enf_cie10 == "1"| dx_target == "TRUE",1,0))



#bd2014
bd2014 <- read_excel("dat/bd_2014.xlsx")
names(bd2014) <- epitrix::clean_labels(names(bd2014))
bd2014 <- bd2014 %>% mutate (name_drug = epitrix::clean_labels(nombre))
#bd2014 - cie10
  bd2014$enf_cie10 <- NA
  bd2014$enf_cie10[bd2014$diagnosticocd %in% cods_cie10$codigo] <- 1
  #bd2014 - cups
  bd2014$enf_cups <- NA
  bd2014$enf_cups[bd2014$procedimientocd %in% cods_cups$codigo] <- 1
  #bd2014 - meds
  bd2014$med_target <- FALSE
  for (i in seq_along(vector_raices_medic)) {
    bd2014$med_target <- if_else (bd2014$med_target == TRUE, bd2014$med_target,
                                  str_detect(string = bd2014$name_drug, pattern = vector_raices_medic[i]))
  }
  #bd2014 - cie10, cups, meds
  bd2014<- mutate(bd2014,
                  casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | med_target == "TRUE",1,0))


#bd2015
bd2015 <- read_excel("dat/bd_2015.xlsx")
names(bd2015) <- epitrix::clean_labels(names(bd2015))
bd2015 <- bd2015 %>% mutate (name_drug = epitrix::clean_labels(medicamento))
#bd2015 - cie10
bd2015$enf_cie10 <- NA
bd2015$enf_cie10[bd2015$diagnosticocd %in% cods_cie10$codigo] <- 1
#bd2015 - meds
bd2015$med_target <- FALSE
for (i in seq_along(vector_raices_medic)) {
  bd2015$med_target <- if_else (bd2015$med_target == TRUE, bd2015$med_target,
                                str_detect(string = bd2015$name_drug, pattern = vector_raices_medic[i]))
}
#bd2015 - cie10, meds
bd2015<- mutate(bd2015,
                casos = ifelse(enf_cie10 == "1"| med_target == "TRUE",1,0))

#EVIDENCIA
evidencia <- read_excel("dat/validacion.xlsx")
evidencia <- evidencia %>% filter(enfermedad == "DM")

#Creación tabla resultados
resultadosdm <- data.frame("enf" = "DM",
                         "bd" = c("bd2011","bd2012","bd2014","bd2015"),
                         "muestra" = c(nrow(bd2011),nrow(bd20122_1),nrow(bd2014),nrow(bd2015)),
                         "casos_CIE10" = c(sum(bd2011$enf_cie10, na.rm = TRUE), sum(bd20122_1$enf_cie10, na.rm = TRUE),
                                           sum(bd2014$enf_cie10, na.rm = TRUE), sum(bd2015$enf_cie10, na.rm = TRUE)),
                         "prev_CIE10" = c(round(sum(bd2011$enf_cie10, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                          round(sum(bd20122_1$enf_cie10, na.rm = TRUE)/nrow(bd20122_1), digits = 4),
                                          round(sum(bd2014$enf_cie10, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                          round(sum(bd2015$enf_cie10, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                         "casos_CUPS" = c(sum(bd2011$enf_cups, na.rm = TRUE),"0",
                                          sum(bd2014$enf_cups, na.rm = TRUE),"0"),
                         "prev_CUPS" = c(round(sum(bd2011$enf_cups, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                         "0",
                                         round(sum(bd2014$enf_cups, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                         "0"),
                         "casos_dx_cups_atc" = c(sum(bd2011$medicamento_target, na.rm = TRUE), sum(bd20122_1$dx_target, na.rm = TRUE),
                                                 sum(bd2014$med_target, na.rm = TRUE), sum(bd2015$med_target, na.rm = TRUE)),
                         "prev_dx_cups_atc" = c(round(sum(bd2011$medicamento_target, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                                round(sum(bd20122_1$dx_target, na.rm = TRUE)/nrow(bd20122_1), digits = 4),
                                                round(sum(bd2014$med_target, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                                round(sum(bd2015$med_target, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                         "casos_total" = c(sum(bd2011$casos, na.rm = TRUE), sum(bd20122_1$casos, na.rm = TRUE),
                                           sum(bd2014$casos, na.rm = TRUE), sum(bd2015$casos, na.rm = TRUE)),
                         "prev_total" = c(round(sum(bd2011$casos, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                          round(sum(bd20122_1$casos, na.rm = TRUE)/nrow(bd20122_1), digits = 4),
                                          round(sum(bd2014$casos, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                          round(sum(bd2015$casos, na.rm = TRUE)/nrow(bd2015), digits = 4)
                         ))

resultadosdm <- resultadosdm %>% mutate(prev_literatura = evidencia$prevalencia)








#ERC
#CÓDIGOS
cods <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "ERC")
names(cods) <- epitrix::clean_labels(names(cods))
cods <- cods %>% mutate(name_cods = epitrix::clean_labels(codigo_significado))
cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
cods_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo)
names_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo_significado)
names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)

#BASE DE DATOS
#bd2011
bd2011 <- read_excel("dat/bd_2011.xlsx")
names(bd2011) <- epitrix::clean_labels(names(bd2011))
bd2011 <- bd2011 %>% mutate(name_drug = epitrix::clean_labels(medicamentodesc))
#bd2011 - cie10
bd2011$enf_cie10 <- NA
bd2011$enf_cie10[bd2011$diagnosticocd %in% cods_cie10$codigo] <- 1
#bd2011 - cups
bd2011$enf_cups <- NA
bd2011$enf_cups[bd2011$procedimientocd %in% cods_cups$codigo] <- 1
#bd2011 - meds
vector_raices_medic <- unique(cods$root)
bd2011$medicamento_target <- FALSE
for (i in seq_along (vector_raices_medic)) {
  bd2011$medicamento_target <- if_else (bd2011$medicamento_target == TRUE, bd2011$medicamento_target,
                                        str_detect(string = bd2011$name_drug, pattern = vector_raices_medic[i]))
}
#bd2011 - cie10, cups, meds
bd2011 <- mutate(bd2011,
                 casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | medicamento_target == "TRUE",1,0))


#bd2012
bd2012 <- read_excel("dat/bd_2012.xlsx")
names(bd2012) <- epitrix::clean_labels(names(bd2012))
bd20122_1 <- strsplit(bd2012$diagnosticoprincipal, "-")
bd20122_1 <- data.frame(bd2012, do.call(rbind, bd20122_1))
names(bd20122_1)
bd20122_1 <- bd20122_1 %>% rename(codcie10 = X1, defcie10 = X2)
bd20122_1 <- bd20122_1 %>% mutate (name_dx = epitrix::clean_labels(defcie10))
bd20122_1$codcie10 <- gsub(" ", "", bd20122_1$codcie10)
#bd2012 - cie10
bd20122_1$enf_cie10 <- NA
bd20122_1$enf_cie10[bd20122_1$codcie10 %in% cods_cie10$codigo] <- 1
#bd2012 - cie10, definición cie10
vector_raices_dx <- unique(cods$root)
bd20122_1$dx_target <- FALSE
for (i in seq_along(vector_raices_dx)) {
  bd20122_1$dx_target <- if_else (bd20122_1$dx_target == TRUE,  bd20122_1$dx_target,
                                  str_detect(string =   bd20122_1$name_dx, pattern = vector_raices_dx[i]))
}
bd20122_1<- mutate(bd20122_1,
                   casos = ifelse(enf_cie10 == "1"| dx_target == "TRUE",1,0))



#bd2014
bd2014 <- read_excel("dat/bd_2014.xlsx")
names(bd2014) <- epitrix::clean_labels(names(bd2014))
bd2014 <- bd2014 %>% mutate (name_drug = epitrix::clean_labels(nombre))
#bd2014 - cie10
bd2014$enf_cie10 <- NA
bd2014$enf_cie10[bd2014$diagnosticocd %in% cods_cie10$codigo] <- 1
#bd2014 - cups
bd2014$enf_cups <- NA
bd2014$enf_cups[bd2014$procedimientocd %in% cods_cups$codigo] <- 1
#bd2014 - meds
bd2014$med_target <- FALSE
for (i in seq_along(vector_raices_medic)) {
  bd2014$med_target <- if_else (bd2014$med_target == TRUE, bd2014$med_target,
                                str_detect(string = bd2014$name_drug, pattern = vector_raices_medic[i]))
}
#bd2014 - cie10, cups, meds
bd2014<- mutate(bd2014,
                casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | med_target == "TRUE",1,0))


#bd2015
bd2015 <- read_excel("dat/bd_2015.xlsx")
names(bd2015) <- epitrix::clean_labels(names(bd2015))
bd2015 <- bd2015 %>% mutate (name_drug = epitrix::clean_labels(medicamento))
#bd2015 - cie10
bd2015$enf_cie10 <- NA
bd2015$enf_cie10[bd2015$diagnosticocd %in% cods_cie10$codigo] <- 1
#bd2015 - meds
bd2015$med_target <- FALSE
for (i in seq_along(vector_raices_medic)) {
  bd2015$med_target <- if_else (bd2015$med_target == TRUE, bd2015$med_target,
                                str_detect(string = bd2015$name_drug, pattern = vector_raices_medic[i]))
}
#bd2015 - cie10, meds
bd2015<- mutate(bd2015,
                casos = ifelse(enf_cie10 == "1"| med_target == "TRUE",1,0))

#EVIDENCIA
evidencia <- read_excel("dat/validacion.xlsx")
evidencia <- evidencia %>% filter(enfermedad == "ERC")

#Creación tabla resultados
resultadoserc <- data.frame("enf" = "ERC",
                         "bd" = c("bd2011","bd2012","bd2014","bd2015"),
                         "muestra" = c(nrow(bd2011),nrow(bd20122_1),nrow(bd2014),nrow(bd2015)),
                         "casos_CIE10" = c(sum(bd2011$enf_cie10, na.rm = TRUE), sum(bd20122_1$enf_cie10, na.rm = TRUE),
                                           sum(bd2014$enf_cie10, na.rm = TRUE), sum(bd2015$enf_cie10, na.rm = TRUE)),
                         "prev_CIE10" = c(round(sum(bd2011$enf_cie10, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                          round(sum(bd20122_1$enf_cie10, na.rm = TRUE)/nrow(bd20122_1), digits = 4),
                                          round(sum(bd2014$enf_cie10, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                          round(sum(bd2015$enf_cie10, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                         "casos_CUPS" = c(sum(bd2011$enf_cups, na.rm = TRUE),"0",
                                          sum(bd2014$enf_cups, na.rm = TRUE),"0"),
                         "prev_CUPS" = c(round(sum(bd2011$enf_cups, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                         "0",
                                         round(sum(bd2014$enf_cups, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                         "0"),
                         "casos_dx_cups_atc" = c(sum(bd2011$medicamento_target, na.rm = TRUE), sum(bd20122_1$dx_target, na.rm = TRUE),
                                                 sum(bd2014$med_target, na.rm = TRUE), sum(bd2015$med_target, na.rm = TRUE)),
                         "prev_dx_cups_atc" = c(round(sum(bd2011$medicamento_target, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                                round(sum(bd20122_1$dx_target, na.rm = TRUE)/nrow(bd20122_1), digits = 4),
                                                round(sum(bd2014$med_target, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                                round(sum(bd2015$med_target, na.rm = TRUE)/nrow(bd2015), digits = 4)),
                         "casos_total" = c(sum(bd2011$casos, na.rm = TRUE), sum(bd20122_1$casos, na.rm = TRUE),
                                           sum(bd2014$casos, na.rm = TRUE), sum(bd2015$casos, na.rm = TRUE)),
                         "prev_total" = c(round(sum(bd2011$casos, na.rm = TRUE)/nrow(bd2011), digits = 4),
                                          round(sum(bd20122_1$casos, na.rm = TRUE)/nrow(bd20122_1), digits = 4),
                                          round(sum(bd2014$casos, na.rm = TRUE)/nrow(bd2014), digits = 4),
                                          round(sum(bd2015$casos, na.rm = TRUE)/nrow(bd2015), digits = 4)
                         ))

resultadoserc <- resultadoserc %>% mutate(prev_literatura = evidencia$prevalencia)






consolidado <- rbind (resultadosdm, resultadoserc)


