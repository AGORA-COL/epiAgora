
# Rutina para verificación de datos de HTA
# Fecha: XXX
# Proyecto AGORA


rm(list=ls())
library(tidyverse)
library(readxl)

#cod
cods <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "HTA")
names(cods) <- epitrix::clean_labels(names(cods))
cods <- cods %>% mutate (name_drug = epitrix::clean_labels(codigo_significado))

#cie10, cups, atc
cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
cods_cups <- cods %>% filter(tipo == "CUPS")  %>% select(codigo)

#bd
View(datp)
#bd2
View(datp2_1)
#bd3
View(datp3)
#bd4
View(datp4)

#APLICACIÓN HTA
#aplicación bd
datp$enf_cie10 <- NA
datp$enf_cie10[datp$diagnosticocd %in% cods_cie10$codigo] <- 1
datp$enf_cups <- NA
datp$enf_cups[datp$diagnosticocd %in% cods_cups$codigo] <- 1
datp$enf_cups[datp$medicamentodesc %in% names_cups] <- 1
sum(datp$enf_cie10, na.rm = TRUE)/nrow(datp)

vector_raices_medic <- unique(cods$root) # aquí leer las raices
datp$medicamento_target <- FALSE
for (i in seq_along (vector_raices_medic)) {
  datp$medicamento_target <- if_else (datp$medicamento_target == TRUE, datp$medicamento_target,
                                      str_detect(string = datp$name_drug , pattern = vector_raices_medic[i]))
}

sum(datp$medicamento_target, na.rm = TRUE)/nrow(datp)
datp %>% filter(enf_cie10 == "1", medicamento_target == "TRUE")
#ensayo, no final
prueba <- datp %>% filter(diagnosticocd%in%cods_cie10$codigo)
table(prueba$diagnosticocd)
prueba1 <- datp %>% filter(datp$medicamento_target)
table(prueba1$diagnosticocd)
cods_cie10$codigo

#aplicación bd2
datp2_1$enf_cie10 <- NA
datp2_1$enf_cie10[datp2_1$codcie10 %in% cods_cie10$codigo] <- 1
sum(datp2_1$enf_cie10, na.rm = TRUE)/nrow(datp2_1)

vector_raices_medic2 <- unique(cods$root)
datp2_1$dx_target <- FALSE
for (i in seq_along(vector_raices_medic2)) {
  datp2_1$dx_target <- if_else (datp2_1$dx_target == TRUE, datp2_1$dx_target,
                                str_detect(string = datp2_1$name_dx, pattern = vector_raices_medic2[i]))
}
sum(datp2_1$dx_target, na.rm = TRUE)/nrow(datp2_1)


#aplicación bd3
datp3$enf_cie10 <- NA
datp3$enf_cie10[datp3$diagnosticocd %in% cods_cie10$codigo] <- 1
sum(datp3$enf_cie10, na.rm = TRUE)/nrow(datp3)


datp3$med_target <- FALSE
for (i in seq_along(vector_raices_medic)) {
  datp3$med_target <- if_else (datp3$med_target == TRUE, datp3$med_target,
                               str_detect(string = datp3$name_drug, pattern = vector_raices_medic[i]))
}
sum(datp3$med_target, na.rm = TRUE)/nrow(datp3)

#aplicación bd4
datp4$enf_cie10 <- NA
datp4$enf_cie10[datp4$diagnosticocd %in% cods_cie10$codigo] <- 1
sum(datp4$enf_cie10, na.rm = TRUE)/nrow(datp4)


datp4$med_target <- FALSE
for (i in seq_along(vector_raices_medic)) {
  datp4$med_target <- if_else (datp4$med_target == TRUE, datp4$med_target,
                               str_detect(string = datp4$name_drug, pattern = vector_raices_medic[i]))
}
sum(datp4$med_target, na.rm = TRUE)/nrow(datp4)


#RESULTADOS
#POR RIPS - CIE10
sum(datp$enf_cie10, na.rm = TRUE)/nrow(datp)
sum(datp2_1$enf_cie10, na.rm = TRUE)/nrow(datp2_1)
sum(datp2_1$dx_target, na.rm = TRUE)/nrow(datp2_1)
sum(datp3$enf_cie10, na.rm = TRUE)/nrow(datp3)
sum(datp4$enf_cie10, na.rm = TRUE)/nrow(datp4)
#POR CUPS - ATC(MED)
sum(datp4$med_target, na.rm = TRUE)/nrow(datp4)
#Restricción?
datp %>% filter(enf_cie10 == "1", medicamento_target == "TRUE")
datp3 %>% filter(enf_cie10 == "1", med_target == "TRUE")
datp4 %>% filter(enf_cie10 == "1", med_target == "TRUE")

resultados <- matrix(c("bd_2011",
                       "bd_2012",
                       "bd_2014",
                       "bd_2015",
                       sum(datp$enf_cie10, na.rm = TRUE),
                       sum(datp2_1$enf_cie10, na.rm = TRUE),
                       sum(datp3$enf_cie10, na.rm = TRUE),
                       sum(datp4$enf_cie10, na.rm = TRUE),
                       nrow(datp),
                       nrow(datp2_1),
                       nrow(datp3),
                       nrow(datp4),
                       round(sum(datp$enf_cie10, na.rm = TRUE)/nrow(datp), digits = 4),
                       round(sum(datp2_1$enf_cie10, na.rm = TRUE)/nrow(datp2_1), digits = 4),
                       round(sum(datp3$enf_cie10, na.rm = TRUE)/nrow(datp3), digits = 4),
                       round(sum(datp4$enf_cie10, na.rm = TRUE)/nrow(datp4), digits = 4),
                       "0",
                       round(sum(datp2_1$dx_target, na.rm = TRUE)/nrow(datp2_1), digits = 4),
                       round(sum(datp3$med_target, na.rm = TRUE)/nrow(datp3), digits = 4),
                       round(sum(datp4$med_target, na.rm = TRUE)/nrow(datp4), digits = 4)),
                     nrow = 4)
colnames(resultados) <- c("BD", "Casos", "Muestra", "Prevalencia por CIE-10", "Prevalencia por CUPS - ATC")
