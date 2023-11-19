
# Rutina para verificación de datos de ERC
# Fecha: NOV-2023
# Proyecto AGORA


rm(list=ls())
library(tidyverse)
library(readxl)

#cod
cods <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "ERC")
names(cods) <- epitrix::clean_labels(names(cods))
cods <- cods %>% mutate (name_drug = epitrix::clean_labels(codigo_significado))

#cie10, cups, atc
cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
cods_cups <- cods %>% filter(tipo == "CUPS")  %>% select(codigo)
names_cups <- cods %>% filter(tipo == "CUPS")  %>% select(codigo_significado)
names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)
cod_atc <- cods  %>% filter(tipo == "ATC") %>% select(codigo)#no están en RIPS
names_atc <- cods %>% filter(tipo == "ATC") %>% select(codigo_significado)
names_atc$codigo_significado <- epitrix::clean_labels(names_atc$codigo_significado)

#bd
datp <- read_excel("dat/bd_2011.xlsx")
names(datp) <- epitrix::clean_labels(names(datp))
datp <- datp %>% mutate (name_drug = epitrix::clean_labels(medicamentodesc))
#bd2
datp2 <- read_excel("dat/bd_2012.xlsx")
names(datp2) <- epitrix::clean_labels(names(datp2))
datp2_1 <- strsplit(datp2$diagnosticoprincipal, "-")
datp2_1 <- data.frame(datp2, do.call(rbind, datp2_1))
names(datp2_1)
datp2_1 <- datp2_1 %>% rename(codcie10 = X1, defcie10 = X2)
datp2_1 <- datp2_1 %>% mutate (name_dx = epitrix::clean_labels(defcie10))
datp2_1$codcie10 <- gsub(" ", "", datp2_1$codcie10)
#bd3
datp3 <- read_excel("dat/bd_2014.xlsx")
names(datp3) <- epitrix::clean_labels(names(datp3))
datp3 <- datp3 %>% mutate (name_drug = epitrix::clean_labels(nombre))
#bd4
datp4 <- read_excel("dat/bd_2015.xlsx")
names(datp4) <- epitrix::clean_labels(names(datp4))
datp4 <- datp4 %>% mutate (name_drug = epitrix::clean_labels(medicamento))

#APLICACIÓN DM
#aplicación bd
datp$enf_cie10 <- NA
datp$enf_cie10[datp$diagnosticocd %in% cods_cie10$codigo] <- 1

datp$enf_cups <- NA
datp$enf_cups[datp$diagnosticocd %in% cods_cups$codigo] <- 1

datp$enf_med <- NA
datp$enf_med[datp$medicamentodesc %in% names_cups] <- 1

sum(datp$enf_cie10, na.rm = TRUE)/nrow(datp)
sum(datp$enf_cups, na.rm = TRUE)/nrow(datp)

library(stringr)

vector_raices_medic <- unique(cods$root) # aquí leer las raices
datp$medicamento_target <- FALSE
for (i in seq_along (vector_raices_medic)) {
  datp$medicamento_target <- if_else (datp$medicamento_target == TRUE, datp$medicamento_target,
                                      str_detect(string = datp$name_drug , pattern = vector_raices_medic[i]))
}

sum(datp$medicamento_target, na.rm = TRUE)/nrow(datp)

#apply(array, margin, ...)

#aplicación bd2 (SIN CUPS, NI ATC)
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

datp3$enf_cups <- NA
datp3$enf_cups[datp3$procedimientocd %in% cods_cups$codigo] <- 1
datp3$enf_cups[datp3$procedimientocd %in% names_cups] <- 1
sum(datp3$enf_cups, na.rm = TRUE)/nrow(datp3)

datp3$med_target <- FALSE
for (i in seq_along(vector_raices_medic)) {
  datp3$med_target <- if_else (datp3$med_target == TRUE, datp3$med_target,
                               str_detect(string = datp3$name_drug, pattern = vector_raices_medic[i]))
}
sum(datp3$med_target, na.rm = TRUE)/nrow(datp3)

#aplicación bd4 (SIN CUPS)
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
#POR DX O CUPS O ATC(MED)
sum(datp$enf_cups, na.rm = TRUE)/nrow(datp)
sum(datp3$med_target, na.rm = TRUE)/nrow(datp3)
sum(datp4$med_target, na.rm = TRUE)/nrow(datp4)
#Restricción
datp <- mutate(datp,
               casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | medicamento_target == "TRUE",1,0))

datp2_1<- mutate(datp2_1,
                 casos = ifelse(enf_cie10 == "1"| dx_target == "TRUE",1,0))
datp3<- mutate(datp3,
               casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | med_target == "TRUE",1,0))
datp4<- mutate(datp4,
               casos = ifelse(enf_cie10 == "1"| med_target == "TRUE",1,0))



resultados <- data.frame("bd" = c("bd_2011","bd_2012","bd_2014","bd_2015"),
                         "muestra" = c(nrow(datp),nrow(datp2_1),nrow(datp3),nrow(datp4)),
                         "casos_CIE10" = c(sum(datp$enf_cie10, na.rm = TRUE),sum(datp2_1$enf_cie10, na.rm = TRUE),
                                           sum(datp3$enf_cie10, na.rm = TRUE),sum(datp4$enf_cie10, na.rm = TRUE)),
                         "prev_CIE10" = c(round(sum(datp$enf_cie10, na.rm = TRUE)/nrow(datp), digits = 4),
                                          round(sum(datp2_1$enf_cie10, na.rm = TRUE)/nrow(datp2_1), digits = 4),
                                          round(sum(datp3$enf_cie10, na.rm = TRUE)/nrow(datp3), digits = 4),
                                          round(sum(datp4$enf_cie10, na.rm = TRUE)/nrow(datp4), digits = 4)),
                         "casos_CUPS" = c(sum(datp$enf_cups, na.rm = TRUE),"0",
                                          sum(datp3$enf_cups, na.rm = TRUE),"0"),
                         "prev_CUPS" = c(round(sum(datp$enf_cups, na.rm = TRUE)/nrow(datp), digits = 4),
                                         "0",
                                         round(sum(datp3$enf_cups, na.rm = TRUE)/nrow(datp3), digits = 4),
                                         "0"),
                         "casos_dx_cups_atc" = c(sum(datp$medicamento_target, na.rm = TRUE),sum(datp2_1$dx_target, na.rm = TRUE),
                                                 sum(datp3$med_target, na.rm = TRUE),sum(datp4$med_target, na.rm = TRUE)),
                         "prev_dx_cups_atc" = c(round(sum(datp$medicamento_target, na.rm = TRUE)/nrow(datp2_1), digits = 4),
                                                round(sum(datp2_1$dx_target, na.rm = TRUE)/nrow(datp2_1), digits = 4),
                                                round(sum(datp3$med_target, na.rm = TRUE)/nrow(datp3), digits = 4),
                                                round(sum(datp4$med_target, na.rm = TRUE)/nrow(datp4), digits = 4)),
                         "casos_total" = c(sum(datp$casos, na.rm = TRUE), sum(datp2_1$casos, na.rm = TRUE),
                                           sum(datp3$casos, na.rm = TRUE), sum(datp4$casos, na.rm = TRUE)),
                         "prev_total" = c(round(sum(datp$casos, na.rm = TRUE)/nrow(datp), digits = 4),
                                          round(sum(datp2_1$casos, na.rm = TRUE)/nrow(datp2_1), digits = 4),
                                          round(sum(datp3$casos, na.rm = TRUE)/nrow(datp3), digits = 4),
                                          round(sum(datp4$casos, na.rm = TRUE)/nrow(datp4), digits = 4)
                         ))

ref_literatura <- read_excel("dat/validacion.xlsx")
ref_literatura <- ref_literatura %>% filter(enfermedad == "ERC")
resultados <- resultados %>% mutate(prev_literatura = ref_literatura$prevalencia)

library(writexl)
write_xlsx(resultados,'resultadosERC.xlsx')

