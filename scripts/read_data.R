

# Other packages to check

# https://cran.r-project.org/src/contrib/Archive/charlson/
# https://github.com/ellessenne/comorbidity/
# https://rpubs.com/nathanh36/jjcdm

rmLlist=ls()
library(tidyverse)
library(readxl)

cods <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM")
names(cods) <- epitrix::clean_labels(names(cods))
cods <- cods %>% mutate (name_drug = epitrix::clean_labels(codigo_significado))

datp <- read_excel("dat/Base UPC Suficiencia 2011 T1.xlsx")
names(datp) <- epitrix::clean_labels(names(datp))
datp <- datp %>% mutate (name_drug = epitrix::clean_labels(medicamentodesc))


cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
cods_cups <- cods %>% filter(tipo == "CUPS")  %>% select(codigo)
names_cups <- cods %>% filter(tipo == "CUPS")  %>% select(codigo_significado)
names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)


datp$enf_cie10 <- NA
datp$enf_cie10[datp$diagnosticocd %in% cods_cie10$codigo] <- 1
datp$enf_cups <- NA
datp$enf_cups[datp$diagnosticocd %in% cods_cups$codigo] <- 1
datp$enf_cups[datp$medicamentodesc %in% names_cups] <- 1


library(stringr)

vector_raices_medic <- unique(cods$root) # aquí leer las raices
datp$medicamento_target <- FALSE
for (i in seq_along (vector_raices_medic)) {
  datp$medicamento_target <- if_else (datp$medicamento_target == TRUE, datp$medicamento_target,
                                      str_detect(string = datp$name_drug , pattern = vector_raices_medic[i]))
}

sum(datp$medicamento_target, na.rm = TRUE)


apply(array, margin, ...)

#Prueba
#PRUEBA
