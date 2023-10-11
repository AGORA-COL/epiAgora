

# Other packages to check

# https://cran.r-project.org/src/contrib/Archive/charlson/
# https://github.com/ellessenne/comorbidity/
# https://rpubs.com/nathanh36/jjcdm

rmLlist=ls()
library(tidyverse)
library(readxl)

cods <- read_excel("dat/ALGORITMOS_CLINICOS_NEW_ZC.xlsx", sheet = "DM")
cods <- cods %>% mutate (name_drug = epitrix::clean_labels(CODIGO_SIGNIFICADO))
datp <- read_excel("dat/Base UPC Suficiencia 2011 T1.xlsx")
datp <- datp %>% mutate (name_drug = epitrix::clean_labels(MedicamentoDesc))


cods_cie10 <- cods %>% filter(TIPO == "CIE-10") %>% select(CODIGO)
cods_cups <- cods %>% filter(TIPO == "CUPS")  %>% select(CODIGO)
names_cups <- cods %>% filter(TIPO == "CUPS")  %>% select(CODIGO_SIGNIFICADO)


datp$enf_cie10 <- NA
datp$enf_cie10[datp$DiagnosticoCD %in% cods_cie10$CODIGO] <- 1
datp$enf_cups <- NA
datp$enf_cups[datp$DiagnosticoCD %in% cods_cups$CODIGO] <- 1
datp$enf_cups[datp$MedicamentoDesc %in% names_cups] <- 1


library(stringr)
sum(str_detect(string = datp$name_drug , pattern = "insulin"), na.rm = TRUE)

