library(arrow)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(comorbidity)
library(tidyr)



#Muestra Bogotá: 35'351.724 (2018 a 2022)
#sample_df_bogota_parquet <- read_parquet("dat/sample_df_bogota_parquet") %>% mutate(ID_uniq = row_number())

#Subuestra Bogotá: 1,767,586 (2018 a 2022)
subsample_df_bogota <- readRDS("dat/subsample_parquet") %>% mutate(ID_uniq = row_number())



#--------------------------------
# USANDO PAQUETE COMORBIDITY
#--------------------------------

#Se crea dataset con pacientes identificados según sus comorbilidades:
comorb_pack<-comorbidity(x = subsample_df_bogota, id = "personaid", code = "dxprincipal", map = "charlson_icd10_quan", assign0 = TRUE)
View(comorb_pack) #1,349,824 sin duplicados

# Se calculan las prevalencias:
p_diab <- mean(comorb_pack$diab == 1, na.rm = TRUE)
p_diabnocompli <- mean(comorb_pack$diabwc == 1, na.rm = TRUE)
p_diabt <- mean(comorb_pack$diab == 1 | comorb_pack$diabwc == 1, na.rm = TRUE)
p_renal <- mean(comorb_pack$rend == 1, na.rm = TRUE)


prevalencias_comorbidity <- comorb_pack %>%
  summarise(
    dm_sin_complicacion = sum(diab == 1) / n(),
    dm_con_complicacion = sum(diabwc == 1) / n(),
    dm_general = sum(diab == 1 | diabwc == 1) / n(),
    enfermedad_renal = sum(rend == 1) / n()
  ) %>%
  melt(id.vars = NULL, value.name = "Prevalencia", variable.name = "Patología") 





# Se muestran las prevalencias:
Prevalencias <- data.frame(
  Variable = c("Diabetes sin complicaciones", "Diabetes con complicaciones", "Diabetes Total", "Enfermedad Renal"),
  Prevalencia = c(p_diab, p_diabnocompli, p_diabt, p_renal)
)



#--------------------------------
# USANDO LISTA DE CÓDIGOS CIE-10
#--------------------------------

cie10_dm <- readxl::read_excel("dat/cupscodes.xlsx", sheet = "cie-dm") 
subsample_bta <- subsample_df_bogota %>%
                 select(ID_uniq, personaid, dxprincipal, codigoprocedimiento, fechaid) %>% #Se seleccionan solo las variables a usar para que quede menos pesada (mas el id único por si se requiere traer alguna otra variable después)
                 mutate(fechaid = as.Date(fechaid, format = "%Y%m%d"),
                        dm = ifelse(dxprincipal %in% cie10_dm$codigo, 1, 0),
                        dm_t1 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t1"], 1, 0),
                        dm_t2 = ifelse(dxprincipal %in% cie10_dm$codigo[cie10_dm$tipo == "t2"], 1, 0))  #se  identifica aquellos que estan en los CIE10 de diabetes


# Contar cuántos dx hay de diabetes, contando una vez cada "personaid"
prevalencias_cie10T <- subsample_bta %>%
  summarise(
    Prevalencia_dm = sum(dm == 1) / n(),
    Prevalencia_dm_t1 = sum(dm_t1 == 1) / n(),
    Prevalencia_dm_t2 = sum(dm_t2 == 1) / n()
  ) %>%
  melt(id.vars = NULL, value.name = "Prevalencia", variable.name = "Tipo")


#Por año
prevalencias_cie10 <- subsample_bta %>%
  mutate(Year = format(fechaid, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    dm = sum(dm == 1) / n(),
    dm_tipo1 = sum(dm_t1 == 1) / n(),
    dm_tipo2 = sum(dm_t2 == 1) / n()
  ) %>%
  melt(id.vars = "Year", value.name = "Prevalencia", variable.name = "Patología") %>% 
  spread(key = Year, value = Prevalencia)




