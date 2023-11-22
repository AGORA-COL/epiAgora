# Rutina para verificación de datos de DM e IRC
# Fecha: NOV-2023
# Proyecto AGORA

#Diabetes
dm <- function(cods, bd2011, bd20122_1, bd2014) {



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





  return(resultados)
  }


#Enfermedad renal crónica
erc <- function(file_path_cods, file_paths_bd, file_path_literatura) {
  library(tidyverse)
  library(readxl)
  library(purrr)
  library(writexl)
  library(dplyr)
  library(openxlsx)
  library(stringr)
  #códigos para ERC
  cods <- read_excel(file_path_cods, sheet = "ERC")
  names(cods) <- epitrix::clean_labels(names(cods))
  cods <- cods %>% mutate(name_cods = epitrix::clean_labels(codigo_significado))

  cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
  cods_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo)
  names_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo_significado)
  names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)
  cod_atc <- cods %>% filter(tipo == "ATC") %>% select(codigo)
  names_atc <- cods %>% filter(tipo == "ATC") %>% select(codigo_significado)
  names_atc$codigo_significado <- epitrix::clean_labels(names_atc$codigo_significado)

  #Bases de datos
  bd_files_path <- "C:/Users/danib/OneDrive/Documentos/quality-rips-surveillance/dat/bd"
  bd_files <- list.files(path = bd_files_path, pattern = ".xlsx", full.names = TRUE)

  read_excel_sheet_safe <- function(file) {
    result <- tryCatch(
      read_xlsx(file),
      error = function(e) NULL
    )
    return(result)
  }

  all_sheets <- bd_files %>%
    map(read_excel_sheet_safe)

  all_sheets <- compact(all_sheets)

  output_workbook <- createWorkbook()
  for (i in seq_along(all_sheets)) {
    addWorksheet(output_workbook, sheetName = paste0("bd201", i))
    writeData(output_workbook, sheet = i, all_sheets[[i]])
  }

  #output_excel_file <- "C:/Users/danib/OneDrive/Documentos/quality-rips-surveillance/dat/bd/bd.xlsx"
  #saveWorkbook(output_workbook, file = output_excel_file)
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  output_excel_file <- paste0("C:/Users/danib/OneDrive/Documentos/quality-rips-surveillance/dat/bd/bd.xlsx", timestamp, ".xlsx")
  saveWorkbook(output_workbook, file = output_excel_file)

  #bd2011
  bd2011 <- read_excel(file_paths_bd, sheet = "bd2011")
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
  bd2012 <- read_excel(file_paths_bd, sheet = "bd2012")
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
  bd2014 <- read_excel(file_paths_bd, sheet = "bd2013")
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
  bd2015 <- read_excel(file_paths_bd, sheet = "bd2014")
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
  #bd2011 - cie10, meds
  bd2015<- mutate(bd2015,
                  casos = ifelse(enf_cie10 == "1"| med_target == "TRUE",1,0))


  #Creación tabla resultados
  resultados <- data.frame("enf" = "ERC",
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
  #Evidencia
  ref_literatura <- read_excel(file_path_literatura)
  ref_literatura <- ref_literatura %>% filter(enfermedad == "ERC")
  resultados <- resultados %>% mutate(prev_literatura = ref_literatura$prevalencia)

  return(resultados)
}


############
class_algorythm_reuma <- function(dat) {


}



class_algorythm_hta <- function(dat) {


}

class_algorythm_hta <- function(dat) {


}
