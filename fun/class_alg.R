# Rutina para verificación de datos de DM e IRC
# Fecha: NOV-2023
# Proyecto AGORA

#clasificación
classification <- function(cods, bd) {
  names(cods) <- epitrix::clean_labels(names(cods))
  cods <- cods %>% mutate(name_cods = epitrix::clean_labels(codigo_significado))
  cods_cie10 <- cods %>% filter(tipo == "CIE-10") %>% select(codigo)
  cods_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo)
  names_cups <- cods %>% filter(tipo == "CUPS") %>% select(codigo_significado)
  names_cups$codigo_significado <- epitrix::clean_labels(names_cups$codigo_significado)

  bd$enf_cie10 <- NA
  bd$enf_cie10[bd$diagnosticocd %in% cods_cie10$codigo] <- 1
  bd$enf_cups <- NA
  bd$enf_cups[bd$procedimientocd %in% cods_cups$codigo] <- 1
  vector_raices <- unique(cods$root)
  bd$target <- FALSE
  for (i in seq_along (vector_raices)) {
    bd$target <- if_else (bd$target == TRUE, bd$target,
                          str_detect(string = bd$name, pattern = vector_raices[i]))
  }
  bd <- bd |> mutate(casos = ifelse(enf_cie10 == "1"| enf_cups == "1" | target == "TRUE",1,0))
  return(bd)
}

#Tabla de resultados
resultados <- function(bd, disease) {
  temp <- data.frame("enf" = disease,
                     "bd" = deparse(substitute(bd)),
                     "muestra" = c(nrow(bd)),
                     "casos_CIE10" = c(sum(bd$enf_cie10, na.rm = TRUE)),
                     "prev_CIE10" = c(round(sum(bd$enf_cie10, na.rm = TRUE)/nrow(bd), digits = 4)),
                     "casos_CUPS" = c(sum(bd$enf_cups, na.rm = TRUE)),
                     "prev_CUPS" = c(round(sum(bd$enf_cups, na.rm = TRUE)/nrow(bd), digits = 4)),
                     "casos_dx_cups_atc" = c(sum(bd$target, na.rm = TRUE)),
                     "prev_dx_cups_atc" = c(round(sum(bd$target, na.rm = TRUE)/nrow(bd), digits = 4)),
                     "casos_total" = c(sum(bd$casos, na.rm = TRUE)),

                     "prev_total" = c(round(sum(bd$casos, na.rm = TRUE)/nrow(bd), digits = 4)))
  return(temp)
}
