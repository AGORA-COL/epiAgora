

# Rutina para verificación de datos de DM e IRC
# Fecha: XXX
# Proyecto AGORA

source("fun/f_check.R")

dat <- read_excel()
cods <- read_excel()

table_check_DM <- check_enf(dat, cods, enf = "DM")

consolidado <- rbind (table_check_DM, table_check_IRC)
