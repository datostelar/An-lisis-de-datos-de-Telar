# PUEBLOS INDIGENAS

# PAQUETES =====================================================================

lapply(c("dplyr"), library, character.only = TRUE)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_19_08_2020.RData")

# DATOS ========================================================================

datos <- data.frame(table(reportes$basicos$grp_ind_cual)) %>%
  filter(Var1 != "Otro")
