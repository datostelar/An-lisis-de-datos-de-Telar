# NUMERO Y PESO DE AGENTES CULTURALES POR AREA RESPECTO AL TOTAL REGISTRADO

# PAQUETES =====================================================================

lapply(c("dplyr"), library, character.only = TRUE)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_03_08_2020.RData")
load("catalogos.RData")

# DATOS ========================================================================

datos <- catalogos$cat_area
datos$total <- 0
datos$porcentaje <- 0
total <- length(unique(reportes$basicos$id))

for (i in 1:14) {
  datos[datos$id == i, 3] <- length(
    unique(filter(reportes$cult_area, id_area == i))$id
  )
  datos[datos$id == i, 4] <- datos$total[datos$id == i]/total
}

write.xlsx(datos, "datos.xlsx")
