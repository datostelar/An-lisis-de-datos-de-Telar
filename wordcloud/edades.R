# EDADES

# PAQUETES =====================================================================

lapply(c("dplyr", "tidyr"), library, character.only = TRUE)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_19_08_2020.RData")
load("colores_oficiales.RData")

# DATOS ========================================================================

# SE OBTIENE LA EDAD A PARTIR DE LA FECHA DE NACIMIENTO
datos <- reportes$basicos %>%
  select(gen, fec_nac) %>%
  filter(gen %in% c("Hombre", "Mujer") & nchar(fec_nac) == 10)

datos_s <- filter(datos, grepl("/", datos$fec_nac, fixed = TRUE)) %>%
  separate(fec_nac, c("A", "B", "C"))
datos_s$year <- ifelse(
  nchar(datos_s$A) == 4,
  datos_s$A,
  ifelse(
    nchar(datos_s$B) == 4,
    datos_s$B,
    datos_s$C
  )
)

datos$year <- 0
datos$year[grepl("/", datos$fec_nac, fixed = TRUE)] <- datos_s$year
datos$year[!grepl("/", datos$fec_nac, fixed = TRUE)] <- format(
  as.Date(
    datos$fec_nac[!grepl("/", datos$fec_nac, fixed = TRUE)],
    format = "%Y-%m-%d"
  ),
  "%Y"
)
rm(datos_s)

# SEGMENTACION POR GRUPOS ETARIOS
datos$edad <- 2020 - as.numeric(datos$year)
datos <- filter(datos, edad %in% c(60:100))
datos <- datos %>%
  select(edad) %>%
  mutate(edad = as.character(edad)) %>%
  group_by(edad) %>%
  summarise(total = n()) %>%
  rename(word = edad, freq = total)