muns <- c(
  "Apodaca",
  "Atlacomulco",
  "Bahía de Banderas",
  "Acuña",
  "El Marqués",
  "Guadalajara",
  "Jesús María",
  "Lagos de Moreno",
  "Mazatlán",
  "Monterrey",
  "Ocotlán",
  "Pachuca de Soto",
  "Reynosa",
  "Tapachula",
  "Tepatitlán de Morelos",
  "Puerto Vallarta"
)

datos <- reportes_gen$basicos %>%
  select(
    nombre,
    estado_residencia,
    municipio_residencia,
    correo_electronico,
    telefono_celular_contacto,
    telefono_fijo_contacto,
    fecha_nacimiento
  ) %>%
  filter(
    estado_residencia == "Ciudad de México" | municipio_residencia %in% muns |
      (estado_residencia == "Nuevo León" & municipio_residencia %in% c(
          "Guadalupe", "Juárez"
        )) |
      estado_residencia == "Sonora" & municipio_residencia == "Nogales"
  )

# Obtiene edad
datos_s <- filter(datos, grepl("/", datos$fecha_nacimiento, fixed = TRUE)) %>%
  separate(fecha_nacimiento, c("A", "B", "C"))
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
datos$year[grepl("/", datos$fecha_nacimiento, fixed = TRUE)] <- datos_s$year
datos$year[!grepl("/", datos$fecha_nacimiento, fixed = TRUE)] <- format(
  as.Date(
    datos$fecha_nacimiento[!grepl("/", datos$fecha_nacimiento, fixed = TRUE)],
    format = "%Y-%m-%d"
  ),
  "%Y"
)
rm(datos_s)
datos$edad <- 2020 - as.numeric(datos$year)

datos <- filter(datos, edad %in% c(18:29)) %>%
  select(-fecha_nacimiento, -year)
