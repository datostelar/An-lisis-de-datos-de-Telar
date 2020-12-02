# CREA UN ARCHIVO RDATA CON LOS REPORTES DESCARGADOS DEL ADMIN DE TELAR
# CREA UN ARCHIVO RDATA CON LOS CATALOGOS DESCARGADOS DEL ADMIN DE TELAR

lapply(c("openxlsx", "dplyr"), library, character.only = TRUE)

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))

# REPORTES =====================================================================

x <- "reporte_sistematizacion_"

reportes <- list(
  cult_acti = read.xlsx(paste0(x, "actividad_cultural_actividades.xlsx")),
  cult_area = read.xlsx(paste0(x, "actividad_cultural_area.xlsx")),
  cult_disc = read.xlsx(paste0(x, "actividad_cultural_disciplinas.xlsx")),
  cult_espe = read.xlsx(paste0(x, "actividad_cultural_especialidades.xlsx")),
  cult_pobl = read.xlsx(paste0(x, "actividad_cultural_poblaciones.xlsx")),
  animacion = read.xlsx(paste0(x, "cultura.xlsx")),
  basicos = read.xlsx(paste0(x, "datosbasicos.xlsx")),
  formacion = read.xlsx(paste0(x, "formacion.xlsx")),
  motivo = read.xlsx(paste0(x, "motivo.xlsx")),
  movilidad = read.xlsx(paste0(x, "movilidad.xlsx")),
  pol_cult = read.xlsx(paste0(x, "politicas_culturales.xlsx")),
  sociodemo = read.xlsx(paste0(x, "sociodemografica.xlsx"))
)

rm(x)

# REPORTES GENERALES ===========================================================

x <- "reporte_general_"

reportes_gen <- list(
  cult_acti = read.xlsx(paste0(x, "actividad_cultural_actividades.xlsx")),
  cult_area = read.xlsx(paste0(x, "actividad_cultural_area.xlsx")),
  cult_disc = read.xlsx(paste0(x, "actividad_cultural_disciplinas.xlsx")),
  cult_espe = read.xlsx(paste0(x, "actividad_cultural_especialidades.xlsx")),
  cult_pobl = read.xlsx(paste0(x, "actividad_cultural_poblaciones.xlsx")),
  animacion = read.xlsx(paste0(x, "cultura.xlsx")),
  basicos = read.xlsx(paste0(x, "datosbasicos.xlsx")),
  formacion = read.xlsx(paste0(x, "formacion.xlsx")),
  motivo = read.xlsx(paste0(x, "motivo.xlsx")),
  movilidad = read.xlsx(paste0(x, "movilidad.xlsx")),
  pol_cult = read.xlsx(paste0(x, "politicas_culturales.xlsx")),
  sociodemo = read.xlsx(paste0(x, "sociodemografica.xlsx"))
)

rm(x)

# CATALOGOS ====================================================================

catalogos <- list(
  cat_acti = read.xlsx("cat_actividades.xlsx"),
  cat_area = read.xlsx("cat_areas.xlsx"),
  cat_cult_activ_comp = read.xlsx("cat_cultura_actividades_comprobables.xlsx"),
  cat_cult_linea = read.xlsx("cat_cultura_linea_accion.xlsx"),
  cat_cult_pob = read.xlsx("cat_cultura_poblacion.xlsx"),
  cat_disc = read.xlsx("cat_disciplinas.xlsx"),
  cat_esp = read.xlsx("cat_especialidades.xlsx"),
  cat_edos = read.xlsx("cat_estados.xlsx"),
  cat_gen = read.xlsx("cat_generos.xlsx"),
  cat_mun = read.xlsx("cat_municipios.xlsx")
)

# EXPORTACION ==================================================================

save(reportes, file = "reportes_17_11_2020.RData")
save(reportes_gen, file = "reportes_gen_21_10_2020.RData")
save(catalogos, file = "catalogos.RData")
