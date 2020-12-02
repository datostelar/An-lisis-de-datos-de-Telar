# MAPA DE CALOR DEL NUMERO DE AGENTES CULTURALES REGISTRADOS A NIVEL ESTATAL

# PAQUETES =====================================================================

lapply(
  c(
    "sf",
    "proj4",
    "leaflet",
    "dplyr",
    "viridis",
    "leaflet.extras"
  ),
  library,
  character.only = TRUE
)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"),"Desktop"))
load("reportes_19_08_2020.RData")
load("INEGI//cat_ent_mun.RData")
load("colores_oficiales.RData")
load("catalogos.RData")

ruta_shps <- paste0(
  file.path(Sys.getenv("USERPROFILE"), "Desktop"), 
  "/INEGI/Shapes/nacional/conjunto_de_datos/"
)

# Estados
s_ent <- st_read(paste0(ruta_shps, "00ent.shp"))
s_ent <- st_transform(s_ent, "+proj=longlat +ellps=WGS84 +datum=WGS84")
s_ent$CVE_ENT <- as.numeric(s_ent$CVE_ENT)

# DATOS ========================================================================

datos <- reportes$basicos %>%
  filter(cve_ent_res %in% c(2:33), gen %in% c("Hombre", "Mujer")) %>%
  left_join(catalogos$cat_edos, by = c("cve_ent_res" = "id_ent")) %>%
  select(id_inegi, entidad) %>%
  group_by(id_inegi, entidad) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  mutate(id_inegi = as.numeric(id_inegi)) %>%
  left_join(select(s_ent, CVE_ENT, geometry), by = c("id_inegi" = "CVE_ENT"))

# MAPA =========================================================================

# Crea funcion para obtener paleta de colores
pal <- colorNumeric(
  palette = "plasma", 
  domain = sqrt(datos$total)
)

# Crea el mapa
m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  # Agrega un mapa base. Usar si se quiere tener un "fondo"
  # addTiles() %>%
  addPolygons(
    data = datos$geometry,         # Informacion geografica
    weight = 2,                    # Grosor de la linea de frontera
    color = pal(sqrt(datos$total)),      # Color de la linea de frontera
    fillColor = pal(sqrt(datos$total)),  # Color de relleno del poligono
    opacity = 1,                   # Opacidad de la linea de frontera
    fillOpacity = 1                # Opacidad del color de relleno
  ) %>%
  # Agrega leyenda del numero de agentes
  addLegend(
    "bottomleft",                 # Posicion de la leyenda
    pal = pal,                    # Escala de color de la leyenda
    values = rev(sqrt(datos$total)),         # Valores a ser representados
    title = "Número de agentes",  # Titulo de la leyenda
    labFormat = labelFormat(
      prefix = "",
      suffix = "",
      between = ", ",
      transform = function(x) x ^ 2
    ),
    opacity = 1
  ) %>%
  setMapWidgetStyle(list(background = "white"))

m  # Visualizar mapa en Viewer

# EXPORTACION ==================================================================

ent2 <- "Tamaulipas" # Workaround para los acentos ¯\_(ツ)_/¯

# Workaround para agregar titulo al mapa png
pic <- image_read(paste0(ent2, ".png"))  # Lee el mapa
# Agrega titulo
pic <- image_annotate(
  pic, 
  paste0("Ubicación geográfica de agentes culturales en ", ent),
  font = "sans",
  gravity = "north",
  size = 30
)
# Guarda como un segundo mapa
image_write(pic, path = paste0(ent, ".png"), format = "png")

