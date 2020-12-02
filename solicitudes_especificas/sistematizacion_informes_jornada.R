# PAQUETES =====================================================================

lapply(
  c(
    "openxlsx",
    "ggplot2",
    "dplyr",
    "tidyr",
    "forcats",
    "extrafont",
    "scales",
    "modeest"
  ),
  library,
  character.only = TRUE
)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("colores_oficiales.RData")
jornadas <- read.xlsx("viables.xlsx")
load("catalogos.RData")
# load("reportes_14_10_2020.RData")
# load("INEGI\\cat_ent_mun.RData")

# CARGA DE SHAPES ==============================================================

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

# Shapes
# Carga de shapes de estados
s_ent <- st_read(
  paste0(
    file.path(Sys.getenv("USERPROFILE"), "Desktop"),
    "/INEGI/Shapes/nacional/conjunto_de_datos/",
    "00ent.shp"
  )
)
# Transforma informacion geografica
s_ent <- st_transform(s_ent, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# Año ==========================================================================

jornadas$Año <- factor(x = jornadas$Año)
datos <- group_by(jornadas, Año) %>%
  summarise(total = n())

image <- ggplot(datos) +
  aes(
    x = Año, 
    y = total, 
    fill = color_oficial[1], 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    position = position_dodge(width = 0.1),
    fontface = "bold",
    size = 5,
    color = "white",
    vjust = 2
  ) + 
  scale_fill_manual(values = color_oficial[c(2)]) +
  #scale_y_continuous(trans = "log") +  # Escala logaritmica
  labs(
    x = "Año", 
    y = "Número de jornadas",
    title = "Jornadas culturales: total por año",
    subtitle = "",
    fill = ""
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(angle = 90),  #Pone el texto en vertical
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_año.png"
  ), 
  plot = image, 
  width = 5, 
  height = 7
)

# Eje de cultura comunitaria ===================================================

datos <- jornadas %>%
  rename(var = Eje.de.Cultura.Comunitaria) %>%
  group_by(var) %>%
  summarise(total = n()) %>%
  drop_na()

image <- ggplot(datos) +
  aes(
    x = var, 
    y = total, 
    fill = color_oficial[1], 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    position = position_dodge(width = 0.1),
    fontface = "bold",
    size = 5,
    color = "white",
    vjust = 2
  ) + 
  scale_fill_manual(values = color_oficial[c(2)]) +
  #scale_y_continuous(trans = "log") +  # Escala logaritmica
  labs(
    x = "Eje de Cultura Comunitaria", 
    y = "Número de jornadas",
    title = "Jornadas culturales: total por eje de Cultura Comunitaria",
    subtitle = "",
    fill = ""
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(angle = 90),  #Pone el texto en vertical
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_eje_cc.png"
  ), 
  plot = image, 
  width = 6, 
  height = 7
)

# Entidad ======================================================================

datos <- jornadas %>%
  rename(var = Entidad) %>%
  group_by(var) %>%
  summarise(total = n()) %>%
  drop_na()

image <- ggplot(datos) +
  aes(
    x = fct_rev(fct_reorder(var, desc(total))), 
    y = total, 
    fill = color_oficial[1], 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2)]) +
  #scale_y_continuous(trans = "log") +  # Escala logaritmica
  labs(
    x = "Entidad", 
    y = "Número de jornadas",
    title = "Jornadas culturales: total por entidades",
    subtitle = "",
    fill = ""
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(angle = 90),  #Pone el texto en vertical
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_jornadas_entidad.png"
  ), 
  plot = image, 
  width = 7, 
  height = 7
)

# Mapa de calor

datos <- jornadas %>%
  rename(var = Entidad) %>%
  group_by(var) %>%
  summarise(total = n()) %>%
  drop_na() %>%
  ungroup()
datos$var[datos$var == "Estado de México"] <- "México"
datos <- datos %>%
  left_join(
    select(catalogos$cat_edos, id_inegi, entidad),
    by = c("var" = "entidad")
  )
datos$var[datos$var == "México"] <- "Estado de México"
datos$id_inegi <- as.numeric(datos$id_inegi)
datos <- datos %>%
  left_join(select(s_ent, CVE_ENT, geometry), by = c("id_inegi" = "CVE_ENT"))

# Crea funcion para obtener paleta de colores
pal <- colorNumeric(
  palette = "plasma", 
  domain = datos$total
)

# Crea el mapa
m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  # Agrega un mapa base. Usar si se quiere tener un "fondo"
  # addTiles() %>%
  addPolygons(
    data = datos$geometry,         # Informacion geografica
    weight = 2,                    # Grosor de la linea de frontera
    color = pal(datos$total),      # Color de la linea de frontera
    fillColor = pal(datos$total),  # Color de relleno del poligono
    opacity = 1,                   # Opacidad de la linea de frontera
    fillOpacity = 1                # Opacidad del color de relleno
  ) %>%
  # Agrega leyenda del numero de agentes
  addLegend(
    "bottomleft",                 # Posicion de la leyenda
    pal = pal,                    # Escala de color de la leyenda
    values = rev(datos$total),         # Valores a ser representados
    title = "Número de jornadas",  # Titulo de la leyenda
    labFormat = labelFormat(
      prefix = "",
      suffix = "",
      between = ", "
    ),
    opacity = 1
  ) %>%
  setMapWidgetStyle(list(background = "white"))

m  # Visualizar mapa en Viewer

# ASISTENTES PROMEDIO A LAS JORNADAS POR ENTIDAD ===============================

datos <- jornadas %>%
  select(Entidad, agentes_asistentes) %>%
  drop_na() %>%
  group_by(Entidad) %>%
  summarise(total = round(mean(as.numeric(agentes_asistentes)), 1))

image <- ggplot(datos) +
  aes(
    x = fct_rev(fct_reorder(Entidad, desc(total))), 
    y = total, 
    fill = color_oficial[1], 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2)]) +
  #scale_y_continuous(trans = "log") +  # Escala logaritmica
  labs(
    x = "Entidad", 
    y = "Promedio de asistentes a las jornadas",
    title = "Jornadas culturales: promedio de asistentes por entidad",
    subtitle = "",
    fill = ""
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(angle = 90),  #Pone el texto en vertical
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "promedio_asistentes_jornadas_entidad.png"
  ), 
  plot = image, 
  width = 9, 
  height = 7
)

# Mapa de calor

datos <- jornadas %>%
  select(Entidad, agentes_asistentes) %>%
  drop_na() %>%
  group_by(Entidad) %>%
  summarise(total = round(mean(as.numeric(agentes_asistentes)), 1)) %>%
  ungroup()
datos$Entidad[datos$Entidad == "Estado de México"] <- "México"
datos <- datos %>%
  left_join(
    select(catalogos$cat_edos, id_inegi, entidad),
    by = c("Entidad" = "entidad")
  )
datos$Entidad[datos$Entidad == "México"] <- "Estado de México"
datos$id_inegi <- as.numeric(datos$id_inegi)
datos <- datos %>%
  left_join(select(s_ent, CVE_ENT, geometry), by = c("id_inegi" = "CVE_ENT"))

# Crea funcion para obtener paleta de colores
pal <- colorNumeric(
  palette = "plasma", 
  domain = datos$total
)

# Crea el mapa
m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  # Agrega un mapa base. Usar si se quiere tener un "fondo"
  # addTiles() %>%
  addPolygons(
    data = datos$geometry,         # Informacion geografica
    weight = 2,                    # Grosor de la linea de frontera
    color = pal(datos$total),      # Color de la linea de frontera
    fillColor = pal(datos$total),  # Color de relleno del poligono
    opacity = 1,                   # Opacidad de la linea de frontera
    fillOpacity = 1                # Opacidad del color de relleno
  ) %>%
  # Agrega leyenda del numero de agentes
  addLegend(
    "bottomleft",                 # Posicion de la leyenda
    pal = pal,                    # Escala de color de la leyenda
    values = rev(datos$total),         # Valores a ser representados
    title = "Número de jornadas",  # Titulo de la leyenda
    labFormat = labelFormat(
      prefix = "",
      suffix = "",
      between = ", "
    ),
    opacity = 1
  ) %>%
  setMapWidgetStyle(list(background = "white"))

m  # Visualizar mapa en Viewer