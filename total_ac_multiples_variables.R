# TOTAL DE AGENTES CULTURALES
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
load("reportes_14_10_2020.RData")
load("INEGI\\cat_ent_mun.RData")
load("colores_oficiales.RData")
load("catalogos.RData")

# CARGA DE SHAPES ==============================================================

lapply(
  c(
    "sf",
    "proj4",
    "leaflet",
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

# DEFINICION DE NOTA PARA LAS GRAFICAS =========================================

nota <- paste0(
  "Información captada del 25 de mayo de 2019 al 19 de agosto de 2020 a través ",
  "de la plataforma Telar.\nRegistro Nacional de Espacios, Prácticas y Agentes ",
  "Culturales.\nDirección de Animación Cultural. Dirección General de ",
  "Vinculación Cultural. Secretaría de Cultura."
)

# TOTAL DE AC POR GENERO =======================================================

datos <- select(reportes$basicos, id, gen) %>%
  drop_na() %>%
  distinct() %>%
  # filter(gen %in% c("Hombre", "Mujer")) %>%
  group_by(gen) %>%
  summarise(total = n())

# GRAFICA 

image <- ggplot(datos) +
  aes(
    x = gen, 
    y = total, 
    fill = gen, 
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
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  #scale_y_continuous(trans = "log") +  # Escala logaritmica
  labs(
    x = "", 
    y = "Agentes culturales",
    title = "Agentes culturales: total por género",
    subtitle = "",
    fill = "Género",
    caption = nota
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
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_artes.png"
  ), 
  plot = image, 
  width = 7, 
  height = 7
)

# TOTAL DE AC POR AREA CULTURAL ================================================

datos <- select(reportes$cult_area, id, id_area) %>%
  distinct() %>%
  # Genero
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  drop_na() %>%
  # Totales
  group_by(id_area) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area)

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario\ny diseño",
  "Artes audiovisuales",
  "Artes de la\nrepresentación\ntradicionales",
  "Artes plásticas\ny visuales",
  "Artes vivas\ny escénicas",
  "Conocimientos y usos\nrelacionados con\nla naturaleza",
  "Interdisciplina",
  "Lengua, tradiciones\norales y narrativa",
  "Literatura",
  "Multimedia\ny arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y\nGestión Cultural"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$area <- factor(  # Variable de la que se reordenaran los niveles
  datos$area,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos$area,
        datos$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(area), 
    y = porc,
    fill = color_oficial[2],
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"  # Para barras apiladas con numeros brutos
    # position = "fill"
  ) +
  geom_text(
    # position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[2]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "Proporción de agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "distribución por área de actuación cultural"
    ),
    subtitle = "",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_area.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC POR AREA CULTURAL Y GENERO =======================================

datos <- select(reportes$cult_area, id, id_area) %>%
  distinct() %>%
  # Genero
  left_join(select(reportes$basicos, id, gen, grp_ind), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & grp_ind == 1) %>%
  drop_na() %>%
  # Totales
  group_by(id_area, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area)

# # Agrega el total a los datos
# datos <- datos %>%
#   add_row(
#     area = "Total",
#     gen = c("Hombre", "Mujer"),
#     total = c(
#       sum(datos$total[datos$gen == "Hombre"]),
#       sum(datos$total[datos$gen == "Mujer"])
#     ),
#     porc = c(
#       sum(datos$total[datos$gen == "Hombre"])/sum(datos$total),
#       sum(datos$total[datos$gen == "Mujer"])/sum(datos$total)
#     ),
#     .before = 1
#   )

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario\ny diseño",
  "Artes audiovisuales",
  "Artes de la\nrepresentación\ntradicionales",
  "Artes plásticas\ny visuales",
  "Artes vivas\ny escénicas",
  "Conocimientos y usos\nrelacionados con\nla naturaleza",
  "Interdisciplina",
  "Lengua,\ntradiciones orales\ny narrativa",
  "Literatura",
  "Multimedia\ny arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y\nGestión Cultural"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$area <- factor(  # Variable de la que se reordenaran los niveles
  datos$area,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$gen == "Mujer", ]$area,
        datos[datos$gen == "Mujer", ]$porc # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(area), 
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "distribución por género en las áreas de actuación cultural"
    ),
    subtitle = "Agentes culturales que forman parte de grupos indígenas",
    fill = "Género",
    caption = paste0(
      #"Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_area_genero_ind.png"
  ), 
  plot = image, 
  width = 11, 
  height = 7
)

# TOTAL DE AC POR AREA CULTURAL Y SITIO DONDE LLEVA SU ACTIVIDAD ===============

datos <- select(reportes$cult_area, id, id_area) %>%
  distinct() %>%
  # Genero
  left_join(select(reportes$movilidad, id, mov_s), by = "id") %>%
  drop_na() %>%
  # Totales
  group_by(id_area, mov_s) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area)

sitios <- c(
  "Espacios privados como: casa habitación o taller propio.",
  paste0(
    "Espacios públicos abiertos como: plaza principal, mercado, ",
    "semáforo, parques, etc."
  ),
  "Espacios públicos cerrados como: ginmasio, foros cerrados, etc.",
  paste0(
    "Lugares especializados en la disciplina que realizo como: teatros, ",
    "salas de conciertos, ferias de artesanías, galerías, museos, etc."
  ),
  "Otros espacios"
)

# Agrega el total a los datos
datos <- datos %>%
  add_row(
    area = "Total",
    mov_s = sitios,
    total = c(
      sum(datos$total[datos$mov_s == sitios[1]]),
      sum(datos$total[datos$mov_s == sitios[2]]),
      sum(datos$total[datos$mov_s == sitios[3]]),
      sum(datos$total[datos$mov_s == sitios[4]]),
      sum(datos$total[datos$mov_s == sitios[5]])
    ),
    porc = c(
      sum(datos$total[datos$mov_s == sitios[1]])/sum(datos$total),
      sum(datos$total[datos$mov_s == sitios[2]])/sum(datos$total),
      sum(datos$total[datos$mov_s == sitios[3]])/sum(datos$total),
      sum(datos$total[datos$mov_s == sitios[4]])/sum(datos$total),
      sum(datos$total[datos$mov_s == sitios[5]])/sum(datos$total)
    ),
    .before = 1
  )
rm(sitios)
# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario\ny diseño",
  "Artes audiovisuales",
  "Artes de la\nrepresentación\ntradicionales",
  "Artes plásticas\ny visuales",
  "Artes vivas\ny escénicas",
  "Conocimientos y usos\nrelacionados con\nla naturaleza",
  "Interdisciplina",
  "Lengua,\ntradiciones orales\ny narrativa",
  "Literatura",
  "Multimedia\ny arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y\nGestión Cultural",
  "Total"  
)
datos$mov_s <- factor(x = datos$mov_s)
levels(datos$mov_s) <- c(
  "Espacios privados",
  "Espacios públicos\nabiertos",
  "Espacios públicos\ncerrados",
  "Lugares\nespecializados",
  "Otros espacios"  
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_relevel(area, "Total"), 
    y = total,
    fill = mov_s,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(1, 6, 2, 7, 8)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "",
    title = paste0(
      "Agentes culturales: porcentaje por espacios donde lleva a cabo",
      " su actividad cultural en las áreas de actuación cultural"
    ),
    subtitle = "",
    fill = "Espacios",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  #Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_area_espacios.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7.5
)

# TOTAL DE AC QUE HABLAN LENGUA INDIGENA POR GENERO ============================

datos <- select(reportes$basicos, id, gen, lng_i) %>%
  distinct() %>%
  filter(gen %in% c("Hombre", "Mujer"))
datos$lng_i[is.na(datos$lng_i)] <- 0
datos <- datos %>%
  group_by(gen, lng_i) %>%
  summarise(total = n()) %>%
  ungroup()
datos$lng_i <- ifelse(datos$lng_i == 1, "Sí", "No")

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = lng_i, 
    y = total,
    fill = gen,
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "¿Habla una lengua indígena?", 
    y = "Agentes culturales",
    title = "Agentes culturales: ¿habla una lengua indígena? Total por género",
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(vjust = 0.5, angle = 90),  #Pone el texto en vertical
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_lngi_genero_artes.png"
  ), 
  plot = image, 
  width = 7, 
  height = 7
)

# TOTAL DE AC POR ENTIDAD DE RESIDENCIA Y GENERO ===============================

datos <- select(reportes$basicos, id, cve_ent_res, gen) %>%
  filter(cve_ent_res != 1 & gen %in% c("Hombre", "Mujer")) %>%
  group_by(cve_ent_res, gen) %>%
  summarise(total = n()) %>%
  left_join(
    select(catalogos$cat_edos, id_ent, id_inegi, entidad),
    by = c("cve_ent_res" = "id_ent")
  ) %>%
  relocate(entidad) %>%
  ungroup() %>%
  select(-id_inegi, -cve_ent_res)

# Agrega totales
datos <- datos %>%
  add_row(
    entidad = "Total", 
    gen = c(
      "Hombre",
      "Mujer"
    ), 
    total = c(
      sum(datos$total[datos$gen == "Hombre"]),
      sum(datos$total[datos$gen == "Mujer"])
    )
  )

datos$entidad <- factor(x = datos$entidad)
levels(datos$entidad)[31] <- c("Veracruz de Ignacio\nde la Llave")

# Reordena los niveles de acuerdo con una regla descendente
datos$entidad <- factor(  # Variable de la que se reordenaran los niveles
  datos$entidad,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$gen == "Mujer", ]$entidad,
        datos[datos$gen == "Mujer", ]$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_relevel(entidad, "Total"), 
    y = total, 
    fill = gen, 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Entidad de residencia", 
    y = "Agentes culturales",
    title = "Agentes culturales: total por entidad de residencia y género",
    subtitle = "",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  #Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_entidad.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# TOTAL DE AC POR ENTIDAD Y MUNICIPIO DE RESIDENCIA Y GENERO ===================

datos <- select(reportes$basicos, id, cve_ent_res, cve_mun_res, gen) %>%
  filter(cve_ent_res != 1 & gen %in% c("Hombre", "Mujer")) %>%
  group_by(cve_ent_res, cve_mun_res, gen) %>%
  summarise(total = n()) %>%
  left_join(
    select(catalogos$cat_edos, id_ent, id_inegi, entidad),
    by = c("cve_ent_res" = "id_ent")
  ) %>%
  left_join(
    select(catalogos$cat_mun, id_mun, mun),
    by = c("cve_mun_res" = "id_mun")
  ) %>%
  relocate(entidad, mun) %>%
  ungroup() %>%
  select(-id_inegi, -cve_ent_res)

# Agrega totales
datos <- datos %>%
  add_row(
    entidad = "Total", 
    gen = c(
      "Hombre",
      "Mujer"
    ), 
    total = c(
      sum(datos$total[datos$gen == "Hombre"]),
      sum(datos$total[datos$gen == "Mujer"])
    )
  )

datos$entidad <- factor(x = datos$entidad)
levels(datos$entidad)[31] <- c("Veracruz de Ignacio\nde la Llave")

# Reordena los niveles de acuerdo con una regla descendente
datos$entidad <- factor(  # Variable de la que se reordenaran los niveles
  datos$entidad,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$gen == "Mujer", ]$entidad,
        datos[datos$gen == "Mujer", ]$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_relevel(entidad, "Total"), 
    y = total, 
    fill = gen, 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Entidad de residencia", 
    y = "Agentes culturales",
    title = "Agentes culturales: total por entidad de residencia y género",
    subtitle = "",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  #Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_entidad.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# TOTAL DE AC POR DISCIPLINAS DE UN AREA ESPECIFICA Y GENERO ===================

# Nota: aqui no tiene sentido agregar el total, pues los ac pueden tener mas de
# una disciplina registrada

# ATENCION: Especificar el area deseada en id_area
datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen), by =  "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id_area == 3) %>%
  drop_na() %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )

# Este par de instrucciones garantizan que .drop = FALSE en el siguiente
# segmento funcione bien. Es necesario que las variables de agrupacion sean
# factores para que las combinaciones de conteo 0 se mantengan
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
# Con .drop = FALSE garantizamos que se cuenten como 0 los conjuntos vacíos
datos <- datos %>%
  group_by(disciplina, gen, .drop = FALSE) %>%
  summarise(total = n())

levels(datos$disciplina)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$disciplina) <- c(
  "Cabaret",
  "Cine",
  "Circo",
  "Cuento",
  "Danza",
  "Danza y bailes\ntradicionales",
  "Dibujo",
  "Diseño\narquitectónico",
  "Escultura",
  "Fotografía",
  "Música\nacadémica",
  "Música\npopular",
  "Musicalización",
  "Narración oral",
  "Ópera",
  "Otra",
  "Performance",
  "Performance\nen internet",
  "Pintura",
  "Stand up",
  "Teatro",
  "Teatro\ncomunitario",
  "Televisión"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$disciplina <- factor(  # Variable de la que se reordenaran los niveles
  datos$disciplina,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$gen == "Mujer", ]$disciplina,
        datos[datos$gen == "Mujer", ]$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = disciplina, 
    y = total, 
    fill = gen, 
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Disciplina ejercida", 
    y = "Agentes culturales",
    title = "Agentes culturales: total por disciplina y género",
    subtitle = "",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_disciplina_genero.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# TOTAL DE AC QUE HABLAN LENGUA INDIGENA POR DISCIPLINAS Y GENERO ==============

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen, lng_i), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id_area == 3 & id %in% ids)
datos$lng_i[is.na(datos$lng_i)] <- 0
datos <- datos %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )
# Este par de instrucciones garantizan que .drop = FALSE en el siguiente
# segmento funcione bien. Es necesario que las variables de agrupacion sean
# factores para que las combinaciones de conteo 0 se mantengan
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
datos$lng_i <- ifelse(datos$lng_i == 1, "Sí", "No")
datos$lng_i <- factor(x = datos$lng_i)

# Con .drop = FALSE garantizamos que se cuenten como 0 los conjuntos vacíos
datos <- datos %>%
  group_by(disciplina, lng_i, gen, .drop = FALSE) %>%
  summarise(total = n())

# GRAFICA
image <- ggplot(datos[81:92, ]) +
  aes(
    x = lng_i, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "¿Habla una lengua indígena?", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ¿habla una lengua indígena?",
      " Total por disciplina y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(vjust = 0.5, angle = 90),  #Pone el texto en vertical
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_lngi_disciplina_genero_artes_5.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC QUE HABLAN LENGUA INDIGENA POR AREAS CULTURALES Y GENERO =========

datos <- select(reportes$cult_area, id, id_area) %>%
  distinct() %>%
  # Genero
  left_join(select(reportes$basicos, id, gen, lng_i), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & lng_i == 1) %>%
  drop_na() %>%
  # Totales
  group_by(id_area, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area)

# Agrega el total a los datos
datos <- datos %>%
  add_row(
    area = "Total",
    gen = c("Hombre", "Mujer"),
    total = c(
      sum(datos$total[datos$gen == "Hombre"]),
      sum(datos$total[datos$gen == "Mujer"])
    ),
    porc = c(
      sum(datos$total[datos$gen == "Hombre"])/sum(datos$total),
      sum(datos$total[datos$gen == "Mujer"])/sum(datos$total)
    ),
    .before = 1
  )

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario\ny diseño",
  "Artes audiovisuales",
  "Artes de la\nrepresentación\ntradicionales",
  "Artes plásticas\ny visuales",
  "Artes vivas\ny escénicas",
  "Conocimientos y usos\nrelacionados con\nla naturaleza",
  "Interdisciplina",
  "Lengua,\ntradiciones orales\ny narrativa",
  "Literatura",
  "Multimedia\ny arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y\nGestión Cultural",
  "Total"  
)

# Reordena los niveles de acuerdo con una regla descendente
datos$area <- factor(  # Variable de la que se reordenaran los niveles
  datos$area,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$gen == "Mujer", ]$area,
        datos[datos$gen == "Mujer", ]$porc # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_relevel(area, "Total"), 
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "porcentaje por género en las áreas de actuación cultural de los",
      " hablantes de lenguas indígenas"
    ),
    subtitle = "",
    fill = "Género",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  #Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )
plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_lngi_area_genero.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC POR ULTIMO GRADO DE ESTUDIOS =====================================

datos <- reportes$basicos %>%
  select(id, gen) %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  drop_na() %>%
  distinct() %>%
  group_by(soc_est) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$soc_est <- factor(x = datos$soc_est)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$soc_est) <- c(
  "Carrera técnica con\nsecundaria terminada",
  "Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  "Ninguno",
  "No sabe / no responde",
  "Normal básica",
  "Preescolar",
  "Preparatoria o\nbachillerato completa",
  "Primaria",
  "Secundaria" 
)

# Reordena los niveles de acuerdo con una regla descendente
datos$soc_est <- factor(  # Variable de la que se reordenaran los niveles
  datos$soc_est,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos$soc_est,
        datos$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(soc_est), 
    y = porc,
    fill = color_oficial[2],
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[2]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Último grado de estudios", 
    y = "Proporción de agentes culturales",
    title = "Agentes culturales: distribución por último grado de estudios",
    subtitle = "",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_estud.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC POR SITIOS DONDE REALIZAN SU ACTIVIDAD ===========================

datos <- reportes$basicos %>%
  select(id, gen) %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  left_join(select(reportes$movilidad, id, mov_s), by = "id") %>%
  drop_na() %>%
  distinct()

x <- c(
  paste0(
    "Lugares especializados privados en la disciplina que realizo como: ",
    "teatros, salas de conciertos, ferias de artesanías, galerías, museos, etc."
  ),
  paste0(
    "Lugares especializados públicos en la disciplina que realizo como: ",
    "teatros, salas de conciertos, ferias de artesanías, galerías, museos, etc."
  )
)

datos$mov_s[datos$mov_s %in% x] <- paste0(
  "Lugares especializados en la disciplina que realizo como: ",
  "teatros, salas de conciertos, ferias de artesanías, galerías, museos, etc."
)

# Establece los factores
datos$mov_s <- factor(x = datos$mov_s)
levels(datos$mov_s) <- c(
  "Espacios privados",
  "Espacios públicos\nabiertos",
  "Espacios públicos\ncerrados",
  "Lugares especializados",
  "Otros espacios"  
)
# Obtiene totales
datos <- datos %>%
  group_by(mov_s) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  mutate(porc = total/sum(total)) %>%
  ungroup() 

# Reordena los niveles de acuerdo con una regla descendente
datos$mov_s <- factor(  # Variable de la que se reordenaran los niveles
  datos$mov_s,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos$mov_s,
        datos$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(mov_s), 
    y = porc,
    fill = color_oficial[2],
    # label = format(total, big.mark = ",")
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[2]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Tipo de espacio", 
    y = "Proporción de agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "distribución por tipo de espacio donde realiza su actividad cultural"
    ),
    subtitle = "",
    caption = paste0(
      #"Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_espacios.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC POR ULTIMO GRADO DE ESTUDIOS, DISCIPLINAS Y GENERO ===============

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id_area == 3 & id %in% ids)
datos$soc_est[is.na(datos$soc_est)] <- "No sabe / no responde"
datos <- datos %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )
# Este par de instrucciones garantizan que .drop = FALSE en el siguiente
# segmento funcione bien. Es necesario que las variables de agrupacion sean
# factores para que las combinaciones de conteo 0 se mantengan
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
datos$soc_est <- factor(x = datos$soc_est)

# Con .drop = FALSE garantizamos que se cuenten como 0 los conjuntos vacíos
datos <- datos %>%
  group_by(disciplina, soc_est, gen) %>%
  summarise(total = n())

levels(datos$soc_est)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$soc_est) <- c(
  "Carrera técnica con\nsecundaria terminada",
  "Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  "Ninguno",
  "No sabe / no responde",
  "Normal básica",
  "Preescolar",
  "Preparatoria o\nbachillerato completa",
  "Primaria",
  "Secundaria" 
)

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[21:23], ]
) +
  aes(
    x = fct_relevel(
      soc_est,
      "No sabe / no responde",
      "Ninguno",
      "Preescolar",
      "Primaria",
      "Secundaria",
      "Carrera técnica con\nsecundaria terminada",
      "Normal básica",
      "Preparatoria o\nbachillerato completa",
      "Estudios técnicos con\npreparatoria terminada",
      "Licenciatura",
      "Maestría",
      "Doctorado"
    ), 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Último grado de estudios", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, último grado de estudios",
      " y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_estud_disciplina_genero_artes_6.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR ACTIVIDAD PRINCIPAL DE INGRESO, DISCIPLINAS Y GENERO ===============

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(select(reportes$formacion, id, for_ocu), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id_area == 3 & id %in% ids) %>%
  drop_na()
datos <- datos %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )
# Este par de instrucciones garantizan que .drop = FALSE en el siguiente
# segmento funcione bien. Es necesario que las variables de agrupacion sean
# factores para que las combinaciones de conteo 0 se mantengan
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
datos$for_ocu <- factor(x = datos$for_ocu)

# Con .drop = FALSE garantizamos que se cuenten como 0 los conjuntos vacíos
datos <- datos %>%
  group_by(disciplina, for_ocu, gen) %>%
  summarise(total = n())

levels(datos$for_ocu)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$for_ocu) <- c(
  "Actualmente\nno labora",
  "Autoempleo",
  "Empleado del\nsector privado",
  "Empleado del\nsector social"
)

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[21:23], ]
) +
  aes(
    x = for_ocu, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Ocupación principal de la cual obtiene ingresos", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, ocupación principal por la ",
      "cual obtiene ingresos y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_ocu_disciplina_genero_artes_5.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR INGRESOS, DISCIPLINAS Y GENERO =====================================

datos <- select(reportes$formacion, id, for_ocu_sec_t) %>%
  left_join(select(reportes$basicos, id, gen), "id") %>%
  left_join(select(reportes$cult_disc, id, id_area, id_disciplina), "id") %>%
  rename(for_ing = for_ocu_sec_t) %>%
  filter(gen %in% c("Hombre", "Mujer") & id %in% ids & id_area == 3) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )
# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)
# Establece los factores
datos$ingreso2 <- factor(x = datos$ingreso2)
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
# Obtiene totales
datos <- datos %>%
  group_by(disciplina, ingreso2, gen) %>%
  summarise(total = n()) %>%
  rename(ingreso = ingreso2)
# Modifica los niveles del ingreso
levels(datos$ingreso)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[21:23], ]
) +
  aes(
    x = fct_relevel(ingreso, "3 o más", after = Inf), 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Ingreso mensual en salarios mínimos", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, ingreso mensual y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      "Salario mínimo de $123.22, de acuerdo con lo publicado en:\n",
      "https://www.gob.mx/cms/uploads/attachment/file/525061/Tabla_de_salarios_m_nmos_vigentes_apartir_del_01_de_enero_de_2020.pdf\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5#,  # Ajusta el texto en el centro del tick
      #angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_salariosmin_disciplina_genero_artes_5.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR INGRESOS Y AREAS ===================================================

datos <- select(reportes$cult_area, id, id_area) %>%
  distinct() %>%
  # Ingresos
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  drop_na()

# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)

# Obtiene totales
datos <- datos %>%
  group_by(id_area, ingreso2) %>%
  summarise(total = n()) %>%
  rename(ingreso = ingreso2) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
# Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area)

# Agrega el total a los datos
# datos <- datos %>%
#   add_row(
#     area = "Total",
#     ingreso = c(
#       "3 o más",
#       "Hasta 1 ($3,697)",
#       "Hasta 2 ($7,393)",
#       "Hasta 3 ($11,090)"
#     ),
#     total = c(
#       sum(datos$total[datos$ingreso == "3 o más"]),
#       sum(datos$total[datos$ingreso == "Hasta 1 ($3,697)"]),
#       sum(datos$total[datos$ingreso == "Hasta 2 ($7,393)"]),
#       sum(datos$total[datos$ingreso == "Hasta 3 ($11,090)"])
#     ),
#     porc = c(
#       sum(datos$total[datos$ingreso == "3 o más"])/sum(datos$total),
#       sum(datos$total[datos$ingreso == "Hasta 1 ($3,697)"])/sum(datos$total),
#       sum(datos$total[datos$ingreso == "Hasta 2 ($7,393)"])/sum(datos$total),
#       sum(datos$total[datos$ingreso == "Hasta 3 ($11,090)"])/sum(datos$total)
#     ),
#     .before = 1
#   )

# Modifica los niveles del ingreso
datos$ingreso <- factor(x = datos$ingreso)
levels(datos$ingreso)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)
# Ordena los niveles de ingreso para que vayan de manera ascendente
datos$ingreso <- fct_rev(
  fct_relevel(
    datos$ingreso,
    "Hasta 1\n($3,697)",
    "Hasta 2\n($7,393)",
    "Hasta 3\n($11,090)",
    "3 o más"
  )
)
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario y diseño",
  "Artes audiovisuales",
  "Artes de la representación\ntradicionales",
  "Artes plásticas y visuales",
  "Artes vivas y escénicas",
  "Conocimientos y usos\nrelacionados con la naturaleza",
  "Interdisciplina",
  "Lengua, tradiciones\norales y narrativa",
  "Literatura",
  "Multimedia y arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y Gestión Cultural"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$area <- factor(  # Variable de la que se reordenaran los niveles
  datos$area,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$ingreso == "Hasta 1\n($3,697)", ]$area,
        datos[datos$ingreso == "Hasta 1\n($3,697)", ]$porc # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    # x = fct_relevel(area, "Total"),
    x = fct_rev(area),
    y = total,
    fill = ingreso,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.3,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(4, 1:2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "ingreso en salarios mínimos por área de actuación cultural"
    ),
    subtitle = "",
    fill = "Salarios mínimos",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      "Salario mínimo de $123.22, de acuerdo con lo publicado en:\n",
      "https://www.gob.mx/cms/uploads/attachment/file/525061/Tabla_de_salarios_m_nmos_vigentes_apartir_del_01_de_enero_de_2020.pdf\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   vjust = 0.5,  # Ajusta el texto en el centro del tick
    #   angle = 90  #Pone el texto en vertical
    # ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_salariosmin_area.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# TDEAC POR INGRESOS Y FASE DEL CICLO CULTURAL =================================

datos <- select(reportes$cult_acti, id, id_actividad) %>%
  # Ingresos
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  distinct() %>%
  drop_na()

# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)

# Obtiene totales
datos <- datos %>%
  group_by(id_actividad, ingreso2) %>%
  summarise(total = n()) %>%
  rename(ingreso = ingreso2) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_actividad) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas
  left_join(catalogos$cat_acti, by = "id_actividad") %>%
  relocate(actividad) %>%
  select(-id_actividad)

# Agrega el total a los datos
# datos <- datos %>%
#   add_row(
#     area = "Total",
#     ingreso = c(
#       "3 o más",
#       "Hasta 1 ($3,697)",
#       "Hasta 2 ($7,393)",
#       "Hasta 3 ($11,090)"
#     ),
#     total = c(
#       sum(datos$total[datos$ingreso == "3 o más"]),
#       sum(datos$total[datos$ingreso == "Hasta 1 ($3,697)"]),
#       sum(datos$total[datos$ingreso == "Hasta 2 ($7,393)"]),
#       sum(datos$total[datos$ingreso == "Hasta 3 ($11,090)"])
#     ),
#     porc = c(
#       sum(datos$total[datos$ingreso == "3 o más"])/sum(datos$total),
#       sum(datos$total[datos$ingreso == "Hasta 1 ($3,697)"])/sum(datos$total),
#       sum(datos$total[datos$ingreso == "Hasta 2 ($7,393)"])/sum(datos$total),
#       sum(datos$total[datos$ingreso == "Hasta 3 ($11,090)"])/sum(datos$total)
#     ),
#     .before = 1
#   )

# Modifica los niveles del ingreso
datos$ingreso <- factor(x = datos$ingreso)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)
# Ordena los niveles de ingreso para que vayan de manera ascendente
datos$ingreso <- fct_rev(
  fct_relevel(
    datos$ingreso,
    "Hasta 1\n($3,697)",
    "Hasta 2\n($7,393)",
    "Hasta 3\n($11,090)",
    "3 o más"
  )
)
datos$actividad <- factor(x = datos$actividad)

# Reordena los niveles de acuerdo con una regla descendente
datos$actividad <- factor(  # Variable de la que se reordenaran los niveles
  datos$actividad,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$ingreso == "Hasta 1\n($3,697)", ]$actividad,
        datos[datos$ingreso == "Hasta 1\n($3,697)", ]$porc # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    # x = fct_relevel(area, "Total"),
    x = fct_rev(actividad),
    y = total,
    fill = ingreso,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.3,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(4, 1:2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Fase del ciclo cultural",
    y = "",
    title = paste0(
      "Agentes culturales: distribución del ingreso en salarios mínimos",
      " por actividad dentro del ciclo cultural"
    ),
    subtitle = "",
    fill = "Salarios mínimos",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   vjust = 0.5,  # Ajusta el texto en el centro del tick
    #   angle = 90  #Pone el texto en vertical
    # ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_salariosmin_acti.png"
  ), 
  plot = image, 
  width = 15, 
  height = 7
)

# TDEAC POR PORCENTAJE DE SUS INGRESOS Y AREA ==================================

datos <- select(reportes$cult_area, id, id_area) %>%
  # Ingresos
  left_join(select(reportes$formacion, id, for_por), by = "id") %>%
  distinct() %>%
  drop_na()

datos$porcentaje <- ""
for (j in seq(25, 75, 25)) {
  sup <- j
  inf <- j - 25
  log <- datos$for_por >= inf & datos$for_por < sup
  datos$porcentaje[log]  <- paste0(j - 25, " a ", j - 1)
}
rm(j)
datos$porcentaje[datos$porcentaje == ""] <- "75 a 100"
# Establece los factores
datos$porcentaje <- factor(x = datos$porcentaje)
levels(datos$porcentaje) <- c(
  "0%-24%",
  "25%-49%",
  "50%-74%",
  "75%-100%"
)

# Obtiene totales
datos <- datos %>%
  group_by(id_area, porcentaje) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area)

datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario y diseño",
  "Artes audiovisuales",
  "Artes de la representación\ntradicionales",
  "Artes plásticas y visuales",
  "Artes vivas y escénicas",
  "Conocimientos y usos\nrelacionados con la naturaleza",
  "Interdisciplina",
  "Lengua, tradiciones\norales y narrativa",
  "Literatura",
  "Multimedia y arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y Gestión Cultural"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$area <- factor(  # Variable de la que se reordenaran los niveles
  datos$area,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos[datos$porcentaje == "0%-24%", ]$area,
        datos[datos$porcentaje == "0%-24%", ]$porc # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = area,
    y = total,
    fill = porcentaje,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(4, 1:2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: porcentaje de sus ingresos por",
      " concepto de su actividad cultural por área de actuación cultural"
    ),
    subtitle = "",
    fill = "Porcentaje de sus ingresos por concepto de su actividad cultural",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   vjust = 0.5,  # Ajusta el texto en el centro del tick
    #   angle = 90  #Pone el texto en vertical
    # ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_porcing_area.png"
  ), 
  plot = image, 
  width = 15, 
  height = 7
)

# TDEAC POR INGRESOS Y GENERO ==================================================

datos <- select(reportes$basicos, id, gen) %>%
  distinct() %>%
  # Ingresos
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  drop_na()

# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)

# Obtiene totales
datos <- datos %>%
  group_by(gen, ingreso2) %>%
  summarise(total = n()) %>%
  rename(ingreso = ingreso2) %>%
  ungroup() %>%
  # Porcentajes
  group_by(gen) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# Agrega el total a los datos
datos <- datos %>%
  add_row(
    gen = "Total",
    ingreso = c(
      "3 o más",
      "Hasta 1 ($3,697)",
      "Hasta 2 ($7,393)",
      "Hasta 3 ($11,090)"
    ),
    total = c(
      sum(datos$total[datos$ingreso == "3 o más"]),
      sum(datos$total[datos$ingreso == "Hasta 1 ($3,697)"]),
      sum(datos$total[datos$ingreso == "Hasta 2 ($7,393)"]),
      sum(datos$total[datos$ingreso == "Hasta 3 ($11,090)"])
    ),
    porc = c(
      sum(datos$total[datos$ingreso == "3 o más"])/sum(datos$total),
      sum(datos$total[datos$ingreso == "Hasta 1 ($3,697)"])/sum(datos$total),
      sum(datos$total[datos$ingreso == "Hasta 2 ($7,393)"])/sum(datos$total),
      sum(datos$total[datos$ingreso == "Hasta 3 ($11,090)"])/sum(datos$total)
    ),
    .before = 1
  )

# Modifica los niveles del ingreso
datos$ingreso <- factor(x = datos$ingreso)
levels(datos$ingreso)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)
# Ordena los niveles de ingreso para que vayan de manera ascendente
datos$ingreso <- fct_rev(
  fct_relevel(
    datos$ingreso,
    "Hasta 1\n($3,697)",
    "Hasta 2\n($7,393)",
    "Hasta 3\n($11,090)",
    "3 o más"
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_relevel(gen, "Total"), 
    y = total,
    fill = ingreso,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(6, 1:2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Género",
    y = "Agentes culturales",
    title = "Agentes culturales: ingreso en salarios mínimos por género",
    subtitle = "",
    fill = "Salarios mínimos",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      "Salario mínimo de $123.22, de acuerdo con lo publicado en:\n",
      "https://www.gob.mx/cms/uploads/attachment/file/525061/Tabla_de_salarios_m_nmos_vigentes_apartir_del_01_de_enero_de_2020.pdf\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_salariosmin_gen.png"
  ), 
  plot = image, 
  width = 9, 
  height = 7
)

# TDEAC POR PORCENTAJE DE SUS INGRESOS, DISCIPLINAS Y GENERO ===================

datos <- select(reportes$formacion, id, for_ing) %>%
  left_join(select(reportes$basicos, id, gen), "id") %>%
  left_join(select(reportes$cult_disc, id, id_area, id_disciplina), "id") %>%
  rename(for_por = for_ing) %>%
  filter(gen %in% c("Hombre", "Mujer") & id %in% ids & id_area == 3) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )
datos$porcentaje <- ""
for (j in seq(25, 75, 25)) {
  sup <- j
  inf <- j - 25
  log <- datos$for_por >= inf & datos$for_por < sup
  datos$porcentaje[log]  <- paste0(j - 25, " a ", j - 1)
}
rm(j)
datos$porcentaje[datos$porcentaje == ""] <- "75 a 100"
# Establece los factores
datos$porcentaje <- factor(x = datos$porcentaje)
levels(datos$porcentaje) <- c(
  "0%-24%",
  "25%-49%",
  "50%-74%",
  "75%-100%"
)
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
# Obtiene totales
datos <- datos %>%
  group_by(disciplina, porcentaje, gen) %>%
  summarise(total = n())

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[21:25], ]
) +
  aes(
    x = porcentaje, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Porcentaje de sus ingresos proveniente de su actividad cultural", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, porcentaje de sus ingresos",
      " proveniente de su actividad cultural y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5#,  # Ajusta el texto en el centro del tick
      #angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_porcing_disciplina_genero_artes_5.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR SITIO DONDE LLEVA A CABO SU ACTIVIDAD, DISCIPLINAS Y GENERO ========

datos <- reportes$cult_disc %>%
  left_join(select(reportes$movilidad, id, mov_s), by = "id") %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id %in% ids & id_area == 3) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  ) %>%
  drop_na()

# Establece los factores
datos$mov_s <- factor(x = datos$mov_s)
levels(datos$mov_s) <- c(
  "Espacios privados",
  "Espacios públicos\nabiertos",
  "Espacios públicos\ncerrados",
  "Lugares\nespecializados",
  "Otros espacios"  
)
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
# Obtiene totales
datos <- datos %>%
  group_by(disciplina, mov_s, gen) %>%
  summarise(total = n())

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[1:6], ]
) +
  aes(
    x = mov_s, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Sitios donde principalmente lleva a cabo su actividad cultural", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, sitios donde principalmente",
      " lleva a cabo su actividad cultural y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_sitios_disciplina_genero_artes_1.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR MANERA COMO APRENDIO SU ACTIVIDAD, DISCIPLINAS Y GENERO ============

datos <- reportes$cult_disc %>%
  left_join(select(reportes$formacion, id, for_apr), by = "id") %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id %in% ids & id_area == 3) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  ) %>%
  drop_na() %>%
  select(-id_area, -id_disciplina, -id)

# Obtiene totales
datos <- datos %>%
  group_by(disciplina, gen, for_apr) %>%
  summarise(total = n())

# # Establece los factores
datos$for_apr <- factor(x = datos$for_apr)
levels(datos$for_apr) <- c(
  "Asistencia a cursos,\nclases y talleres",
  "Carrera técnica con\nespecialidad en artes\ncon secundaria terminada",
  "Cursó algún diplomado\no taller en artes",
  "Cursó estudios\nde doctorado",
  "Cursó estudios\nde maestría",
  "Enseñanza de amigos\ny/o compañeros",
  "Enseñanza de padres\ny/o familiares directos",
  "Estudios de\nbachillerato\nen artes",
  "Otra",
  "Por su propia\ncuenta",
  "Tiene estudios\nde licenciatura",
  "Tutoriales en\ninternet o televisión"
)
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[19:21], ]
) +
  aes(
    x = for_apr, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Manera en que aprendió su conocimiento en el campo cultural", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, manera en que aprendió su ",
      "conocimiento en el campo cultural y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_manera_disciplina_genero_artes_7.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR EVENTOS DONDE LLEVA A CABO SU ACTIVIDAD, DISCIPLINAS Y GENERO ======

datos <- reportes$cult_disc %>%
  left_join(
    select(
      reportes$movilidad,
      id,
      mov_tev_01,
      mov_tev_02,
      mov_tev_03,
      mov_tev_04,
      mov_tev_05,
      mov_tev_06,
      mov_tev_07,
      mov_tev_08,
      mov_tev_09,
      mov_tev_11
    ),
    by = "id"
  ) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id %in% ids & id_area == 3) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  ) %>%
  drop_na() %>%
  select(-id_area, -id_disciplina, -id)

# Obtiene totales
datos <- datos %>%
  group_by(disciplina, gen) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(-c(disciplina, gen), names_to = "evento", values_to = "total")

datos$evento[datos$evento == "mov_tev_01_total"] <- "Muestras"
datos$evento[datos$evento == "mov_tev_02_total"] <- "Festivales"
datos$evento[datos$evento == "mov_tev_03_total"] <- "Ferias"
datos$evento[datos$evento == "mov_tev_04_total"] <- "Conferencias"
datos$evento[datos$evento == "mov_tev_05_total"] <- "Exposiciones"
datos$evento[datos$evento == "mov_tev_06_total"] <- "Caravana artística"
datos$evento[datos$evento == "mov_tev_07_total"] <- "Concierto"
datos$evento[datos$evento == "mov_tev_08_total"] <- "Expo venta"
datos$evento[datos$evento == "mov_tev_09_total"] <- "Feria del libro"
datos$evento[datos$evento == "mov_tev_11_total"] <- "Otra"

# # Establece los factores
datos$evento <- factor(x = datos$evento)
levels(datos$evento) <- c(
  "Caravana\nartística",
  "Concierto",
  "Conferencias",
  "Expo venta",
  "Exposiciones",
  "Feria del\nlibro",
  "Ferias",
  "Festivales",
  "Muestras",
  "Otra"   
)
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[21:23], ]
) +
  aes(
    x = evento, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Eventos donde ha llevado a cabo su actividad cultural", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, eventos donde ha",
      " llevado a cabo su actividad cultural y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_eventos_disciplina_genero_artes_5.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# NUBE DE PALABRAS DE TECNICA EMPLEADA EN SU ACTIVIDAD CULTURAL ================

# Pendiente. Mala calidad de la informacion
datos <- reportes$cult_area %>%
  filter(id %in% ids) %>%
  select(tecnica) %>%
  drop_na()

datos <- as.data.frame(table(datos))

# TDEAC POR TIPO DE ACTIVIDAD DENTRO DEL AREA DE ACTUACION, DISCIP Y GENERO ====

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(select(reportes$cult_acti, id, id_actividad), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id_area == 3 & id %in% ids) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  ) %>%
  left_join(
    select(catalogos$cat_acti, id_actividad, actividad), by = "id_actividad"
  ) %>%
  distinct() %>%
  drop_na()
# Este par de instrucciones garantizan que .drop = FALSE en el siguiente
# segmento funcione bien. Es necesario que las variables de agrupacion sean
# factores para que las combinaciones de conteo 0 se mantengan
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
datos$actividad <- factor(x = datos$actividad)

datos <- datos %>%
  group_by(disciplina, actividad, gen) %>%
  summarise(total = n())

levels(datos$actividad)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$actividad) <- c(
  "Creación\nDiseño\nInterpretación",
  "Difusión\nDivulgación",
  "Docencia\nEnseñanza\nCapacitación",
  "Gestión cultural\nPromotor/a cultural",
  "Investigación\nPreservación\nDocumentación",
  "Producción\nDistribución\nExhibición"
)

# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[21:23], ]
) +
  aes(
    x = actividad, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Actividad desempeñada en el área de actuación cultural", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, actividad desempeñada en el ",
      "área de actuación cultural y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_actividad_disciplina_genero_artes_5.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC POR TIPO DE POBLACION ATENDIDA ===================================

datos <- reportes$basicos %>%
  select(id, gen) %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  left_join(select(reportes$cult_pobl, id, id_poblacion), by = "id") %>%
  left_join(
    select(catalogos$cat_cult_pob, id_pob, poblacion),
    by = c("id_poblacion" = "id_pob")
  ) %>%
  distinct() %>%
  drop_na() %>%
  group_by(poblacion) %>%
  summarise(total = n()) %>%
  filter(poblacion != "Ninguna de las anteriores") %>%
  ungroup() %>%
  # Porcentajes
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$poblacion <- factor(x = datos$poblacion)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$poblacion) <- c(
  "Adolescentes\n(12 a 17 años)",
  "Adultos\n(30 a 59 años)",
  "Comunidades diversidad\nsexual y de género",
  "Enfoque de género",
  "Enfoque incluyente",
  "Familiares de desaparecidos\ny desaparecidas",
  "Jóvenes\n(18 a 29 años)",
  "Mujeres",
  "Mujeres víctimas\nde violencias",
  "Otras",
  "Personas adultas mayores\n(Mayores de 60 años)",
  "Personas con discapacidad",
  "Personas privadas\nde la libertad",
  "Población afromexicana",
  "Población en condición\nde migración o refugio",
  "Población hospitalizada",
  "Población indígena",
  "Primera infancia\n(0 a 8 años)",
  "Segunda infancia\n(9 a 12 años)",
  "Todo público",
  "Víctimas directas\ne indirectas de violencias"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$poblacion <- factor(  # Variable de la que se reordenaran los niveles
  datos$poblacion,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos$poblacion,
        datos$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(poblacion), 
    y = porc,
    fill = color_oficial[2],
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[2]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Tipo de población con la que trabaja", 
    y = "Proporción de agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "distribución por tipo de población con la que trabaja"
    ),
    subtitle = "",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_poblacion.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TDEAC POR TIPO DE POBLACION ATENDIDA, DISCIP Y GENERO ========================

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(select(reportes$cult_pobl, id, id_poblacion), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & id_area == 3 & id %in% ids) %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  ) %>%
  left_join(
    select(catalogos$cat_cult_pob, id_pob, poblacion),
    by = c("id_poblacion" = "id_pob")
  ) %>%
  distinct() %>%
  drop_na()
# Este par de instrucciones garantizan que .drop = FALSE en el siguiente
# segmento funcione bien. Es necesario que las variables de agrupacion sean
# factores para que las combinaciones de conteo 0 se mantengan
datos$disciplina <- factor(x = datos$disciplina)
datos$gen <- factor(x = datos$gen)
datos$poblacion <- factor(x = datos$poblacion)

datos <- datos %>%
  group_by(disciplina, poblacion, gen) %>%
  summarise(total = n())

levels(datos$poblacion)  # Ejecutar esta instruccion para ver los niveles
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$poblacion) <- c(
  "Adolescentes\n(12 a 17 años)",
  "Adultos\n(30 a 59 años)",
  "Comunidades\ndiversidad sexual\ny de género",
  "Enfoque\nde género",
  "Enfoque\nincluyente",
  "Familiares\nde desaparecidos\ny desaparecidas",
  "Jóvenes\n(18 a 29 años)",
  "Mujeres",
  "Mujeres víctimas\nde violencias",
  "Otras",
  "Personas\nadultas mayores\n(Mayores de 60 años)",
  "Personas\ncon discapacidad",
  "Personas privadas\nde la libertad",
  "Población\nafromexicana",
  "Población\nen condición\nde migración o refugio",
  "Población\nhospitalizada",
  "Población\nindígena",
  "Primera infancia\n(0 a 8 años)",
  "Segunda infancia\n(9 a 12 años)",
  "Todo público",
  "Víctimas directas\ne indirectas\nde violencias"
)
  
# GRAFICA
image <- ggplot(
  datos[datos$disciplina %in% levels(datos$disciplina)[22:23], ]
) +
  aes(
    x = poblacion, 
    y = total,
    fill = gen,
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(.~disciplina) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Población a la que está dirigido su trabajo", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: total por disciplina, poblaciones a las que dirige ",
      "su trabajo y género"
    ),
    subtitle = "Agentes culturales que practican las artes vivas y escénicas",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      vjust = 0.5,  # Ajusta el texto en el centro del tick
      angle = 90  # Pone el texto en vertical
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "total_ac_poblaciones_disciplina_genero_artes_22.png"
  ), 
  plot = image, 
  width = 10, 
  height = 7
)

# TDEAC EXTRANJEROS ============================================================

datos <- reportes$basicos %>%
  filter(gen %in% c("Hombre", "Mujer"), pais_res != "México") %>%
  select(id, pais_res) %>%
  left_join(select(reportes$cult_area, id, id_area), by = "id") %>%
  drop_na()

# RELACION DE AÑOS DE EXPERIENCIA CON SU NIVEL DE INGRESO ======================

# Selecciona datos basicos
datos <- reportes$basicos %>%
  select(id, fec_nac, gen) %>%
  left_join(select(reportes$cult_area, id, inicio_practica), by = "id") %>%
  filter(
    nchar(fec_nac) == 10,
    nchar(inicio_practica) %in% c(4, 10),
    gen %in% c("Hombre", "Mujer")
  ) %>%
  select(-gen) %>%
  distinct() %>%
  drop_na()

# Obtiene edad
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
datos$edad <- 2020 - as.numeric(datos$year)
datos <- filter(datos, edad %in% c(18:100)) %>%
  select(-fec_nac, -year)

# Obtiene el tiempo que lleva de actividad cultural (experiencia)
# Obtiene los casos en que inicio_practica es una fecha
datos$t_act <- 0
datos$t_act[nchar(datos$inicio_practica) == 10] <- 2020 - as.numeric(
  format(
    as.Date(  # Convierte la fecha a formato estandar
      datos$inicio_practica[nchar(datos$inicio_practica) == 10],
      format = "%d/%m/%Y"
    ),
    format = "%Y"  # Obtiene el año despues de convertir la fecha a formato estandar
  )
)
# Obtiene los casos en que inicio_practica es un año
datos$t_act[nchar(datos$inicio_practica) != 10] <- 2020 - as.numeric(
  datos$inicio_practica[nchar(datos$inicio_practica) != 10]
)
datos <- filter(datos, t_act > 0)

# Obtiene diferencia entre edad y tiempo de actividad,
# para garantizar consistencia (diferencia no menor a 5 años)
datos <- datos %>%
  mutate(diferencia = edad - t_act) %>%
  filter(diferencia >= 5) %>%
  select(-diferencia, -inicio_practica)

# Anexa informacion de salarios
datos <- datos %>%
  # Ingresos
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  drop_na()

# Encontrando outliers
quantile(datos$for_ing, probs = seq(0, 1, 0.01))
# Filtra outliers (datos por arriba del cuantil 99)
datos <- datos %>%
  filter(for_ing <= quantile(datos$for_ing, probs = seq(0, 1, 0.01))[100])
# Otra posibilidad: cuantil 99.5
# datos <- datos %>%
#   filter(for_ing <= quantile(datos$for_ing, probs = seq(0, 1, 0.001))[996])

# Grafica
image <- ggplot(datos) +
  aes(
    x = t_act,
    y = for_ing
  ) +
  geom_point() +
  labs(
    x = "Años de experiencia",
    y = "Ingreso mensual",
    title = paste0(
      "Agentes culturales: ",
      "ingreso vs años de experiencia"
    ),
    subtitle = "",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

# ACCESO A SERVICIO MEDICO POR FASES DEL CICLO CULTURAL ========================

datos <- select(reportes$cult_acti, id, id_actividad) %>%
  left_join(
    select(
      reportes$sociodemo,
      id,
      soc_sm_01,
      soc_sm_02,
      soc_sm_03,
      soc_sm_04,
      soc_sm_05,
      soc_sm_06
    ),
    by = "id"
  ) %>%
  distinct() %>%
  drop_na() %>%
  select(-id) %>%
  group_by(id_actividad) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(
    -c(id_actividad),
    names_to = "sm",
    values_to = "total"
  ) %>%
  left_join(catalogos$cat_acti, by = "id_actividad") %>%
  relocate(actividad) %>%
  select(-id_actividad) %>%
  ungroup() %>%
  group_by(actividad) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$sm[datos$sm == "soc_sm_01_total"] <- "IMSS, ISSSTE, Pemex, Defensa o Marina"
datos$sm[datos$sm == "soc_sm_02_total"] <- "Seguro Popular"
datos$sm[datos$sm == "soc_sm_03_total"] <- "De un seguro privado"
datos$sm[datos$sm == "soc_sm_04_total"] <- "De otra institución"
datos$sm[datos$sm == "soc_sm_05_total"] <- "No tiene derecho a servicio médico"
datos$sm[datos$sm == "soc_sm_06_total"] <- "No sabe / no responde"

# Modifica el texto de los niveles para hacerlos mas graficables
datos$actividad <- factor(x = datos$actividad)
levels(datos$actividad) <- c(
  "Creación\nDiseño\nInterpretación",
  "Difusión\nDivulgación",
  "Docencia\nEnseñanza\nCapacitación",
  "Gestión cultural\nPromotor/a cultural",
  "Investigación\nPreservación\nDocumentación",
  "Producción\nDistribución\nExhibición"
)

datos$sm <- factor(x = datos$sm)
levels(datos$sm) <- c(
  "De otra institución",
  "De un seguro privado",
  "IMSS, ISSSTE, Pemex, Defensa o Marina",
  "No sabe / no responde",
  "No tiene derecho a servicio médico",
  "Seguro Popular"
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = actividad, 
    y = total,
    fill = fct_rev(
      fct_relevel(
        sm,
        "Seguro Popular",
        "De un seguro privado",
        "IMSS, ISSSTE, Pemex, Defensa o Marina",
        "De otra institución",
        "No sabe / no responde",
        "No tiene derecho a servicio médico"
        
      )
    ),
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.2,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(4, 3, 1, 2, 7:8)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Fase del ciclo cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "acceso a servicio médico por fases del ciclo cultural"
    ),
    subtitle = "",
    fill = "Servicio médico",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_sm_fases.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# ACCESO A SERVICIO MEDICO POR AREA CULTURAL ===================================

datos <- select(reportes$cult_area, id, id_area) %>%
  left_join(
    select(
      reportes$sociodemo,
      id,
      soc_sm_01,
      soc_sm_02,
      soc_sm_03,
      soc_sm_04,
      soc_sm_05,
      soc_sm_06
    ),
    by = "id"
  ) %>%
  distinct() %>%
  drop_na() %>%
  select(-id) %>%
  group_by(id_area) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(
    -c(id_area),
    names_to = "sm",
    values_to = "total"
  ) %>%
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  select(-id_area) %>%
  ungroup() %>%
  group_by(area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$sm[datos$sm == "soc_sm_01_total"] <- "IMSS, ISSSTE, Pemex, Defensa o Marina"
datos$sm[datos$sm == "soc_sm_02_total"] <- "Seguro Popular"
datos$sm[datos$sm == "soc_sm_03_total"] <- "De un seguro privado"
datos$sm[datos$sm == "soc_sm_04_total"] <- "De otra institución"
datos$sm[datos$sm == "soc_sm_05_total"] <- "No tiene derecho a servicio médico"
datos$sm[datos$sm == "soc_sm_06_total"] <- "No sabe / no responde"

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario\ny diseño",
  "Artes audiovisuales",
  "Artes de la\nrepresentación\ntradicionales",
  "Artes plásticas\ny visuales",
  "Artes vivas\ny escénicas",
  "Conocimientos y usos\nrelacionados con\nla naturaleza",
  "Interdisciplina",
  "Lengua, tradiciones\norales y narrativa",
  "Literatura",
  "Multimedia\ny arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y\nGestión Cultural"
)

datos$sm <- factor(x = datos$sm)
levels(datos$sm) <- c(
  "De otra institución",
  "De un seguro privado",
  "IMSS, ISSSTE, Pemex, Defensa o Marina",
  "No sabe / no responde",
  "No tiene derecho a servicio médico",
  "Seguro Popular"
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = area, 
    y = total,
    fill = fct_rev(
      fct_relevel(
        sm,
        "Seguro Popular",
        "De un seguro privado",
        "IMSS, ISSSTE, Pemex, Defensa o Marina",
        "De otra institución",
        "No sabe / no responde",
        "No tiene derecho a servicio médico"
      )
    ),
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.2,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(4, 3, 1, 2, 7:8)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "acceso a servicio médico por áreas culturales"
    ),
    subtitle = "",
    fill = "Servicio médico",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_sm_areas.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# INGRESOS Y NIVEL DE ESTUDIOS =================================================

datos <- select(reportes$basicos, id, gen) %>%
  # Ingresos
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na()

# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)

# Obtiene totales
datos <- datos %>%
  group_by(soc_est, ingreso2) %>%
  summarise(total = n()) %>%
  rename(ingreso = ingreso2) %>%
  ungroup() %>%
  # Porcentajes
  group_by(soc_est) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# Modifica los niveles del ingreso
datos$ingreso <- factor(x = datos$ingreso)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)
# Ordena los niveles de ingreso para que vayan de manera ascendente
datos$ingreso <- fct_rev(
  fct_relevel(
    datos$ingreso,
    "Hasta 1\n($3,697)",
    "Hasta 2\n($7,393)",
    "Hasta 3\n($11,090)",
    "3 o más"
  )
)

datos$soc_est <- factor(x = datos$soc_est)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$soc_est) <- c(
  "Carrera técnica con\nsecundaria terminada",
  "Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  "Ninguno",
  "No sabe / no responde",
  "Normal básica",
  "Preescolar",
  "Preparatoria o\nbachillerato completa",
  "Primaria",
  "Secundaria" 
)

# Reordena los niveles de acuerdo con una regla descendente
datos$soc_est <- factor(  # Variable de la que se reordenaran los niveles
  datos$soc_est,
  levels =  levels(
    reorder(
      datos[datos$ingreso == "Hasta 1\n($3,697)", ]$soc_est,
      datos[datos$ingreso == "Hasta 1\n($3,697)", ]$porc # Variable de referencia
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = soc_est,
    y = total,
    fill = ingreso,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(4, 1:2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Último grado de estudios",
    y = "",
    title = paste0(
      "Agentes culturales: distribución del nivel de ingreso en salarios ",
      "mínimos por último grado de estudios"
    ),
    subtitle = "",
    fill = "Salarios mínimos",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      "Salario mínimo de $123.22, de acuerdo con lo publicado en:\n",
      "https://www.gob.mx/cms/uploads/attachment/file/525061/Tabla_de_salarios_m_nmos_vigentes_apartir_del_01_de_enero_de_2020.pdf\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_salariosmin_estudios.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# TOTAL DE AC POR ULTIMO GRADO DE ESTUDIOS Y GENERO ============================

datos <- reportes$basicos %>%
  select(id, gen) %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  drop_na() %>%
  distinct() %>%
  group_by(soc_est, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(soc_est) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$soc_est <- factor(x = datos$soc_est)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$soc_est) <- c(
  "Carrera técnica con\nsecundaria terminada",
  "Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  "Ninguno",
  "No sabe / no responde",
  "Normal básica",
  "Preescolar",
  "Preparatoria o\nbachillerato completa",
  "Primaria",
  "Secundaria" 
)

# Reordena los niveles de acuerdo con una regla descendente
datos$soc_est <- factor(  # Variable de la que se reordenaran los niveles
  datos$soc_est,
  levels = rev(  # Orden descendente
    levels(
      reorder(
        datos$soc_est,
        datos$total # Variable de referencia
      )
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    # x = fct_relevel(
    #   soc_est,
    #   "No sabe / no responde",
    #   "Ninguno",
    #   "Preescolar",
    #   "Primaria",
    #   "Secundaria",
    #   "Carrera técnica con\nsecundaria terminada",
    #   "Preparatoria o\nbachillerato completa",
    #   "Normal básica",
    #   "Estudios técnicos con\npreparatoria terminada",
    #   "Licenciatura",
    #   "Maestría",
    #   "Doctorado"
    # ), 
    x = fct_rev(soc_est),
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    # position = "fill"  # Para barras apiladas de 100%
    position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    # position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "",
    y = "Último grado de estudios",
    title = paste0(
      "Agentes culturales: ",
      "distribución por género en el último grado de estudios"
    ),
    subtitle = "",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "stack_ac_estudios_genero_orden.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# MEDIO DE ADQUISICION DE SU CONOCIMIENTO CULTURAL Y GENERO ====================

datos <- select(reportes$formacion, id, for_apr) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na() %>%
  group_by(for_apr, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(for_apr) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# # Establece los factores
datos$for_apr <- factor(x = datos$for_apr)
levels(datos$for_apr) <- c(
  "Asistencia a cursos,\nclases y talleres",
  "Carrera técnica con\nespecialidad en artes\ncon secundaria terminada",
  "Cursó algún diplomado\no taller en artes",
  "Cursó estudios\nde doctorado",
  "Cursó estudios\nde maestría",
  "Enseñanza de amigos\ny/o compañeros",
  "Enseñanza de padres\ny/o familiares directos",
  "Estudios de\nbachillerato\nen artes",
  "Otra",
  "Por su propia\ncuenta",
  "Tiene estudios\nde licenciatura",
  "Tutoriales en\ninternet o televisión"
)
datos$gen <- factor(x = datos$gen)

# Reordena los niveles de acuerdo con una regla descendente
datos$for_apr <- factor(  # Variable de la que se reordenaran los niveles
  datos$for_apr,
  levels = levels(
    reorder(
      datos$for_apr,
      datos$total # Variable de referencia
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = for_apr, 
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(
    stat = "identity",
    # position = "fill"  # Para barras apiladas de 100%
    position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Manera en que aprendió su conocimiento en el campo cultural", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: distribución de la manera en que aprendió su ",
      "conocimiento en el campo cultural por género"
    ),
    subtitle = "",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "stack_ac_manera_genero_orden.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# BRECHA SALARIAL POR AREAS DE ACTUACION Y GENERO ==============================

datos <- select(reportes$cult_area, id, id_area) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na()

# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)

# Encontrando outliers
quantile(datos$for_ing, probs = seq(0, 1, 0.01))
# Filtra outliers (datos por arriba del cuantil 99)
datos <- datos %>%
  filter(for_ing <= quantile(datos$for_ing, probs = seq(0, 1, 0.01))[100])
# Otra posibilidad: cuantil 99.5
# datos <- datos %>%
#   filter(for_ing <= quantile(datos$for_ing, probs = seq(0, 1, 0.001))[996])

# Obtiene totales
datos <- datos %>%
  select(-id, -id_area, -ingreso, -cont) %>%
  rename(ingreso = ingreso2) %>%
  group_by(area, gen, ingreso) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(area, gen) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# Modifica los niveles del ingreso
datos$ingreso <- factor(x = datos$ingreso)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)
# Ordena los niveles de ingreso para que vayan de manera ascendente
datos$ingreso <- fct_rev(
  fct_relevel(
    datos$ingreso,
    "Hasta 1\n($3,697)",
    "Hasta 2\n($7,393)",
    "Hasta 3\n($11,090)",
    "3 o más"
  )
)

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)

# GRAFICA
image <- ggplot(datos[97:112, ]) +
  aes(
    x = gen, 
    y = total,
    fill = ingreso,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(.~area) +  # Separa en distintas vistas
  geom_text(
    position = position_fill(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(4, 1:2, 7)]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Género", 
    y = "",
    title = paste0(
      "Agentes culturales: distribución del nivel de ingreso en salarios",
      " mínimos por género y área de actuación cultural"
    ),
    subtitle = "",
    fill = "Ingreso en salarios mínimos",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      "Salario mínimo de $123.22, de acuerdo con lo publicado en:\n",
      "https://www.gob.mx/cms/uploads/attachment/file/525061/Tabla_de_salarios_m_nmos_vigentes_apartir_del_01_de_enero_de_2020.pdf\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "dist_ac_area_genero_ingreso_5.png"
  ), 
  plot = image, 
  width = 12, 
  height = 7
)

# TDEAC POR SITIO DONDE LLEVA A CABO SU ACTIVIDAD Y GENERO =====================

datos <- select(reportes$basicos, id, gen) %>%
  left_join(select(reportes$movilidad, id, mov_s), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na() %>%
  select(-id)
x <- c(
  paste0(
    "Lugares especializados privados en la disciplina que realizo como: ",
    "teatros, salas de conciertos, ferias de artesanías, galerías, museos, etc."
  ),
  paste0(
    "Lugares especializados públicos en la disciplina que realizo como: ",
    "teatros, salas de conciertos, ferias de artesanías, galerías, museos, etc."
  )
)
datos$mov_s[datos$mov_s %in% x] <- paste0(
  "Lugares especializados en la disciplina que realizo como: ",
  "teatros, salas de conciertos, ferias de artesanías, galerías, museos, etc."
)
rm(x)
datos <- datos %>%
  group_by(mov_s, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(mov_s) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# Establece los factores
datos$mov_s <- factor(x = datos$mov_s)
levels(datos$mov_s) <- c(
  "Espacios privados",
  "Espacios públicos\nabiertos",
  "Espacios públicos\ncerrados",
  "Lugares\nespecializados",
  "Otros espacios"  
)

# Reordena los niveles de acuerdo con una regla descendente
datos$mov_s <- factor(  # Variable de la que se reordenaran los niveles
  datos$mov_s,
  levels = levels(
    reorder(
      datos[datos$gen == "Mujer", ]$mov_s,
      datos[datos$gen == "Mujer", ]$porc # Variable de referencia
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = mov_s, 
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    position = position_fill(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Sitios donde principalmente lleva a cabo su actividad cultural", 
    y = "",
    title = paste0(
      "Agentes culturales: distribución por sitios donde principalmente",
      " lleva a cabo su actividad cultural y género"
    ),
    subtitle = "",
    fill = "Género",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "dist_ac_sitios_genero.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)


# ORIGEN DE LOS RECURSOS EMPLEADOS PARA MOVILIDAD POR GENERO ===================

datos <- select(reportes$basicos, id, gen) %>%
  left_join(
    select(
      reportes$movilidad,
      id,
      mov_t_01,
      mov_t_02,
      mov_t_03,
      mov_t_04,
      mov_t_05,
      mov_t_06,
      mov_t_07
    ),
    by = "id"
  ) %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na() %>%
  select(-id) %>%
  group_by(gen) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(
    -c(gen),
    names_to = "mov",
    values_to = "total"
  ) %>%
  ungroup() %>%
  relocate(mov, gen) %>%
  group_by(mov) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$mov[datos$mov == "mov_t_01_total"] <- "Recursos propios"
datos$mov[datos$mov == "mov_t_02_total"] <- "Recursos generados por la venta de bienes y/o servicios relacionados a la actividad cultural que realizo"
datos$mov[datos$mov == "mov_t_03_total"] <- "Apoyos gubernamentales nacionales"
datos$mov[datos$mov == "mov_t_04_total"] <- "Donaciones o aportaciones de empresas"
datos$mov[datos$mov == "mov_t_05_total"] <- "Donaciones o aportaciones de familiares y amigos"
datos$mov[datos$mov == "mov_t_06_total"] <- "Donaciones o aportaciones de organizaciones comunitarias"
datos$mov[datos$mov == "mov_t_07_total"] <- "Apoyos internacionales"

# Modifica el texto de los niveles para hacerlos mas graficables
datos$mov <- factor(x = datos$mov)
levels(datos$mov) <- c(
  "Apoyos gubernamentales\nnacionales",
  "Apoyos internacionales",
  "Donaciones o aportaciones\nde empresas",
  "Donaciones o aportaciones\nde familiares y amigos",
  "Donaciones o aportaciones\nde organizaciones comunitarias",
  "Recursos generados por la venta\nde bienes y/o servicios relacionados a\n la actividad cultural que realizo",
  "Recursos propios"
)

# Reordena los niveles de acuerdo con una regla descendente
datos$mov <- factor(  # Variable de la que se reordenaran los niveles
  datos$mov,
  levels = levels(
    reorder(
      datos[datos$gen == "Mujer", ]$mov,
      datos[datos$gen == "Mujer", ]$porc # Variable de referencia
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = mov, 
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.2,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Origen de los recursos con los que solventa su movilidad",
    y = "",
    title = paste0(
      "Agentes culturales: distribución del origen de los recursos con los",
      " que solventa su movilidad por género"
    ),
    subtitle = "",
    fill = "Género",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_mov_gen.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# MOVILIDAD POR GENERO =========================================================

datos <- select(reportes$basicos, id, gen) %>%
  left_join(
    select(
      reportes$movilidad,
      id,
      mov_tev_01,
      mov_tev_02,
      mov_tev_03,
      mov_tev_04,
      mov_tev_05,
      mov_tev_06,
      mov_tev_07,
      mov_tev_08,
      mov_tev_09,
      mov_tev_11
    ),
    by = "id"
  ) %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na() %>%
  select(-id) %>%
  group_by(gen) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(
    -c(gen),
    names_to = "tev",
    values_to = "total"
  ) %>%
  ungroup() %>%
  relocate(tev, gen) %>%
  group_by(tev) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$tev[datos$tev == "mov_tev_01_total"] <- "Muestras"
datos$tev[datos$tev == "mov_tev_02_total"] <- "Festivales"
datos$tev[datos$tev == "mov_tev_03_total"] <- "Ferias"
datos$tev[datos$tev == "mov_tev_04_total"] <- "Conferencias"
datos$tev[datos$tev == "mov_tev_05_total"] <- "Exposiciones"
datos$tev[datos$tev == "mov_tev_06_total"] <- "Caravana artística"
datos$tev[datos$tev == "mov_tev_07_total"] <- "Concierto"
datos$tev[datos$tev == "mov_tev_08_total"] <- "Expo venta"
datos$tev[datos$tev == "mov_tev_09_total"] <- "Feria del libro"
datos$tev[datos$tev == "mov_tev_11_total"] <- "Otra"

# Modifica el texto de los niveles para hacerlos mas graficables
datos$tev <- factor(x = datos$tev)

# Reordena los niveles de acuerdo con una regla descendente
datos$tev <- factor(  # Variable de la que se reordenaran los niveles
  datos$tev,
  levels = levels(
    reorder(
      datos[datos$gen == "Mujer", ]$tev,
      datos[datos$gen == "Mujer", ]$porc # Variable de referencia
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = tev, 
    y = total,
    fill = gen,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.2,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Eventos donde ha llevado a cabo su práctica cultural",
    y = "",
    title = paste0(
      "Agentes culturales: distribución de los sitios donde han llevado a cabo",
      " su práctica cultural por género"
    ),
    subtitle = "",
    fill = "Género",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_tev_gen.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# DATOS GENERALES DE OTROS GENEROS =============================================

ids <- unique(
  reportes$basicos[
    !reportes$basicos$gen %in% c("Hombre", "Mujer") & !is.na(reportes$basicos$gen),
    "id"
  ]
)

# Areas culturales
datos <- select(reportes$cult_area, id, id_area) %>%
  filter(id %in% ids) %>%
  left_join(select(catalogos$cat_area, id_area, area), by = "id_area") %>%
  distinct() %>%
  drop_na() %>%
  group_by(area) %>%
  summarise(total = n())

# Ultimo grado de estudios
datos <- select(reportes$sociodemo, id, soc_est) %>%
  filter(id %in% ids) %>%
  distinct() %>%
  drop_na() %>%
  group_by(soc_est) %>%
  summarise(total = n())

# Ingresos
datos <- select(reportes$formacion, id, for_ing) %>%
  filter(id %in% ids) %>%
  distinct() %>%
  drop_na()
# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)
# Obtiene totales
datos <- datos %>%
  group_by(ingreso2) %>%
  summarise(total = n()) %>%
  rename(ingreso = ingreso2) %>%
  ungroup()

# BRECHA SALARIAL POR AREAS DE ACTUACION Y PERTENENCIA A GRUPO INDIGENA ========

datos <- select(reportes$cult_area, id, id_area) %>%
  left_join(select(reportes$basicos, id, grp_ind), by = "id") %>%
  left_join(select(reportes$formacion, id, for_ing), by = "id") %>%
  # Areas
  left_join(catalogos$cat_area, by = "id_area") %>%
  relocate(area) %>%
  filter(!is.na(grp_ind)) %>%
  distinct() %>%
  drop_na() %>%
  ungroup() %>%
  mutate(grp_ind = ifelse(grp_ind == 1, "Pertenece", "No pertenece"))

# Obtiene categorias de salarios minimos
sm <- 123.22
smm <- sm*30
datos$ingreso <- ""
datos$ingreso2 <- ""
datos$cont <- 0
for (j in 1:3) {
  sup <- smm*j - 1
  inf <- smm*(j - 1)
  log <- datos$for_ing >= inf & datos$for_ing < sup
  datos$ingreso[log]  <- paste0(smm*(j - 1), " - ", smm*j - 1)
  datos$cont[log] <- j
  datos$ingreso2[log] <- paste0(
    "Hasta ", 
    j,
    " ($",
    formatC(
      smm*j, 
      format = "f", 
      digits = 0, 
      big.mark = ","
    ),
    ")"
  )
}
datos$ingreso[datos$ingreso == ""] <- paste0(smm*(j - 1), " - en adelante")
datos$ingreso2[datos$ingreso2 == ""] <- "3 o más"
datos$cont[datos$cont == 0] <- j + 1
rm(inf, j, log, sm, smm, sup)

# Encontrando outliers
quantile(datos$for_ing, probs = seq(0, 1, 0.01))
# Filtra outliers (datos por arriba del cuantil 99)
datos <- datos %>%
  filter(for_ing <= quantile(datos$for_ing, probs = seq(0, 1, 0.01))[100])
# Otra posibilidad: cuantil 99.5
# datos <- datos %>%
#   filter(for_ing <= quantile(datos$for_ing, probs = seq(0, 1, 0.001))[996])

# Obtiene totales
datos <- datos %>%
  select(-id, -id_area, -ingreso, -cont) %>%
  rename(ingreso = ingreso2) %>%
  group_by(area, grp_ind, ingreso) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(area, grp_ind) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# Modifica los niveles del ingreso
datos$ingreso <- factor(x = datos$ingreso)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$ingreso) <- c(
  "3 o más",
  "Hasta 1\n($3,697)",
  "Hasta 2\n($7,393)",
  "Hasta 3\n($11,090)"
)
# Ordena los niveles de ingreso para que vayan de manera ascendente
datos$ingreso <- fct_rev(
  fct_relevel(
    datos$ingreso,
    "Hasta 1\n($3,697)",
    "Hasta 2\n($7,393)",
    "Hasta 3\n($11,090)",
    "3 o más"
  )
)

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)

# GRAFICA
image <- ggplot(datos[93:108, ]) +
  aes(
    x = grp_ind, 
    y = total,
    fill = ingreso,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(.~area) +  # Separa en distintas vistas
  geom_text(
    position = position_fill(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(4, 1:2, 7)]) +
  scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Pertenencia a grupo indígena", 
    y = "",
    title = paste0(
      "Agentes culturales: distribución del nivel de ingreso en salarios",
      " mínimos por área de actuación cultural y pertenencia a grupo indígena"
    ),
    subtitle = "",
    fill = "Ingreso en salarios mínimos",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      "Salario mínimo de $123.22, de acuerdo con lo publicado en:\n",
      "https://www.gob.mx/cms/uploads/attachment/file/525061/Tabla_de_salarios_m_nmos_vigentes_apartir_del_01_de_enero_de_2020.pdf\n\n",
      nota
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "dist_ac_area_grp_ind_ingreso_5.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# MODALIDAD DE REGISTRO POR GRUPOS ETARIOS =====================================

obtener_cortes <- function(vector_edad, corte) {
  if (corte == 0) {
    return(
      case_when(
        vector_edad <= 29 ~ "18 a 29 años",
        vector_edad <= 59 ~ "30 a 59 años",
        TRUE ~ "60 años y más"
      )
    )
  } else {
    return(
      ifelse(
        (vector_edad + corte - 1)%/%(corte)*(corte) == vector_edad,
        paste0(
          vector_edad - (corte - 1),
          " a ",
          vector_edad,
          " años"
        ),
        paste0(
          (vector_edad + corte - 1)%/%(corte)*(corte) - corte + 1,
          " a ",
          (vector_edad + corte - 1)%/%(corte)*(corte),
          " años"
        )
      )
    )
  }
}

datos <- reportes$basicos %>%
  filter(gen %in% c("Hombre", "Mujer") & nchar(fec_nac) == 10) %>%
  select(modalidad, fec_nac)

# SE OBTIENE LA EDAD A PARTIR DE LA FECHA DE NACIMIENTO
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
rm(datos_s, obtener_cortes)

# SEGMENTACION POR GRUPOS ETARIOS
datos$edad <- 2020 - as.numeric(datos$year)
datos <- filter(datos, edad %in% c(18:100))
datos$etario <- obtener_cortes(datos$edad, corte = 0)

# Obtiene totales
datos <- datos %>%
  group_by(etario, modalidad) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(etario) %>%
  mutate(porc = total/sum(total))

# Grafica
image <- ggplot(datos) +
  aes(
    x = etario, 
    y = total, 
    fill = modalidad, 
    # label = format(total, big.mark = ",")
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    position = position_dodge(width = 1),
    fontface = "bold",
    size = 4,
    color = "white",
    vjust = 2
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Grupos etarios", 
    y = "",
    title = "Agentes culturales: modalidad de registro por grupos etarios",
    subtitle = "",
    fill = "Modalidad de registro",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
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
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "mod_etarios.png"
  ), 
  plot = image, 
  width = 7, 
  height = 7
)

# ULTIMO GRADO DE ESTUDIOS Y MODALIDAD DE REGISTRO =============================

datos <- select(reportes$basicos, id, gen, modalidad) %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na() %>%
  select(soc_est, modalidad) %>%
  group_by(soc_est, modalidad) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(soc_est) %>%
  mutate(porc = total/sum(total))

datos$soc_est <- factor(x = datos$soc_est)
levels(datos$soc_est) <- c(
  "Carrera técnica con\nsecundaria terminada",
  "Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  "Ninguno",
  "No sabe / no responde",
  "Normal básica",
  "Preescolar",
  "Preparatoria o\nbachillerato completa",
  "Primaria",
  "Secundaria" 
)

# Reordena los niveles de acuerdo con una regla descendente
datos$soc_est <- factor(  # Variable de la que se reordenaran los niveles
  datos$soc_est,
  levels = levels(
    reorder(
      datos[datos$modalidad == "B", ]$soc_est,
      datos[datos$modalidad == "B", ]$porc # Variable de referencia
    )
  )
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = soc_est, 
    y = total,
    fill = modalidad,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
  ) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    position = position_fill(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Último grado de estudios", 
    y = "",
    title = paste0(
      "Agentes culturales: distribución de la modalidad de registro ",
      "por último grado de estudios"
    ),
    subtitle = "",
    fill = "Modalidad de registro",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "dist_mod_estudios.png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# DISTRIBUCION DE FASES DEL CICLO CULTURAL EN LAS AREAS DE ACTUACION ===========

datos <- reportes$cult_acti %>%
  distinct() %>%
  drop_na() %>%
  # Totales
  group_by(id_area, id_actividad) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(id_area) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Areas y actividades
  left_join(catalogos$cat_area, by = "id_area") %>%
  left_join(catalogos$cat_acti, by = "id_actividad") %>%
  relocate(area, actividad) %>%
  select(-id_area, -id_actividad) %>%
  ungroup()

# Modifica el texto de los niveles para hacerlos mas graficables
datos$area <- factor(x = datos$area)
levels(datos$area) <- c(
  "Arte utilitario\ny diseño",
  "Artes audiovisuales",
  "Artes de la\nrepresentación\ntradicionales",
  "Artes plásticas\ny visuales",
  "Artes vivas\ny escénicas",
  "Conocimientos y usos\nrelacionados con\nla naturaleza",
  "Interdisciplina",
  "Lengua,\ntradiciones orales\ny narrativa",
  "Literatura",
  "Multimedia\ny arte digital",
  "Música",
  "Prácticas artesanales\ny oficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y\nGestión Cultural"
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(area), 
    y = total,
    fill = actividad,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 5,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(3:4, 6, 2, 8:7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Área de actuación cultural",
    y = "",
    title = paste0(
      "Agentes culturales: distribución de las fases del ciclo cultural en las",
      " áreas de actuación"
    ),
    subtitle = "",
    fill = "Fases del ciclo cultural",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "dist_fases_areas.png"
  ), 
  plot = image, 
  width = 14, 
  height = 10
)

# DISTRIBUCION DE FASES DEL CICLO CULTURAL POR ENTIDAD =========================

datos <- reportes$cult_acti %>%
  left_join(select(reportes$basicos, id, cve_ent_res), by = "id") %>%
  filter(cve_ent_res != 1) %>%
  distinct() %>%
  drop_na() %>%
  # Totales
  group_by(cve_ent_res, id_actividad) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  # Porcentajes
  group_by(cve_ent_res) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup() %>%
  # Fases y entidades
  left_join(catalogos$cat_acti, by = "id_actividad") %>%
  left_join(
    select(catalogos$cat_edos, id_ent, entidad),
    by = c("cve_ent_res" = "id_ent")
  ) %>%
  relocate(entidad, actividad) %>%
  select(-cve_ent_res, -id_actividad) %>%
  ungroup()

# Modifica el texto de los niveles para hacerlos mas graficables
datos$entidad <- factor(x = datos$entidad)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = fct_rev(entidad),
    y = total,
    fill = actividad,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 5,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(3:4, 6, 2, 8:7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Entidad",
    y = "",
    title = paste0(
      "Agentes culturales: distribución de las fases del ciclo cultural en las",
      " entidades"
    ),
    subtitle = "",
    fill = "Fases del ciclo cultural",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "dist_fases_entidades.png"
  ), 
  plot = image, 
  width = 15, 
  height = 15
)

# DISTRIBUCION DEL ACCESO A FINANCIAMIENTO POR FASE DEL CICLO CULTURAL =========

datos <- reportes$cult_acti %>%
  left_join(
    select(
      reportes$movilidad,
      id,
      mov_t_01,
      mov_t_02,
      mov_t_03,
      mov_t_04,
      mov_t_05,
      mov_t_06,
      mov_t_07
    ),
    by = "id"
  ) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer")) %>%
  distinct() %>%
  drop_na() %>%
  select(-id, -id_area, -gen) %>%
  group_by(id_actividad) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(
    -c(id_actividad),
    names_to = "mov",
    values_to = "total"
  ) %>%
  ungroup() %>%
  left_join(catalogos$cat_acti, by = "id_actividad") %>%
  relocate(actividad, mov) %>%
  select(-id_actividad) %>%
  group_by(actividad) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

datos$mov[datos$mov == "mov_t_01_total"] <- "Recursos propios"
datos$mov[datos$mov == "mov_t_02_total"] <- "Recursos generados por la venta de bienes y/o servicios relacionados a la actividad cultural que realizo"
datos$mov[datos$mov == "mov_t_03_total"] <- "Apoyos gubernamentales nacionales"
datos$mov[datos$mov == "mov_t_04_total"] <- "Donaciones o aportaciones de empresas"
datos$mov[datos$mov == "mov_t_05_total"] <- "Donaciones o aportaciones de familiares y amigos"
datos$mov[datos$mov == "mov_t_06_total"] <- "Donaciones o aportaciones de organizaciones comunitarias"
datos$mov[datos$mov == "mov_t_07_total"] <- "Apoyos internacionales"

# Modifica el texto de los niveles para hacerlos mas graficables
datos$mov <- factor(x = datos$mov)
datos$actividad <- factor(x = datos$actividad)
levels(datos$actividad) <- c(
  "Creación\nDiseño\nInterpretación",
  "Difusión\nDivulgación",
  "Docencia\nEnseñanza\nCapacitación",
  "Gestión cultural\nPromotor/a cultural",
  "Investigación\nPreservación\nDocumentación",
  "Producción\nDistribución\nExhibición"
)

# GRAFICA
image <- ggplot(datos) +
  aes(
    x = actividad, 
    y = total,
    fill = mov,
    label = percent(porc, accuracy = 0.01)  # Porcentajes
    # label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),  # Usar con barras apiladas 100%
    # position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 3.2,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[rev(c(4, 3, 1, 2, 6:8))]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  # scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Fase del ciclo cultural",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: distribución del origen de los recursos con los que",
      " solventa su movilidad por fases del ciclo cultural"
    ),
    subtitle = "",
    fill = "Origen de los recursos",
    caption = paste0(
      # "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
      nota
    )
  ) +
  coord_flip() +
  guides(
    fill = guide_legend(
      title.position = "top",
      ncol = 2
    )
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    # axis.text.x = element_text(
    #   angle = 90,  #Pone el texto en vertical
    #   vjust = 0.5  # Ajusta el texto en el centro del tick
    # ),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "porc_ac_recursos_fases.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)
