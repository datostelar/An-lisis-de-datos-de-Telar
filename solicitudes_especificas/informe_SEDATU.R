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
    "scales"
  ),
  library,
  character.only = TRUE
)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_15_09_2020.RData")
load("INEGI\\cat_ent_mun.RData")
load("colores_oficiales.RData")
load("catalogos.RData")

# DEFINICION DE NOTA PARA LAS GRAFICAS =========================================

nota <- paste0(
  "Información captada del 25 de mayo de 2019 al 15 de septiembre de 2020 a través ",
  "de la plataforma Telar.\nRegistro Nacional de Espacios, Prácticas y Agentes ",
  "Culturales.\nDirección de Animación Cultural. Dirección General de ",
  "Vinculación Cultural. Secretaría de Cultura."
)

# MUNICIPIOS ===================================================================

# Campeche 25
# Los Cabos 21
# San Luis Rio Colorado 1955
# Juarez 193
# Mexicali 14
# Solidaridad 1820
# Tijuana 17
# Acapulco 374
# Coatzacoalcos 2139
# Oaxaca 1077
# Centro 1973
# Nicolas Romero 726
# Cuautitlan Izcalli 695

mun <- 695
sub <- "Cuautitlán Izcalli, Estado de México"
txt <- "izcalli"

# TOTAL DE AC POR GENERO =======================================================

datos <- select(reportes$basicos, id, gen, cve_mun_res) %>%
  left_join(select(reportes$sociodemo, id, soc_ind), by = "id") %>%
  drop_na() %>%
  filter(gen %in% c("Hombre", "Mujer") & cve_mun_res == mun) %>%
  group_by(gen, soc_ind) %>%
  summarise(total = n())

datos$soc_ind[datos$soc_ind == 0] <- "No"
datos$soc_ind[datos$soc_ind == 1] <- "Sí"
datos$soc_ind <- factor(x = datos$soc_ind)

# GRAFICA 
image <- ggplot(datos) +
  aes(
    x = gen, 
    y = total, 
    fill = soc_ind, 
    label = format(total, big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    position = position_stack(vjust = 1),
    fontface = "bold",
    size = 5,
    color = "white",
    vjust = 2
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  #scale_y_continuous(trans = "log") +  # Escala logaritmica
  labs(
    x = "", 
    y = "",
    title = "Agentes culturales: total por género y autoadscripción indígena",
    subtitle = sub,
    fill = "¿Se considera indígena?",
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
    "tot_gen_",
    txt,
    ".png"
  ), 
  plot = image, 
  width = 7, 
  height = 7
)

# TOTAL DE AC POR AREAS, DISCIPLINAS Y GENERO ==================================

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen, cve_mun_res), by =  "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & cve_mun_res == mun) %>%
  distinct() %>%
  drop_na() %>%
  left_join(catalogos$cat_area, by = "id_area") %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  )

datos$disciplina <- factor(x = datos$disciplina)
datos$area <- factor(x = datos$area)
datos <- datos %>%
  group_by(area, disciplina, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(area, disciplina) %>%
  mutate(tot_dis = sum(total)) %>%
  ungroup() %>%
  group_by(area) %>%
  mutate(tot_area = sum(total)) %>%
  arrange(desc(tot_area), desc(tot_dis))

levels(datos$area) <- c(
  #"Arte utilitario y diseño",
  "Artes audiovisuales",
  #"Artes de la representación\ntradicionales",
  "Artes plásticas y visuales",
  "Artes vivas y escénicas",
  #"Conocimientos y usos\nrelacionados con la naturaleza",
  "Interdisciplina",
  "Lengua, tradiciones orales\ny narrativa",
  #"Literatura",
  "Multimedia y arte digital",
  "Música",
  "Prácticas artesanales y\noficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y Gestión Cultural"
)
levels(datos$disciplina)
levels(datos$disciplina)[levels(datos$disciplina) == "Conocimientos tradicionales sobre diversidad y medio ambiente"] <- "Conocimientos tradicionales sobre\ndiversidad y medio ambiente"
levels(datos$disciplina)[levels(datos$disciplina) == "Conocimiento de plantas medicinales (Herbolaria)"] <- "Conocimiento de plantas medicinales\n(Herbolaria)"
levels(datos$disciplina)[levels(datos$disciplina) == "Arte en internet / Net Art / Redes sociales"] <- "Arte en internet / Net Art\nRedes sociales"

span <- c(36:44)

# GRAFICA
image <- ggplot(datos[span, ]) +
  aes(
    x = fct_relevel(
      disciplina,
      rev(c(unique(as.character(datos$disciplina[span]))))
    ), 
    y = total, 
    fill = gen, 
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(
    rows = vars(area),
    scales = "free",
    space = "free"
  ) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Disciplina ejercida", 
    y = "",
    title = "Agentes culturales: total por área, disciplina y género",
    subtitle = sub,
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
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.x = element_text(
    #   angle = 90
    # ),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    strip.text.y = element_text(angle = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "tot_disc_gen_",
    txt,
    "3.png"
  ), 
  plot = image,
  width = 10,
  height = 7
)

write.xlsx(datos, paste0("areas_gen_", txt, ".xlsx"))

# MEDIO DE ADQUISICION DE SU CONOCIMIENTO CULTURAL Y GENERO ====================

datos <- select(reportes$formacion, id, for_apr) %>%
  left_join(select(reportes$basicos, id, gen, cve_mun_res), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & cve_mun_res == mun) %>%
  distinct() %>%
  drop_na() %>%
  group_by(for_apr, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(for_apr) %>%
  mutate(porc = total/sum(total)) %>%
  ungroup()

# Establece los factores
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
  "Tiene estudios\nde licenciatura"
  #"Tutoriales en\ninternet o televisión"
)
datos$gen <- factor(x = datos$gen)

# Reordena los niveles de acuerdo con una regla descendente
datos$for_apr <- factor(  # Variable de la que se reordenaran los niveles
  datos$for_apr,
  levels = levels(
    reorder(
      datos[datos$gen == "Mujer", ]$for_apr,
      datos[datos$gen == "Mujer", ]$porc # Variable de referencia
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
    position = "fill"  # Para barras apiladas de 100%
    # position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Manera en que aprendió su conocimiento en el campo cultural", 
    y = "",
    title = paste0(
      "Agentes culturales: distribución de la manera en que aprendió su ",
      "conocimiento en el campo cultural por género"
    ),
    subtitle = sub,
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
    "manera_gen_",
    txt,
    ".png"
  ), 
  plot = image, 
  width = 13, 
  height = 7
)

# TOTAL DE AC POR ULTIMO GRADO DE ESTUDIOS Y GENERO ============================

datos <- reportes$basicos %>%
  select(id, gen, cve_mun_res) %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & cve_mun_res == mun) %>%
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
  #"Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  #"Ninguno",
  "No sabe / no responde",
  #"Normal básica",
  #"Preescolar",
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
        datos[datos$gen == "Mujer", ]$soc_est,
        datos[datos$gen == "Mujer", ]$porc # Variable de referencia
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
    x = "",
    y = "Último grado de estudios",
    title = paste0(
      "Agentes culturales: ",
      "distribución por género en el último grado de estudios"
    ),
    subtitle = sub,
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
    "estudios_gen_",
    txt,
    ".png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

# TOTAL DE AC POR AREAS Y ULTIMO GRADO DE ESTUDIOS =============================

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, gen, cve_mun_res), by =  "id") %>%
  left_join(select(reportes$sociodemo, id, soc_est), by = "id") %>%
  filter(gen %in% c("Hombre", "Mujer") & cve_mun_res == mun) %>%
  distinct() %>%
  drop_na() %>%
  left_join(catalogos$cat_area, by = "id_area")

datos$area <- factor(x = datos$area)
datos <- datos %>%
  group_by(area, soc_est) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(area) %>%
  mutate(tot_area = sum(total)) %>%
  arrange(desc(tot_area), desc(total))

levels(datos$area) <- c(
  #"Arte utilitario y diseño",
  "Artes audiovisuales",
  #"Artes de la representación tradicionales",
  "Artes plásticas y visuales",
  "Artes vivas y escénicas",
  #"Conocimientos y usos\nrelacionados con la naturaleza",
  "Interdisciplina",
  #"Lengua, tradiciones orales\ny narrativa",
  #"Literatura",
  "Multimedia y arte digital",
  "Música",
  "Prácticas artesanales y\noficio tradicional",
  "Prácticas sociales,\nrituales y festividades",
  "Promoción y Gestión Cultural"
)
datos$soc_est <- factor(x = datos$soc_est)
# Modificar los niveles obtenidos con la instruccion anterior:
levels(datos$soc_est) <- c(
  "Carrera técnica con\nsecundaria terminada",
  #"Doctorado",
  "Estudios técnicos con\npreparatoria terminada",
  "Licenciatura",
  "Maestría",
  #"Ninguno",
  "No sabe / no responde",
  #"Normal básica",
  #"Preescolar",
  "Preparatoria o\nbachillerato completa",
  "Primaria",
  "Secundaria"
)

span <- c(11:14)

# GRAFICA
image <- ggplot(datos[c(span), ]) +
  aes(
    x = fct_relevel(
      soc_est,
      rev(c(unique(as.character(datos$soc_est[span]))))
    ), 
    y = total, 
    fill = color_oficial[c(2)],
    label = format(ifelse(total == 0, "", total), big.mark = ",")
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(
    rows = vars(area),
    scales = "free",
    space = "free"
  ) +  # Separa en distintas vistas
  geom_text(
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 3.5,
    color = "white"
  ) + 
  scale_fill_manual(values = color_oficial[c(2)]) +
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Último grado de estudios", 
    y = "",
    title = "Agentes culturales: total por área y último grado de estudios",
    subtitle = sub,
    fill = "",
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
    # axis.text.x = element_text(
    #   angle = 90
    # ),
    legend.position = "none",
    plot.caption = element_text(hjust = 0),
    strip.text.y = element_text(angle = 0)
  )

plot(image)

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "area_est_",
    txt,
    "3.png"
  ), 
  plot = image, 
  width = 11, 
  height = 7
)

write.xlsx(datos, paste0("areas_est_", txt, ".xlsx"))
