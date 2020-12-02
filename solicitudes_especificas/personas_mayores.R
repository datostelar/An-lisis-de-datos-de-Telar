# PERSONAS MAYORES DE 57 AÑOS

# PAQUETES =====================================================================

lapply(
  c("openxlsx", "ggplot2", "dplyr", "tidyr", "forcats", "extrafont"),
  library,
  character.only = TRUE
)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_gen_07_08_2020.RData")
load("INEGI\\cat_ent_mun.RData")
load("colores_oficiales.RData")
load("catalogos.RData")

# DEFINICION DE NOTA PARA LAS GRAFICAS =========================================

nota <- paste0(
  "Información captada del 25 de mayo de 2019 al 7 de agosto de 2020 a través ",
  "de la plataforma Telar.\nRegistro Nacional de Espacios, Prácticas y Agentes ",
  "Culturales.\nDirección de Animación Cultural. Dirección General de ",
  "Vinculación Cultural. Secretaría de Cultura."
)

# PERSONAS MAYORES POR GENERO ==================================================

# Selecciona datos basicos
datos <- reportes_gen$basicos %>%
  select(id, genero, curp, estado_residencia) %>%
  left_join(select(reportes_gen$cult_area, id, inicio_practica), by = "id") %>%
  distinct() %>%
  drop_na() %>%
  filter(
    nchar(curp) == 18,
    nchar(inicio_practica) %in% c(4, 10),
    genero %in% c("Hombre", "Mujer")
  ) %>%
  rename(ent = estado_residencia)

# Modifica etiquetas de entidades
datos$ent[datos$ent == "Veracruz"] <- "Veracruz de Ignacio de la Llave"
datos$ent[datos$ent == "Querétaro de Arteaga"] <- "Querétaro"

# Obtiene la edad por medio del CURP
datos <- datos %>%
  mutate(year = as.numeric(substr(curp, 5, 6))) %>%
  filter(year %in% c(27:63)) %>%  # Mayores de 57 años
  mutate(edad = 2020 - 1900 - year) %>%
  select(-year, -curp)

# Obtiene el tiempo que lleva de actividad cultural
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
# Filtra los casos
datos <- filter(datos, t_act >= 25) %>%
  select(-inicio_practica)

# Obtiene diferencia entre edad y tiempo de actividad,
# para garantizar consistencia (diferencia no menor a 5 años)
datos <- datos %>%
  mutate(diferencia = edad - t_act) %>%
  filter(diferencia >= 5) %>%
  select(-diferencia)

# GRAFICA POR GRUPOS ETARIOS ===================================================

# Obtiene grupos etarios
datos_g <- datos %>%
  mutate(
    etario = case_when(
      edad == 57 ~ "57 años",
      edad == 58 ~ "58 años",
      edad == 59 ~ "59 años",
      TRUE ~ "60 años y más"
    )
  ) %>%
  group_by(etario, genero) %>%
  summarise(total = n())

# GRAFICA
image <- ggplot(datos_g) +
  aes(
    x = etario,
    y = total,
    fill = genero,
    label = format(total, big.mark = ",")  # Numeros brutos
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"  # Para barras apiladas con numeros brutos
  ) +
  geom_text(
    position = position_stack(vjust = 0.5),  # Usar con barras apiladas
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  # Escala logaritmica. No usar con barras apiladas 100%
  scale_y_continuous(trans = "sqrt") +
  labs(
    x = "Edad",
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: ",
      "personas mayores* con 25 años o más de actividad cultural por género"
    ),
    subtitle = "*Personas que en 2020 han cumplido o cumplirán 57 años o más",
    fill = "Género",
    caption = paste0(
      "Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
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
    "mayores_genero.png"
  ), 
  plot = image, 
  width = 11, 
  height = 7
)

write.xlsx(datos_g, "mayores_genero.xlsx")

# GRAFICA POR ENTIDAD DE RESIDENCIA Y GENERO ===================================

# Obtiene total por entidad
datos_g <- datos %>%
  group_by(ent, genero) %>%
  summarise(total = n()) %>%
  filter(ent != "No aplica")

# Modifica factores de entidad
datos_g$ent <- factor(x = datos_g$ent)
levels(datos_g$ent)[3] <- "Baja California\nSur"
levels(datos_g$ent)[8] <- "Coahuila\nde Zaragoza"
levels(datos_g$ent)[16] <- "Michoacán\nde Ocampo"
levels(datos_g$ent)[30] <- "Veracruz\nde Ignacio\nde la Llave"

# GRAFICA
image <- ggplot(datos_g) +
  aes(
    x = ent,
    y = total,
    fill = genero,
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
  # scale_y_continuous(trans = "sqrt") +  # Transformacion
  labs(
    x = "Entidad de residencia", 
    y = "Agentes culturales",
    title = paste0(
      "Agentes culturales: personas mayores* con 25 años o más de",
      " actividad cultural por entidad de residencia y género"
    ),
    subtitle = "*Personas que en 2020 han cumplido o cumplirán 57 años o más",
    fill = "Género",
    caption = paste0(
      #"Diferencias suavizadas con transformación de raíz cuadrada.\n\n",
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
    "mayores_entidad.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)

write.xlsx(datos_g, "mayores_entidad.xlsx")
