# PIRAMIDE POBLACIONAL

# PAQUETES =====================================================================

lapply(
  c(
    "dplyr",
    "ggplot2",
    "forcats",
    "tidyr",
    "extrafont"
  ), 
  library,
  character.only = TRUE
)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_19_08_2020.RData")
load("catalogos.RData")
load("colores_oficiales.RData")

# FUNCIONES ====================================================================

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

# DATOS ========================================================================

# SE OBTIENE LA EDAD A PARTIR DE LA FECHA DE NACIMIENTO
datos <- reportes$basicos %>%
  filter(gen %in% c("Hombre", "Mujer") & nchar(fec_nac) == 10 & grp_ind == 1) %>%
  select(gen, fec_nac)

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
datos <- filter(datos, edad %in% c(18:100))
datos$etario <- obtener_cortes(datos$edad, corte = 0)

# 10
x <- c("71 a 80 años", "81 a 90 años", "91 a 100 años")
datos$etario[datos$etario %in% x] <- "71 a 100 años"
x <- c("11 a 20 años")
datos$etario[datos$etario %in% x] <- "18 a 20 años"
rm(x)

# 5
x <- c(
  "71 a 75 años",
  "76 a 80 años",
  "81 a 85 años",
  "86 a 90 años",
  "91 a 95 años",
  "96 a 100 años"
)
datos$etario[datos$etario %in% x] <- "71 a 100 años"
x <- c("16 a 20 años")
datos$etario[datos$etario %in% x] <- "18 a 20 años"
rm(x)

datos <- datos %>%
  group_by(etario, gen) %>%
  summarise(agentes = n())

# GRAFICA DE PIRAMIDE POBLACIONAL ==============================================

nota <- paste0(
  "Información captada del 25 de mayo de 2019 al 19 de agosto de 2020 a través ",
  "de la plataforma Telar.\nRegistro Nacional de Espacios, Prácticas y Agentes ",
  "Culturales.\nDirección de Animación Cultural. Dirección General de ",
  "Vinculación Cultural. Secretaría de Cultura."
)

image <- ggplot(datos) +
  geom_bar(
    data = subset(datos, gen == "Mujer"), 
    aes(x = etario, y = agentes, fill = gen),
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    data = subset(datos, gen == "Mujer"), 
    aes(
      x = etario, 
      y = agentes, 
      fill = gen, 
      # label = scales::percent(agentes/sum(datos$agentes), accuracy = 0.01)
      label = paste0(
        format(agentes, big.mark = ","),
        "\n(",
        scales::percent(agentes/sum(datos$agentes), accuracy = 0.01),
        ")"
      )
    ),
    position = position_dodge(width = 1),
    fontface = "bold",
    size = 4.5,
    color = "white",
    hjust = 1.2
  ) +
  geom_bar(
    data = subset(datos, gen == "Hombre"), 
    aes(x = etario, y = -agentes, fill = gen, label = -agentes),
    stat = "identity",
    position = "dodge"
  ) + 
  geom_text(
    data = subset(datos, gen == "Hombre"), 
    aes(
      x = etario, 
      y = -agentes, 
      fill = gen, 
      # label = scales::percent(agentes/sum(datos$agentes), accuracy = 0.01)
      label = paste0(
        format(agentes, big.mark = ","),
        "\n(",
        scales::percent(agentes/sum(datos$agentes), accuracy = 0.01),
        ")"
      )
    ),
    position = position_dodge(width = 1),
    fontface = "bold",
    size = 4.5,
    color = "white",
    hjust = -0.2
  ) +
  scale_fill_manual(values = color_oficial[c(2, 7)]) +
  coord_flip() +
  labs(
    x = "Grupos etarios", 
    y = "Agentes culturales",
    title = "Agentes culturales: total por grupo etario y género ",
    subtitle = "Agentes culturales que forman parte de grupos indígenas",
    fill = "Género",
    caption = nota
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )
plot(image)  # Visualiza la grafica

ggsave(
  file = paste0(
    paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/"), 
    "piramide_estandar.png"
  ), 
  plot = image, 
  width = 14, 
  height = 7
)
