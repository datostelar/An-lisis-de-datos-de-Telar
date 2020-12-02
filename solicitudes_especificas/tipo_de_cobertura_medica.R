cuantas personas 18 a 64 años y el tipo de cobertura médica que tienen.

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_15_09_2020.RData")
load("INEGI\\cat_ent_mun.RData")
load("colores_oficiales.RData")
load("catalogos.RData")

# DATOS ========================================================================

# SE OBTIENE LA EDAD A PARTIR DE LA FECHA DE NACIMIENTO
datos <- reportes$basicos %>%
  filter(gen %in% c("Hombre", "Mujer") & nchar(fec_nac) == 10) %>%
  select(id, fec_nac) %>%
  distinct() %>%
  drop_na()

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
datos <- filter(datos, edad %in% c(18:64))

ids <- unique(datos$id)

# ACCESO A SERVICIO MEDICO POR AREA CULTURAL ===================================

datos <- select(
  reportes$sociodemo,
  id,
  soc_sm_01,
  soc_sm_02,
  soc_sm_03,
  soc_sm_04,
  soc_sm_05,
  soc_sm_06
) %>%
  distinct() %>%
  drop_na() %>%
  filter(id %in% ids) %>%
  mutate(aux = 1) %>%
  select(-id) %>%
  group_by(aux) %>%
  summarise_all(list(total = sum)) %>%
  pivot_longer(
    -c(aux),
    names_to = "sm",
    values_to = "total"
  ) %>%
  select(-aux)

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
