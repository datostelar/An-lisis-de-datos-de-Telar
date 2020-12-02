# NUBE DE PALABRAS

# PAQUETES =====================================================================

lapply(
  c("viridis", "extrafont", "wordcloud", "wordcloud2"),
  library,
  character.only = TRUE
)

# NUBE DE PALABRAS =============================================================

datos <- datos %>%
  rename(word = Var1, freq = Freq) %>%
  mutate(freq = log(freq))

# Nube 1
datos2 <- datos %>%
  mutate(freq = log(freq)) %>%
  rename(word = Var1) %>%
  select(-Freq)

wordcloud2(
  data = datos,
  fontFamily = "Montserrat",
  color = c(color_oficial, color_oficial, color_oficial, color_oficial),
  backgroundColor = "white",
  size = 0.5,
  shape = "circle",
  minRotation = 0, 
  maxRotation = 0, 
  rotateRatio = 1
)

# # Nube 2
# wordcloud(
#   words = datos$Var1,  # Cuando proviene de una instruccion data.frame(table())
#   freq = datos$Freq,  # Idem
#   scale = c(1, 0.8),
#   min.freq = 1,
#   max.words = 200,
#   random.order = F,
#   rot.per = 0.1,
#   colors = viridis_pal(
#     option = "plasma",
#     begin = 0,
#     end = 0.6,
#     direction = -1
#   )(100),
#   family = "Montserrat"
# )
