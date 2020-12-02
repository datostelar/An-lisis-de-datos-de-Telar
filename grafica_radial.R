# Library
library(fmsb)
library(openxlsx)
library(dplyr)
library(extrafont)

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
resultados <- read.xlsx("facilitadores.xlsx")
nombres <- resultados$nombre
resultados <- select(resultados, -nombre)
resultados <- rbind(rep(10,14) , rep(0,14) , resultados)

i <- 3
datos <- slice(resultados, c(1:2, i))
radarchart(
  datos,
  axistype = 0,  # Tipo de eje
  # title = nombres[i-2],
  title = "Evaluación del equipo de facilitadores",
  seg = 10,  # Número de segmentos en los ejes
  pty = 16,  # Estilo de los puntos. 32 = sin puntos
  pcol = "#621132",  # Color de la línea 
  plty = 1,  # Tipo de linea. 1 = continua
  plwd = 2,  # Grosor de la linea
  cglty = 1,  # Tipo de linea de los ejes,
  cglwd = 1,  # Grosor de linea de los ejes
  pfcol = "#62113266",  # Color de relleno del poligono
  cglcol = "grey",
  # vlabels = c(), # Vector de caracteres para nombres de variables
  vlcex = 0.8  # Tamaño de las etiquetas de grupo
)



