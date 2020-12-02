# COLORES OFICIALES

color_oficial <- c(
  "#621132",  # Pantone 7421
  "#9D2449",  # Pantone 7420
  "#13322B",  # Pantone 627
  "#285C4D",  # Pantone 626
  "#4E232E",  # Pantone 504
  "#56242A",  # Pantone 490
  "#B38E5D",  # Pantone 465
  "#D4C19C"  # Pantone 468
)

color_telar <- c(
  "#95D80F",
  "#F08D33"
)

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
save(color_oficial, color_telar, file = "colores_oficiales.RData")
