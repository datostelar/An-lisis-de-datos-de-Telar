# DATOS ESPECIFICOS SOBRE COCINA

# PAQUETES =====================================================================

lapply(c(), library, character.only = TRUE)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_03_08_2020.RData")
load("catalogos.RData")

# DATOS ========================================================================

# Genero y disciplina
datos <- reportes$cult_disc %>%
  filter(
    id_area == 8,  # Practicas sociales, rituales y festividades
    id_disciplina %in% c(69, 75)  # Cocina tradicional y Platos, guisos...
  ) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  group_by(gen, id_disciplina) %>%
  summarise(total = n())

# Genero y estilo/tecnica
datos <- reportes$cult_disc %>%
  filter(
    id_area == 8,  # Practicas sociales, rituales y festividades
    id_disciplina %in% c(69, 75)  # Cocina tradicional y Platos, guisos...
  ) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(
    select(reportes$cult_area, id, id_area, tecnica), 
    by = c("id", "id_area")
  ) %>%
  distinct() %>%
  mutate(tecnica = tolower(trimws(tecnica))) %>%
  group_by(tecnica) %>%
  summarise(total = n())

# Especialidad
datos <- reportes$cult_disc %>%
  filter(
    id_area == 8,  # Practicas sociales, rituales y festividades
    id_disciplina %in% c(69, 75)  # Cocina tradicional y Platos, guisos...
  ) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  left_join(
    reportes$cult_espe,
    by = c("id", "id_area")
  ) %>%
  distinct() %>%
  left_join(catalogos$cat_esp, by = c("id_area", "id_especialidad")) %>%
  group_by(especialidad) %>%
  summarise(total = n())

# Todas las disciplinas
datos <- reportes$cult_disc %>%
  filter(id_area == 8) %>%
  group_by(id_area, id_disciplina) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  left_join(catalogos$cat_disc, by = c("id_area", "id_disciplina"))

# Numero total de personas que se registraron en el area 8
datos <- reportes$cult_disc %>%
  filter(id_area == 8) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  group_by(id_area, id_disciplina) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  left_join(catalogos$cat_disc, by = c("id_area", "id_disciplina"))

datos <- reportes$cult_area %>%
  select(id, id_area) %>%
  filter(id_area == 8) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  distinct() %>%
  group_by(gen) %>%
  summarise(total = n())


write.xlsx(datos, "datos.xlsx")
