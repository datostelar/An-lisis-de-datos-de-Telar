# TOTAL DE AGENTES CULTURALES POR ENTIDAD, AREA DE ACTUACION Y DISCIPLINA

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
load("reportes_13_10_2020.RData")
load("catalogos.RData")

# TABLA ========================================================================

datos <- reportes$cult_disc %>%
  left_join(select(reportes$basicos, id, cve_ent_res), by = "id") %>%
  distinct() %>%
  drop_na() %>%
  left_join(
    select(catalogos$cat_edos, id_ent, entidad), by = c("cve_ent_res" = "id_ent")
  ) %>%
  group_by(entidad, id_area, id_disciplina) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(entidad, id_area) %>%
  mutate(max = ifelse(total == max(total), 1, 0)) %>%
  ungroup() %>%
  group_by(entidad) %>%
  mutate(max_a = ifelse(total == max(total), 1, 0)) %>%
  ungroup() %>%
  left_join(catalogos$cat_area, by = "id_area") %>%
  left_join(
    select(catalogos$cat_disc, id_disciplina, disciplina), by = "id_disciplina"
  ) %>%
  select(entidad, area, disciplina, total, max, max_a) %>%
  filter(disciplina != "Otra")


write.xlsx(datos, "datos.xlsx")
