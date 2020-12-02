# NUMERO DE AGENTES CULTURALES POR ENTIDADES

# PAQUETES =====================================================================

lapply(c("dplyr", "openxlsx"), library, character.only = TRUE)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_03_08_2020.RData")
load("catalogos.RData")

# DATOS ========================================================================

datos <- reportes$basicos %>%
  select(id, gen, cve_ent_res) %>%
  distinct() %>%
  drop_na() %>%
  group_by(cve_ent_res, gen) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  left_join(
    select(catalogos$cat_edos, id_ent, entidad),
    by = c("cve_ent_res" = "id_ent")
  ) %>%
  filter(entidad != "No aplica") %>%
  select(entidad, gen, total) %>%
  pivot_wider(names_from = gen, values_from = total) %>%
  mutate_all(replace_na, 0)

write.xlsx(datos, "datos.xlsx")