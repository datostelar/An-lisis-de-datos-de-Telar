# total por genero
# cuantos son indigenas
# areas culturales que practican

datos <- reportes$basicos %>%
  select(id, gen, cve_ent_res, cve_mun_res) %>%
  left_join(select(reportes$sociodemo, id, soc_ind), by = "id") %>%
  left_join(select(reportes$cult_area, id, id_area), by = "id") %>%
  left_join(catalogos$cat_area, by = "id_area") %>%
  left_join(
    select(catalogos$cat_edos, id_ent, entidad), by = c("cve_ent_res" = "id_ent")
  ) %>%
  left_join(
    select(catalogos$cat_mun, id_mun, mun), by = c("cve_mun_res" = "id_mun")
  ) %>%
  distinct() %>%
  filter(mun == "Navolato") %>%
  drop_na()


datos <- reportes$basicos %>%
  select(id, gen, cve_ent_res) %>%
  left_join(
    select(catalogos$cat_edos, id_ent, entidad), by = c("cve_ent_res" = "id_ent")
  ) %>%
  distinct() %>%
  drop_na() %>%
  group_by(entidad, gen) %>%
  summarise(total = n())

datos <- reportes$basicos %>%
  select(id, gen, cve_ent_res) %>%
  left_join(
    select(catalogos$cat_edos, id_ent, entidad), by = c("cve_ent_res" = "id_ent")
  ) %>%
  distinct() %>%
  drop_na() %>%
  group_by(gen) %>%
  summarise(total = n())
