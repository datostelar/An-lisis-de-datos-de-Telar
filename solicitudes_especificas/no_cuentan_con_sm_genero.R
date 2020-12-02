no cuentan con servicio medico por genero

datos <- reportes$sociodemo %>%
  select(id, soc_sm_05) %>%
  left_join(select(reportes$basicos, id, gen), by = "id") %>%
  distinct() %>%
  drop_na() %>%
  filter(gen %in% c("Hombre", "Mujer") & soc_sm_05 == 1) %>%
  group_by(gen, soc_sm_05) %>%
  summarise(total = n())
