# LENGUAS INDIGENAS

# PAQUETES =====================================================================

lapply(c("dplyr"), library, character.only = TRUE)

# CARGA DE DATOS ===============================================================

setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))
load("reportes_19_08_2020.RData")

# DATOS ========================================================================

datos <- data.frame(table(reportes$sociodemo$soc_ind_cual)) %>%
  filter(!Var1 %in% c("Otro", "Otra") & Freq > 5)

# Al parecer, wordcloud no soporta insertar una linea con /n
# datos$Var1 <- factor(x = datos$Var1)
# levels(datos$Var1)
# levels(datos$Var1) <- c(
#   "Ayuujk/n(mixe)",
#   "Bats'i k'op/nTsotsil",
#   "Bats'il k'op/nTseltal",
#   "Ch'ol",
#   "Didxazá/n(zapoteco del Istmo)",
#   "Énná/n(mazateco)",
#   "Guarijío",
#   "Hñähñu/n(del centro)",
#   "Hñähñu/n(otomí)",
#   "Jiak Noki/n(yaqui)",
#   "kumiay",
#   "Kuti'/n(akateko)",
#   "Maayat'aan/n(maya)",
#   "Mè'phàà/n(tlapaneco)",
#   "Mexicano/nTlajtol",
#   "Mexikatlahtolli/n(náhuatl)",
#   "Náayari/n(cora)",
#   "Ngiwa/n(popoloca)",
#   "Nuntajiiyi'/n(de la Sierra)",
#   "Ñomndaa/n(amuzgo)",
#   "Ñuju/n(de la Sierra)",
#   "P'urhepecha",
#   "Ralámuli raicha/n(tarahumara)",
#   "Triqui",
#   "Tu'un Savi/n(mixteco)",
#   "Tutunakú",
#   "Wixárika/n(huichol)",
#   "Yokot'an/n(chontal de Tabasco)",
#   "Yoremnokki/n(mayo)",
#   "Yühmu",
#   "Zapoteco"
# )
