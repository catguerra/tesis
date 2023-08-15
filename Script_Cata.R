setwd("/Users/alicia_leon/Desktop/tesis/Bases")
list.files()


library(tidyverse)

sol <- read.csv("PuntosPhd_Revisados_CBN - PuntosPhd_Revisados_CBN.csv", header = T)
cft <- read.csv("Coordenadas_finales_WPs_Terrenos - Base de datos.csv", header = T)
frac <- read.csv("fracciones.csv", header = T)

sol <- sol[order(sol$ID),]
row.names(sol) <- 1:nrow(sol)

sol <- sol[,c(1:3,14:21)]
cft <- cft[,c(1,6,22:26)]

sol_db <- full_join(sol, cft, by = "ID")
sol_db <- full_join(sol_db, frac, by = "ID")

sol_db <- sol_db %>% mutate(., across(.cols = everything(), tolower))


sol_db$micorriza <- case_when(
  str_detect(sol_db$ESPECI1_CI, regex("eucalyptus", ignore_case = TRUE)) == T ~ "NA",
  str_detect(sol_db$Landcover, regex("plantacion forestal", ignore_case = TRUE)) == T ~ "Ecto",
  str_detect(sol_db$Landcover, regex("cultivos", ignore_case = TRUE)) == T &
    str_detect(sol_db$Landcover, regex("matorral", ignore_case = TRUE)) == T ~ "Arbuscular",
  str_detect(sol_db$ESPECI1_CI, regex("nothofagus", ignore_case = TRUE)) == T |
    str_detect(sol_db$ESPECI1_CI, regex("pinus", ignore_case = TRUE)) == T ~ "Ecto",
  (str_detect(sol_db$ESPECI1_CI, regex("pinus", ignore_case = TRUE)) == F |
     str_detect(sol_db$ESPECI1_CI, regex("nothofagus", ignore_case = TRUE)) == F) & 
    (str_detect(sol_db$ESPECI2_CI, regex("nothofagus", ignore_case = TRUE)) == T |
       str_detect(sol_db$ESPECI3_CI, regex("nothofagus", ignore_case = TRUE)) == T) ~ "Transicion",
  str_detect(sol_db$ESPECI1_CI, regex("nothofagus", ignore_case = TRUE)) == F |
    str_detect(sol_db$ESPECI2_CI, regex("nothofagus", ignore_case = TRUE)) == F |
    str_detect(sol_db$ESPECI3_CI, regex("nothofagus", ignore_case = TRUE)) == F ~ "Arbuscular"
)

write.csv(sol_db, "Sol_Cata_db.csv", row.names = F)


