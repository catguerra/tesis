#Base de datos de ignacio + mías, con ayuda de Vicente 20 junio 2023
#primero lo que hizo e lvicho
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

#luego lo mío 

library(tidyverse)
library(readxl)
        
setwd("/Users/alicia_leon/Desktop/tesis/Bases")
sol_mico <- read.csv("~/Desktop/tesis/Bases/Sol_Cata_db.csv")

#suma de C en pom y maom

sol_mico$suma <- sol_mico$pC_maom + sol_mico$pC_pom

#solo bosques y plantaciones (para despues ver solo estos)
forest <- sol_mico[sol_mico$Landcover=="bosque nativo" | sol_mico$Landcover=="plantacion forestal", ]

#estabilidá (haciendo gráficas como las de Wu, comparando SOM con C en cada fracción)

m <- sol_mico %>% ggplot() + 
  geom_point(aes(x=MO.,y=pC_maom, color=micorriza)) + theme_minimal()

p <- sol_mico %>% ggplot() + 
  geom_point(aes(x=MO.,y=pC_pom, color=micorriza)) + theme_minimal()
library(cowplot)
plot_grid(m, p, nrow = 1)


# estabilidad (est) con lineas de regresion lineal
#(se le da nombre para dps hacer un solo plot)
est_mc <- sol_mico %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_maom, color=micorriza)) + 
  geom_point() + theme(legend.position = "none") + geom_smooth(method = "lm", se = F) + 
  labs(x = "% Materia Orgánica", y = "C en MAOM")
view(est_mc)
est_pc <- sol_mico %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_pom, color=micorriza)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + 
  labs(x = "% Materia Orgánica", y = "C en POM") + theme(legend.position = "top")
view(est_pc)
est_mn <-sol_mico %>% drop_na() %>% ggplot(aes(x=MO.,y=pN_maom, color=micorriza)) + 
  geom_point() + theme(legend.position = "none") + geom_smooth(method = "lm", se = F)  +
  labs(x = "% Materia Orgánica", y = "N en MAOM")
view(est_mn)
est_pn <- sol_mico %>% drop_na() %>% ggplot(aes(x=MO.,y=pN_pom, color=micorriza)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + theme(legend.position = "none")+ 
  labs(x = "% Materia Orgánica", y = "N en POM")
view(est_pn)
#colapsar tablas en un plot

plot_grid(est_mc, est_pc, est_mn, est_pn, nrow = 2)

#boplot para estabilidad [LA GRÁFICA QUE HABÍAMOS IMAGINADO]
sol_mico$est <- sol_mico$pC_maom/sol_mico$pC_pom
sol_mico %>% drop_na() %>% ggplot() + geom_boxplot(aes(x=micorriza, y=est)) + 
  theme(legend.position = "top")  +
  labs(x = "% Materia Orgánica", y = "MAOM/POM")

#estabilidad con regresiones para bosques [con nombre para colapsarlas en 1 plot]
F1 <- forest %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_maom, color=micorriza)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + theme_minimal()

F2 <- forest %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_pom, color=micorriza)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + theme_minimal()

F3 <- forest %>% drop_na() %>% ggplot(aes(x=MO.,y=pN_maom, color=micorriza)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + theme_minimal()

F4 <- forest %>% drop_na() %>% ggplot(aes(x=MO.,y=pN_pom, color=micorriza)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + theme_minimal()

#colapso en un plot

plot_grid(F1,  F2, F3, F4, nrow = 2) #muy interesante lo que pasa con el N!!

# calidad entre maom y pom con regresion lineal por tipo micorriza, todos los puntos
cal_mc <- sol_mico %>% drop_na() %>% ggplot(aes(x=MO.,y=CN_maom, color=micorriza)) + 
  geom_point() + theme(legend.position = "top") + geom_smooth(method = "lm", se = F) + 
  labs(x = "% Materia Orgánica", y = "CN en MAOM")

cal_mn <-sol_mico %>% drop_na() %>% ggplot(aes(x=MO.,y=CN_pom, color=micorriza)) + 
  geom_point() + theme(legend.position = "top") + geom_smooth(method = "lm", se = F)  +
  labs(x = "% Materia Orgánica", y = "CN en POM")
# calidad en suelo completo
sol_mico %>% drop_na() %>% ggplot(aes(x=MO., y=CN, color=micorriza)) + 
  geom_point() + theme(legend.position = "top") + geom_smooth(method = "lm", se = F)  +
  labs(x = "% Materia Orgánica", y = "CN en POM")
#boplot para calidad por micorriza [LA GRÁFICA QUE HABÍAMOS IMAGINADO]
sol_mico %>% drop_na() %>% ggplot() + geom_boxplot(aes(x=micorriza, y=CN)) + 
  theme(legend.position = "top")  +
  labs(x = "% Materia Orgánica", y = "CN en SOM")





#tabla linda con promedios y desviaciones para c/dato, seguir agregadno todo en el orden que uno quiera
#sol_mico %>% drop_na() %>% group_by(Landcover, micorriza) %>%
 # summarise(Prom.MAT = mean(MAT), sd.MAT = sd(MAT), )

#se pueden separar los dartos entre centro norte, centro centro y centro sur

f_norte <- c(1:36)
f_centro <- c(37:72)
f_sur <- c(73:108)


sol_n <- sol_mico[f_norte, ]
sol_c <- sol_mico[f_centro, ]
sol_s <- sol_mico[f_sur, ]

n_maom <- sol_n %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_maom, color=micorriza)) + geom_point() +
  geom_smooth(method = "lm", se = F)
n_pom <-sol_n %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_pom, color=micorriza)) + geom_point() +
  geom_smooth(method = "lm", se = F)

c_maom <- sol_c %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_maom, color=micorriza)) + geom_point() +
  geom_smooth(method = "lm", se = F)
c_pom <- sol_c %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_pom, color=micorriza)) + geom_point() +
  geom_smooth(method = "lm", se = F)

s_maom <- sol_s %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_maom, color=micorriza)) + geom_point() +
  geom_smooth(method = "lm", se = F)
s_pom <- sol_s %>% drop_na() %>% ggplot(aes(x=MO.,y=pC_pom, color=micorriza)) + geom_point() +
  geom_smooth(method = "lm", se = F)

plot_grid(n_maom,  n_pom, c_maom, c_pom, s_maom, s_pom, nrow = 3) #muy interesante lo que pasa con el N!!

#barplot con cada uno diferente
# para eso voy a hacer una nueva base que tenga una columna con fraccion, id, micorriza
library(readxl)
data <- read_excel("Resultados Isotopicos Suelos Catalina Guerra Mayo 2023 VF2.xls", 
                   sheet = "Hoja1", range = "C1:O216", col_types = c("text", 
                                                                     "numeric", "text", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric"))

# hacer un objeto con las columnas que quiero de cada una df_doomie

DF_sol <- sol_mico[,c(1,13,25,25,30,31)]

data <- full_join(data, DF_sol, by = "ID")

data %>% drop_na() %>% ggplot(aes(x=micorriza, y=MO., fill=fraccion)) + geom_bar(stat="identity")

data %>% drop_na() %>% ggplot(aes(x=micorriza, y=suma, fill=fraccion)) + geom_bar(stat="identity")

  
