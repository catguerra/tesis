#Abrir todas las bases y ordenarlas

library(readxl)
library(tidyverse)
library(dplyr)

#metadata: una fila por observacion, tiene datos de pom y maom sumados a los datos 
#del suelo total

#data: tiene la info de las fracciones pero tiene 2 observaciones por dato
#una para maom y otra para pom

#data_ : tiene 2 observaciones por dato, pero ademas tiene en las filas maom y pom por separado
#entonces tiene esas filas repetidas en maom y pom segun corresponde

#datos tiene la info de la muestra completa sin fraccionar y 
#datos_ tiene esa info pero con los datos depurados

#PRIMERO -DATA-
#----
setwd("/Users/alicia_leon/Desktop/tesis/Bases")
data <- read_excel("Resultados Isotopicos Suelos Catalina Guerra Mayo 2023 VF2.xls", 
                   sheet = "Hoja1", 
                   range = "C1:O216", 
                   col_types = c("text",
                                 "numeric", "text", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"))
names(data)
#cambio de nombre
colnames(data) <- c("uso", "ID", "fraccion", "peso",
                    "N2mg", "N2ug", "D15N", "Cmg", "Cug",
                    "D13C", "pN", "pC", "CN")
#se hace un promedio de los datos que estan duplicados, en group by 
#se ponen en orden todas las columnas que se toman en cuenta para agrupar
#en el vector se ponen las columnas sin contar las que estan dentro de group by
data <- data %>% group_by(uso, ID, fraccion) %>% reframe(across(c(1:10), mean))
#filtrar outliers
data <- data %>% filter(ID != 45)
data <- data %>% filter(ID != 75)
data <- data %>% filter(ID != 77)
data <- data %>% filter(ID != 102)
#----
#AHORA FRAC
#----
#ahora con esto para hacer una grafica con una sola categoria, primero hay qeue
frac <- data[,c(1:3,7,10:13)] %>% 
  pivot_wider(names_from = "fraccion", 
              values_from = c("D15N", "D13C", "pN", "pC", "CN"))

# añadir g de C y N por K de suelo, multiplicando por 10los % 
#que coincidirá con los de meta
frac$C_maom <- frac$pC_maom*10
frac$C_pom <- frac$pC_pom*10
frac$N_maom <- frac$pN_maom*10
frac$N_pom <- frac$pN_pom*10
#filtrar outliers
frac <- frac %>% filter(ID != 45)
frac <- frac %>% filter(ID != 75)
frac <- frac %>% filter(ID != 77)
frac <- frac %>% filter(ID != 102)
#----
#DATA_ 1
#UNIR FRAC Y DATA, PARA PODER HACER ESTADISTICA (O INTENTAR)
#----
# merge(x, y, ...)
data_ <-  merge(x=frac, y=data, all = T) # Columnas usadas para unir
names(data_)
#eliminar columnas repetidas y seleccionar solo las que necesito
data_ <- select(data_, ID , uso, fraccion, CN, D15N_maom, D15N_pom,
                D13C_maom, D13C_pom, pN_maom,
                pN_pom, pC_maom, pC_pom, CN_maom, CN_pom, 
                C_pom, C_maom, N_pom, N_maom ) # Forma simple 2
colnames(data_) <- c("ID", "uso", "fraccion", "CN_frac", "D15N_maom", "D15N_pom", 
                     "D13C_maom", "D13C_pom", "pN_maom",
                     "pN_pom", "pC_maom", "pC_pom", "CN_maom","CN_pom",
                     "C_pom", "C_maom", "N_pom", "N_maom")

#filtrar outliers
data_ <- data_ %>% filter(ID != 45)
data_ <- data_ %>% filter(ID != 75)
data_ <- data_ %>% filter(ID != 77)
data_ <- data_ %>% filter(ID != 102)
#----
#BASE CON TODO META DATA
#----
meta_data <- read_excel("~/Desktop/tesis/Bases/meta_data.xlsx",
                        sheet = "cap1_meta", 
                        col_types = c("numeric","text", "numeric",
                                      "numeric", "text", 
                                      "text", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "text", "numeric",
                                      "numeric", "numeric", "numeric"))
# cambiar nombre columnas 
colnames(meta_data) <- c( "ID", "ROL","LAT", "LONG","uso","zona","pH","bd",
                          "CE","MO.","Ctot",
                          "Ntot","Ptot","CN","silt","sand","clay", 
                          "D15N_maom","D15N_pom",
                          "D13C_maom", "D13C_pom","pN_maom",
                          "pN_pom","pC_maom","pC_pom", 
                          "CN_maom","CN_pom","Altitud", 
                          "Slope","Aspect",
                          "LST","FlowAcc", "Climate","SATVI",
                          "NDWI","NDMI","NDSI")

# el porcentaje de C de la n45 es outlier si o si, así que:
meta_data <- meta_data %>% filter(ID != 45)
meta_data <- meta_data %>% filter(ID != 75)
meta_data <- meta_data %>% filter(ID != 77)
meta_data <- meta_data %>% filter(ID != 102)

#primero añadir g de C y N por K de suelo, multiplicando por 10los %
meta_data$C_maom <- meta_data$pC_maom*10
meta_data$C_pom <- meta_data$pC_pom*10
meta_data$N_maom <- meta_data$pN_maom*10
meta_data$N_pom <- meta_data$pN_pom*10
#----
#ahora unir con los datos de las muestras totales no fraccionadas
#ABRIR BASE QUE TIENE LA INFO PARA LA MUESTRA TOTAL
#DATOS y DATOS_
#----
datos <- read.csv("~/Desktop/tesis/Bases/Coordenadas_finales_WPs_Terrenos - Base de datos.csv")
colnames(datos) <- c("ID","ROL","Anillo","LAT","LONG","Landcover","Zona","Sector",
                     "t.Infiltración..segs.", "t.Infiltración..min.","cubic.inches.of.H2O",
                     "Infiltracion..in.min.","pH.1","pH.2","pH_fin","Peso.bd..g.",
                     "Peso.bolsa","Vol.cilindro..cm3.","bden..g.cm3.","CE..uS.cm.",
                     "Temp.CE","MO.","Ctot","Ntot","Ptot","CN","Silt.", "Sand.",
                     "Clay.","Coarse..Frag","Altitud","Slope","Aspect","LST",
                     "FlowAcc","Climate", "SATVI","NDWI","NDMI" )
#seleccionar los datos que me importan de esta base
datos_ <- select(datos, "ID", "ROL", "LAT", "LONG", "Zona", "pH.1", "MO.", 
                 "Ctot", "Ntot", "Ptot", "CN", "bden..g.cm3." , "Silt.",
                 "Sand.", "Clay.", "Altitud", "Slope", "Aspect", "LST",
                 "FlowAcc", "Climate", "SATVI", "NDWI", "NDMI")
#cambiar nombre de las columnas porque algunas coiciden con las de pom y maom
colnames(datos_) <- c("ID", "ROL", "LAT", "LONG", "Zona", "pH", "MO.", 
                      "Ctot", "Ntot", "Ptot", "CNtot", "bd" , "silt",
                      "sand", "clay", "Altitud", "Slope", "Aspect", "LST", 
                      "FlowAcc", "Climate", "SATVI", "NDWI", "NDMI")
# el porcentaje de C de la n45 es outlier si o si, así que:
datos_ <- datos_ %>% filter(ID != 45)
datos_ <- datos_ %>% filter(ID != 75)
datos_ <- datos_ %>% filter(ID != 77)
datos_ <- datos_ %>% filter(ID != 102)
#----
#ahora sobre escribir data_ para agregar los datos de suelo total
#DATA_ 2
#----
data_ <- merge(x=data_, y=datos_, by = "ID") # Columnas usadas para unir
names(data_)
#calculr el contenido en g por kg de suelo de n y c
data_$MOtot_Kg <- (data_$MO.)*10
#filtrar
data_ <- data_ %>% filter(ID != 45)
data_ <- data_ %>% filter(ID != 75)
data_ <- data_ %>% filter(ID != 77)
data_ <- data_ %>% filter(ID != 102)

# PRIMERO agregar una fila con el total pom + maom y otras con las proporciones

data_$p_m <- data_$pC_maom + data_$pC_pom #para pom + maom
data_$prop_pom <- data_$pC_pom -data_$p_m #para pom/total
data_$prop_maom <- data_$pC_maom -data_$p_m #para maom/total
