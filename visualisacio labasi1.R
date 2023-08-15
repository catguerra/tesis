# Datos de POM y MAOM de LABASI, visualización inicial

#IMPORTAR
library(readxl)
library(ggplot2)
Data <- read_excel("Resultados Isotopicos Suelos Catalina Guerra Mayo 2023 VF2.xls", 
sheet = "Hoja1", range = "C1:O216", col_types = c("text", 
"numeric", "text", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric"))

View(Data)   

#cambio de nombre
colnames(Data) <- c("uso", "ID", "fraccion", "peso", "N2mg", "N2ug", "D15N", "Cmg", "Cug",
                    "D13C", "pN", "pC", "CN")

#visualización inicial
ggplot(Data, aes(x = D13C, y = D15N, color = uso)) +
         labs(title = "delta isotopico entre fracciones",
              x = "D C 13",
              y = "D N 15") + geom_point(size = 3) + theme_minimal()

#ahora por uso de suelo
BN <- Data[Data$uso=="BN",]
CU <- Data[Data$uso=="CU",]
MA <- Data[Data$uso=="MA",]
PF <- Data[Data$uso=="PF",]

#EXPLORAR RELACION ENTRE FRACCIONES POR USO
ggplot(BN, aes(x = D13C, y = D15N, color = fraccion)) +
  labs(title = "delta isotopico entre fracciones",
       x = "D C 13",
       y = "D N 15") + geom_point(size = 3) + theme_minimal()
ggplot(CU, aes(x = D13C, y = D15N, color = fraccion)) +
  labs(title = "delta isotopico entre fracciones",
       x = "D C 13",
       y = "D N 15") + geom_point(size = 3) + theme_minimal()       
ggplot(MA, aes(x = D13C, y = D15N, color = fraccion)) +
  labs(title = "delta isotopico entre fracciones",
       x = "D C 13",
       y = "D N 15") + geom_point(size = 3) + theme_minimal()
ggplot(PF, aes(x = D13C, y = D15N, color = fraccion)) +
  labs(title = "delta isotopico entre fracciones",
       x = "D C 13",
       y = "D N 15") + geom_point(size = 3) + theme_minimal()
