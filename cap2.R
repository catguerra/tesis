library(usethis)
usethis::use_git()
usethis::create_github_token()
library(gitcreds)
gitcreds::gitcreds_set()
setwd("/Users/alicia_leon/Desktop/tesis/Bases")
library(readxl)
library(ggplot2)
#BASE CON TODO
meta_data <- read_excel("~/Desktop/tesis/Bases/meta_data.xlsx",
                        sheet = "cap1_meta", 
                        col_types = c("numeric","text", "numeric", "numeric", "text", 
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
colnames(meta_data) <- c( "ID", "ROL","LAT", "LONG","uso","zona","pH","bd","CE","MO.","Ctot",
                     "Ntot","Ptot","CN","silt","sand","clay", "D15N_maom","D15N_pom",
                     "D13C_maom", "D13C_pom","pN_maom","pN_pom","pC_maom","pC_pom", 
                     "CN_maom","CN_pom","Altitud", "Slope","Aspect",
                     "LST","FlowAcc", "Climate","SATVI","NDWI","NDMI","NDSI")

# el porcentaje de C de la n45 es outlier si o si, así que:
library(dplyr)
library(dplyr)
meta_data <- meta_data %>% filter(ID != 45)
meta_data <- meta_data %>% filter(ID != 75)
meta_data <- meta_data %>% filter(ID != 77)
#primero añadir g de C y N por K de suelo, multiplicando por 10los %

meta_data$C_maom <- meta_data$pC_maom*10
meta_data$C_pom <- meta_data$pC_pom*10
meta_data$N_maom <- meta_data$pN_maom*10
meta_data$N_pom <- meta_data$pN_pom*10

#es necesario tener la base en un formato estirado tambien
#POR LO TANTO UNIRÉ DATA QUE TIENE LOS DATOS CRUDOS DE BIOGEO
#CON LA BASE FRAC QUE TIENE LA VERSIÓN ACORTADA DE LA MISMA BASE

# añadir g de C y N por K de suelo, multiplicando por 10los % 
#que coincidirá con los de meta
frac$C_maom <- frac$pC_maom*10
frac$C_pom <- frac$pC_pom*10
frac$N_maom <- frac$pN_maom*10
frac$N_pom <- frac$pN_pom*10
# merge(x, y, ...)
data_ <-  merge(x=frac, y=data, all = T) # Columnas usadas para unir
names(data_)
#eliminar columnas repetidas y seleccionar solo las que necesito
data_ <- select(data_, ID , uso.x, fraccion, CN, D15N_maom, D15N_pom,
                D13C_maom, D13C_pom, pN_maom,
                  pN_pom, pC_maom, pC_pom, CN_maom, CN_pom, 
                C_pom, C_maom, N_pom, N_maom ) # Forma simple 2
colnames(data_) <- c("ID", "uso", "fraccion", "D15N_maom", "D15N_pom", 
                     "D13C_maom", "D13C_pom", "pN_maom",
                     "pN_pom", "pC_maom", "pC_pom", "CN_maom", 
                     "CN_pom", "C_pom", "C_maom", "N_pom", "N_maom")

#ahora unir con los datos de las muestras totales no fraccionadas
#ABRIR BASE QUE TIENE LA INFO PARA LA MUESTRA TOTAL
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

#ahora sobre escribir data_ para agregar los datos de suelo total
data_ <- merge(x=data_, y=datos_, by = "ID") # Columnas usadas para unir
#calculr el contenido en g por kg de suelo de n y c
data_$Ctot_Kg <- (data_$MO.)*10
#ahora la grafica de porcentajes

# PRIMERO agregar una fila con el total pom + maom y otras con las proporciones

data_$p_m <- data_$pC_maom + data_$pC_pom #para pom + maom
data_$prop_pom <- data_$pC_pom -data_$p_m #para pom/total
data_$prop_maom <- data_$pC_maom -data_$p_m #para maom/total


#separar por uso de suelo para tener las ds y means
bosque <- data_[data_$uso=="BN",]
frutales <- data_[data_$uso=="CU",]
matorral <- data_[data_$uso=="MA",]
forestal <- data_[data_$uso=="PF",]

#GRAFICAS DE BARRAS RELACION POM - MAOM
colnames(data) <- c("uso","ID","fraccion", "peso","N2mg","N2ug",
                    "D15N_frac","Cmg","Cug","D13C_frac", "pN_frac","pC_frac","CN")
#sacar outliers
library(dplyr)
data <- data %>% filter(ID != 45)
data <- data %>% filter(ID != 75)
data <- data %>% filter(ID != 77)
data <- data %>% filter(ID != 102)
#agregar g por kg de suelo 
data$gC_frac <- data$pC_frac*10
data$gN_frac <- data$pN_frac*10
#oredenar uso y fraccion
data$uso = factor(data$uso, levels = c("PF", "CU", "BN", "MA"))
data$fraccion = factor(data$fraccion, levels = c("pom", "maom"))

#100% es el maximo y ahi divide
ggplot(data, aes(fill=fraccion, y=gC_frac, x=uso)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values=c('orange','green4')) +
  ylab("% C en el suelo") + xlab("Uso de suelo")  +
  theme_bw() +
  theme(axis.text.x = element_text(face =
                                                  "bold", 
                                                color = 
                                                  "black",
                                                size = 12), 
axis.text.y = element_text(face = "bold", 
                           color = "black", size = 12)) + 
  coord_flip() + theme(legend.position="top")
  

#el maximo es la cantidad real
ggplot(data, aes(x=uso, y=gC_frac, fill=fraccion)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c('orange','green4')) +
  ylab("%C en ell suelo") + xlab("Uso de suelo")  +
  scale_x_discrete(labels=c("Bosque Nativo",
                            "Cultivo", 
                            "Matorral", 
                            "Plantacion Forestal")) +
  theme_bw() +
  theme(text = element_text(size=rel(4.5)),
        axis.text.x = element_text(face =
                                     "bold", 
                                   color = 
                                     "black",
                                   size = 12), 
        axis.text.y = element_text(face = "bold", 
                                   color = "black", size = 12)) + 
  coord_flip() + theme(legend.position="top")
#ahora pom al lado de maom y no sobre
ggplot(data, aes(x=uso, y=gC_frac, fill=fraccion)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c('orange','green4')) +
  ylab("%C en ell suelo") + xlab("Uso de suelo")  +
  scale_x_discrete(labels=c("Bosque Nativo",
                            "Cultivo", 
                            "Matorral", 
                            "Plantacion Forestal")) +
  theme_bw() +
  theme(axis.text.x = element_text(face =
                                     "bold", 
                                   color = 
                                     "black",
                                   size = 12), 
        axis.text.y = element_text(face = "bold", 
                                   color = "black", size = 12)) + 
  coord_flip() + theme(legend.position="top")

j <- ggplot(data, aes(x=uso, y=gC_frac, fill=fraccion)) +
  geom_boxplot() +
  scale_fill_manual(values=c('orange','green4')) +
  ylab("gC por Kg de suelo") + xlab("Uso de suelo")  +
  theme_bw()  
j + facet_wrap(~fraccion) + 
  stat_summary(fun.y="mean", colour = "black")+
  theme(axis.text.x = element_text(face ="bold", color ="black",
                                   size = 12), 
        axis.text.y = element_text(face = "bold", 
                                   color = "black", size = 12)) + 
   theme(legend.position="top") +
  scale_x_discrete(labels=c("Bosque Nativo",
                            "Cultivo", 
                            "Matorral", 
                            "Plantacion Forestal"))
j+ stat_summary(fun.y="mean", colour = "black") 
 

#DESVIACION ESTANDAR Y MEDIAS PARA CADA USO DE SUELO
#matriz de correlacion
#----
#funcion para calcular la media y sd para cada grupo
bosque %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=sd))
bosque %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=mean))
# DS --> 1 maom      3.97 ;  pom       4.26
# MEAN -->  maom      8.12 ; pom       6.03
frutales %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=sd))
frutales %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=mean))
# DS:  maom      3.03 ; pom       2.57
# MEAN :  maom      4.65 ; pom       4.06
na.omit(matorral) %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=sd))
na.omit(matorral) %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=mean))
# DS: 1 maom      4.04 ; pom       3.26
# MEAN  maom      5.37 ; pom       3.47
na.omit(forestal) %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=sd))
na.omit(forestal) %>%  group_by(fraccion) %>% summarise_at(vars(pC_frac), list(name=mean))
# SD: maom      2.84 ; pom       5.65
# MEAN : maom      4.99 ; pom       7.04

install.packages("GGally")
library(GGally)
ggcorr(data_, method = c("everything", "pearson"))
ggpairs(bosque , columns = 14:23, ggplot2::aes(colour=fraccion))

#-----

#graficas

#boxplot de g de C por kg de suelo por fraccion por uso
ggplot(data_, aes(y=C_g_per_Kg, x=uso))  + stat_summary(fun.y="mean") +
  geom_boxplot(aes(fill = fraccion)) + theme_bw()  

ggplot(data_, aes(y=C_g_per_Kg/N_g_per_Kg, x=uso))  + stat_summary(fun.y="mean") +
  geom_boxplot(aes(fill = fraccion)) + theme_bw()

#SCATTER PARA ESTAS SHITS

ggscatter(data_ , x="Ctot_g_per_Kg",y="C_maom", 
          add = "reg.line",
          conf.int = T,
          color = "uso", palette = "jco",
          shape = "uso") +
  stat_cor(aes(color=uso), label.x = 0) + geom_abline(intercept = 0, slope = 1)

library(ggplot2)
library(cowplot)
library(devtools)

a <- data_ %>% ggplot(aes(x=Ctot_g_per_Kg,y=C_g_per_Kg, col = fraccion)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F) 

#  geom_text(x= 10, y= 10, label = paste("y=",
                  #        round(coef(lm(pC_maom ~ Ctot, data_))[2],2),
                       #         "x +",
                      #          round(coef(lm(pC_maom ~ Ctot, data_))[1],2)),
                      #    hjust = 0, vjust = 1, size = 2)

b <- data_ %>% ggplot(aes(x=Ctot,y=pC_pom, col = uso)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F)

plot_grid(a, b, nrow = 1)

library(ggpubr)

bosque %>% ggplot(aes(x=clay,y=MO., col = fraccion)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")),
           color = "red", geom = "label")

ggscatter(data_ , x= "PC", y="pC_maom",
          color = "uso", palette = "jco",
          ellipse = T)

meta_data %>% ggplot(aes(x=clay,y=CN_maom, col = uso)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F)

meta_data %>% ggplot(aes(x=pC_maom,y= pC_pom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + theme_bw() +
  geom_smooth(method = "lm", se = F)

meta_data %>% ggplot(aes(x=clay,y=pC_pom/pC_maom, col = uso)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F)

meta_data %>% ggplot(aes(x=MO.,y=pC_maom, col = uso)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F)

meta_data %>% ggplot(aes(x=MO.,y=pC_pom, col = uso)) + 
  geom_point()  + theme_bw() + geom_smooth(method = "lm", se = F)

#voy a intentar hacer un random forest
library(randomForest)
#primero hay que hacer una columna que sea la diferencia que quiero poner a 
#prueba, en este cado, la diferecnia entre d13c y d15 n así como cn entre
#pom y maom

data_$dif15n <- data_$D15N_maom -data_$D15N_pom #para d15n
data_$dif13c <- data_$D13C_maom -data_$D13C_pom #para d13c
data_$difcn <- data_$CN_maom -data_$CN_pom #para cn
#agegar las nuevas listas a las bases por uso
bosque <- data_[data_$uso=="BN",]
frutales <- data_[data_$uso=="CU",]
matorral <- data_[data_$uso=="MA",]
forestal <- data_[data_$uso=="PF",]


#ahora voy por le modelo RANDOM FOREST

#primero hay que eliminar los na
dataRF <- na.omit(data_)
bosqueRF <- na.omit(bosque)
RF <- randomForest(dif15n ~ clay + pH + MO. + LAT + LONG + bd  + Ntot + Ptot + 
                     sand + CN + NDWI + Ctot + silt,
                   data = bosqueRF, ntree = 500)
print(RF)
#ahora se puede ver la importancia de cada variable expxlicatoria
importanciaRF <- importance(RF)
varImpPlot(RF)
#tambien se puede ver en grafico de barras
barplot(importanciaRF[ , 1], names.arg = rownames(importanciaRF))

# al parecer lo que más importa es la densidad aparente
#ahora para delta C 13

meta_dataRF <- na.omit(meta_data)
RF1 <- randomForest(dif13c ~ clay + pH + MO. + LAT + LONG + bd + CE + Ntot + Ptot + 
                      sand + CN,
                    data = meta_dataRF, ntree = 100)
print(RF1)
#ahora se puede ver la importancia de cada variable expxlicatoria
importanciaRF1 <- importance(RF1)
print(importanciaRF1)
varImpPlot(RF1)
#tambien se puede ver en grafico de barras
barplot(importanciaRF1[ , 1], names.arg = rownames(importanciaRF1))



RF2 <- randomForest(difcn ~ clay + pH + MO. + LAT + LONG + bd + CE + Ntot + Ptot + 
                      sand + CN , 
                    data = meta_dataRF, ntree = 100)
print(RF2)
#ahora se puede ver la importancia de cada variable expxlicatoria
importanciaRF2 <- importance(RF2)
print(importanciaRF2)
varImpPlot(RF2)
#tambien se puede ver en grafico de barras
barplot(importanciaRF2[ , 1], names.arg = rownames(importanciaRF2))

# ahora voy a apilar los datos de modo tal que estén las 3 
#diferencias en una sola columna para ver que pasa con el modelo
library(tidyr)
library(dplyr)
#primero hacer una matriz con llals variables respuesta
dif_matrix <- cbind( meta_dataRF$dif13c, meta_dataRF$dif15n, meta_dataRF$difcn)
colnames(dif_matrix) <- c( "dif13c", "dif15n", "difcn")
dif_matrix <- as.data.frame(dif_matrix)
df_combinado <- dif_matrix %>%inner_join(meta_dataRF, by = "ID")

# Paso 3: Utilizar el modelo Random Forest con las diferencias apiladas
set.seed(123)  # Fijamos una semilla para reproducibilidad (opcional)
modelo_rf <- randomForest(dif_matrix ~ clay + pH + MO. + LAT + LONG +
                            bd + CE + Ntot + Ptot + sand + CN,
                          data = meta_dataRF, ntree = 300)

importanciarf <- importance(modelo_rf)
print(importanciarf)
varImpPlot(modelo_rf)

#ahora ver que onda un PCA
library(stats)
variables_pca <- dataRF[, c("clay", "pH", "MO.", "LAT","sand")]
pca_result <- prcomp(variables_pca, scale. = TRUE)
pca_result
#visualizacion 

pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])

#agregar los pca al dataframe
dataRF$PC1 <- pca_result$x[, 1]
dataRF$PC2 <- pca_result$x[, 2]
dataRF <- cbind(dataRF, pca_data)
# Eliminar columnas de un dataframe (AGREGUE DOS VECES SIN DARME CUENTA LOS)
borrar <- c("PC1.1","PC2.1")
dataRF <- dataRF[ , !(names(dataRF) %in% borrar)]
# Graficar las observaciones en el espacio de las primeras dos componentes principales
ggscatter(dataRF, x = "PC1", y = "PC2", color = "uso", palette = "jco",
                      shape = "uso", ellipse = T, ellipse.type = "convex")

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

biplot(x = pca_result, sacle=0, cex = 0.6, col = c("black","pink"))
#proporcion de la varianza explicada
prop_varianza <- pca_result$sdev^2 / sum(pca_result$sdev^2)
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:13), aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) + scale_y_continuous(limits = c(0,1)) +
  theme_bw() + labs(x = "Componente principal", y = "Prop. de varianza explicada")

#graficas ENTRE FRACCIONES#

#carbono en pom y maom modo 1
#grafico de puntos con los puntos encerrados mas barras en  e y

SC <- ggscatter(data_ , x="D15N_pom",y="D15N_maom",
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") +
  stat_cor(aes(color=uso), label.x = -5) + geom_abline(intercept = 0, slope = 1) +
  xlim(-7,10) + ylim(-7,10)
xplot <- ggboxplot(data_, x="uso", y="D15N_pom",
                   color = "uso", fill = "uso", palette = "jco",
                   alpha = 0.5 , ggtheme = theme_bw()) + rotate()
yplot <- ggboxplot(data_, x="uso", y="D15N_maom",
                   color = "uso", fill = "uso", palette = "jco",
                   alpha = 0.5 , ggtheme = theme_bw()) 
#limpiar graficar para unirlas
SC <- SC + rremove("legend")
yplot <- yplot + clean_theme() + rremove("legend")
xplot <- xplot + clean_theme() + rremove("legend")
#plotear todo
library(cowplot)
plot_grid(xplot, NULL, SC, yplot, ncol = 2, align = "hv", 
          rel_widths = c(2, 1), rel_heights = c(1, 2))

#ahora intentar poner dos plots juntos mediante scatter

ggscatter(data_ , x="MO.",y=c("pC_maom","pC_pom"), size = 0.3, combine = T,
          ylab = "% de C", color = "uso", palette = "lancet",
          add = "reg.line", conf.int=T) +
  stat_cor(aes(color=uso), method = "spearman") 

ggscatter(data_ , x="CN_pom",y="CN_maom",
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") +
  stat_cor(aes(color=uso), label.x = -5) + geom_abline(intercept = 0, slope = 1) +
  ylim(0,55) + xlim(0,55)

#carbono en pom vs maom modo 2

ggscatter(data_ , x="pC_pom",y="pC_maom", 
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") + 
  stat_cor(aes(color=uso), label.x = 0) + geom_abline(intercept = 0, slope = 1) +
  xlim(0,22) + ylim(0,22) + xlab("% C en POM") + ylab("%C en MAOM")

ggscatter(data_ , x="pN_pom",y="pN_maom", 
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") + 
  stat_cor(aes(color=uso), label.x = 0) + geom_abline(intercept = 0, slope = 1) +
  xlim(0,1) + ylim(0,1) + xlab("% N en POM") + ylab("% N en MAOM")

ggscatter(data_ , x="CN_pom",y="CN_maom", 
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") + 
  stat_cor(aes(color=uso), label.x = 0) + geom_abline(intercept = 0, slope = 1) +
  xlim(0,22) + ylim(0,22) + xlab("CN en POM") + ylab("CN en MAOM")

