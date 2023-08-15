#ABRIR
library(tidyverse)
library(readxl)
library(cowplot) #para usar stamp del paquete lubridate hay que hacer stamp::lubridate
library(ggplot2)
library(randomForest)
library(stats)

setwd("/Users/alicia_leon/Desktop/tesis/Bases")

#visualización inicial
ggplot(data, aes(x = D13C, y = D15N, color = uso)) +
  labs(title = "delta isotopico entre fracciones",
       x = "D C 13",
       y = "D N 15") + geom_point(size = 3) + theme_minimal()

ggplot(data, aes(x = D13C, y = D15N, color = uso)) +
  labs(title = "delta isotopico entre fracciones",
       x = "D C 13",
       y = "D N 15") + geom_point(size = 3) + theme_minimal()

#ahora por uso de suelo
BN <- data[data$uso=="BN",]
CU <- data[data$uso=="CU",]
MA <- data[data$uso=="MA",]
PF <- data[data$uso=="PF",]

#EXPLORAR RELACION ENTRE FRACCIONES POR USO
bb <- ggplot(BN, aes(x = D13C, y = D15N, color = fraccion))  +
  geom_smooth(method = "lm", se = F) +
  theme(legend.text = element_text(size = 7), 
        legend.title = element_text(size=7)) +
  labs(title = "delta isotopico entre fracciones, BN",
       x = "D C 13", y = "D N 15") + geom_point(size = 3)
cc <- ggplot(CU, aes(x = D13C, y = D15N, color = fraccion))  +
  geom_smooth(method = "lm", se = F) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size=7)) +
  labs(title = "delta isotopico entre fracciones, CU",
       x = "D C 13", y = "D N 15") + geom_point(size = 3)       
mm <- ggplot(MA, aes(x = D13C, y = D15N, color = fraccion))  +
  geom_smooth(method = "lm", se = F) +
  theme(legend.text = element_text(size = 7), 
        legend.title = element_text(size=7)) +
  labs(title = "delta isotopico entre fracciones, MA",
       x = "D C 13", y = "D N 15") + geom_point(size = 3) 

pp <- ggplot(PF, aes(x = D13C, y = D15N, color = fraccion)) +
   theme(legend.text = element_text(size = 7), 
         legend.title = element_text(size=7)) +
  labs(title = "delta isotopico entre fracciones, PF",
       x = "D C 13", y = "D N 15") + geom_point(size = 3)  

plot_grid(bb, cc, mm, pp, nrow = 2)
#graficas entre pom y maom
#----
#relacion isotopica de N entre pom y maom
a <- frac %>% ggplot(aes(x=D15N_pom,y=D15N_maom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + theme_bw() + 
  ylim(-6,10) + xlim(-6,10)

#relacion isotopica de C para pom y maom
b <- frac %>% ggplot(aes(x=D13C_pom,y=D13C_maom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + theme_bw() + 
  ylim(-31,-22) + xlim(-31,-22) 

#relacion en nutrientes
c <- frac %>% ggplot(aes(x=CN_pom,y=CN_maom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + theme_bw() + 
  ylim(0,52) + xlim(0,52)

plot_grid(a, b, c, nrow = 1) 

#ANALISIS ENTRE RAZONES ISOTÓPICAS
fc13 <- frac %>% ggplot(aes(x=D15N_pom,y=D15N_maom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + 
  theme(legend.text = element_text(size = 7), 
        legend.title = element_text(size=7)) +
  labs(title = "Delta isotopico de N entre fracciones",
       x = "D N 15 POM", y = "D N 15 MAOM") + geom_point(size = 3) +
  ylim(-10,10) + xlim(-10,10)

fn15 <- frac %>% ggplot(aes(x=D13C_pom,y=D13C_maom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + 
  theme(legend.text = element_text(size = 7), 
        legend.title = element_text(size=7)) +
  labs(title = "Delta isotopico de C entre fracciones",
       x = "D C 13 POM", y = "D C 13 MAOM") + geom_point(size = 3) +
  ylim(-32,-23) + xlim(-32,-23)
plot_grid(fc13, fn15, nrow = 1)

#ANALISIS ENTRE CALIDADES

frac %>% ggplot(aes(x=CN_pom,y=CN_maom, col = uso)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + 
  theme(legend.text = element_text(size = 7), 
        legend.title = element_text(size=7)) +
  labs(title = "Razón C/N entre fracciones",
       x = "C/N POM", y = "C/N MAOM") + geom_point(size = 3) +
  ylim(0,55) + xlim(0,55)
#carbono en pom vs maom modo 2

ggscatter(data_ , x="pC_pom",y="pC_maom", 
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") + 
  stat_cor(aes(color=uso), label.x = 0) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,22) + ylim(0,22) + xlab("% C en POM") + ylab("%C en MAOM")

ggscatter(data_ , x="pN_pom",y="pN_maom", 
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") + 
  stat_cor(aes(color=uso), label.x = 0) + 
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,1) + ylim(0,1) + xlab("% N en POM") + ylab("% N en MAOM")

ggscatter(data_ , x="CN_pom",y="CN_maom", 
          color = "uso", palette = "jco",
          shape = "uso", ellipse = T, ellipse.type = "convex") + 
  stat_cor(aes(color=uso), label.x = 0) + 
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,22) + ylim(0,22) + xlab("CN en POM") + ylab("CN en MAOM")
#----
#intento de graficops que son para ver como se distribuye 
#las valores de una variable
#----
# se puede hacer una prueva de colmogorov smirnoff para ver si son diferentes 
pCdensity <- ggplot(frac, aes(pC_maom, fill=uso)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("green",'red',"blue",'#E69F00')) + 
  theme(legend.position = "bottom")
pCdensity

pNdensity <- ggplot(frac, aes(pN_maom, fill=uso)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("green",'red',"blue",'#E69F00')) + 
  theme(legend.position = "bottom")
pNdensity

pCpdensity <- ggplot(frac, aes(pC_pom, fill=uso)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("green",'#999999',"blue",'#E69F00')) + 
  theme(legend.position = "bottom")
pCpdensity
#----
#graficas separadas por uso entre pom y maom
#-----
#bosque nativo, se nombra cada grafica para hacer un grid arrange
dN_bn <- frac[frac$uso=="BN",] %>% ggplot(aes(x=D15N_pom,y=D15N_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de N entre fracciones en Bosque Nativo",
       x = "D N 15 POM", y = "D N 15 MAOM") + geom_point(size = 3) +
  ylim(-5,6) + xlim(-5,6)

dC_bn <-frac[frac$uso=="BN",] %>% ggplot(aes(x=D13C_pom,y=D13C_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de C entre fracciones en Bosque Nativo",
       x = "D C 13 POM", y = "D C 13 MAOM") + geom_point(size = 3) +
  ylim(-31,-24) + xlim(-31,-24)

CN_bn <- frac[frac$uso=="BN",] %>% ggplot(aes(x=CN_pom,y=CN_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "Razón C/N entre fracciones en Bosque Nativo",
       x = "C/N POM", y = "C/N MAOM") + geom_point(size = 3) +
  xlim(0,40) + ylim(0,40)

plot_grid(dN_bn, dC_bn, CN_bn, nrow = 1)

#para cultivos

dN_cu <- frac[frac$uso=="CU",] %>% ggplot(aes(x=D15N_pom,y=D15N_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de N entre fracciones en Cultivos",
       x = "D N 15 POM", y = "D N 15 MAOM") + geom_point(size = 3) +
  ylim(-5,6) + xlim(-5,6)

dC_cu <-frac[frac$uso=="CU",] %>% ggplot(aes(x=D13C_pom,y=D13C_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de C entre fracciones en Cultivos",
       x = "D C 13 POM", y = "D C 13 MAOM") + geom_point(size = 3) +
  ylim(-31,-24) + xlim(-31,-24)
CN_cu <- frac[frac$uso=="CU",] %>% ggplot(aes(x=CN_pom,y=CN_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "Razón C/N entre fracciones en Cultivos",
       x = "C/N POM", y = "C/N MAOM") + geom_point(size = 3) +
  xlim(0,40) + ylim(0,40)


plot_grid(dN_cu, dC_cu, CN_cu, nrow = 1)

#para matorrales

dN_ma <- frac[frac$uso=="MA",] %>% ggplot(aes(x=D15N_pom,y=D15N_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de N entre fracciones en Matorrales",
       x = "D N 15 POM", y = "D N 15 MAOM") + geom_point(size = 3) +
  ylim(-5,6) + xlim(-5,6)
dC_ma <-frac[frac$uso=="MA",] %>% ggplot(aes(x=D13C_pom,y=D13C_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de C entre fracciones en Matorrales",
       x = "D C 13 POM", y = "D C 13 MAOM") + geom_point(size = 3) +
  ylim(-31,-24) + xlim(-31,-24)
CN_ma <- frac[frac$uso=="MA",] %>% ggplot(aes(x=CN_pom,y=CN_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "Razón C/N entre fracciones en Matorrales",
       x = "C/N POM", y = "C/N MAOM") + geom_point(size = 3) +
  xlim(0,40) + ylim(0,40)

plot_grid(dN_ma, dC_ma, CN_ma, nrow = 1)

#para plantacion forestal

dN_PF <- frac[frac$uso=="PF",] %>% ggplot(aes(x=D15N_pom,y=D15N_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de N entre fracciones en P. Forestales",
       x = "D N 15 POM", y = "D N 15 MAOM") + geom_point(size = 3) +
  ylim(-5,6) + xlim(-5,6)
dC_PF <-frac[frac$uso=="PF",] %>% ggplot(aes(x=D13C_pom,y=D13C_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "D isotópico de C entre fracciones en P. Forestales",
       x = "D C 13 POM", y = "D C 13 MAOM") + geom_point(size = 3) +
  ylim(-31,-24) + xlim(-31,-24)
CN_PF <- frac[frac$uso=="PF",] %>% ggplot(aes(x=CN_pom,y=CN_maom)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)  +
  labs(title = "Razón C/N entre fracciones en P. Forestales",
       x = "C/N POM", y = "C/N MAOM") + geom_point(size = 3) +
  xlim(0,40) + ylim(0,40)

plot_grid(dN_PF, dC_PF, CN_PF, nrow = 1)

#grid de todos
plot_grid(dN_bn, dC_bn, CN_bn, dN_cu, dC_cu, CN_cu, dN_ma, dC_ma, CN_ma, dN_PF, 
          dC_PF, CN_PF, nrow=4, ncol = 3)
#----
#capacidad de c por unidad de n para fracciones
frac %>% ggplot(aes(x=pC_maom,y=pN_maom, color = uso)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + geom_smooth(method = "lm", se = F)  +
  labs(title = "C por unidad de N en maom",
       x = "C", y = "N") + geom_point(size = 3)

frac %>% ggplot(aes(x=pC_pom,y=pN_pom, color = uso)) +
  geom_smooth(method = "lm", se = F) + 
  geom_point() + geom_smooth(method = "lm", se = F)  +
  labs(title = "C por unidad de N en pom",
       x = "C", y = "N") + geom_point(size = 3)

#relacion entre C y N para la base de Ignacio y mía
datos_ %>% ggplot(aes(x=Ctot,y=Ntot, color = Landcover)) + 
  geom_point() + geom_smooth(method = "lm", se = F)  +
  labs(title = "C por unidad de N",
       x = "C", y = "N") + geom_point(size = 3)

datos_ %>% ggplot(aes(x=clay,y=MO./clay, color = Landcover)) + 
  geom_point() + geom_smooth(method = "lm", se = F)  +
  labs(title = "OM:clay por unidad de arcilla",
       x = "clay content", y = "OM/clay") + geom_point(size = 3)

datos_ %>% ggplot(aes(x=NDWI,y=Ctot, color = Landcover)) + 
  geom_point() + geom_smooth(method = "lm", se = F)  +
  labs(title = "C por NDWI",
       x = "NDWI", y = "C") + geom_point(size = 3)
#----

# explorar estadísitca para saber si las pendientes estan significativamente arriba
#o abajo de la linea 1:1

#t studnet pareada para ver si los puntos estan significativamente sobre o bajo 
#la linea 1:1 en el caso de las diferencias pom vs. maom
#----
#puedo hacer lo mismo para uso de suelo para comparar dentro
frac <- na.omit(frac) 
#diferencias entre los dos valores de la misma replica dato a comparar
frac$DN_dif <- frac$D15N_maom - frac$D15N_pom
frac$DC_dif <- frac$D13C_maom - frac$D13C_pom
frac$CN_dif <- frac$CN_pom - frac$CN_maom  
#los valores del estadigrafo  t
t.dn <- abs(mean(frac$DN_dif) / (sd(frac$DN_dif)/sqrt(nrow(frac))))
t.dc <- abs(mean(frac$DC_dif) / (sd(frac$DC_dif)/sqrt(nrow(frac))))
t.cn <- abs(mean(frac$CN_dif) / (sd(frac$CN_dif)/sqrt(nrow(frac))))
#vcalcular el valor de p (dos colas)
p.dn <- 2*pt(t.dn, nrow(frac), lower.tail = F)
p.dc <- 2*pt(t.dc, nrow(frac), lower.tail = F)
p.cn <- 2*pt(t.cn, nrow(frac), lower.tail = F)

#son normales los datos?
qqnorm(frac$DN_dif)
qqline(frac$DN_dif) #aproixmadamente normal, colas pesadas pero pasable
qqnorm(frac$DC_dif)
qqline(frac$DC_dif)
qqnorm(frac$CN_dif)
qqline(frac$CN_dif)


#para cultivos
CU_ <- frac[frac$uso=="CU",]
#D15
# Paso 1: Calcular la diferencia entre "x" e "y" para cada par de medidas repetidas
diferencias <- CU_$D15N_maom - CU_$D15N_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado <- t.test(diferencias)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado) #p-value = 0.004574 ; mean of x : 0.5443229


#d13
diferencias1 <- CU_$D13C_maom - CU_$D13C_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado1 <- t.test(diferencias1)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado1) #p-value = 2.126e-10; mean of x 0.875746 


#CN
diferencias2 <- CU_$CN_maom - CU_$CN_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado2 <- t.test(diferencias2)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado2)  #p-value =0.03484 ; mean of x : -1.871248 


# matorrales ()
MA_ <- frac[frac$uso=="MA",]

#d15
diferencias3 <- MA_$D15N_maom - MA_$D15N_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado3 <- t.test(diferencias3)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado3) #p-value = 0.1299 ; mean of x : 0.8176839 

#c13
diferencias4 <- MA_$D13C_maom - MA_$D13C_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado4 <- t.test(diferencias4)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado4) # p-value = 3.134e-07 : mean of x : 1.036917 

#hacer un loop pa no estar como las weonas repitiendolo pa cada weá

#CN
diferencias5 <- MA_$CN_maom - MA_$CN_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado5 <- t.test(diferencias5)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado5)  #p-value = 0.0003456 ; mean of x : -6.198583

#bosque nativo
BN_ <- frac[frac$uso=="BN",]
#CN
diferencias6 <- BN_$CN_maom - BN_$CN_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado6 <- t.test(diferencias6)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado6) # p-value = 0.06503 ; mean of x : -1.6693 


# C13
diferencias7 <- BN_$D13C_maom - BN_$D13C_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado7 <- t.test(diferencias7)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado7)  # p-value = 8.242e-08 ; mean of x : 0.8626626 

#N15

diferencias8 <- BN_$D15N_maom - BN_$D15N_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado8 <- t.test(diferencias8)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado8) #p-value = 0.01119 ; mean of x : 0.7115085 

#forestales
PF_ <- frac[frac$uso=="PF",]
#CN
diferencias9 <- PF_$CN_maom - PF_$CN_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado9 <- t.test(diferencias9)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado9) # p-value = 0.0001751 ; mean of x -7.316063  


# C13
diferencias10 <- PF_$D13C_maom - PF_$D13C_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado10 <- t.test(diferencias10)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado10)  # p-value = 8.242e-08 ; mean of x : 0.8626626 


#N15
diferencias11 <- PF_$D15N_maom - PF_$D15N_pom
# Paso 2: Realizar el t de Student pareado
resultado_t_pareado11 <- t.test(diferencias11)
# Paso 3: Imprimir los resultados del t de Student pareado
cat("Resultado del t de Student pareado:\n")
print(resultado_t_pareado11) #p-value = 0.0004918 ; mean of x : 0.863656 


