#PCA
dataRF <- na.omit(data_)
forestalRF <- na.omit(forestal)

RF <- randomForest(dif15n ~ clay + pH + MO. + LAT + LONG + bd  + Ntot + Ptot + 
                     sand + CN + NDWI + Ctot + silt,
                   data = forestalRF, ntree = 500)
print(RF)
#ahora se puede ver la importancia de cada variable expxlicatoria
importanciaRF <- importance(RF)
varImpPlot(RF)
#tambien se puede ver en grafico de barras
barplot(importanciaRF[ , 1], names.arg = rownames(importanciaRF))

