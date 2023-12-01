#IMPORTANDO LIBRERIA
library(FactoMineR)
library(ISLR)
library(factoextra)
library(ggplot2)

#Leer datos del portapapeles, sep = "\t"
datos_nci <- read.delim("clipboard", header = TRUE)

#VISUALIZAR LA DATA
head(datos_nci)[, 2:8]

#---------------------PCA ------
#Creando PCA para determinar cantidad de Componentes
pca_nci <- prcomp(datos_nci[, 2:8], scale = TRUE)

# Muestra de los primeros 7 elementos del vector de loadings de los 5 primeros componentes
head(pca_nci$rotation)[, 1:7]

#IDENTIFICAR EL NÚMERO TOTAL DE COMPONENTES PRINCIPALES DE DATAFRAME
dim(pca_nci$rotation)

#Data de los valores del PCA 
head(pca_nci$x)[,1:7]

#presentación del PCA 
summary(pca_nci)

#---------------------Grafico de PCA -------------------------------------------------
#colecta de Posisciónes del eje x,y del gráfico Individuals -PCA
plot <- fviz_pca_ind(pca_nci, geom.ind = "point",
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 

#Gráfica de representación de las observaciones sobre las dos primeras componentes principales
fviz_pca_ind(pca_nci, geom.ind = "point",
             col.ind = "#FFFFFF", 
             axes = c(1, 2), 
             pointsize = 1.5, ) + 
  geom_text(aes(x = plot[["data"]][["x"]], 
                y = plot[["data"]][["y"]], label = datos_nci$Plot), 
            size = 2.3)

#---------------------Biplot------------------------------
#Gráfico de Representación de los vectores en el plano cartesiano según su Cos2
fviz_pca_var(pca2.nci, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)

#Generación del biplot con los componentes principales
biplot(pca_nci, scale = 0, cex = 0.5, col.var = "white", repel = TRUE, geom = "text")

#-------------------Elección del número de componentes principales-------
#Barplot de componentes principales explicados del modelo PCA
fviz_screeplot(pca_nci, addlabels = TRUE, ylim = c(0, 35))


#------------------------------Extracción de datos de los ejes según su plot--------
# Extraer la información de las coordenadas y la columna usada en las etiquetas
data_extracted <- plot$data

# Agregar la columna utilizada en las etiquetas (datos_nci$Plot)
data_extracted$Plot <- datos_nci$Plot

# Exportar el dataframe a un archivo CSV en una ubicación específica
write.csv(data_extracted, file = "D:/OneDrive - Camposol S.A/Desktop/draft/México/PCA-results.csv", row.names = FALSE)
