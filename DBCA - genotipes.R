#Limpiar memoria
rm(list = ls())
gc()
rm()

###DBCA
library(agricolae)

#subir la base de datos
dbca =read.delim("clipboard")

#Reviando datos
attach(dbca)
str(dbca)

#Realizando caja boxplot
ggplot(dbca, aes(x = genotipo, y = yield)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ANOVA
mod2 = aov(dbca$yield ~ dbca$rep + dbca$genotipo)
summary(mod2)

#Prueba de Normalidad (Shapiro Test)
shapiro.test(residuals(mod2))

#Prueba de Homogeneidad de varianzas(Bartlett Test)
bartlett.test(dbca$yield ~ dbca$genotipo)

#Mostrar el ANOVA
summary(mod2)

#Observar el coeficiente de variación
cv.model(mod2)

# Realizar la prueba de Tukey
tukey <- HSD.test(mod2, "dbca$genotipo", console = TRUE)


#Graficando
bar.err(tukey$means, variation = "SD" ,col=terrain.colors(length(unique(dbca))) , ylab= "yield for genotipe",
        xlab= "Variety", ylim = c(0, 2500))

#Imprimir el barplot
bar.group(tukey$groups,horiz=FALSE,col=terrain.colors(length(unique(dbca))),main = "yield for genotipe Tukey-test",ylab= "yield",
          xlab= "Variety", ylim = c(0, 3000), las = 2)

