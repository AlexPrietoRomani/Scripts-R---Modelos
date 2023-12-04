
#08-03-2022
#Diseño cuadrado latino (DCL)
lsd=read.delim("clipboard")
attach(lsd)
str(lsd)
summary(lsd)

boxplot(rdt ~ trt)
#construir el ANVA para DCL
dcl_m = aov(rdt ~ F + C + trt)
shapiro.test(residuals(dcl_m))
bartlett.test(rdt ~ trt)
summary(dcl_m)
library(agricolae)
cv.model(dcl_m)

##Comparacion de medias Prueba t
SNK.test(rdt, trt, 12, 4.72,
         alpha = 0.05, group=TRUE,console=TRUE)
B=SNK.test(dcl_m,"trt", console=T)
bar.err(B$means,variation="SD",ylim=c(0,70), col="green", ylab="Rendimiento")
box()        
bar.group(B$groups,ylim=c(0,70), col="blue", ylab="Rendimiento",
          main="Comparación de medias SNK.test")
box() 
