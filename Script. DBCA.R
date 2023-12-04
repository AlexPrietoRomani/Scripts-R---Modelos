
###DBCA
#subir la base de datos

david=read.delim("clipboard")
attach(david)
str(david)

boxplot(yield ~ genotipo)
mod2 = aov(yield ~ rep + genotipo)
shapiro.test(residuals(mod2))
bartlett.test(yield ~ genotipo)
summary(mod2)
cv.model(mod2)






########
afidos=read.delim("clipboard")
attach(afidos)
str(afidos)

boxplot(Aphidos ~ TRAT)
mod3= aov(Aphidos ~ BLOQ + TRAT)
shapiro.test(residuals(mod3))
bartlett.test(Aphidos ~ TRAT)

mod4 = aov(Aphidos.T ~BLOQ + TRAT)
shapiro.test(residuals(mod4))
bartlett.test(Aphidos.T ~ TRAT)

##PRUEBAS NO PARAMETRICAS
kruskal(Aphidos, TRAT, alpha = 0.05, p.adj=c("bonferroni"), group=TRUE, main = NULL,console=T)

####DBCA
dbca=read.delim("clipboard")
attach(dbca)
str(dbca)

boxplot(rdt ~ trt)
mod5 = aov(rdt ~ bloq + trt)
shapiro.test(residuals(mod5))
bartlett.test(rdt ~ trt)
summary(mod5)
cv.model(mod5)
LSD.test(mod5, "trt", console=T)

