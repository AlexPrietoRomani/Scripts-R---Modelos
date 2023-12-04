
###factoriales
fdbca = read.delim("clipboard")
attach(fdbca)
str(fdbca)

boxplot(Y ~ V*D, las=2)
mod.f = aov(Y ~ Bloque + V + D + V*D)
shapiro.test(residuals(mod.f))
bartlett.test(Y ~ interaction(V,D))
summary(mod.f)
interaction.plot(V, D, Y , col=2:5)
interaction.plot(D, V, Y , col=2:5)

mc=SNK.test(mod.f, c("V","D"),console=T)
mc=SNK.test(mod.f, c("D"),console=T)

HSD.test(mod.f,c("V","D"), console=T)

##PARCELAS DIVIDIDAS
parcela=read.delim("clipboard")
attach(parcela)
str(parcela)

boxplot(data=parcela, Y ~ var*dens, las=2)
library(agricolae)
sp.plot(bloq, var, dens, Y)

mod123=aov(data=parcela, Y ~ bloq + var + dens+ var*dens)
shapiro.test(residuals(mod123))
bartlett.test(Y ~ interaction(var,dens))
#summary(mod123)#no es correcto
