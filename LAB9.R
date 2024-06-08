#LABORATORIO 9

rm(list = ls())
stress <- read.table(file.choose(), header =T)
head(stress)
attach(stress)
plot(mese, risposte)

#no lm, ma glm perche è appunto un glm (generalized linear model)
glm(risposte~mese, poisson(link = log)) #link = log e' come non scrivere niente
#, log e' un legame di default (funzione di legame)

#altre funzioni di legame:
glm(risposte~mese, poisson(link = sqrt))
glm(risposte~mese, poisson(link = identity))


modello = glm(risposte~mese, poisson)

summary(modello)

1 - pchisq(50.843-24.57,1)
#alpha oss piccolo: rifiuto H_0

#modello stimato sulla scala delle y
curve(exp(2.8-0.084*x), col = 'red', lwd = 3, add = T)
#eteroschedasticita'




#analisi residui 
res = residuals(modello, type = 'deviance') #default
res = residuals(modello, type = 'pearson') #residui di pearson

#cosa sono i residuals? ci ricordiamo che i residui non hanno una definizione 
#ovvia, perche' non ci sono gli errori, quindi non ho i rappresentanti 
#campionari degli errori 

shapiro.test(res)
#accetto normalita' (pvalue alto)

plot(fitted(modello), res)
#non volgio vedere andamenti sistematici.




#nuovo modello 
modello2 = glm(risposte~mese + I(mese^2), poisson)

anova(modello, modello2, test = 'Chisq')


#altro modello
modello3 = glm(risposte~mese, poisson(link=sqrt))
summary(modello3)

curve((3.94-0.122*x)^2, col = 'blue', lwd = 3, add = T)

summary(glm(risposte~mese, poisson(link = identity)))
curve(14.53 - 0.6693 * x, col = 'green', lwd = 3, add = T)
#linea retta, legame identico

