#LABORATORIO 8

#e' di interesse verificare se esistono differenze in media nei risultati 
#maschili e femminili, tenendo presente la relazione che esiste tra l'anno e il
#record
#c'e interazione tra sesso e anno?

rm(list = ls())
r <- read.table(file.choose(), header = T, stringsAsFactors = T) #olimpiadim_mf
#stringsAsFactors rende le modalita' di sesso M e W delle livelli di un 
#fattore, cioe' di una variabile qualitativa
head(r)

r$Sesso
#rappresentazione grafica per avere un'idea di cio' che ci aspettiamo
plot(Risultato~Anno, pch = as.character(Sesso), data = r)

#notiamo che il risultato migliora nel tempo, e uomini e donne hanno un 
#record distinto




#Poiche' il grafico mostra una relazione monotona decrescente tra anno e record,
#consideriamo dapprima un modello di regressione lineare semplice che non tiene 
#conto della differenza tra uomini e donne
summary(lm(Risultato~Anno, data = r))


#dal grafico si nota anche che i punti distinti per maschi e femmine mostrano 
#un andamento simile, ma su livelli diversi. percio' stimiamo il modello senza
#la variabile anno
summary(lm(Risultato~Sesso, data = r))

#si, effettivamente la variabile sesso e' sicuramente rilevante


#estraggo i b1cap e b2cap nel modello stimato. questa è la retta di regressione 
#stimata. 
abline(coef(fit0))
#la distanza delle osservaizoni dalla retta è grande, in quanto R^2 e' piccolo

#non posso usare una sola retta per spiegare il modello. Devo tener conto anche
#del sesso. 
#Il modello passa da marginale ad analisi della covarianza.

fit1 <- lm(Risultato~Anno + Sesso, data = r)
summary(fit1)

#I risultati suggeriscono che tutti i coefficienti di regressione, 
#singolarmente considerati, sono significativi

#Il modello stimato prevede due rette parallele, con coefficiente angolare 
#βˆ2 = −0.013, ma differenti intercette: βˆ1 = 36.220 per i maschi e 
#βˆ1 + βˆ3 = 36.220 + 1.072 = 37.292 per le femmine. In altre parole, 
#la differenza prevista tra i record degli uomini e i record delle donne e'
#costante nel tempo20 ed e' di circa un secondo in piu' per le donne.

#Il modello fit1 mostra un adattamento migliore ai dati, con un coefficiente di
#determinazione pari a R2 = 0.883.

abline(coef(fit1)[1:2], lty = 2)
abline(coef(fit1)[1:2]+c(coef(fit1)[3],0), lty = 2)


#Per verificare se e' ragionevole l’ipotesi per cui la differenza tra uomini 
#e donne e' costante nel tempo (assenza di interazione)
fit2 <- lm(Risultato~Anno+Sesso+Anno:Sesso, data = r)
summary(fit2)
#oppure
summary(lm(Risultato~Anno*Sesso,data=r))
abline(coef(fit2)[1:2], lty = 3)
abline(coef(fit2)[1:2]+coef(fit2)[3:4], lty = 3)









###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################









#esempio 2
rm(list = ls())
Gatti <- read.table(file.choose(), col.names = c('pcorpo', 'pcuore', 'sesso'))
head(Gatti)
attach(Gatti)
Gatti$sesso[Gatti$sesso == 1] = 'F'
Gatti$sesso[Gatti$sesso == 2] = 'M'
Gatti$sesso = as.factor(Gatti$sesso)

plot(pcuore~sesso)
plot(pcuore~pcorpo)
shapiro.test(pcuore[sesso=='F'])

shapiro.test(pcuore[sesso == 'M'])

var.test(pcuore[sesso == 'F'], pcuore[sesso =='M'])

t.test(pcuore[sesso == 'F'], pcuore[sesso =='M'], alternative = 'less', var.equal = T)


#test t dal punto di vista della regressione (analisi marginale)
summary(lm(pcuore~sesso))

#effetto marginale del peso del cuore
1 - pt(4.842, 46)#46 sono i gradi di liberta'

#effetto marginale del peso del corpo
summary(lm(pcuore~pcorpo))


plot(pcuore~pcorpo, pch = as.character(sesso))
#fissato il peso del corpo, non ce una differenza sistematica tra M e F

gatti.lm <- lm(pcuore~pcorpo*sesso)
#in questo modo tiene conto anche dell'interazione 

summary(gatti.lm)
#i pvalue di sessoM e pcorpo:sessoM non significano che posso togleire sesso 
#dal modello
#stimo un modello senza interazione per vedere se sesso e' significativo o no

#test F 
gatti.lm.rid <-  lm(pcuore~pcorpo)
anova(gatti.lm.rid, gatti.lm)
#accetta ip. nulla b3 = b4
#posso togliere 

