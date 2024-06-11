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
#ci sono gia i 'levels' perche' nel read.table e' stato messo stringsAsFactors
#= T
#rappresentazione grafica per avere un'idea di cio' che ci aspettiamo
plot(Risultato~Anno, pch = as.character(Sesso), data = r)

#notiamo che il risultato migliora nel tempo, e uomini e donne hanno un 
#record diverso: gli uomini sembrano essere piu' veloci




#Poiche' il grafico mostra una relazione monotona decrescente tra anno e record,
#consideriamo dapprima un modello di regressione lineare semplice che non tiene 
#conto della differenza tra uomini e donne: analisi marginali

#c'e un effetto marginale dell'anno sul risultato?
fit0 <- lm(Risultato~Anno, data = r)
summary(fit0)
#coeff. ang. negativo, cioe' ogni anno che passa ho un miglioramento del record 
#di -0.009148 rispetto all'anno precedente.
#il pvalue e' motlo basso quindi l'anno e' significativo 


#dal grafico si nota anche che i punti distinti per maschi e femmine mostrano 
#un andamento simile, ma su livelli diversi. percio' stimiamo il modello senza
#la variabile anno
summary(lm(Risultato~Sesso, data = r))

#si, effettivamente la variabile sesso e' sicuramente rilevante


#estraggo i b1cap e b2cap nel modello stimato. questa è la retta di regressione 
#stimata. 
abline(coef(fit0))
#la distanza delle osservaizoni dalla retta è grande, in quanto R^2 e' piccolo.
#i dati non vengono spiegati bene da questo modello di regressione

#non posso usare una sola retta per spiegare il modello. Devo tener conto anche
#del sesso, altrimenti la somma dei residui e' molto grande 

#Il modello passa da marginale (regr. semplice)
#ad analisi della covarianza (senza interazione)

#stiamo pensando che il sesso dell'atleta influenzi il record ma non la 
#tra record e anno 
#c'e un cambiamento di intercetta da M a W, non di coefficiente angolare.

#ora e' regressione multipla. non stiamo studiando l'effetto marginale, bensi'
#l'effetto di ciascuna v. exp. condizionatamente rispetto all'altra 

fit1 <- lm(Risultato~Anno + Sesso, data = r)
summary(fit1)

#I risultati suggeriscono che tutti i coefficienti di regressione, 
#singolarmente considerati, sono significativi

#Il modello stimato prevede due rette parallele, con coefficiente angolare 
#βˆ2 = −0.013, ma differenti intercette: βˆ1 = 36.220 per i maschi e 
#βˆ1 + βˆ3 = 36.220 + 1.072 = 37.292 per le femmine. In altre parole, 
#la differenza prevista tra i record degli uomini e i record delle donne e'
#costante nel tempo ed e' di circa un secondo in piu' per le donne: 1.0724886
#il tempo migliora di ogni anno di circa 0.013 secondi

#Il modello fit1 mostra un adattamento migliore ai dati, con un coefficiente di
#determinazione pari a R2 = 0.883, in quanto la somma dei quadrati dei residui 
#piccola

#anno e sesso hanno sia un effetto marginale, che un effetto condizionato sign.

#il tempo del record migliora

abline(coef(fit1)[1:2], lty = 2, col = 'green') #uomini
abline(coef(fit1)[1:2]+c(coef(fit1)[3],0), lty = 2, col = 'orange') #donne   
#cambia solo l'intercetta, b1, ma non la pendenza delle rette
#nella retta delle donne l'intercetta e' b1+b3, per gli uomini e' solo b1



#-----------------------STIMA DEL MODELLO CON INTERAZIONE-----------------------
#prossima domanda da porsi:
#l'evoluzione del tempo del record di anno in anno e' la stessa per M e W? ossia
#ha senso presupporre lo stesso coeff. ang. per M e W, o c'e' una differenza
#significativa tra coeff. ang. di M e W? ossia
#il modo che ha il sesso dell'atleta di influenzare il tempo del record cambia 
#di anno in anno? ossia
#il sesso dell'atleta influenza solo il tempo del record o anche la relazione
#tra recod e anno? ossia (stringi stringi)
#cambia il coeff. ang. o no?


#Per verificare se e' ragionevole l’ipotesi per cui la differenza tra uomini 
#e donne e' costante nel tempo (assenza di interazione)
fit2 <- lm(Risultato~Anno+Sesso+Anno:Sesso, data = r)
summary(fit2)
#oppure
summary(lm(Risultato~Anno*Sesso,data=r))


#notiamo che il sesso e l'interazione non sono significative, 
#la non significativita' dell'interazione significa che non ho evidenza che 
#il sesso dell'atleta influenzi la relazione tra record e anno 
#pero' tra le due ha senso togliere soltanto l'intereazione, altrimenti cio' 
#che diremmo non avrebbe senso.
#RICORDA CHE SI PUO' TOGLIERE UNA VARIABILE ALLA VOLTA, NON PIU' VARIABILI ALLA
#VOLTA. TOLGO UNA E LASCIO TUTTO IL RESTO (TOLGO L'INTERAZIONE)
#torno quindi al modello finale senza interazione

# l’effetto della variabile Sesso sulla variabile risposta Risultato non cambia 
#a seconda dell’Anno


abline(coef(fit2)[1:2], lty = 3, col = 'blue')
abline(coef(fit2)[1:2]+coef(fit2)[3:4], lty = 3, col = 'violet')

#noto che non cambia molto rispetto al modello senza interazione:
#le rette anche se hanno il vincolo di non parallelismo sono quasi parallele, 
#cio' indica la non singificativita' dell'interazione
#ergo non serve integrare l'effetto l'interazione nel modello.






###############################################################################
###################################ESEMPIO 2####################################
###############################################################################

#studio del peso del cuore dei gatti dalle variabili peso del corpo (covariata)
#e il fattore sesso del gatto 
rm(list = ls())
Gatti <- read.table(file.choose(), col.names = c('pcorpo', 'pcuore', 'sesso'))
head(Gatti)
attach(Gatti)
#suddivido i dati per sesso dei gatti 
Gatti$sesso[Gatti$sesso == 1] = 'F' #a tutti gli 1 sostituisci 'F'
Gatti$sesso[Gatti$sesso == 2] = 'M'
Gatti$sesso = as.factor(Gatti$sesso)#con questo comando rendo 'sesso' un fattore
#ok, ora ho la v. qualitativa pronta


#studio marginale 
plot(pcuore~sesso)
#cuore meno pesante nelle donne (1)
plot(pcuore~pcorpo)
#il peso del cuore cresce con il crescere del peso del corpo

#test t student si basa sulla nromalita' e osmosch.

#test normalita'
shapiro.test(pcuore[sesso == 'F'])
#verificata normalita' (>0.05)

shapiro.test(pcuore[sesso == 'M'])
#ampiamente accettata la normalita'

var.test(pcuore[sesso == 'F'], pcuore[sesso =='M'])
#pvalue abbastanza alto (>0.05): non ho grosse evidenze contro l'omosch.


t.test(pcuore[sesso == 'F'], pcuore[sesso =='M'], 
       alternative = 'less', var.equal = T)

#assumo omosch. var.equal = T

#alternativa unidirez. sinistra, allontanamento verso sinistra .
#1° (femmine) gruppo minore del 2° gruppo (maschi) alternative = 'less'

#la differenza tra le medie e' ampiamente significativa



#@@@@@@     PUNTO DI VISTA DELLA REGRESSIONE
#test t dal punto di vista della regressione (analisi marginale, solo effetto
#del sesso)
summary(lm(pcuore~sesso))


#alternativa unidirezionale dx, b2 > 0
#pvalue bidirezionale
1 - pt(4.842, 46)#46 sono i gradi di liberta'
#equivalente a pvalue di 
t.test(pcuore[sesso == 'F'], pcuore[sesso =='M'], 
              alternative = 'less', var.equal = T)
#ossia del test unidirezionale sinistro



#effetto marginale del peso del corpo
summary(lm(pcuore~pcorpo))
#anche il peso del corpo ha effetto marginale sul peso del cuore



#STIMA CON UN UNICO MODELLO
plot(pcuore~pcorpo, pch = as.character(sesso))
#fissato il peso del corpo, NON c'e' una differenza sistematica tra M e F
#infatti noto che a diversi livelli di ascissa non e' detto che M abbia un peso
#del cuore piu' alto di F
#quindi ci viene da postulare che il peso del corpo sia la variabile che spiega 
#meglio il peso del cuore

gatti.lm <- lm(pcuore~pcorpo*sesso)
#in questo modo tiene conto anche dell'interazione 

summary(gatti.lm)
#i pvalue di sessoM e pcorpo:sessoM NON significano che posso togliere 
#contemporaneamente sesso e interazione dal modello 


#stimo un modello senza interazione per vedere se sesso e' significativo o no:
#test F: H_0 : b3 = b4 = 0
gatti.lm.rid <-  lm(pcuore~pcorpo+sesso)
anova(gatti.lm.rid, gatti.lm)
#accetta ip. nulla b3 = b4 = 0
#posso togliere sesso e l'interazione dal modello. 
#LI POSSO TOGLIRE CONTEMPORANEAMENTE. L'UNICA VARIABILE IMPORTANTE E' IL PESO
#il sesso ha effetto marginale ma non effetto condizionato

