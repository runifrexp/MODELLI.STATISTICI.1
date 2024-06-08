#lab4


### DATI HILLS.
rm(list=ls())
Corse = read.table(file.choose()) #hills.dat
#y = time (tempo migliore)
#x1 = dist (distanza percorsa)
#x2 = climb (dislivello)
Corse
attach(Corse)
head(Corse)

pairs(Corse)
#con questo comando osservo le correlazioni tra le v. exp e la corr tra le
#v. exp e la v. risposta (dist, climb), (dist, time), (climb, time) 
#osserviamo le relazioni marginali tra le variabili
#si vede relazione positiva tra climb e time, presumo che una delle due 
#esplicative sia inutile, e temo che non potro' stimare il modello

#non voglio esplicative ridondanti. se ce relazione lin. tra le esplicative 
#fa capire che non si puo' stimare il modello

#per osservare la correlazione 
cor(Corse)

#stimando un modello di regressione lineare semplice con la sola variabile 
#esplicativa distanza, che risulta maggiormente correlata con il tempo migliore
#comportamento di time al netto di climb 
corse.1.lm = lm(time~dist)
summary(corse.1.lm)
# - pvalue b1 = 0.406, quindi sembra si possa togliere b1 dal modello
# - pvalue b2 < 0.001, quindi non si puo' rifiutare H_0 nel senso che 
#   b2 e' significativo per la stima del modello
# - R2 e' sodddisfacente: l'85% della variabilita' del modello viene spiegata 
#   attraverso la dipendenza dalla variabile distanza



#comportamento di climb al netto di dist
climb.rstand = rstandard(lm(climb~dist))
climb.rstand

#comportamento di time al netto di dist
#Per valutare se comunque la variabile dislivello possa contribuire a migliorare
#il modello, consideriamo il diagramma di dispersione dei residui 
#studentizzati rispetto a tale variabile

corse.1.rstand=rstandard(corse.1.lm)

#grafico di climb e e di time al netto di dist
plot(climb.rstand, corse.1.rstand)

#vedo una relazione positiva, quindi:
#climb ha ancora qualcosa da dirmi sul tempo anche quando la distanza ha 
#spiegato quello che poteva spiegare sul comportamento del tempo di percorrenza

#senso del modello: cosa ha climb da dirmi in piu' rispetto a dist, ossia quella 
#gia' presente nel modello.

#in questo grafico, distanza non gioca alcun ruolo, perche' e' stata tolta sia 
#dalle ascisse che dalle ordinate.
# - ordinate: comportamento del tempo al netto della dist.
# - ascisse: comportamento di climb al netto della dist.
#dato che c'e' relazione, climb ha qualcosa da dirmi in piu' sulla risposta.


#questa discussione ci induce a includere sia dist che climb per la stima di 
#time



##ecco il modello multiplo

#stima della reressione multipla
corse.2.lm = lm(time~dist+climb)
summary(corse.2.lm)

#climb (b3) 
# - estimate = 0.011048 (stima di b3)
# - std. error = 0.002051 (radice della stima della varianza della stima di b3)
# - t value = 5.387 (t3 oss = stima b3/std.error, st. alla WALD, 
#             b3 stimato - b3 dichiarato)
# - Pr(>|t|) = 6.45e-06 *** (alfaoss), quindi rif H_0. c'e' una relazione 
#              altamente significativa tra climb e time

#- res std error = 14.68 on 32 degrees of freedom (s = sqrt(s2)) 32 = n-p = 35-3
#- Multiple R-squared = 0.9191 (R2)
#- Adjusted R-squared = 0.914 (RC2) (R2 corretto, anche per modelli non annidati)
#- F-statistic = 181.7 on 2 and 32 DF  (si fa la verifica della 
#              significativita' dell'intero modello).
#              ci sono due esplicative quindi sto verificando l'ipotesi di poterle
#              togliere entrambe, rispetto all'alternativa che almeno una sia 
#              rilevante. p-p0 = 3-1 = 2, n-p = 34-2 = 32
# - p-value: < 2.2e-16 (alfaoss F-statistic): quasi zero, quindi rifiuto H_0, 
#              ergo entrambe le variabili sono molto significative



#aggiorno il modello precedente solo con dist. per modelli grandi lavoro meglio
#aggiornando il modello man mano, se devo togliere un esplicativa basta mettere
#meno al psoto di +climb
corse.2.lm = update(corse.1.lm, .~.+climb)
corse.2.lm =lm(time~dist+ climb)



#test equivalente, un altro punto di vista dell'ipotesi di nullita' di b3 
#test alla WILKS. test F al posto del test t per la verifica di nullita' 
#di un parametro (b3)
#M0 e' quello senza climb, M1 e' il modello con climb. come si fa in R il 
#confronto tra modelli annidati? cosi': si confrontano le varianze stimate dai
#modelli.
anova(corse.1.lm, corse.2.lm) #confronta i due modelli (M0, M1) p=3, p0=2
# - Model1 : M0
# - Model2 : M1

# - Res. Df 
#1   33 (n-p0)
#2   32 (n-p)

# - RSS (residual sum of squares)
#1   13141.6 (n*sigma2.cap) (eT0*e0)
#2   6891.9 (n*sigma2.tilde) (eT*e)

# - Df
#    1         p-p0 = (n-p0) - (n-p)

#Sum of Sq
#    6249.7    (eT0*e0) - (eT*e) = n*(sigma2.tilde-sigma2.cap)

#F
#    29.018     Foss

#Pr(>F)  
#  6.445e-06 ***     p(F_(p-p0, n-p) > Foss)


#test F sulla significativita' dell'intero modello
anova(lm(time~1),corse.2.lm) 
#uguale all'ultima riga di:
#corse.2.lm = lm(time~dist+climb)
#summary(corse.2.lm)



#plot del modello completo stimato
plot(corse.2.lm, which = c(1,2)) 

#1 grafico: la distribuzione non sembra seguire un modello. 
## vengono indicati degli otuliers per la 7ma e la 18ma corsa

#2 grafico: il qqplot sembra seguire normalita' ma non per le corse 7 e 18


#dopo quindi aver notato che ci sono due outliers estraggo i residui 
#studentizzati per verificarne la normalita' con il test di Shapiro-Wilk

#residui std
corse.standard = rstandard(corse.2.lm)
shapiro.test(corse.standard)

#viene rifiutato per via di due outliers, voglio ripetere l'analisi del dataset 
#senza gli outliers, che ho gia' individuato 
#grazie a plot(corse.2.lm, which = c(1,2)) 
Corse[c(7,18),]
Corse1 = Corse[-c(7,18),]
Corse1 #come corse ma senza osservazioni anomale
detach(Corse)
attach(Corse1)
#e rifaccio tutta l'analisi fatta finora

#si trova che climb e dist sono significative ma si accetta la normalita'








### DATI US.CRIME
#ci sono tante v. exp, devo trovare quelle importanti 
#ci sono 47 us (alcuni stati USA)

rm(list=ls())
UScrime = read.table(file.choose(), header = T) #UScrime.dat
head(UScrime)

round(cor(UScrime), 2)

#come si usava la procedura per capire quali erano le variabili imprescindibili 
#per stimare il modello: la procedura in avanti e in indietro
#pero' sono problematiche
lm.1 = lm(Crime~Po1, data=UScrime)
summary(lm.1)

lm.t = lm(Crime~., data=UScrime)
summary(lm.t)

add1(lm.1,scope=lm.t,data=UScrime,test="F")

#l'indice AIC mi dice di introdurre la variabile Ineq
#provo ad aggiugnere una variabile in più fino a quando ottengo tutti pvalue 
#grandi, piu grandi di 0.05, e quello che ottengo è il modello finale 

#posso fare anche l'opposto ossia togliere le vairabili con pvalue alto, e tolgo
#finche' togliere qualsiasi cosa provoca un danno significativo

#problemi di questi modi di procedere
# • l'importanza di una variabile potrebbe cambiare da modello a modello.
# • maledizione dei test multipli (vedi comp.)




#usiamo questo nuovo approccio per selezionare le variabili:
#confronto tutti i sottoinsiemi di v. exp, confronto tra modelli non
#annidati basati sul Ck di Mallows

library(leaps)
leaps.crime = leaps(UScrime[,1:15], UScrime$Crime, nbest = 1)
#primo argomento: v. exp candidate
#secondo argomento: v. risposta (16ma colonna, posso chiamarla per nome)

#cosa fa leaps? con il Ck di Mallows 
#stima tutti i modelli possibili con una variabile, poi due v. exp. poi tre, 
#fino a 15
#tra tutti i modelli che includono una sola variabile mostrami solo il migliore
#(nbest), quello con il Ck di Mallows piu' piccolo 
#e cosi' via per due v. exp, tre, fino a 15




leaps.tab = data.frame(k = leaps.crime$size, Ck = leaps.crime$Cp)
#$size corrisponde a quello che nella teoria abbiamo chiamato k
#$Cp e' il nostro Ck (in letteratura si chiama Cp)
round(leaps.tab,2)


##    k    Ck
##1   2 40.00
##2   3 25.07
##3   4 13.64
##4   5 10.16
##5   6  6.26
##6   7  3.86   <- qui mi fermo: e' il k piu' piccolo con Ck < k. e' il 6°
##7   8  4.49      modello stimato, quello con 6 v. exp. (intercetta + 6 exp)
##8   9  4.24
##9  10  5.64
##10 11  7.13
##11 12  8.75
##12 13 10.48
##13 14 12.24
##14 15 14.00
##15 16 16.00


#ora devo identificare quali sono le 6 v. exp.: come faccio?

leaps.crime$which
#mi dice quali variabili sono incluse e quali no per tutti i 
#modelli (con 1,2,...15 v. exp.)
#FALSE in corrispondenza delle ariabili NON introdotte 
#FALSE in corrispondenza delle ariabili  introdotte 

#per v.exp = 6
#TRUE FALSE TRUE TRUE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE TRUE TRUE FALSE
#quindi verranno introdotte la 1,3,4,11,13,14 -me v. exp nel miglior modello
#dal punto di vista del Ck di Mallows, ossia seguendo BDA e parsimonia.


#--------------------------RIASSUNTO LEAPS--------------------------------------
#ricorda: scegli il modello con # di variabili in cui Ck < k per capire
#quante variabili tenere nel modello migliore
#scegli le variabili tramite leaps.crime$which


#con dei semplici comandi: 
v.espl = names(UScrime)[1:15]
v.espl
v.espl[leaps.crime$which[6,]]
#queste sono le variabili da mettere nel modello finale, scelte tra tutte le 
#variabili di tutti i possibili modelli 








