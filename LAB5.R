#LAB 5



#ESEMPIO 1: MEMORIA
rm(list = ls())

#Degli psicologi decidono di effettuare un esperimento per verificare se con 
#l’avanzare dell’eta' diventa piu' difficile memorizzare. A tal fine 
#selezionano casualmente 10 adulti e 10 giovani e a ciascun individuo mostrano 
#una lista di 27 parole invitandolo a memorizzarla. Dopo aver fatto scorrere 
#la lista al soggetto tre volte, gli viene chiesto di scrivere le parole che 
#ricorda; viene poi registrato il numero di parole ricordate correttamente.
#E' di interesse stabilire se i risultati sperimentali siano compatibili con 
#l’ipotesi per cui con l’eta' la capacita' di memorizzazione viene meno.

mem = read.table(file.choose) #memoria1.txt
mem
attach(mem)

#indizi grafici per stabilire se c'e' differenza tra le medie delle due 
#popolazioni
boxplot(mem)


###verifica degli assunti (dal punto di vista del test t student a due campioni)


## - normalita'
#qq plot di younger (per la normalita')
qqnorm(Younger)
qqline(Younger)

#qq plot di older (per la normalita')
qqnorm(Older)
qqline(Older)

#tutti i dati devono venire dalla stessa distribuzione, con il boxlplot si nota 
#che le distribuzioni di younger e older non sono uguali, quindi faccio due test
#diversi
shapiro.test(Younger)
shapiro.test(Older)
#accetto la normalità (H_0) in entrambi i casi (alphaoss grandi)


## - omoschedasticita' 
#test F
var.test(Younger, Older)

var(Younger)/var(Older)





#procediamo, dopo aver verificato le ipotesi, al test t di student a due campioni
ttest = t.test(Younger, Older, var.equal = T) 
ttest
#var.equal è T in modo tale da assumere omoschedasticità, altrimenti sarebbe un 
#altro test, ossia il test di Welch

# - t = 5.0229 e' toss
# - df = 18 sono i gradi di liberta' (nA+nB-2 = 10+10-2 = 18)
# - p-value = 8.836e-05 e' alphaoss. H_0:  muA = muB viene rifiutata poiche' 
#                       alphaoss e' molto piccolo


#se non si potesse assumere omoschedasticita' si farebbe il test di Welch:
ttest.eter = t.test(Younger, Older) 
ttest.eter



##### DAL PUNTO DI VISTA DELLA REGRESSIONE #####

#modello i dati in modo tale da poterli analizzare dal punto di vista della
#regressione
mem.df = data.frame(parole=c(mem$Younger,mem$Older), 
                    eta=c(rep('Younger',10), rep('Older',10)))
mem.df
attach(mem.df)
parole 
eta

fit = lm(parole ~ eta) #nel modello inserisco la v. exp qualitativa eta'
summary(fit)
#notiamo che la significativita' della nullita' di b2 e' alta, quindi non posso
#fare una buona stima del modellos senza tener conto di b2. 
#questo risultato equivale al risultato precedente ottenuto tramite il test t 
#student a due campioni

#per vedere la matrice delle variabili esplicative: 
model.matrix(fit) #matrice X (PUÒ CHIEDERE ALL'ESAME)

#una possibile riparametrizzazione puo' essere 
fit2 = lm(parole ~ -1+eta)
summary(fit2)

fit3 = lm(parole ~ 0+eta) #equivalente a riga 89 (trasformazione identica)


#analisi residui per le assunzioni di normalita' e omoschedasticita' per il 
#test sulla verifica dell'uguaglianza di due medie dal punto di vista della 
#regressione
res = rstandard(fit)

# - normalita'
qqnorm(res)
qqline(res) #sembra esserci normalita'. Verifico con shapiro test per sicurezza
shapiro.test(res) #accetto normalita'

# - omoschedasticita'
var.test(res[eta=='Younger'], res[eta== 'Older']) #test F sui residui
#pvalue alto: accetto H_0 che ipotizza omoscehdasticita' tra due campioni
#posso usare sia res studentizzati che standardizzati






#ESEMPIO 2: CIPOLLE 

rm(list=ls())
cipolle = read.table(file.choose(), header = T)
cipolle
head(cipolle)
attach(cipolle)
densPL = dens[loc == 'PL']
densV = dens[loc == 'V']
boxplot(densPL, densV, names = c('PL', 'V'))
#guardando il boxplot probabilemnte accetteremo H_0


#verifica della assunzioni

# - normalita'
qqnorm(densPL)  #andamento parabolico
qqline(densPL) 
qqnorm(densV)   #andamento parabolico
qqline(densV)
shapiro.test(densV)
#non posso assumere normalità, non servirebbe neanche controllare su PL

#provo a ovviare il problema della non-normalita' con una trasformata logaritica 
#e' conveniente usare la trasformata logaritmica dato che dal qqplot si 
#nota che i dati seguono piu' o meno un andamento parabolico
ldensPL = log(densPL)
ldensV = log(densV)
#bsiogna anche cambiare il sistema d'ipotesi: E(log(densityPL)) = E(log(densityV))


##verifica assunzioni 

# - normalita' 
shapiro.test(ldensPL)
#ora con la trasformata logaritmica accetto la normalità
shapiro.test(ldensV)
#accetto anche qua la normalità

# - omoschedasticita'
var.test(ldensPL,ldensV)
#accetto omoschedasticita'

t.test(ldensPL,ldensV, var.equal = T) #var.equal = T perche assumo omosch.
#l'intervallo di confidenza contiene lo zero e il pvalue è alto quindi 
#non c'e' una differenza significativa tra le medie delle due distribuzioni 
#riparametrizzate logaritmicamente



ldens= log(dens)    #densita' logaritmica di tutto il campione
fit = lm(ldens~loc) 
summary(fit)
#p-value = 0.1446 (test t a di)
# 0.145 : accetto H_0


#analogo all'analisi fatta nell'esempio del file memoria (ESEMPIO 1)
















