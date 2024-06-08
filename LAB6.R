#LABORATORIO 6


rm(list=ls())
m = read.table(file.choose(),sep=";",header=TRUE,stringsAsFactor=T) #memoria.csv
head(m) 
#age: qual. SI/NO K = 2
#process: qual. 5 modalita': J = 5
#words: quant. 

#misurare il diverso livello di memorizzazione a seconda del tipo di 
#elaborazione mentale richiesta sulle parole in questione
#27 parole totali: words elenca il numero di parole che sono state 'completate'
#sotto diversi punti di vista: Process

plot(m$Words~m$Age) #grafico delle parole ricordate in base all'eta' : --Age-- 
plot(m$Words~m$Process) #grafico delle parole ricordate in base al tipo di 
     #compito: --Process--
#counting e rhyming non sono molto efficaci come metodi di memorizzazione, 
#al conrario di intentional ad esempio



#marginalizziamo rispetto a giovani (K=1) per valutare se tra i giovani la 
#memorizzazione e' diversa a seconda del processo a tale domanda corrisponde una
#ANOVA a un fattore, *sui soli dati relativi ai giovani*

#si potrebbe anche marginalizzare rispetto al processo

m.g <- m[m$Age == 'Younger',] #creo una matrie composta solo dai giovani 
m.g
dim(m.g)
attach(m.g) #rendo disponibili Words e Process solo per i giovani


####STUDIAMO IL PROBLEMA DAL PUNTO DI VISTA DELLA REGRESSIONE#### (non 
#scomposizione della devianza)
Words #mostra le parole portate a compimento per i vari compiti da ogni giovane
Process #mostra il processo da eseguire per ogni giovane
plot(Words~Process) #studio se il numero di parole differisce in media a seconda 
#del compito da svolgere

#ora l'ordine delle modalita' (livelli) e' alfabetico, se cambiassi l'ordine
#avrei una regressione diversa, in quanto l'intercetta (dummy non rappresentata)
#risulterebbe essere un'altra, ora la modalita' non rappresenta e' 'Adjective'
#in seguito verra' cambiaoto l'ordine delle esplicative
fit.g = lm(Words~Process) 
model.matrix(fit.g)
#la colonna di 1 e' l'intercetta, in seguito ci sono le dummy
#dalla 21 alla 30 capsico che la modalita' e quella lasciata fuori dal modello 
#(tutti 0 tranne la 1^ colonna)


summary(fit.g)

# - F-statistic: 53.06: Foss per test di ugualgianza tra le medie dei vari
#                       compiti
# - on 4 and 45 DF: J-1 = 5-1 = # modalita' (livelli) - 1 = 4
#                   n-J = 50-5 = num. campionaria - livelli = 45      
# - p-value: < 2.2e-16: alphaoss: dato che e' piccolo Rif. H_0: le medie non si 
#                       equivalgono

#media del numero di parole ricordate dagli individui che sonosono sottoposti a 
#Adjective
mean(Words[Process == 'Adjective'])
# = 14.8, che e' esattamente la stima dell'intercetta trovata com summary(fit.g)
#oppure per Counting...
mean(Words[Process == 'Counting'])
# = 6.5

#nel modello con gruppo di riferimenteo Adjective (quello appena stimato)
#(dove la dummy esclusa e' x_Adj), si ha b1.cap = ybar_Adj

# - Estimate (Counting) = -8.3000, b2: differenza tra la variazione attesa nel 
#                         passare dal gruppo di riferimento, ossia Adjective,
#                         al gruppo Counting (6.5-14.8 = -8.3000), 
#                         @@@@@@@(ybar.Count - ybar.Adj = b2)@@@@@@@@
mean(Words[Process == 'Counting']) - mean(Words[Process == 'Adjective'])

#e cosi' via...
#per Imagery: 2.8000 = ybar.Adj - ybar.Imag = 17.6 - 14.8 = b3
mean(Words[Process == 'Imagery']) - mean(Words[Process == 'Adjective'])  
 


# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

#ecco il citato in precedenza cambio dell'ordine delle modalita' inserite nel 
#modello

#Ai fini delle analisi, conviene trasformare le variabili Age e Process in 
#fattori e ordinare il secondo in base alla complessita' dell’elaborazione 
#richiesta (stesso ordine del libro)
Process = factor(Process, levels = c('Counting', 'Rhyming', 'Adjective', 
                                     'Imagery', 'Intentional'))
Process #si nota che sono ordianti in ordine diverso
fit.g2 <- lm(Words~Process) 
plot(Words~Process) #ora sono ordianti per media di parole per ogni livello
model.matrix(fit.g2) 


summary(fit.g2)

#(Intercept)          6.5000     0.7986   8.139 2.16e-10 ***
#ProcessRhyming       1.1000     1.1294   0.974    0.335    
#ProcessAdjective     8.3000     1.1294   7.349 3.10e-09 ***
#ProcessImagery      11.1000     1.1294   9.828 8.92e-13 ***
#ProcessIntentional  12.8000     1.1294  11.333 8.93e-15 ***

#In base alle stime dei coefficienti, si trova che: un individuo che conta 
#(Intercept) le lettere ricorda in media 6.5 parole; un individuo che cerca 
#le rime non ricorda una media significativamente piu' alta; un individuo 
#che effettua elaborazioni piu' complesse:Imag o Int,ricorda una media di parole 
#significativamente maggiore

#per trovare b1
b1.g2 <- mean(Words[Process == 'Counting']) 
#per stimare b3 nel modello g2:
b3.g2 <- mean(Words[Process == 'Adjective']) - mean(Words[Process == 'Counting'])
b3.g2
#se volessi trovare la media di Adj: 
b1.g2 + b3.g2
#si evince che le medie delle modalita' non cambiano anche se nel modello viene
#modificato l'ordine 

#il valore previsto non cambia a seconda del gruppo di riferimento
#dato che non cambia il valore previsto, non cambiano nemmeno i residui
#y.oss - y.prev
#percio' non cambiano nemmeno SQres e nemmeno il test F per la signifcativita'
#dell'intero modello



################   TUTTO QUESTO PER DIRE CHE LA SCELTA DEL GRUPPO DI ### 
################   RIFERIMENTO NON FA VARIARE I RISULTATI DEL TEST F ###


#DAL PUNTO DI VISTA DELL TEST F (PRIMA ERA DAL PTO DI VISTA DELLA REGRESSIONE)

#L’analisi della varianza si ottiene anche con il comando:
fit.aov <- aov(Words~Process)
summary(fit.aov)
#tabella di scomposizione della varianza (analisi varianza ANOVA)

#PROCESS: devianza tra i gruppi definiti dal fattore Process: 
#equivalente a devianza spiegata

#RESIDUALS: devianza entro i gruppi:
#equivalente a devianza residua

# - Df 
#Process      4       J-1
#Residuals   45       n-J

# - Sum SQ
#Process     1354     devianza spiegata: SQreg    (per regressione)  
#                     dev tra i gruppi            (per ANOVA)
#Residuals   287      devianza residua: SQres     (per regressione) 
#                     dev entro i gruppi          (per ANOVA)

# - Mean SQ
#Process     338.4     = Sum Sq / Df = 1354/4
#Residuals   6.4       = Sum Sq / Df = 287/45

# - F value = 53.06:  Foss = MeanSq(Process)/MeanSq(Process)

# - Pr(>F) <2e-16 ***     : alphaoss = P(F_{J-1,n-J} > Foss): 
#rifiuto con forza l'ip che non ci sia variazione attesa nel numero di parole 
#ricordate a seconda del processo mentale (Rif H_0)



#test equivalente dal punto di vista del confronto tra modelli annidati 


#modello nullo (solo intercetta)
fit.g.0 <- lm(Words~1) 
anova(fit.g.0, fit.g) 

#ANOVA 'CLASSICO'
#prima valutavo quanto grande e' la @devianza spiegata@ da Process per spiegare
#il comportamento di Words

#REGRESSIONE
#valuto di quanto incrementa la devianza residua se dal modello che include 
#process (M1), tolgo process (M0), e lascio solo l'intercetta


#Model 1: Words ~ 1
#Model 2: Words ~ Process

# - Res.Df
#1 49 (n-p0)
#2 45 (n-p)

# - RSS : Residuals Sum Squares
#1 1640.7     dato che siamo nel modello ridotto SQres0 = SQtot
#2 287.0      SQres
#posso trovare SQreg come SQres0(=SQtot) - SQres = 1640.7 - 287.0 = 1353.7

# - Df : p-p0 = 5-1 = (n-p0) - (n-p) = (50-1) - (50-5) = 49 - 45 = 4

# - Sum of Sq = 1353.7          SQreg = SQtot - SQres 1640.7 - 287.0 = 1353.7

# - F = Foss (come nel punto di vista dell'ANOVA classico) = 53.064

# - Pr(>F) <2e-16 ***     : alphaoss = P(F_{J-1,n-J} > Foss)
#(anche questo come nel punto di vista dell'ANOVA classico)





#verifica di normalità, omoschedasticità, test di Bartlett, test Shapiro-Wilk
#sui residui standardizzati per esser sicuri che i test fatti possano 
#portare a conclusioni affidabili
#normalità
res = rstandard(fit.g)
bartlett.test(res~Process) #suddividi a seconda dei gruppi definiti da Process
#in modo tale da farlo su tutte le modalita'
#accetto omoschedasticita' H_0
shapiro.test(res)
#accetto normalita' H_0


#MALEDIZIONE DEI TEST MULTIPLI: ANALISI POST-HOC
#Mentre c'e' un solo modo in cui le medie dei gruppi possono essere uguali tra 
#loro (H0), ci sono molti modi in cui possono essere diverse (H1). 
#L'analisi che mira a individuare la particolare struttura nella popolazione 
#che ha provocato il rifiuto dell’ipotesi nulla viene chiamata analisi post-hoc,
#poiche' viene svolta dopo che l’esperimento principale si e' concluso. 
#Poiche' l'analisi post-hoc si compone usualmente di diversi test che mirano 
#a individuare le medie effettivamente diverse tra loro, particolare attenzione 
#deve essere posta nel fare in modo che la procedura di verifica di ipotesi 
#complessiva abbia il livello di significativita' alpha dichiarato. 
#Senza questa attenzione, si potrebbe essere accusati di stare “dragando” 
#i dati, ossia di stare continuando a fare test fino a che uno di essi rifiuta 
#H0, magari solo come effetto del caso (un test con alpha = 0.05 rifiuta H0 
#erroneamente una volta su venti). Un semplice accorgimento da usare quando 
#vengono fatti test multipli e' la correzione di Bonferroni. In R, la funzione 
#pairwise.t.test implementa diverse correzioni in presenza di test multipli. 
#I dettagli delle analisi post-hoc vanno oltre gli obiettivi del presente 
#volume


