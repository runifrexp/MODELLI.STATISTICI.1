#LABORATORIO 7


rm(list=ls())
dati = read.table(file.choose(), header = T) #nails.dat


#sull’efficacia di uno smacchiatore per smalto per unghie. Si considerano 3 
#tipi di smalto (fattore I) e 2 tipi di solvente (fattore II).
#Ogni combinazione smalto-solvente viene sperimentata su 5 macchie. 
#Per ogni macchia, viene osservato il tempo di smacchiatura.

head(dati)
attach(dati)
smalto
solvente
tempo

#rendo le variabili esplicative (in quanto sono numeri, quantita') 
#(smalto, solvente) dei fattori (delle qualita'), in modo da poter usarle 
#nell'analisi ANOVA
solvente = as.factor(solvente)
smalto = as.factor(smalto)
smalto
solvente
#levels indica le modalita' (livelli) delle variabili (fattori)


#AOV:
#STUDIO BASATO SULLE DEVIANZE SPIEGATE DAI VARI COMPONENTI DEL MODELLO


#analisi grafica preliminare della distribuzione marginale rispetto prima a
#smalto (quindi solvente non viene considerato)

plot(tempo~smalto) #(analisi varianza a singolo fattore lab. 6)
#boxplot, vediamo se la media del tempo di resistenza cambia da tipo di smalto 
#a tipo di smalto
#si vede che la media dei tre tipi di smalto e' diversa.

plot(tempo~solvente)
#boxplot per vedere se la durata delle unghie dipende dal tipo di solvente usato
#sembra di si.


#analisi grafica dell'interazione (dom. 5)
interaction.plot(smalto, solvente, tempo)
#ascissa: 3 livelli di smalto
#distinguere i livelli dell'altro fattore: spezzate diverse
#ordinata: 6 medie del tempo delle combinazioni dei vari livelli dei fattori
#(condizionate in base a un certo solvente e a un certo smalto)


#analisi descrittiva (guardo solo medie campionarie)

# • LE SPEZZATE NON SONO @@@SOVRAPPOSTE@@@: (dom. 3)
#     quando mi condiziono a un livello di smalto (ascissa), c'e' un cambiamento
#     nella media campionaria nel passare da un solvente all'altro:
#
#c'e' un effetto condizionato rispetto allo smalto del solvente 


# • LE SPEZZATE NON SONO @@@ORIZZONTALI@@@: (dom. 4)
#     quando passo da uno smalto a un altro, ponendomi su una spezzata, cioe'
#     condizionatamente al solvente, cambia la media che osservo.
#
#c'e' un effetto condizionato rispetto al solvente dello smalto


# • LE SPEZZATE NON SONO @@@PARALLELE@@@: (dom. 5, interazione)
#     le spezzate sono sempre alla stessa distanza? non proprio
#     l'effetto passando da una spezzata a un altra (cioe' l'effetto del 
#     solvente) cambia a seconda dello smalto a cui mi condiziono (1,2,3)

#c'e' interazione: quando l'effetto cambia a seconda di cosa mi condiziono
#SIMMETRICAMENTE: l'effetto dello smalto cambia a seconda del sovente a cui 
#mi condiziono


#le conclusioni solo le stesse anche se cambio l'ascissa con le varie spezzate
interaction.plot(solvente, smalto, tempo)




#verifica l'ipotesi di interazione, visto che si dispone di piu' di 
#un’osservazione per ogni combinazione dei fattori

#L’ANOVA a due fattori con interazione puo' essere svolta con la funzione aov
dati.aov1 <-  aov(tempo ~ smalto + solvente + smalto:solvente)
summary(dati.aov1)

#smalto (1° fattore)
#solvente (2° fattore)
#smalto:solvente (interazione)
#Residuals (residui)

#analisi output di aov 

# - Df
#smalto = 2 = J-1 = 3-1
#solvente = 1 = K-1 = 2-1
#smalto:solvente = 2 = (J-1)*(K-1) = (3-1)*(2-1) #dummy introdotte nel modello
#Residuals = 24 = n-J*K = 30 - 3*2               #per permettere all'effetto
                                                 #condizionato di cambiare a 
                                                 #seconda di cosa mi condiziono

# - Sum Sq
#smalto = 18.64                     ##################
#solvente = 97.42                   ##vedi foglietto##
#smalto:solvente = 7.67             ##################
#Residuals = 209.10 


# - Mean Sq
#smalto = 9.32   
#solvente = 97.42                   = Sum Sq / Df
#smalto:solvente = 3.84
#Residuals = 8.71


# - F value
#smalto = 1.069                           SQ(fonte)/Df(fonte)
#solvente = 11.181               = Foss = -------------------
#smalto:solvente = 0.440                    SQ(res)/Df(res)


# - Pr(>F)
#smalto = 0.35899                           
#solvente = 0.00271 **        = alphaoss = P(F value > F_{Df(fonte), Df(res)})       
#smalto:solvente = 0.64893


#questa tabella NON mi dice se posso togliere conemporaneamente smalto e 
#interazione. la tabella fa dei test singoli, non in contemporanea
#da questa tabella posso togliere lo smalto e lasciare tutto il resto
#lo stesso con l'interazione, posso toglierla lasciando il resto



#devo quindi decidere se togliere 
# • l'interazione o 
# • lo smalto

#ha senso solamente togliere l'interazione, in quanto se togliessimo lo smalto
#sarebbe come dire che lo smalto non ha effetto, pero' c'e' interazione, quindi 
#l'effetto dello smalto cmabia a seconda del solvente a cui mi condiziono

#NON HA SENSO TOGLIERE UN FATTORE DAL MODELLO E LASCIARE L'INTERAZIONE
#L'INTERAZIONE E' TRA DUE COSE, DOVE PUO' ESSERE SE CE N'E' SOLO UNA?

#oltretutto notiamo nela tabella che 
# - Pr(>F)
#samlto = 0.35899
#smalto:solvente = 0.64893
#percio' tolgo interazione in quanto ha un pvalue piu' alto, quindi posso 
#accettare con piu' forza H_0: assenza di interazione
#(avrei tolto l'interazione anche se il pvalue fosse stato piu' basso)

dati.aov2 <- aov(tempo~smalto*solvente) #è la stessa cosa di dati.aov1
summary(dati.aov2)



#ANOVA AL POSTO DI AOV: 
#ANALISI DAL PUNTO DI VISTA DELLA REGRESSIONE: CONFRONTO TRA MODELLI ANNIDATI:
#MODELLI RIDOTTI VS MODELLI COMPLETI
#se volessi verificare l'efftto dell'interazione:
#confronto tra modelli annidati: stimo M=, guardo al dev res per M0 (senza
#interazione), stimo M1 (con interazione).
#valuto l'aumento di devianza residua nel passare da M1 a M0.
#se l'incremento e' grande vuol dire che l'adattamento peggiora di molto
#quando viene tolta l'interazione
#se togliendo l'interazione cresce di povo dev res in M0 rispetto a M1, 
#posso accettare di usare M0


#stima di M0 (senza interazione)
dati.lm.ridotto <- lm(tempo~smalto+solvente)

#stima di M1 (con interazione)
dati.lm.completo <- lm(tempo~smalto * solvente) 


#visualizziamo la matrice X
model.matrix(dati.lm.ridotto)

model.matrix(dati.lm.completo)

#ANOVA per confrontare modelli annidati
anova(dati.lm.ridotto, dati.lm.completo)

#Model 1: tempo ~ smalto + solvente       M0
#Model 2: tempo ~ smalto * solvente       M1

# - Res.Df    
#1     26 = n-(J+K-1) = 30-(3+2-1)                     
#2     24 = n-(J*K) = 30-(3*2)

# - RSS 
#1     216.78 = SQres0 = SQtot = e0T*e0 - eT*e
#2     209.10 = SQres 

# Df 
#      2 = p-p0 = 26-24 = J*K - (J+K-1)

# - Sum of Sq    
#      7.6726 = RSS(1) (M0) - RSS(2) (M1) = 216.78 - 209.10       

# - F 
#      0.4403 = Foss


# - Pr(>F)
#      0.6489 = P(F{2,24}>Foss)
#dato l'alphaoss ha senso togliere l'interazione



summary(dati.aov2) # uguale a comando sotto
anova(dati.lm.completo) #contributo di tutti i termini al modello completo
#in questo modo sto vedendo le cose invece come devianze spiegate, come 
#incremento di devianze residue che ci sono togliendo i vari termini dal modello



#stima del modello senza interazione
dati.aov2 <- aov(tempo~smalto + solvente)
summary(dati.aov2)

#si nota che lo smalto, anche senza interazione, non e' significativo, quindi 
#nel modello finale tolgo anche smalto e lascio solo solvente

#nel modello senza interazione, SQres e' diversa dal modello con interazione. 
# 7.67264 + 209.10352 (SQsmalto:solvente + SQResiduals nel modello con 
#interazione) 


#stima del modello definitivo
dati.aov3 <- aov(tempo~solvente)
summary(dati.aov3)

anova(lm(tempo~solvente))

# o summary(aov)
#oppure anova(lm)




# ESEMPIO IN CUI NON CI PUO' ESSERE INTERAZIONE

rm(list = ls())
Pen = read.table(file.choose(), header = T) #penicillin.dat
attach(Pen)
head(Pen)

anova(lm(penicillina~miscela*modo))
#analisi varianza di un modello come se avesse interazione, in realta' non c'e
#il test da errori non disponibili in quanto c'e uno 0/0
#indeterminato il rapporto tra devianza residua e gradi di liberta' (n-J*K) = 0

#SE HO UNA SOLA OSSERVAZIONE PER GRUPPO NON POSSO STIMARE L'INTERAZIONE 
#NON HO ABBASTANZA INFORMAZIONE PER STIMARE LA DEVIANZA 
























