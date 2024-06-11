#LABORATORIO 9

#Il file stress.dat contiene misurazioni su soggetti che affermano di aver 
#affrontato, nell’arco degli ultimi 18 mesi, un evento che ha loro provocato 
#una tensione nervosa eccezionale. Le risposte sono classificate in base a 
#quanti mesi addietro l’evento si e' verificato.

#Siano y_i i numeri di risposte (risposte) per ciascun mese e si consideri come 
#variabile esplicativa x_i = i (mese), i = 1,...,18. 
#Obiettivo e' studiare la dipendenza del numero di risposte rispetto ai mesi.

rm(list = ls())
stress <- read.table(file.choose(), header =T) #stress.dat
head(stress)
attach(stress)
plot(mese, risposte)
#non sembra proprio esserci una vera e propria forma funzionale tra x e y

#data la natura della variabile y sembra opportuno definire il modello come 
#un modello di regressione Poisson


#@@@@ATTENZIONE@@@@
#NON lm, BENSI' glm perche è appunto un glm (generalized linear model)
glm(risposte~mese, family = poisson(link = log)) 
#link = log e' come non scrivere niente, log e' un legame di default 
#(funzione di legame)

#L'argomento family precisa la distribuzione della variabile risposta 

#L'argomento family precisa la distri- buzione della variabile risposta 
#all'interno di una classe di distribuzioni utilizzabili nel comando

#altre funzioni di legame:
glm(risposte~mese, poisson(link = sqrt))
glm(risposte~mese, poisson(link = identity))


modello = glm(risposte~mese, poisson)
summary(modello)

#Il ricorso all’algoritmo iterativo di stima (Fisher Scoring) fornisce 
#(Estimate) le stime βˆ1 = 2.80 e βˆ2 = −0.084 per i parametri β1 e β2,
#rispettivamente. Il modello stimato e':
#                         log (mu_i) = 2.80 − 0.084 * x_i

1 - pchisq(50.843-24.57,1) # 1 = p-p0
#alpha oss piccolo: rifiuto H_0: 

#modello stimato sulla scala delle y
curve(exp(2.8-0.084*x), col = 'red', lwd = 3, add = T)

var.test(mese, risposte)
#eteroschedasticita'


#Per la verifica della bonta' di adattamento del modello si puo' anche 
#considerare un'analisi dei residui:

res = residuals(modello, type = 'deviance') #default
res = residuals(modello, type = 'pearson') #residui di pearson

#cosa sono i residuals? ci ricordiamo che i residui non hanno una definizione 
#ovvia, perche' non ci sono gli errori, quindi non ho i rappresentanti 
#campionari degli errori, sono un loro corrispondente 

shapiro.test(res)
#accetto normalita' (pvalue alto), accetto normalita'

plot(fitted(modello), res)
#come sperato non si notano andamenti sistematici.




#il risultato della funzione glm puo' essere dato come argomento alla funzione 
#anova per un'analisi della devianza sulle variabili esplicative del modello.

#nuovo modello (modello quadratico)
modello2 = glm(risposte ~ mese + I(mese^2), poisson)

anova(modello, modello2, test = 'Chisq')

#La differenza tra le due devianze `e di 1.05 che `e anche il valore della 
#statistica test del log- rapporto di verosimiglianza per verificare 
#l’adattamento del modello log μi = β1 + β2 xi contro log
#μi = β1 + β2 xi + β3 x2i . Dato che il numero di parametri del modello corrente
#e ridotto sono, rispettivamente, p = 2 e p0 = 1, confrontando il valore 1.05 
#con la distribuzione χ2p−p0 , ossia χ21, si ottiene un livello di 
#significativita osservato pari a 0.305 (P(>|Chi|)), e pertanto viene scelto il 
#modello piu` semplice, ovvero modello.


#altro modello, ottenuto cambiando la funzione di legame (usiamo sqrt)
modello3 = glm(risposte~mese, poisson(link=sqrt))
summary(modello3)
#I modelli modello e modello3 non possono essere confrontati con la funzione 
#anova, in quanto non sono modelli annidati. Un possibile criterio per 
#confrontare i due modelli prevede di utilizzare l’indice AIC, che ci fa 
#preferire il modello con indice AIC piu' basso
modello3$aic
modello$aic
#quindi preferiamo modello3 a modello1

curve((3.94-0.122*x)^2, col = 'blue', lwd = 3, add = T)


#ulteriore modello, con funzione di legame la funzione identita'
summary(glm(risposte~mese, poisson(link = identity)))
curve(14.53 - 0.6693 * x, col = 'green', lwd = 3, add = T)
#linea retta, legame identico
#confrontanto ancora i modelli in base all'indice AIC, sarebbe prefereibile 
#quest'ultimo modello

