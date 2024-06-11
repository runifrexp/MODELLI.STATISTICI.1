#LABORATORIO 10

#nei dati si hanno le registrazioni dei guasti occorsi alle guarnizioni ad 
#anelli in 23 lanci dello Space Shuttle e le temperature atmosferiche al 
#momento del lancio 

#In particolare, la variabile failures vale 0 se non si e' verificato alcun 
#guasto e vale 1 se si e' verificato almeno un guasto; temp e' la temperatura.
#L'obiettivo dell'analisi e' stabilire se la probabilita' di un guasto dipenda 
#dalla temperatura.

#in sostanza provo a prevedere il comportamento atteso della risposta, ossia 
#un numero in [0,1]

rm(list = ls())
chl <-read.table(file.choose(), header = T) #challenger.dat
head(chl)
attach(chl)
plot(temp, failures)
#si nota che al crescere della temperatura le guarnizioni si rompono di meno
plot(factor(failures), temp)
#lo si nota anche dal boxplot

#questa iniziale analisi grafica ci fa pensare che le temperature fredde
#siano legate a una probabilita' piu' alta di rottura delle guarnizioni

#modello logit 
chl.fit = glm(failures ~ temp, binomial) #Il legame logit e' quello predefinito
summary(chl.fit)

#Coefficients:

# - Estimate 
#(Intercept)  15.0429      = b1.cap    
#  temp         -0.2322    = b2.cap  

# - Std. Error 
#(Intercept)    7.3786    = sqrt[(X^T Vbr.cap X)^-1_rr]
#  temp         0.1082    = sqrt(stima.var.br.cap)

# - z value 
#(Intercept)        2.039   
#  temp            -2.145   = br.cap / Std. Error 

# - Pr(>|z|): sara' la prima cosa che guardo
#(Intercept) = 0.0415 *
#temp  = 0.0320 *         sono entrambe debolmente significative


#Null deviance: W_{O|S} = 28.267  on 22 = n-1  degrees of freedom (nullo)
#Residual deviance: W_{C|S} = 20.315  on 21 = n-p = 23 - 2 degrees of freedom
#(corrente)

#AIC: 24.315


#PROVIAMO A STIMARE IL MODELLO CON UAN DIVERSA FUNZIONE DI LEGAME: LA PROBIT 
#ossia la funzione quantile, ossia la funzione inversa della FdR di una N(0,1)
#non centra la normalita' come assunzione distributiva

chl.fit2 <- glm(failures~temp, binomial(link=probit))
summary(chl.fit2)


#estrazione e calcolo di certi valori del modello iniziale chl.fit (f. legame
#logit, 1° modello stimato)

#betar.cap
b <- coef(chl.fit) #beta.cap:  b1, b2
b

#standard.error
b2.se <- summary(chl.fit)$coef[2,2] #std. error di b2
b2.se

#z value
z2 <- b[2]/b2.se #zoss
z2

#Pr(>|z|) (per b2)
2* pnorm(-abs(z2)) #alphaoss




#### RAGIOANMENTO ALLA WILKS
#confronto M0 con M1, quindi valuto l'importanza dell'unica v. exp. (temp)
#confronto tra modello nullo e modello correntes

1 - pchisq(28.267 - 20.315, 1) # null deviance / residual deviance
#28.267 - 20.315 = stat.test W(O|C) = W(O|S) - W(C|S)
#dist. asintotica chi quadrato con p-p0 Df (= p-1 = 2 - 1 = 1)

#dato che il valore trovato e' basso, rifiuto H_0, ossia il parametro temp
#e' significativo, non puo' essere tolto per fare una buona stima del modello

#l'approccio alla WILKS e' piu' affidabile (non si entra nei dettagli)


#probabilita' mu.cap prevista in riferimento al giorno in cui e' stato 
#fatto il lancio, in cui temp = 36 F (circa 2° C)

range(temp)
#ci si accorge che il giorno del lancio la temperatura era molto inferiore a
#quella che c'era nei test. in piu' si era consapevoli del legame tra 
#temperatura e failures. la pressione mediatica e altri fattori hanno fatto 
#dare il via libera per il lancio nonostante si fosse consapevoli che sarebbe 
#stato un suicidio per gli astronauti 

pl.f <- b[1] + b[2] *36
mu.f <-  exp(pl.f)/(1+exp(pl.f))
mu.f
## 0.9987521: lancio altamente sconsigliato: la probabilita' di rottura delle 
#guarnizioni e' quasi certa (ricorda che il valore e' in [0,1])



#plot del mu.cap per ogni temperatura:
#plot dei dati
plot(temp, failures, xlim = c(35,82))

#MODELL0 1 CON F.D.L. LOGIT
#sovrappongo il modello stimato (con f. legame logit, 1° modello stimato)
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), col = 'blue', lwd = 3, add = T)

#MODELLO 2 CON F.D.L. PROBIT
#modello con f. di legame probit (2° modello stimato)
b = coef(chl.fit2) 
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), col = 'red', lwd = 3, add = T)
#NO, NON E' QUESTA LA FUNZIONE DI LEGAME GIUSTA. DOBBIAMO CAMBIARLA
#ricordiamo che la funzione di legame e' probit, ossia la funzione quantile
#di una N(0,1). percio' mu.cap = phi(b1.cap + b2.cap * x)
curve(pnorm(b[1]+b[2]*x), col = 'green', lwd = 3, add = T)




###################FINITE LE ISTRUZIONI PER IL COMPITO##########################


#analisi dati ICUdata.dat pg. 188
rm(list = ls())
ICU <- read.table(file.choose(), header = T)
head(ICU)
attach(ICU)

#trasformo le variabili quantitative causa e coscienza in qualitative
causa = factor(causa)
levels(causa) = c('P', 'E')

coscienza = factor(coscienza)
levels(coscienza) = c('SI', 'NO')


#stima del modello
ICU.fit = glm(stato~eta+causa+ coscienza, binomial)
summary(ICU.fit)

#valutare sigificativita' intero modello: ANOVA

ICU.fit0 <- glm(stato~1, binomial)

anova(ICU.fit0, ICU.fit, test = 'Chisq')
#Pr(>Chi) = 7.39e-12 *** : rifiuto H_0 di (rifiuto M0)

b = coef(ICU.fit)
pl1 = b[1] + b[2] * 50 + b[3] #paziente di 50 anni cosciente in emergenza
pl2 = b[1] + b[2] * 50 #paziente di 50 anni cosciente, NON in emergenza

mu1 <- exp(pl1)/(1+exp(pl1))
mu2 <- exp(pl2)/(1+exp(pl2))
mu1- mu2

#aumenta del 0.1325686 la probabilita' di decesso se il paziente e' in emergenza












#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ MODELLI BERNOULLIANI@@@@@@@@@


mu.hat = predict(ICU.fit, type = 'response') #type = 'response' ottiene
#velocemente i valori attesi

as.double(mu.hat > 0.5) #oppure as.numeric
table(y.prev, stato)



