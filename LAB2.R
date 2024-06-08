#LAB 2 MODELLI

rm(list = ls())
dati = read.table(file.choose(), sep = ';', header = T) #per scegliere i dati 
#dall'interfaccia grafica, oppure 
dati = read.table('directory',sep=";",header=TRUE)

head(dati) #per visualizzare la prima parte del data frame

attach(dati) #per ricordare a R che i dati che verranno usati sono quelli tra 
#parentesi, è come 'fissarli'

x = Anno #oppure dati$Anno
y = Risultato #oppure dati$Risultato
x
y
n = length(x)
n
plot(x,y) 
#assumiamo che le y_1, ..., y_29 sono realizzazioni del modello lineare semplice 

#sono SMV ottenute con il metodo dei minimi quadrati 
beta2.stima = (sum(x*y)/n-mean(x)*mean(y))/(mean(x^2)-mean(x)^2) #oppure
beta2.stima = cov(x,y)/var(x)
beta2.stima
## [1] -0.01288517
#prevedo (perchè parlo della stima, altrimenti dovrei parlare del comportamento 
#atteso se parlo del vero b2) prevedo che il record miglori di 1.3 centesimo 
#ogni anno che passa

beta1.stima = mean(y)-beta2.stima*mean(x)
beta1.stima
## [1] 35.55483

#sovrapponiamo la retta stimata al grafico delle occorrenze per confrontarle
abline(beta1.stima,beta2.stima)

#valori previsti
y.previsti = beta1.stima + beta2.stima * x
#points sovrappone dei punti a un grafico già esistente. Sovrapponiamo i valori
#stimati 
points(x, y.previsti, pch=4) 

#residui 
residui = y - y.previsti
residui

sum(residui) #dovrebbe essere (circa) 0


#verifica della devianza totale
devtot = sum((y-mean(y))^2) #devianza totale
devtot
#[1] 8.089655

devsp = sum((y.previsti-mean(y))^2) #devianza spiegata
devsp
#[1] 6.669132

devrs = sum(residui^2) #devianza residua
devrs
#[1] 1.420524

#noto che 
devtot == devsp + devrs

#con queste quantità si ottiene il coefficiente di correlazione R2
R2 = 1-devrs/devtot
R2
#[1] 0.8244025
#L’interpretazione è che l’82.4% della variabilità della risposta y è
#spiegata dalla retta di regressione.

#SMV di sigma quadro
sigma2.stima = sum(residui^2)/n
sigma2.stima
#[1] 0.04898357

#la varianza campionaria corretta è
s2 = sigma2.stima*n/(n-2)
s2
## [1] 0.05261198


#grazie a s2 si possono calcolare delle stime non distorte delle varianze di
#beta1.stima e beta2.stima:

#beta2.stima
var.beta2.stima = s2/sum((x-mean(x))^2)
var.beta2.stima

#beta1.stima
var.beta1.stima = s2*(1/n+mean(x)^2/sum((x-mean(x))^2))
var.beta1.stima


#I.C. per beta2 a livello 1-alpha=0.95
alpha = 0.05
beta2.stima + c(-1,1) * qt(1-alpha/2, n-2)*sqrt(var.beta2.stima)
#[1] -0.01523339 -0.01053694 non contiene zero, è molto probabile rifiutare
#il comando qt(p,df) restituisce il quantile di livello p della distribuzione t 
#di Student con df gradi di libertà.



#ora si vuole verificare il sistema d'ipotesi con H_0 l'assenza di variabili 
#esplicative. (beta2 = 0)

t2 = beta2.stima/sqrt(var.beta2.stima)
#la statistica test vale
t2


qt(1-alpha/2,n-2)
#[1] 2.051831
#non posso togleire x dal modello (rifiuto se (t^oss)^2 > t_n-2,1-alpha/2)
abs(t2) > qt(1-alpha/2, n-2)
#dato che l'esito è T, rifiuto H_0

alfaoss = 2*min(pt(t2,n-2),1-pt(t2,n-2))
alfaoss
#[1] 1.052034e-11 
#è un valore piccolissimo, quindi rifiuto H_0. o confronto di alpha dichiarato
#(0.05), quindi rifiuto in quanto alphaoss è minore di alpha dichairato.
#pt(q,df) che calcola la funzione di ripartizione della distribuzione t di 
#Student con df gradi di libertà nel punto q.

#previsione putnuale:
y

#-------------------------------------------------------------------------------
#esercizio: Si verifichi l’ipotesi di nullita' di beta1. Si verifichi inoltre 
#l’ipotesi che beta2 sia uguale a -0.01 contro l’ipotesi alternativa beta2<−0.01

t2beta1 <- beta1.stima/sqrt(var.beta1.stima)

t2oss <- qt(1-alpha/2, n-2)

abs(t2beta1) > 
  t2oss
##[1] TRUE
#quindi rifiuto l'ipotesi nulla di togliere beta1 dai parametri
#-------------------------------------------------------------------------------

#previsione per la prossima olimpiade:
t.2020 = beta1.stima+beta2.stima*2020
t.2020
#[1] 9.526795

#intevallo di confidenza
t.2020.ic = t.2020+c(-1,1)*qt(0.975,n-2)*sqrt(s2*(1/n+(2020-mean(x))^2
                                                  /sum((x-mean(x))^2)))
t.2020.ic
#[1] 9.354204 9.699387

#intervallo di previsione
t.2020.iprev = t.2020 + c(-1,1)*qt(0.975,n-2)*
  sqrt(s2*(1+1/n+(2020-mean(x))^2/sum((x-mean(x))^2)))
t.2020.iprev
#[1]  9.025512 10.028078




#funzione modello lineare che fa i conti da sola:
fit = lm(y~x) #risposta~esplicativa 

fit$coef
#a named vector of coefficients

fit$fit
#the fitted mean values.

#gli argomenti elementari coef, fitted e resid estraggono, rispettivamente, 
#le stime dei coefficienti del modello, i valori previsti e i residui.
coef(fit)
resid(fit)
fitted(fit)

#per avere una visione d'insieme.
summary(fit)




