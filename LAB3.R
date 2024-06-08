#LAB 3 MODELLI




#ESEMPIO DI MIGLIRAMENTO DEL MODELLO GRAZIE A UNA TRASFORMAZIONE
TVadv = read.table(file.choose(),header=TRUE)
attach(TVadv)
head(TVadv)

x = spend
y = milimp
fit = lm(x ~ y)
summary(fit)
plot(x,y)
abline(fit) #la retta stimata non passa per niente immezzo ai dati

#verifica grafica omoschedasticita'
plot(x, residuals(fit))
abline(h=0) #non ho oscillazioni regolari intorno allo zero
#si nota che i residui sono molto più densi tra -20 e 0, poi hanno un range più 
#grande. Ho evidenza di eteroschedasticità


#provo a trasformare logaritmicamente le variabili 
z = log(y) #log base 10
w = log(x)
fit2 = lm(z~w)
summary(fit2)
plot(w,z)
abline(fit2)#l'andamento non è propriamente lineare, ma posso facilmente 
#immaginare una retta immezzo ai dati

#analisi grafica omoschedasticita'
plot(w, residuals(fit2))
abline(h=0)
#non ho oscillazioni regolari
#basta una trasformazione per avere un modello migliore, 
#in questo secondo modello se c'è eteroschedasticità è molto bassa








#ORA SI INIZIA CON IL VERO E PROPRIO LABORATORIO2

rm(list = ls())
dati = read.table('/Users/lorenzokarolgobbo/Desktop/statistics/2_ANNO/MODELLI STATISTICI 1/Dati/olimpiadi100m.csv', sep = ';', header=TRUE)
dati = read.table(file.choose(), sep = ';', header = T)
attach(dati)
head(dati)
x = Anno
y = Risultato
fit = lm(y~x) #risposta~esplicativa
residui = residuals(fit)
sum(residui)
y.previsti = fitted(fit)
n = length(y)
n
h = 1/n + (x-mean(x))^2/sum((x-mean(x))^2)


#residui standardizzati
res.std = residui/sqrt((1-1/n-(x-mean(x))^2/sum((x-mean(x))^2)))
res.std
sum(res.std)
#molto piu' veloce: 
res.std <- residui/sqrt(1-h)
res.std
sum(res.std)


#residui studentizzati
s2 = sum(residui^2)/(n-2)
res.st = res.std /sqrt(s2)
res.st = rstandard(fit)
res.st


#preparo una finestra grafica per confrontare i tre tipi di residui
par(mfrow = c(1,3)) #prepara finestra grafica con 1 riga e 3 colonne
plot(x, residui)
abline(h = 0, lty = 2)
plot(x, res.std)
abline(h = 0, lty = 2)
plot(x, res.st)
abline(h = 0, lty = 2)


par(mfrow = c(1,1))
plot(y.previsti, res.std)



#VERIFICA OMOSCHEDASTICITA' 
#test di Bartlett
groups = c(rep(1,12), rep(2,6), rep(3,11))
groups
res.std.sort = order(res.std)
bartlett.test(res.std.sort, groups)

#rifaccio il test togliendo il valore anomalo
bartlett.test(res.std.sort[-n], groups[-n]) #[-n] serve per togliere l'ultimo valore
#pvalue piu' alto, posso accettare




#VERIFICA NORMALITA'

#densità
hist(res.st, prob = T, nclass = 10) #con prob = T le probabilità si sommano a 1
curve(dnorm, add = T, lwd = 3, col = 2)
range(res.st)

#test chi quadro
breaks = c(-2,-1,0,1,5)
freq = hist(res.st, breaks = breaks)$counts
breaks[1] = -Inf
breaks[5] = +Inf
breaks
prob = pnorm(breaks[2:5]) - pnorm(breaks[1:4])
n*prob
chisq.test(freq, p = prob)


#test Shapiro-Wilk
shapiro.test(res.st)
#noto che pvalue mi fa rifiutare senza ombra di dubbio
shapiro.test(res.st[-1])
#non rifiuto se tolgo il valore anomalo








