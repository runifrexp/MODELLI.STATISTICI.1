#LABORATORIO 10


rm(list = ls())
chl <-read.table(file.choose(), header = T)
head(chl)
attach(chl)
plot(temp, failures)
plot(factor(failures), temp)


chl.fit = glm(failures ~ temp, binomial)
summary(chl.fit)





# - Pr(>|z|): prima cosa che guardo
#temp  = 0.0320 *         e' debolmente significativa




#Coefficients:
# - Estimate 
#(Intercept)  15.0429      
#  temp         -0.2322     

# - Std. Error 
#(Intercept)    7.3786    
#  temp         0.1082   



# - z value 
#(Intercept)        2.039   
#  temp            -2.145    




# - Pr(>|z|)  
#(Intercept)      0.0415 *
#  temp           0.0320 *





chl.fit2 <- glm(failures~temp, binomial(link=probit))
summary(chl.fit2)

b <- coef(chl.fit) #coefficienti b1, b2
b

b2.se <- summary(chl.fit)$coef[2,2] #std. error di b2

z2 <- b[2]/b2.se #zoss
z2

2* pnorm(-abs(z2))







####ALLA WILKS
#confronto M0 con M1, quindi valuto l'importanza dell'unica v. exp. (temp)


1 - pchisq(28.267 - 20.315, 1) # null deviance / residual deviance



#####


#Nel giorno del lancio temp = 36 F (circa 2° C)


range(temp)
#ci si accorge che il giorno del lancio la temperatura era molto inferiore a
#quella che c'era nei test. in piu' si era consapevoli del legame tra 
#temperatura e failures




pl.f <- b[1] + b[2] *36
mu.f <-  exp(pl.f)/(1+exp(pl.f))
mu.f
## 0.9987521: lancio altamente sconsigliato. (il valore e' in [0,1])



plot(temp, failures, xlim = c(35,82))

#sovrappongo il modello stimato
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), col = 'blue', lwd = 3, add = T)


b = coef(chl.fit2)
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), col = 'red', lwd = 3, add = T)
#NO, NON E' QUESTA LA FUNZIONE DI LEGAME. DOBBIAMO MODIFICARLA

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



