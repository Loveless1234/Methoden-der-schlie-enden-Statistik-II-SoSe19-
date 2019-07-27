#Ranjit sah,836261


#Hausaufgabe mss2

load(file.choose()) #Laden wir die neu  Datensätze 
View(Telecom)#view die datensätze
head(Telecom)#Gibt den ersten  Teil eines Vektors zur?ck
tail(Telecom)#Gibt den letzten Teil eines Vektors zur?ck
attach(Telecom) #Datenbank ist Suchpfad angehängt
names(Telecom) # alle variable im datensätze
summary(Telecom) #summary  für all variable
plot(Telecom)#Grafik f?r Dataset Telecom


# Logistics Regression Durch Aic

#Anfang mit Null Modell zu entwicklen
Nullmod <- glm(Churn ~ 1,data = Telecom, family=binomial)#Fange mit dem Null Modell als das aktuelle Modell an
summary(Nullmod)#summary of Nullmodell
#Berechnen Sie alle Einzelbegriffe im Argument scope, die hinzugefügt werden können
add1(Nullmod,scope=Churn~Gender+SeniorCitizen+Tenure+MultipleLines+InternetService+OnlineBackup+StreamingTV
                    +StreamingMovies+Contract+PaperlessBilling+PaymentMethod,test="Chisq")
AIC(Nullmod)#zu sehen aic wert von aktuell modell



#Füge eine Variable zu dem Modell hinzu und notiere den AIC-Wert
#Ein Modell mit contract im Modell ergibt den kleinsten AIC Wert

#erste und aktuell  modell
Aktuall_mod1<-glm(Churn~Contract,data=Telecom,family = binomial())
summary(Aktuall_mod1)
add1(Aktuall_mod1,scope=Churn~Gender+SeniorCitizen+Tenure+MultipleLines+InternetService+OnlineBackup+StreamingTV
     +StreamingMovies+Contract+PaperlessBilling+PaymentMethod,test="Chisq")
AIC(Aktuall_mod1)#zu sehen aic wert von aktuell modell


#zweite und aktuell  modell

#Ein Modell mit InternetService im Modell ergibt den kleinsten AIC Wert
Aktuall_mod2<-glm(Churn~Contract+InternetService,data=Telecom,family = binomial())
summary(Aktuall_mod2)
add1(Aktuall_mod2,scope=Churn~Gender+SeniorCitizen+Tenure+MultipleLines+InternetService+OnlineBackup+StreamingTV
     +StreamingMovies+Contract+PaperlessBilling+PaymentMethod,test="Chisq")
AIC(Aktuall_mod2)#zu sehen aic wert von aktuell modell


#dritte und aktuell  modell

#Ein Modell mit tenure im Modell ergibt den kleinsten AIC Wert
Aktuall_mod3<-glm(Churn~Contract+InternetService+Tenure,data=Telecom,family = binomial())
summary(Aktuall_mod3)
add1(Aktuall_mod3,scope=Churn~Gender+SeniorCitizen+Tenure+MultipleLines+InternetService+OnlineBackup+StreamingTV
     +StreamingMovies+Contract+PaperlessBilling+PaymentMethod,test="Chisq")
AIC(Aktuall_mod3)#zu sehen aic wert von aktuell modell

#vierte  und aktuell  modell
#Ein Modell mit  PaymentMethod im Modell ergibt den kleinsten AIC Wert
Aktuall_mod4<-glm(Churn~Contract+InternetService+Tenure+PaymentMethod,data=Telecom,family = binomial())
summary(Aktuall_mod4)
add1(Aktuall_mod4,scope=Churn~Gender+SeniorCitizen+Tenure+MultipleLines+InternetService+OnlineBackup+StreamingTV
     +StreamingMovies+Contract+PaperlessBilling+PaymentMethod,test="Chisq")
AIC(Aktuall_mod4)#zu sehen aic wert von aktuell modell

#funfte und aktuell  modell

#Ein Modell mit  Paperlessbilling im Modell ergibt den kleinsten AIC Wert
Aktuall_mod5<-glm(Churn~Contract+InternetService+Tenure+PaymentMethod+PaperlessBilling,data=Telecom,family = binomial())
summary(Aktuall_mod5)
add1(Aktuall_mod5,scope=Churn~Gender+SeniorCitizen+Tenure+MultipleLines+InternetService+OnlineBackup+StreamingTV
     +StreamingMovies+Contract+PaperlessBilling+PaymentMethod,test="Chisq")
AIC(Aktuall_mod5)#zu sehen aic wert von aktuell modell

# zum schluss wir bekommen Aktuall_mod5 als aktuell und End modell  weil  AIC-wert kleiner ist

#Grafik ?berblick Von endmodell
#bargraph

par(mfcol=c(2,3))
{
  barplot(table(Churn,Contract),col=c("darkblue","yellow"),legend = rownames(table(Churn,Contract)))
  barplot(table(Churn,InternetService),main="churn gegen internetservuse",xlab="internetservices", col=c("darkblue","yellow"),legend = rownames(table(Churn,InternetService)))
  barplot(table(Churn,Tenure),main="churn gegen Tenure",xlab="Tenure", col=c("darkblue","yellow"),legend = rownames(table(Churn,Tenure)))
  barplot(table(Churn,PaymentMethod),main="churn gegen paymentmethod",xlab="paymentmethod", col=c("darkblue","yellow"),legend = rownames(table(Churn,PaymentMethod)))
  barplot(table(Churn,PaperlessBilling),main="churn gegen papersbilling",xlab="papersbilling", col=c("darkblue","yellow"),legend = rownames(table(Churn,PaperlessBilling)))
  
}

  
 


#Bei der Interpretation hilft das R-Paket effects:
require(effects)
library(effects)
plot(allEffects(Aktuall_mod5),MultipleLines=TRUE)

#plot effects mit  mein ausgew?hlt variable internetservice
plot(allEffects(Aktuall_mod5),2)


#mit analysis of variance
anova(Aktuall_mod5,test = "Chisq")

#Interpretation
churn.lin.pred<-Aktuall_mod5$linear.predictors# die linearen  prädiktor
churn.angepasst<-Aktuall_mod5$fitted.values# die angepassten wahrscheinlichkeiten
pos.prog<-(churn.angepasst>0.5)#wahrscheinlichkeit von 50% ein positive prognose
prop.table(table(pos.prog,Telecom$Churn))#kreuztabelle für pos.prog und churn

library(glm.predict)
predicts(Aktuall_mod5,"0,1;F", position = 1)


# mit variable internetservice berechen wir odd Ratio,log odd ratio ,relative risk ratio
OR<-exp(Aktuall_mod5$coefficients);OR
log(OR)





