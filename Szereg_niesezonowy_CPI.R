############### SZEREG NIESEZONOWY ######################


### Import dataset Mexico ###

install.packages("readxl")
library("readxl")
mexico<- read_excel("Mexico.xls")

# Struktura
str(mexico)

# Zmiana CPI na zmiennÄ… ts i datÄ™ jako data
mexico$CPI <-log(mexico$CPI) #zmiana na logarytm ###!!!!!!!!!! JESZCZE NIE WIEM

mexico.ts <- ts(mexico$CPI, freq = 1, start = c(1960,1), end=c(2020,1))
mexico.ts


# WstÄ™pna wizualizacja
plot(mexico.ts,
     main = "CPI",
     xlab = "Data",
     ylab = "Wartosci")

abline(h =mean(mexico.ts), col="red", lty = 2)

#####################################################
######### MODELE EKSTRAPOLACYJNE #####################
#####################################################

# Na start, w ramach tego, ĹĽe potem model Holta, bÄ™dzie porĂłwnywany z ARIMÄ„, to rozdzielam prĂłbÄ™ na in-sample i out-of-sample
mexico.ts.train <- window(mexico.ts,
         end = c(2017,1))

mexico.ts.test <- window(mexico.ts, start=c(2018,1))

##############################
# 1. WygĹ‚adzenie wykĹ‚adnicze
##############################

# Do procesu wygĹ‚adzenia danych wykorzystam okres bez ostatnich 3 obserwacji
# t < c(2018,1)
# a nastÄ™pnie oszacuje prognozÄ™ dla tego okresu (ostatnie 3 obserwacje

# Proste wygladzanie wykladniczea (bez trendu, bez efektĂłw sezonowych)
mexico.SES_ <- HoltWinters(mexico.ts.train,
                           alpha = 0.8,
                           beta  = FALSE, # beta jest czynnikiem trendu
                           gamma = FALSE) # gamma jest czynnikiem sezonowym


# modeli liniowy Holta (bez efektĂłw sezonowych) #pozwalam zoptymalizowaÄ‡ alphe
mexico.SES <- HoltWinters(mexico.ts.train,
                          beta  = FALSE, # beta jest czynnikiem trendu
                          gamma = FALSE) # gamma jest czynnikiem sezonowym

mexico.SES # info o modelu


plot(mexico.SES)


# wartoĹ›ci wygĹ‚adzone przechowywane sÄ… w $fitted 
# (razem z poszczegĂłlnymi komponentami po zdekomponowaniu)

plot(mexico.SES$fitted)

# pierwsza kolumna w $fitted zawiera wartoĹ›ci wygĹ‚adzone 

plot(mexico.SES$fitted[, 1])

# policzmy prognozÄ™ na 3 obserwacje do przodu
mexico.SES.forecast <- predict(mexico.SES,
                               n.ahead = 3,
                               prediction.interval = TRUE)

mexico.SES_.forecast <- predict(mexico.SES,
                               n.ahead = 3,
                               prediction.interval = TRUE)

# porĂłwnamy prognozÄ™ z oryginalnym szeregiem
plot(mexico.ts)
lines(mexico.SES$fitted[,1], col ='red')
lines(mexico.SES.forecast[, 1], col = "red") # prognozy 
lines(mexico.SES.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
lines(mexico.SES.forecast[, 3], col = "red", lty = 2) # gĂłrna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
abline(v = c(2018,1), lty = 2) # dodajemy pionowÄ… liniÄ™ referencyjnÄ…

plot(mexico.ts)
lines(mexico.SES_.forecast[, 1], col = "blue") # prognozy 
lines(mexico.SES_.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
lines(mexico.SES_.forecast[, 3], col = "red", lty = 2) # gĂłrna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
abline(v = c(2018,1), lty = 2) # dodajemy pionowÄ… liniÄ™ referencyjnÄ…


# 4. 
# utwĂłrzmy teraz szereg z wartoĹ›ciami wygĹ‚adzonymi dla okresu in-sample
# oraz prognozami dla okresu out-of-sample

mexico.SES$fitted[, 1]

mexico.SES.summary <- 
  window(mexico.SES$fitted[, 1], end = c(2020,1), extend = TRUE)

mexico.SES_.summary <- 
  window(mexico.SES_$fitted[, 1], end = c(2020,1), extend = TRUE)

# w miejsce brakĂłw danych na koĹ„cu wstawiamy prognozy 
window(mexico.SES.summary, start = c(2018,1)) <-
  mexico.SES.forecast[, 1]

window(mexico.SES_.summary, start = c(2018,1)) <-
  mexico.SES_.forecast[, 1]


##############################
# 2. Metoda Holta
##############################

# modeli liniowy Holta (bez efektĂłw sezonowych)
mexico.Holt_ <- HoltWinters(mexico.ts.train,
                            alpha = 0.8,
                            beta  = 0.3,
                            gamma = FALSE) # gamma jest czynnikiem sezonowym
# jeĹĽeli FALSE -> szacujemy wtedy model liniowy Holta

# model liniowy Holta dla innych parametrĂłw
mexico.Holtt <- HoltWinters(mexico.ts.train,
                            alpha = 0.5,
                            beta  = 0.5,
                            gamma = FALSE) # gamma jest czynnikiem sezonowym
# jeĹĽeli FALSE -> szacujemy wtedy model liniowy Holta

#############################################
# Optymalny wybĂłr parametrĂłw alpha i beta
###########################################

# modeli liniowy Holta (bez efektĂłw sezonowych) 
mexico.Holt <- HoltWinters(mexico.ts.train,
                           gamma = FALSE) # gamma jest czynnikiem sezonowym

mexico.Holt # info o modelu


plot(mexico.Holt)

# policzmy prognozÄ™ na 3 obserwacje do przodu
mexico.Holt.forecast <- predict(mexico.Holt,
                                n.ahead = 3,
                                prediction.interval = TRUE)
                              
mexico.Holt_.forecast <- predict(mexico.Holt_,
n.ahead = 3,
prediction.interval = TRUE)
                                                                
mexico.Holtt.forecast <- predict(mexico.Holtt,
n.ahead = 3,
prediction.interval = TRUE)

# porĂłwnanie prognozy z oryginalnym szeregiem                                
plot(mexico.ts)
lines(mexico.Holt$fitted[, 1], col = "green")
lines(mexico.Holt.forecast[, 1], col = "green") # prognozy 
lines(mexico.Holt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
lines(mexico.Holt.forecast[, 3], col = "red", lty = 2) # gĂłrna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
abline(v=c(2018,1), lty = 2) #pionowa linia referencyjna

plot(mexico.ts)
lines(mexico.Holt_.forecast[, 1], col = "blue") # prognozy 
lines(mexico.Holt_.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
lines(mexico.Holt_.forecast[, 3], col = "red", lty = 2) # gĂłrna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
abline(v=c(2018,1), lty = 2) #pionowa linia referencyjna

plot(mexico.ts)
lines(mexico.Holtt.forecast[, 1], col = "blue") # prognozy 
lines(mexico.Holtt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
lines(mexico.Holtt.forecast[, 3], col = "red", lty = 2) # gĂłrna granica przedziaĹ‚u ufnoĹ›ci dla prognozy
abline(v=c(2018,1), lty = 2) #pionowa linia referencyjna

#### PorĂłwnanie prognozy

# 4. 
# utwĂłrzmy teraz szereg z wartoĹ›ciami wygĹ‚adzonymi dla okresu in-sample
# oraz prognozami dla okresu out-of-sample

mexico.Holt$fitted[, 1]

mexico.Holt.summary <- 
  window(mexico.Holt$fitted[, 1], end = c(2020,1), extend = TRUE)

mexico.Holt_.summary <- 
  window(mexico.Holt$fitted[, 1], end = c(2020,1), extend = TRUE)

mexico.Holtt.summary <- 
  window(mexico.Holt$fitted[, 1], end = c(2020,1), extend = TRUE)

# w miejsce brakĂłw danych na koĹ„cu wstawiamy prognozy 
window(mexico.Holt.summary, start = c(2018,1)) <-
  mexico.Holt.forecast[, 1]

window(mexico.Holt_.summary, start = c(2018,1)) <-
  mexico.Holt.forecast[, 1]

window(mexico.Holtt.summary, start = c(2018,1)) <-
  mexico.Holt.forecast[, 1]

# Ĺ‚Ä…czymy wszystkie kolumny razem 
mexico.summary <- ts.union(mexico.ts,
                           mexico.SES.summary,
                           mexico.SES_.summary,
                           mexico.Holt.summary,
                           mexico.Holt_.summary,
                           mexico.Holtt.summary)



#####################
# PorĂłwnanie modeli #
#####################

# utworzymy takĹĽe zmiennÄ… binarnÄ… dzielÄ…cÄ… caĹ‚y zbiĂłr obserwacji
# na okresy in-sample i out-of-sample

library(xts)
mexico.summary = as.xts(mexico.summary)

ifelse(index(mexico.summary) < '2018-01-01', 1, 0)

sample_period <-
  ts(ifelse(index(mexico.summary) < '2018-01-01', 1, 0), 
     start  =1, freq = 1)


colnames(mexico.summary)


names(mexico.summary)

# nazwy trochÄ™ uproĹ›cimy

names(mexico.summary)<-c("CPI","SES","SES_","Holt","Holt_","Holtt")

# utworzymy teĹĽ zmiennÄ… indeksujÄ…cÄ… czas (date)

mexico.summary <- as.data.frame(mexico.summary)
mexico.summary$sample_period <- sample_period





# 7.
# porownanie przecietnych bledow prognoz w okresie out-of-sample 
# z tymi z okresu in-sample

mexico.summary$mae_SES     <- abs(mexico.summary$SES-mexico.summary$CPI)
mexico.summary$mse_SES     <- (mexico.summary$SES-mexico.summary$CPI)^2
mexico.summary$mape_SES    <- abs((mexico.summary$SES-mexico.summary$CPI)/mexico.summary$CPI)
mexico.summary$amape_SES   <- abs((mexico.summary$SES-mexico.summary$CPI)/(mexico.summary$SES+mexico.summary$CPI))

mexico.summary$mae_SES_     <- abs(mexico.summary$SES_-mexico.summary$CPI)
mexico.summary$mse_SES_     <- (mexico.summary$SES_-mexico.summary$CPI)^2
mexico.summary$mape_SES_    <- abs((mexico.summary$SES_-mexico.summary$CPI)/mexico.summary$CPI)
mexico.summary$amape_SES_   <- abs((mexico.summary$SES_-mexico.summary$CPI)/(mexico.summary$SES_+mexico.summary$CPI))

mexico.summary$mae_Holt     <- abs(mexico.summary$Holt-mexico.summary$CPI)
mexico.summary$mse_Holt    <- (mexico.summary$Holt-mexico.summary$CPI)^2
mexico.summary$mape_Holt    <- abs((mexico.summary$Holt-mexico.summary$CPI)/mexico.summary$CPI)
mexico.summary$amape_Holt   <- abs((mexico.summary$Holt-mexico.summary$CPI)/(mexico.summary$Holt+mexico.summary$CPI))

mexico.summary$mae_Holt_     <- abs(mexico.summary$Holt_-mexico.summary$CPI)
mexico.summary$mse_Holt_    <- (mexico.summary$Holt_-mexico.summary$CPI)^2
mexico.summary$mape_Holt_    <- abs((mexico.summary$Holt_-mexico.summary$CPI)/mexico.summary$CPI)
mexico.summary$amape_Holt_   <- abs((mexico.summary$Holt_-mexico.summary$CPI)/(mexico.summary$Holt_+mexico.summary$CPI))

mexico.summary$mae_Holtt     <- abs(mexico.summary$Holtt-mexico.summary$CPI)
mexico.summary$mse_Holtt    <- (mexico.summary$Holtt-mexico.summary$CPI)^2
mexico.summary$mape_Holtt    <- abs((mexico.summary$Holtt-mexico.summary$CPI)/mexico.summary$CPI)
mexico.summary$amape_Holtt   <- abs((mexico.summary$Holtt-mexico.summary$CPI)/(mexico.summary$Holtt+mexico.summary$CPI))


# nastepnie je usredniamy 
# oddzielnie dla okresu in-sample oraz dla okresu prognozy (out-of-sample)

names(mexico.summary)

# wartosci do usrednienie mamy w kolumnach 9:28
# musimy zatem uwzgladniac braki danych (zignorowac je w obliczeniach)
aggregate(mexico.summary[, 8:27],
          by = list(mexico.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

colMeans(na.omit(mexico.summary[,8:27]))

# UWAGA! model z najlepszym dopasowaniem w okresie in-sample
# wcale nie musi generowaÄ‡ najlepszej prognozy!

########################
# Porownanie prognozy ##
########################
plot(mexico.ts)
lines(mexico.Holt$fitted[, 1], col = "green")
lines(mexico.Holt.forecast[, 1], col = "green") # prognozy 
lines(mexico.SES$fitted[, 1], col = "red")
lines(mexico.SES.forecast[, 1], col = "red") # prognozy 
abline(v = c(2018,1), lty = 2) # dodajemy pionowÄ… liniÄ™ referencyjnÄ…


plot(mexico.ts, main = "3 roczna prognoza dla log_mexico",
     xlim = c(2016, 2020), ylim = c(1, 2.5))
lines(mexico.Holt$fitted[, 1], col = "green")
lines(mexico.Holt.forecast[, 1], col = "green") # prognozy 
lines(mexico.SES$fitted[, 1], col = "red")
lines(mexico.SES.forecast[, 1], col = "red") # prognozy 
abline(v = c(2018,1), lty = 2) # dodajemy pionowÄ… liniÄ™ referencyjnÄ…


################################
####### MODEL ARIMA ############
################################

### PO ZROBIENIU MODELI EKSTRAPOLACYJNYCH ZALADOWAC JESZCZE RAZ PLIK I NIE SPRAWDZAC JUZ TYCH MODELI !!!! 

## IN SAMPLE ##


mexico.ar <- mexico.ts.train

###################################################
################## Proces stacjonarny #############
###################################################

## Wykrywanie niestacjonarości ##


### Sprawdzanie autokorelacji reszt ###

## Test Breuscha-Godfreya ## <-  jest testem wykorzystującym metodę mnożników Lagrange’a. Ma on przewagę nad testem
#Durbina-Watsona, ponieważ jest w stanie wykrywać obecność autokorelacji
#wyższych rzędów

library(lmtest)
#install.packages('urca')
library(urca) # pamietac ze, zaladowanie tej biblioteki chowa użycie funkcji as.Data as.Date.numeric
df.test.mexico.ar <- ur.df(mexico.ar, type = c("none"), lags = 0) # lags = 0, bo prosty test DF
resids.mexico <- df.test.mexico.ar@testreg$residuals
bg1 <- bgtest(resids.mexico ~ 1, order = 1)
bg1

#pvalue = 0.4219 brak podstaw do odrzucenia h0 mówiącej o braku autokorelacji reszt MOGE INTERPRETOWAC DICKEYA FULLERA (0.78 dla logarytmu)

# Test Dickeya - Fullera #

#lag = 0

summary(df.test.mexico.ar)

# Critical value = -1.97

# test pokazał, iż mam brak podstaw do odrzucenia h0 mówiącej o niestacjonarości zmiennej dla wartosci krytycznej przy pvalue=0.10 (tau -2.6)
# postanawiam wieć zróżnicowań zmienną ( DLA LOGARYTMU -0.75 czyli czyli brak podstaw do odrzutu h0)
# Pierwsza różnica #
library(xts)

Dy<-diff.xts(mexico.ar) ####albo diff.xts

# wykres
plot(Dy,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze roznice dla szeregu: Bladzenie przypadkowe")

#Ponownie sprawdzam autokorelacje
df.test.Dy <- ur.df(na.omit(Dy), type = c("none"), lags = 0)
resids.Dy <- df.test.Dy@testreg$residuals
bg2 <- bgtest(resids.Dy ~ 1, order = 1)
bg2
# pvalue = 0.8128 czyli brak podstaw do odrzutu h0 (dla LOG 0.92)

# Test Dickey Fullera
summary(df.test.Dy)
# Value of test statistic = -6.9006 w zwiazku z czym odrzucam hipoteze h0 dla kazdej z 3 wartosci krytycznych (dla logarytmu -7.3185)

# Test KPSS dla zmiennej zintegrowanej #
# ur.kpss(y, type = c("mu", "tau")) - "mu" - staĹ‚a w rĂłwnaniu testowym; "tau" - trend w rĂłwnaniu testowym

kpss.test <- ur.kpss(Dy, type = c("mu"))  # staĹ‚a w rĂłwnaniu testowym
summary(kpss.test)

# Wartosc statystyki testowej = 0.0901 tak wiec nie ma podstaw do odrzucenia hipotezy zerowej mowiacej o stacjonarosci zmiennej
# Tak wiec ROZNICOWANIE 1szego stopnia było ODPOWIEDNIE (DLA LOGARYTMU 0.13)

##################################################
# Test na White-Noise dla zróżnicowanego szeregu #
##################################################

# test Ljung-Box
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem # DO TEGO JESZZCZE WROCIC< BO NWM CZY DOBRA ZMIENNA
Box.test(Dy, lag=24, type="Ljung-Box")
#pvalue = 0.08707 ,czyli mam podejrzenie o biały szum (DLA LOGARYTMU 0.0134)

# test Box-Pierce
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem # DO TEGO JESZCZE WROCIC < BO NWM CZY DOBRA ZMIENNA
Box.test(Dy, lag=24, type="Box-Pierce")
#pvalue = 0.1946, nie mam podstaw do odrzutu h0 (0.1921)

plot(Dy,
     type = "l",      # rodzaj wykresu (l=line)
     col  = "black",  # kolor linii
     lwd  = 2,        # podwĂłjna grubosÄ‡
     main = "Normalny Bialy szum")

#### ACF I PACF ###
# jak wygladaja ACF i PACF?
# przedstawmy je na wspolnym diagramie
# dodatkowo ograniczamy wartosci na osi pionowej

plot(Dy, type = "l", main = "WN")

par(mfrow = c(1, 2))
acf(Dy, lag.max = 24,
    ylim = c(-1, 1),
    xlim = c(1,24),
    lwd = 4, col = "red",
    na.action=na.pass)
# MA = 1?
pacf(Dy, lag.max = 24,
     ylim = c(-1, 1),
     xlim = c(1,24),     
     lwd = 4, col = "red",
     na.action=na.pass)
# AR = 0?

par(mfrow = c(1, 1))


# wartosci dla ACF i PACF
acf(Dy, lag.max = 24, plot=FALSE, na.action = na.pass) #na.action=na.pass dodałem bo mamy wartosci NaN ze wzgledu na roznicowanie
pacf(Dy, lag.max = 24, plot=FALSE, na.action = na.pass) #na.action=na.pass dodałem bo mamy wartosci NaN ze wzgledu na roznicowanie

# Alternatywnie ACF I PACF
#install.packages('forecast')
library("forecast")
tsdisplay(Dy, lag.max=24)

######## 2. Estymacja - od ogółu do szczegółu + AIC/BIC

##### ARIMY
#ARIMA(3,1,3)
arima313 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(3, 0, 3),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)


#ARIMA(3,1,2)
arima312 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(3, 0, 2),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(2,1,3)
arima213 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(2, 0, 3),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(2,1,2)
arima212 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(2, 0, 2),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(2,1,1)
arima211 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(2, 0, 1),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(1,1,2)
arima112 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(1, 0, 2),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(1,1,1)
arima111 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(1, 0, 1),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(0,1,1)
arima011 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(0, 0, 1),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(1,1,0)
arima110 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(1, 0, 0),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

#ARIMA(0,1,0)
arima010 <- arima(Dy,  # zmienna zaleĹĽna
                  order = c(0, 0, 0),
                  # rzÄ™dy (p,d,q)
                  #xreg =  # regresory egzogeniczne
)

## AIC BIC
#AIC
AIC(arima313)
AIC(arima312)
AIC(arima213)
AIC(arima212) # dla logarytmu najmniejszy
AIC(arima211)
AIC(arima112) ###najmniejszy
AIC(arima111)
AIC(arima011)
AIC(arima110)
AIC(arima010)

#BIC
BIC(arima313)
BIC(arima312)
BIC(arima213)
BIC(arima212)
BIC(arima211)
BIC(arima112) #najmniejszy
BIC(arima111)
BIC(arima011)
BIC(arima110)
BIC(arima010) #najmniejszy dla logarytmu

# Czy reszty sÄ… biaĹ‚ym szumem? # ARIMA 112

library('forecast')

## TEST ARIMY 010

par(mfrow = c(1, 2))
Acf(resid(arima010), lag.max = 24,
    ylim = c(-1, 1),
    xlim=c(1,24),
    lwd = 4, col = "red")
Pacf(resid(arima010), lag.max =24 ,
     lwd = 4, col = "red")

par(mfrow = c(1, 1))

# Test Ljung-Boxa (do sensowego opĂłĹşnienia).
Box.test(resid(arima010), type = "Ljung-Box", lag = 24)
Box.test(resid(arima010), type = "Box-Pierce", lag = 24)

## istotności

#ARIMA 010
coeftest(arima010)

###### PROGNOZA

# prognozy dla dodatkowych regresorĂłw

# zbior zawiera 2 elementy:
# pred - prognozy
# se - bĹ‚Ä…d standardowy prognozy

# prognozy dla lwpi - model ARIMA(0,1,0 )
length(mexico.ar)
length(mexico)
# prognozy dla lwpi - model ARIMA(0,1,0)
nobs <- length(mexico.ar)

# szacujemy model na prĂłbie IN SAMPLE
arima010_ <- arima(mexico.ar, # zmienna zaleĹĽna
                    order = c(0, 1, 0),  # rzÄ™dy (p,d,q)
                    xreg = 1:nobs,       # dodatkowe regresory - stala
)

summary(arima010_)

forecast_010 <- predict(arima010_, n.ahead = 3,
                          newxreg = (nobs + 1) : (nobs + 3)) # prognozy dla dodatkowych regresorĂłw
# obejrzyjmy wynik
forecast_010

# zbior zawiera 2 elementy:
# pred - prognozy
# se - bĹ‚Ä…d standardowy prognozy


?predict

# wykres prognozy
plot(mexico.ts)
abline(v = c(2018,1), lty = 2, col = "gray")
lines(forecast_010$pred, col = "red", lwd = 2)
lines(forecast_010$pred + 2 * forecast_010$se, col = "red", lty = 3)
lines(forecast_010$pred - 2 * forecast_010$se, col = "red", lty = 3)

# i raz jeszcze,w zbliĹĽeniu
plot(mexico.ts, main = "3-miesięczna prognoza dla CPI meksyku",
     xlim = c(2016, 2020), ylim = c(0,2))
abline(v = c(2017,1), lty = 2, col = "gray")
lines(forecast_010$pred, col = "red", lwd = 2)
lines(forecast_010$pred + 2 * forecast_010$se, col = "red", lty = 3)
#lines(forecast_010$pred - 2 * forecast_010$se, col = "red", lty = 3) ## gdyby to dodac to u mnie widac dolny przedzial ufnosci



# Ĺ‚Ä…czymy prognozy z oryginalnym szeregiem
mexico_forecast <- data.frame(forecast = forecast_010$pred,
                                   window(mexico.ts,
                                          start = c(2018, 1)))

mexico_forecast

str(mexico_forecast)

mexico_forecast$mae <- abs(as.numeric(mexico_forecast$window.mexico.ts..start...c.2018..1..) - mexico_forecast$forecast)
mexico_forecast$mse <- (as.numeric(mexico_forecast$window.mexico.ts..start...c.2018..1..) - mexico_forecast$forecast) ^ 2
mexico_forecast$mape <- abs((as.numeric(mexico_forecast$window.mexico.ts..start...c.2018..1..) - 
                                    mexico_forecast$forecast) / as.numeric(mexico_forecast$window.mexico.ts..start...c.2018..1..))
mexico_forecast$amape <- abs((as.numeric(mexico_forecast$window.mexico.ts..start...c.2018..1..) - mexico_forecast$forecast) /
                                    (as.numeric(mexico_forecast$window.mexico.ts..start...c.2018..1..) + mexico_forecast$forecast))
str(mexico_forecast[, 3:6])

colMeans(mexico_forecast[, 3:6])

# aby zmieniÄ‡ 'naukowy' format liczb
# moĹĽemy skorzystaÄ‡ z opcji scipen
options(scipen = 5)
round(colMeans(mexico_forecast[, 3:6]), 3)

###### POROWNANIE <- wygladzenie iteracyjnie, holt iteracyjnie, arima(0,1,0)

plot(mexico.ts, main = "3 roczna prognoza dla CPI Meksyku")
#lines(mexico.Holt$fitted[, 1], col = "blue")
lines(mexico.Holt.forecast[, 1], col = "blue") # prognozy 
#lines(mexico.SES$fitted[, 1], col = "red")
lines(mexico.SES.forecast[, 1], col = "red") # prognozy
lines(forecast_010$pred, col = "green")
abline(v = c(2017,1), lty = 2) # dodajemy pionowÄ… liniÄ™ referencyjnÄ…


plot(mexico.ts,
     xlim = c(2016, 2020), ylim = c(1, 2.5))
#lines(mexico.Holt$fitted[, 1], col = "blue")
#lines(mexico.Holt.forecast[, 1], col = "blue") # prognozy 
#lines(mexico.SES$fitted[, 1], col = "red")
#lines(mexico.SES.forecast[, 1], col = "red", lwd=2) # prognozy
lines(forecast_010$pred, col = "green")
abline(v = c(2018,1), lty = 2) # dodajemy pionowÄ… liniÄ™ referencyjnÄ…

## ARIMA WIEC SIE POKRYWA Z WYGLADZENIEM W MOIM PRZYPADKU