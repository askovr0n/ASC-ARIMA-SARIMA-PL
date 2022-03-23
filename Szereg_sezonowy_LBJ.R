                                      #######################################################
                                      ################## SZEREG SEZONOWY ####################
                                      #######################################################

#  1. 
# Importuje dane 

library("readxl")
lbj<- read_excel("LebronJames.xls")
#################################################################################
#### NOTE: EKSTRAPOLACJA BEDZIE ROBIONA NA PAKIECIE TS, NATOMIAST SARIMA NA XTS #
#################################################################################
# weryfikujemy strukture obiektu
str(lbj)

# Demetra podpowiada zeby zastosowac logarytm

lbj$Time <- log(lbj$Time)

# utworzymy obiekt typu ts (time series) dla danych 
lbj.ts <- 
  ts(lbj$Time, start = c(2010, 1), freq = 12)

lbj.ts
str(lbj.ts)

#  2. 
#  Wizualizacja szeregu 

plot(lbj.ts,
     main = "lbj",
     xlab = "data",
     ylab = "lbj")


# 3.

# do procesu wygÄąâ€šadzenia danych wykorzystamy okres bez ostatnich 12 obserwacji
# a nastĂ„â„˘pnie oszacujemy prognozy dla tego okresu (ostatnie 12 obserwacje)

# do zawĂ„â„˘ÄąÄ˝enia zbioru obserwacji moÄąÄ˝emy wykorzystaĂ„â€ˇ funkcjĂ„â„˘ window()
lbj.ts.train <- 
  window(lbj.ts,
         end = c(2019, 12))

lbj.ts.test <- 
  window(lbj.ts,
         start = c(2020, 1))

# 4.
# addytywny model Holta-Wintersa 
lbj.HWadd <- HoltWinters(lbj.ts.train,
                            seasonal = "additive")
lbj.HWadd

# multiplikatywny model Holta-Wintersa 
lbj.HWmult <- HoltWinters(lbj.ts.train,
                             seasonal="multiplicative")
lbj.HWmult # info o modelu

plot(lbj.HWmult)

# oszacujmy prognozĂ„â„˘ na 3 okresu naprzÄ‚Ĺ‚d
lbj.HWmult.forecast <- predict(lbj.HWmult,
                                  n.ahead = 12,
                                  prediction.interval = TRUE)

lbj.HWadd.forecast <- predict(lbj.HWadd,
                               n.ahead = 12,
                               prediction.interval = TRUE)

# a nastĂ„â„˘pnie porÄ‚Ĺ‚wnajmy prognozĂ„â„˘ z oryginalnym szeregiem 
plot(lbj.ts)
lines(lbj.HWmult.forecast[, 1], col = "blue") # prognoza
lines(lbj.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaÄąâ€šu ufnoÄąâ€şci
lines(lbj.HWmult.forecast[, 3], col = "red", lty = 2) # gÄ‚Ĺ‚rna granica przedziaÄąâ€šu ufnoÄąâ€şci
abline(v = c(2020,1), lty = 2) # dodajemy pionowĂ„â€¦ liniĂ„â„˘ referencyjnĂ„â€¦

plot(lbj.ts)
lines(lbj.HWadd.forecast[, 1], col = "blue") # prognoza
lines(lbj.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziaÄąâ€šu ufnoÄąâ€şci
lines(lbj.HWadd.forecast[, 3], col = "red", lty = 2) # gÄ‚Ĺ‚rna granica przedziaÄąâ€šu ufnoÄąâ€şci
abline(v = c(2020,1), lty = 2) # dodajemy pionowĂ„â€¦ liniĂ„â„˘ referencyjnĂ„â€¦

# obejrzyjmy prognozĂ„â„˘ w zbliÄąÄ˝eniu
length(lbj.ts)

# zmienimy w tym celu jedynie pierwszĂ„â€¦ liniĂ„â„˘ (pierwsze polecenie)
plot(window(lbj.ts, start = c(2020, 1)))
lines(lbj.HWmult.forecast[, 1], col = "blue")  
lines(lbj.HWmult.forecast[, 2], col = "red", lty = 2) 
lines(lbj.HWmult.forecast[, 3], col = "red", lty = 2) 
abline(v = c(2020,1), lty = 2)

plot(lbj.ts) #, xlim = c(2020,2021), ylim = c(2.5, 4.1)
lines(lbj.HWadd.forecast[, 1], col = "blue")
lines(lbj.HWmult.forecast[, 1], col = "red")
#lines(lbj.HWadd.forecast[, 2], col = "red", lty = 2) 
#lines(lbj.HWadd.forecast[, 3], col = "red", lty = 2) 
abline(v = c(2020,1), lty = 2) 

lbj.HWadd.summary <- 
  window(lbj.HWadd$fitted[, 1], end = c(2020,12), extend = TRUE)
window(lbj.HWadd.summary, start = c(2020,1)) <-
  lbj.HWadd.forecast[, 1]

lbj.HWmult.summary <- 
  window(lbj.HWmult$fitted[, 1], end = c(2020,12), extend = TRUE)
window(lbj.HWmult.summary, start = c(2020,1)) <-
  lbj.HWmult.forecast[, 1]

#### WYZNACZNEIE BĹÄDĂ“W

# Äąâ€šĂ„â€¦czymy wszystkie kolumny razem

lbj.summary <- ts.union(lbj.ts,
                        lbj.HWadd.summary,
                        lbj.HWmult.summary)

library(xts)
lbj.summary = as.xts(lbj.summary)

ifelse(index(lbj.summary) < 'sty 2020', 1, 0)

sample_period <-
  ts(ifelse(index(lbj.summary) < 'sty 2020', 1, 0), 
     start  =1, freq = 1)


colnames(lbj.summary)


names(lbj.summary)

# nazwy trochĂ„â„˘ uproÄąâ€şcimy

names(lbj.summary)<-c("Clicks","Add","Multi")

# utworzymy teÄąÄ˝ zmiennĂ„â€¦ indeksujĂ„â€¦cĂ„â€¦ czas (date)

lbj.summary <- as.data.frame(lbj.summary)
lbj.summary$sample_period <- sample_period




# 7.
# porownanie przecietnych bledow prognoz w okresie out-of-sample 
# z tymi z okresu in-sample


lbj.summary$mae_HWadd     <- abs(lbj.summary$Add-lbj.summary$Clicks)
lbj.summary$mse_Hwadd     <- (lbj.summary$Add-lbj.summary$Clicks)^2
lbj.summary$mape_HWadd    <- abs((lbj.summary$Add-lbj.summary$Clicks)/lbj.summary$Clicks)
lbj.summary$amape_HWadd   <- abs((lbj.summary$Add-lbj.summary$Clicks)/(lbj.summary$Add+lbj.summary$Clicks))



lbj.summary$mae_HWmult <- abs(lbj.summary$Multi-lbj.summary$Clicks)
lbj.summary$mse_HWmult <- (lbj.summary$Multi-lbj.summary$Clicks)^2
lbj.summary$mape_HWmult <- abs((lbj.summary$Multi-lbj.summary$Clicks)/lbj.summary$Clicks)
lbj.summary$amape_HWmult <- abs((lbj.summary$Multi-lbj.summary$Clicks)/(lbj.summary$Multi+lbj.summary$Clicks))

# nastepnie je usredniamy 
# oddzielnie dla okresu in-sample oraz dla okresu prognozy (out-of-sample)

names(lbj.summary)

####################    TUTAJ HEEEEEEEELP ###########
## UPDATE: JUZ JEST ROZWIAZANIE


aggregate(lbj.summary[, 5:12],
          by = list(lbj.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T)) 


################################
####### MODEL SARIMA ############
################################

########################################################
################## Proces stacjonarny ##################
########################################################


# wczytanie pakietow do pamieci

library(foreign)
library(xts)
library(urca)    # JAKBY NIE DZIALALY TESTY DF TO PONOWNIE ZALADOWAC
library(lmtest) # JAKbY NIE DZIALALY COEFTESTY TO ZALADOWAC TA BIBLIOTEKE JESZCZE RAZ
library(fBasics)
library(forecast)
library(tseries)
library(fUnitRoots)


# wczytanie funkcji do wykorzystania w analizie #ALE testdf2 mi nie dziala, problem z wektorami DAFAQ
source("funs/testdf2.R")

###########################################################################
# I. Przygogowanie danych                                                 #
###########################################################################

# 1. Import danych, oczywiscie te same dane co wczesniej, ale po prostu sobie je inaczej nazwe

king<- read_excel("LebronJames.xls")

# obejrzymy dane zaimportowane
str(king)

# formatujemy datĂ„â„˘ na format zrozumiaÄąâ€šy dla R
king$Data <- as.Date(king$Data, format="%Y-%m-%d") ###

# przekonwertujmy dane do obiektu typu xts
# (podobny do obiektu ts)
king.xts <- xts(king$Time,   # kolumny z danymi NOTE: U MNIE KOLUMNA "TIME" to kolumna z danymi, bo google je slabo pobralo
                king$Data)  # kolumny z datĂ„â€¦/czasem
names(king.xts)[1] <- "king"
tail(king.xts)

# 2. Ustalamy okres IN-SAMPLE
king_sample.xts <- window(king.xts, end = as.Date("2019-12-01"))
tail(king_sample.xts)

###########################################################################
# PRACUJEMY NA PROBIE IN-SAMPLE                                           #
###########################################################################

# 3. wykresy szeregu wejsciowego i pierwszych roznic

# wykresy szeregu wejsciowego
plot(king_sample.xts)
# widoczna sezonowÄąâ€şĂ„â€ˇ i trend nieliniowy

# wprowadzenie roznic regularnych
king_sample.xts$dking <- diff.xts(king_sample.xts$king, lag = 1)
plot(king_sample.xts$dking)

# 4. logarytmujemy szereg ma to na celu zminiejszenie wariancji
king_sample.xts$lking <- log(king_sample.xts$king)
plot(king_sample.xts$lking)

# Obejrzyjmy korelogramy dla pierwszych rÄ‚Ĺ‚ÄąÄ˝nic
par(mar = rep(2, 4))

par(mfrow = c(2, 1))
acf(king_sample.xts$lking,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(king_sample.xts$lking, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# 5. wprowadzenie roznic regularnych lking
king_sample.xts$dlking <- diff.xts(king_sample.xts$lking, lag = 1)
plot(king_sample.xts$dlking)

# Obejrzyjmy korelogramy dla pierwszych rÄ‚Ĺ‚ÄąÄ˝nic
par(mfrow = c(2, 1))
acf(king_sample.xts$dlking,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(king_sample.xts$dlking, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# Pierwsze rÄ‚Ĺ‚ÄąÄ˝nice wykazujĂ„â€¦ silnĂ„â€¦ sezonowoÄąâ€şĂ„â€ˇ
# (wolno wygasajĂ„â€¦ca ACF wielokrotnoÄąâ€şci 12-tego opÄ‚Ĺ‚ÄąĹźnienia)
# A zatem dodatkowo rÄ‚Ĺ‚ÄąÄ˝nicujemy szereg sezonowo, tj. obliczamy dwunaste rÄ‚Ĺ‚ÄąÄ˝nice
# (poniewaÄąÄ˝ mamy dane miesiĂ„â„˘czne).


# 6. wprowadzenie roznic sezonowych dla dlking (ale z roznicami sezonowymi) BYC MOZE BEDZIE POTRZEBNE
king_sample.xts$d12dlking <- diff.xts(king_sample.xts$dlking, lag = 12)
plot(king_sample.xts$d12dlking)

# Obejrzyjmy korelogramy dla pierwszych rÄ‚Ĺ‚ÄąÄ˝nic
par(mfrow = c(2, 1))
acf(king_sample.xts$d12dlking,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(king_sample.xts$d12dlking, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# 7. Formalne testowanie
# Krok 1. Test Dickeya, Haszy, Fullera (DHF) dla lking BADAMY SEZONOWOSC ZMIENNEJ <- CHCEMY SIE JEJ POZBYC

library(urca)

king_sample.xts$d12lking <- diff.xts(king_sample.xts$lking, lag = 12) # ZMIENNA ODSEZONOWANA BEZ ROZNICOWANIA REGULARNEGO
king_sample.xts$lag12lking <- lag.xts(king_sample.xts$lking, k = 12)

plot(king_sample.xts$d12lking)

model1=lm(d12lking~0+lag12lking, data=king_sample.xts)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1

#pvalue ponizej 0.000, czyli nie moge interpretowac testu DHF bo mam autokoralcje, przechodze do ADHF

# Test ADHF dla lking
king_sample.xts$lagd12lking <- lag.xts(king_sample.xts$d12lking, k = 1)

model2=lm(d12lking~0+lag12lking+lagd12lking, data=king_sample.xts)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1
# pvalue ponizej 0

king_sample.xts$lag2d12lking <- lag.xts(king_sample.xts$d12lking, k = 2) #NOTE WAZNE: JEZELI DLA K=2 tez wam wyjdzie autokorelacja to zwiekszacie dalej parametr k, az w koncu jej nie bedzie

model3=lm(d12lking~0+lag12lking+lagd12lking+lag2d12lking, data=king_sample.xts) #NOTE 2: ZWIEKSZAJAC PARAMETR K NALEZY TUTAJ DO WZORU DOPISYWAC NOWO UTWORZONE ZMIENNE 
summary(model3)                                                                 # SPOJRZ NA LINIJKE 295,297,303,305 KAZDORAZOWO DO WZORU DODAWALEM NOWO UTWORZONA ZMIENNA
bg1 <- bgtest(model3, order = 1)
bg1
bg2 <- bgtest(model3, order = 2)
bg2
bg3 <- bgtest(model3, order = 3)
bg3
bg4 <- bgtest(model3, order = 4)
bg4
bg5 <- bgtest(model3, order = 5)
bg5
bg6 <- bgtest(model3, order = 6)
bg6 ### MAJAC DANE KWARTALNE TYLKO DO ORDER=4

# okej jest tu dobrze, nie mam autokorelacji pvalue >0.10/0.05

# PodsumowujĂ„â€¦c test ADHF:
# Statystyka testowa: -0.880
# Statystyka krytyczna: -5.86 dla pvalue = 0.05  ### NOTE GIGA WAZNE <- SPOGLADAC NA TABLICE DLA ADHF BO DLA ROZNEJ LICZBY OBSERWACJI I DLA ROZNYCH DANYCH (kwartalne/miesieczne) STATYSTYKA KRYTYCZYNA DLA pvalue=0.05 JEST INNA !!!!!!!!!!!

# Decyzja: nie ma podstaw do odrzucenia H0, czyli rÄ‚Ĺ‚znice sezonowe sa potrzebne bo -0.880 > -5.86

#test DF BADAMY STACJONAROSC ZMIENNEJ
# H0: zmienna d12lking jest zmienna niestacjonarna

df.king.ar <- ur.df(na.omit(king_sample.xts$d12lking), type = c("none"), lags = 1) # dla lags = 0 miaĹ‚em AUTOKORELACJE, wiec zrobilem dla lags=1 i juz bylo ok (czyli zastosowalem ADF)
resids.king <- df.king.ar@testreg$residuals
bg1 <- bgtest(resids.king ~ 1, order = 1)
bg1
bg2 <- bgtest(resids.king ~ 1, order = 2)
bg2
bg3 <- bgtest(resids.king ~ 1, order = 3)
bg3
bg4 <- bgtest(resids.king ~ 1, order = 4)
bg4
bg5 <- bgtest(resids.king ~ 1, order = 5)
bg5
bg6 <- bgtest(resids.king ~ 1, order = 6)
bg6
summary(df.king.ar) # STATYSTYKA KRYTYCZNA -4.04 CZYLI ODRZUCAM HIPOTEZE h0 o niestacjonarnosci zmiennej

########### GIGA WAZNE ###################
# JESLI WAM TU WYJDZIE NA ODWROT TO STOSOWAC DO DALSZYCH TESTOW ZMIENNA Z LINIJKI 278 I PAMIETAC ZE W TAKIM RAZIE BEDZIE MIEC ROZNICOWANIE RZEDU 1 (d=1)
#########################################

# brak podstaw do odrzucenia H0.
testdf2(variable = king.xts$d12lking ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6) # TO NIE DZIALA, WIEC POMINAC
#test DF
# H0: zmienna d12dlking jest zmienna niestacjonarna

kpss.test <- ur.kpss(king_sample.xts$d12lking, type = c("mu")) 
summary(kpss.test)



# brak podstaw do odrzutu h0.

# podsumowanie: otrzymaliÄąâ€şmy zatem szereg stacjonarny.
# d=0, D=1

# nalezy dodatkowo przeprowadzic test KPSS

# 8. Czy zmienna d12dlking jest bialym szumem?

# H0: zmienna d12dlking jest bialym szumem

# Test Ljung-Boxa
Box.test(king_sample.xts$d12lking, type = "Ljung-Box", lag = 36)

# Test Boxa-Pierce
Box.test(king_sample.xts$d12lking, type = "Box-Pierce", lag = 36)

# Nie ma biaĹ‚ego szumu

# ACF i PACF
par(mfrow = c(2, 1))
acf(king_sample.xts$d12lking,  lag.max = 36, 
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(king_sample.xts$d12lking, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

## SMA = 1 SAR = 1 MA=5 max AR = 2 okej zeby bylo mniej wiecej rowno zaczne od SARIMA(2,0,3)(1,1,1)

###########################################################################
# II. Identyfikacja                                                       #
###########################################################################

# IDENTYFIKACJA rzĂ„â„˘dÄ‚Ĺ‚w P i Q

# Analiza korelogramow ACF i PACF dla szeregu d12lwpi 

par(mfrow = c(2, 1))
acf(king_sample.xts$d12lking,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(king_sample.xts$d12lking, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))


# Korelogramy sugerujĂ„â€¦, ÄąÄ˝e moÄąÄ˝emy mieĂ„â€ˇ do czynienia z sezonowym procesem MA
# (malejĂ„â€¦ca PACF dla wielokrotnoÄąâ€şĂ„â€ˇi 12 opÄ‚Ĺ‚ÄąĹźnienia)

###########################################################################
# III. Estymacja                                                          #
###########################################################################
nobs <- length(king_sample.xts$lking)

# SARIMA(2,0,3)(2,1,2)
arima203212 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 3),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(2, 1, 2),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima203212
coeftest(arima203212)
# SAR najpierw

# SARIMA(2,0,3)(1,1,2)
arima203112 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 3),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 2),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima203112
coeftest(arima203112)


# SARIMA(2,0,3)(2,1,1)
arima203211 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 3),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(2, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima203211
coeftest(arima203211) # NIE WIEM CZEMU ALE JEST BLAD

# SARIMA(2,0,3)(1,1,1)
arima203111 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 3),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima203111
coeftest(arima203111)
#SAR teraz

# SARIMA(2,0,3)(0,1,1)
arima203011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 3),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima203011
coeftest(arima203011)
# TAK JUZ ZOSTAJE czyli SAR = 0, SMA = 1
# DALEJ BEDE USUWAL SMA 

# SARIMA(2,0,3)(1,1,0)
arima203110 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 3),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima203110
coeftest(arima203110)
#USUWAC MA

# SARIMA(2,0,2)(0,1,1)
arima202011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 2),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)


arima202011
coeftest(arima202011)
# WARTO SIE ZAJAC NAJPIERW MA I AR


# Estymacji modelu SARIMA(1,0,2)(0,1,1)

arima102011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(1, 0, 2),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima102011
coeftest(arima102011)
# MA USUWAM

# Estymacji modelu SARIMA(2,0,1)(0,1,1)

arima201011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(2, 0, 1),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima201011
coeftest(arima201011)
# ma nieistotne

# Estymacji modelu SARIMA(1,0,1)(0,1,1)

arima101011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(1, 0, 1),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima101011
coeftest(arima101011)
# wszystko ladnie istotne ale sprawdze jeszcze dla ar =0 lub ma = 0

# Estymacji modelu SARIMA(0,0,1)(0,1,1)
arima001011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(0, 0, 1),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima001011
coeftest(arima001011)
# tez jest ladnie istotne

# Estymacji modelu SARIMA(1,0,0)(0,1,1)
arima100011 <- arima(king_sample.xts$lking,
                     # rzĂ„â„˘dy (p,d,q)
                     order = c(1, 0, 0),
                     # rzĂ„â„˘dy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                     period = 12),
                     #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima100011
coeftest(arima100011)
# tez ladnie istotne


#Wszystko istotne

# PODSUMOWUJAC: 3 modele sezonowe wydaja bardzo dobrze i sa istotne - procedura od ogolu do szczegolu WYDAJE SIE BYC NAJLEPSZA

# wartoÄąâ€şci AIC
AIC(arima203212, arima203112, arima203111, arima203011,arima203110, arima202011, arima102011, arima201011, arima101011, arima001011, arima100011) #najmniejsze dla 203112

# wartosci BIC
BIC(arima203212, arima203112, arima203111, arima203011,arima203110, arima202011, arima102011, arima201011, arima101011, arima001011, arima100011) #najmniejsza dla arima101011 i ten model teĹĽ wybiore i wszystkie wartosci istotne



# Estymacji modelu SARIMA(1,0,1)(0,1,1)

arima101011_ <- arima(king_sample.xts$lking,
                      # rzĂ„â„˘dy (p,d,q)
                      order = c(1, 0, 1),
                      # rzĂ„â„˘dy sezonowe (P,D,Q)
                      seasonal = list(order = c(0, 1, 1),
                                      # czĂ„â„˘stotliwoÄąâ€şĂ„â€ˇ danych (12 dla danych miesiĂ„â„˘cznych)
                                      period = 12),
                      #xreg = 1:nobs       # dodatkowe regresory - stala
)

arima101011_
coeftest(arima101011_)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
acf(resid(arima101011_), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima101011_), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))


#Efekty sezonowe mozna uznac za wygasĹ‚e

# przystĂ„â„˘pujemy do identyfikacji efektÄ‚Ĺ‚W regularnych

# SARIMA(1,0,1)(0,1,1)
arima101011a <- arima(king_sample.xts$lking,
                      order = c(1, 0, 1),
                      seasonal = list(order = c(0, 1, 1),
                                      period = 12)
)
arima101011a
coeftest(arima101011a)

# jest super

par(mfrow = c(2, 1))
acf(resid(arima101011a),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima101011a), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty sĂ„â€¦ biaÄąâ€šym szumem?
# test Ljung-Boxa
Box.test(resid(arima101011a), type = "Ljung-Box", lag = 36)
Box.test(resid(arima101011a), type = "Box-Pierce", lag = 36)


# Reszty zdecyowanie sa bialym szumem, pvalue > 0.05/0.10




###########################################################################
# IV. Diagnostyka                                                         #
###########################################################################


# BIORE NA TEN MOMENT TYLKO SARIME (1,0,1)(0,1,1)

###########################################################################
# V. Prognoza                                                             #
###########################################################################

# SARIMA(1,0,1)(0,1,1)
arima011011b <- arima(king_sample.xts$lking,
                      order = c(1, 0, 1),
                      seasonal = list(order = c(0, 1, 1),
                                      period = 12)
)


forecast <- predict(arima011011b, n.ahead = 12)

# obejrzyjmy wyniki
forecast
str(forecast)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - bÄąâ€šĂ„â€¦d standardowy prognozy

king.xts$lking<-log(king.xts$king)

# wykres prognoz
ts.plot(king.xts[, 2],
        main = "12 months forecast of Lebron James clicks") # TYTUL MOZNA SOBIE DAROWAC
# pocztek okresu prognozy
abline(v = 120, lty = 2, col = "gray")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col = "red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col = "red", lty = 30)

# obejrzyjmy z bliÄąÄ˝eniu
ts.plot(king.xts[, 2],
        main = "12 months forecast of king", xlim = c(120,132), ylim=c(2.5,4.5))
abline(v = 120, lty = 2, col = "gray")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col ="red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col ="red", lty = 3)

# Äąâ€šĂ„â€¦czymy prognozy z oryginalnym szeregiem
king_forecast <- data.frame(forecast = forecast$pred,
                            window(king.xts$lking,
                                   start = as.Date("2020-01-01")))
king_forecast

# 7.
# sprawdzamy jakoÄąâ€şĂ„â€ˇ prognozy
king_forecast$mae <- abs(king_forecast$lking -
                           king_forecast$forecast)
king_forecast$mse <- (king_forecast$lking -
                        king_forecast$forecast)^2
king_forecast$mape <- abs((king_forecast$lking -
                             king_forecast$forecast) /
                            king_forecast$lking)
king_forecast$amape <- abs((king_forecast$lking -
                              king_forecast$forecast) /
                             (king_forecast$lking +
                                king_forecast$forecast))

colMeans(king_forecast[, 3:6])

# aby zmieniĂ„â€ˇ 'naukowy' format liczb
# moÄąÄ˝emy skorzystaĂ„â€ˇ z opcji scipen
options(scipen = 5)
round(colMeans(king_forecast[, 3:6]), 3)

# Porownanie modeli 

king = ts(data=king$Time, frequency = 12,             
         start=c(2010,1), end=c(2020,12))
lking <- log(king)

lking2019 <- window(lbj.ts, start=c(2019,12))
lking2019Dates <- as.Date(lking2019)
plot(lking2019,ylim=c(2.50,4.5), xaxt="n")
axis(1, decimal_date(lking2019Dates), format(lking2019Dates, "%b %Y"))
lines(lbj.HWmult.forecast[, 1], col = "red")
lines(lbj.HWadd.forecast[, 1], col = "blue")
lines(ts(forecast$pred, start=c(2020, 1), frequency = 12), col = "green", lwd = 2)
abline(v = c(2020,1), lty = 2)
