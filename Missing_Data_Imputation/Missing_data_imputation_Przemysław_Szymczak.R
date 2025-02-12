#### instalacja i wczytanie bibliotek ####

install.packages("glm2")
install.packages("Amelia")
install.packages("VIM")
install.packages("ModelMetrics")
install.packages("xlsx")

library(glm2)
library(Amelia)
library(Zelig)
library(VIM)
library(ModelMetrics)
library(xlsx)



#### Dane kompletne ####
# zaczytanie danych
dane_kompletne = read.csv2("Dane.csv", header=TRUE, sep=";", dec=",")
dane_kompletne$LP=NULL # usunięcie zbędnej kolumny



#### Dane niekompletne - generowanie braków ####

# rozmiary danych
wiersze = nrow(dane_kompletne) # liczba wierszy
kolumny = ncol(dane_kompletne) # liczba kolumn


## Zbiór A ##

# braki MCAR na zmiennych - wiek, kwota, staz, okres
# braki MAR na zmiennych - dodchód (zależnie od wieku), rata (zależnie od stażu)
# generowanie braków - ziarno 2943
set.seed(2943)
# macierz R1 z brakami MCAR - losowanie ze zwracaniem ze zbioru (1, 0) prawdopodobieństwo
# braku (0/NA) 20%  - wykorzystane do 4 zmiennych 
R1 = matrix(sample(c(1,NA), wiersze*4, prob = c(0.8, 0.2), replace = TRUE), nrow=wiersze)

# łączna macierz braków
R_a = cbind(1,R1[,1:3],1,1,R1[,4])

# macierz R2 z brakami MAR
# kolumna z prawdopodobieństwem braków na 5. zmiennej równym 50% (przy spełnieniu zależności od innej)
R2 = matrix(sample(c(1,0), wiersze, prob = c(0.5, 0.5), replace = TRUE), nrow=wiersze)
# kolumna z prawdopodobieństwem braków na 6. zmiennej równym 70% (przy spełnieniu zależności od innej)
R2 = cbind(R2, sample(c(1,0), wiersze, prob = c(0.3, 0.7), replace = TRUE))
# kolumna z prawdopodobieństwem braków na 6. zmiennej równym 20% (rekordy, dla których nie spełniona jest zależność)
R2 = cbind(R2, sample(c(1,0), wiersze, prob = c(0.9, 0.1), replace = TRUE))

# wstawienie braków MAR w 4. kolumnie, jeżeli wiek dla danego wiersza wynosi co najmniej 40
# i wylosowany został brak (wartość 0) dla tego wiersza
R_a[which((dane_kompletne$WIEK>=40)&(R2[,1]==0)),5]=NA

# wstawanie braków MAR w 5. kolumnie, jeżeli staż wynosi co najwyżej 13 (70% prawdopodobieństwa braku)
# a jeżeli staż jest wyższy niż 13 to prawdopodobieństwo=10%
R_a[which(((dane_kompletne$STAZ<=13)&(R2[,2]==0))|((dane_kompletne$STAZ>13)&(R2[,3]==0))),6]=NA

# wprowadzenie braków jako iloczyn ramki danych i macierzy braków 
# (operacja na komórkach, nie mnożenie macierzy):
zbior_A = dane_kompletne*R_a

# zapisanie danych do Excela
write.xlsx(zbior_A, file="zbior_A.xlsx")

# graficzna reprezantacja informacji o brakach
aggr(zbior_A)


## Zbiór B ##

# braki NMAR na zmiennych - wiek, dochód, okres
# ziarno = 4912
set.seed(4912)
R3 = matrix(sample(c(1,0), wiersze*3, prob = c(0.3, 0.7), replace = TRUE), nrow=wiersze)
R3 = cbind(R3, matrix(sample(c(1,0), wiersze*3, prob = c(0.8, 0.2), replace = TRUE), nrow=wiersze))

R_b = matrix(1, nrow=wiersze, ncol=kolumny)
R_b[which(((dane_kompletne$WIEK<=23)&(R3[,1]==0))|((dane_kompletne$WIEK>23)&(R3[,4]==0))),2]=NA
R_b[which(((dane_kompletne$DOCHOD>3200)&(R3[,2]==0))|((dane_kompletne$DOCHOD<=3200)&(R3[,5]==0))),5]=NA
R_b[which(((dane_kompletne$OKRES<14)&(R3[,3]==0))|((dane_kompletne$OKRES>=14)&(R3[,4]==6))),7]=NA

zbior_B = dane_kompletne*R_b

# zapisanie danych do Excela
write.xlsx(zbior_B, file="zbior_B.xlsx")

# graficzna reprezantacja informacji o brakach
aggr(zbior_B)



#### Imputacja ####

## 1. metoda - średnia ##
srednie_A = colMeans(zbior_A, na.rm=TRUE) # średnie dla zmiennych bez uwzględniania braków
zbior_A_imp_mean = zbior_A # ramka danych, na której dokonana zostanie imputacja

srednie_B = colMeans(zbior_B, na.rm=TRUE)
zbior_B_imp_mean = zbior_B 

# imputacja w pętli - w każdej kolumnie braki zamieniane są na średnią wartość w danej kolumnie
for (i in 1:kolumny) {
  zbior_A_imp_mean[is.na(zbior_A_imp_mean[, i]), i] = srednie_A[i]
}

for (i in 1:kolumny) {
  zbior_B_imp_mean[is.na(zbior_B_imp_mean[, i]), i] = srednie_B[i]
}


## 2. metoda - kNN ##

# imputacja metodą k najbliższych sąsiadów - na danych z brakami, imputacja w kolumnach
# gdzie braki występują
zbior_A_imp_kNN = kNN(zbior_A, variable=colnames(zbior_A[,2:7]))
aggr(zbior_A_imp_kNN, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

zbior_B_imp_kNN = kNN(zbior_B, variable=colnames(zbior_B[,2:7]))
aggr(zbior_B_imp_kNN, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))


## 3. metoda - Imputacja wielokrotna ##

# minimalne i maksymalne wartości w kolumnach stanowiące granice w imputacji
dol_granice_A = round(apply(zbior_A[2:7], MARGIN=2, function(x) min(x, na.rm=TRUE)),0)
gora_granice_A = round(apply(zbior_A[2:7], MARGIN=2, function(x) max(x, na.rm=TRUE)),0)

dol_granice_B = round(apply(zbior_B[2:7], MARGIN=2, function(x) min(x, na.rm=TRUE)),0)
gora_granice_B = round(apply(zbior_B[2:7], MARGIN=2, function(x) max(x, na.rm=TRUE)),0)

# zebranie informacji o granicach dla kolejnych zmiennych w jednej macierzy
granice_A = matrix(c(2:7,dol_granice_A, gora_granice_A), nrow=6, ncol=3) 
granice_B = matrix(c(2:7,dol_granice_B, gora_granice_B), nrow=6, ncol=3) 

# imoutacja wielokrotna na danych z brakami
zbior_A_imp_multi = amelia(zbior_A, m=5, bound=granice_A, max.resample=1000)
zbior_B_imp_multi = amelia(zbior_B, m=5, bound=granice_B, max.resample=1000)

# dane z imputacji wielokrotnej
dane_A_imp_multi = zbior_A_imp_multi$imputations
dane_B_imp_multi = zbior_B_imp_multi$imputations


# funkcja zwracająca macierz klasyfikacji dla modelu podanego w argumencie
macierz_klasyfikacji = function(x) {
  y_pred = ifelse(x$fitted.values>0.5,1,0)
  y = x$y
  confusionMatrix(y, y_pred)
}



#### Porównanie modeli ####

## dane kompletne ##
fit1 = glm2(data = dane_kompletne, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
            family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit1)
macierz_klasyfikacji(fit1)


## dane z brakami - ##
# Zbiór A
fit2A = glm2(data = zbior_A, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
            family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit2A)
macierz_klasyfikacji(fit2A)

# Zbiór B
fit2B = glm2(data = zbior_B, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
              family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit2B)
macierz_klasyfikacji(fit2B)


## 1. metoda - średnia ##
fit3.1A = glm2(data = zbior_A_imp_mean, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
            family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit3.1A)
macierz_klasyfikacji(fit3.1A)

fit3.1B = glm2(data = zbior_B_imp_mean, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
               family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit3.1B)
macierz_klasyfikacji(fit3.1B)


## 2. metoda - kNN ##
fit3.2A = glm2(data = zbior_A_imp_kNN, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
              family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit3.2A)
macierz_klasyfikacji(fit3.2A)

fit3.2B = glm2(data = zbior_B_imp_kNN, formula = GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, 
              family=binomial(link="logit"), control=glm.control(trace=TRUE))
summary(fit3.2B)
macierz_klasyfikacji(fit3.2B)


## 3. metoda - Imputacja wielowymiarowa ##
fit3.3A = zelig(GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, data=dane_A_imp_multi, model="logit")
summary(fit3.3A)
macierz_klasyfikacji(fit3.3A[[1]])
macierz_klasyfikacji(fit3.3A[[2]])
macierz_klasyfikacji(fit3.3A[[3]])
macierz_klasyfikacji(fit3.3A[[4]])
macierz_klasyfikacji(fit3.3A[[5]])

fit3.3B = zelig(GRUPA~WIEK+KWOTA+STAZ+DOCHOD+RATA+OKRES, data=dane_B_imp_multi, model="logit")
summary(fit3.3B)
macierz_klasyfikacji(fit3.3B[[1]])
macierz_klasyfikacji(fit3.3B[[2]])
macierz_klasyfikacji(fit3.3B[[3]])
macierz_klasyfikacji(fit3.3B[[4]])
macierz_klasyfikacji(fit3.3B[[5]])



