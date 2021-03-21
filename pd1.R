#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

print("Zad.1.")

czyPodzielnaBezReszty <- function(x,y){
  wyn <- x%%y==0
  print(paste("Czy liczba ", x, "jest podzielna przez ", y, "wynik jako TRUE albo FALSE:", wyn))
}

czyPodzielnaBezReszty(4,3)
czyPodzielnaBezReszty(4,2)


#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.

#Drugą połowę przejechał ze średnią prędkością 90 km/h.

#Jaka była średnia prędkość pociągu.

print("Zad.2.")
x1 <- 120
x2 <- 90
wynik2 <- (x1+x2)/2
print(paste("średnia predkość pociągu to:", wynik2, " km/k"))


#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#sWczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.


print("Zad.3.")
df<-read.table(file="dane.csv",header=TRUE, sep=";")


obliczWspKorelacjiPearsona <- function(df){
  w1 <- cor(df$waga, df$wzrost)
  w2 <- cor(df)
  print("macierz korelacji Pearsona:")
  print(w2)
  print("wsp korelacji Pearsona dla wagi i wzrostu:")
  print(w1)
  w1
}

wynik3 <-obliczWspKorelacjiPearsona(df)
# wartość współczynnika na poziomioe 0,979 (dodatnia) oznacza, że obie zmienne, tj. waga i wzrost są silnie ze sobą proporcjonalnie skorelowane.


#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)


print("Zad.4.")


utworzDataFrame <- function(ile=1){
  komunikat<-"podaj nazwy kolumn (oddzielone przecinkiem): "
  wektorKolumn<-(strsplit(readline(komunikat),",")[[1]])
  wektorKolumn
  wektorKolumnLen = length(wektorKolumn)
  wektorKolumnLen
  if (wektorKolumnLen <= 0) stop("error message - nie podano kolumn")
  
  # if (ile == 1){
  #   komunikat1<-"podaj ile wierszy chcesz dodać (jedna liczba): "
  #   ileWierszy<-as.numeric((readline(komunikat1)))
  #   if(is.na(ileWierszy)) ileWierszy <- 1
  #   else if (ileWierszy < 1 ) ileWierszy <- 1
  # }
  # else {
  #   ileWierszy <- ile
  # }
  ileWierszy <-ile
  print(paste("Podaj wartości dla", ileWierszy, "wierszy"))
  df <- data.frame()
  for (i in 1:ileWierszy){
    komunikat2 <- paste("podaj wartości dla wiersza nr", i, "(oddzielone przecinkiem): ")
    wektorWierszy<-as.numeric(strsplit(readline(komunikat2),",")[[1]])
    wektorWierszy
    wektorWierszyLen = length(wektorWierszy)
    wektorWierszyLen
    if (wektorWierszyLen != wektorKolumnLen) stop("error message - liczba wartości rozna od liczby kolumn")
    df <-rbind(df,wektorWierszy)
  }
  colnames(df) <- wektorKolumn
  print("Utworzono dataframe jak poniżej")
  print(df)
  df
}

wynik4 <-utworzDataFrame(2)
#a1,a2,a3
#1,2,3
#22,33,44


#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.

# liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
#   
#   #...
#   
# }
# 
# Lista plików w katalogu: 
#   
#   list.files(sciezka)
# 
# Omijanie na : na.omit(myDataFrame[[nazwaKolumny]])
# Do złączania stringów: 
#   
#   paste("string1","string2",sep="TU WSTAW SEPARATOR")
# Gdy mamy, rózne oznaczenia NA w plikach możemy wykorzystać ( w tym wypadku pusty znak i NA:
#                                                                na.strings=c("","NA")



liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  nazwaKolumny <- paste0("X", nazwaKolumny)
  listaPlikow <- list.files(sciezka)
  listaPlikowLen <- length(listaPlikow)
  
  if(DlaIluPlikow > listaPlikowLen) { DlaIluPlikow <-listaPlikowLen 
  } else if(DlaIluPlikow < 1) { DlaIluPlikow <- 1 }
  lista = list()
  listaWynik = list()
  for(i in 1:DlaIluPlikow) {
    #print(listaPlikow[i])
    sciezkaDoPliku <- paste0(sciezka, "/", listaPlikow[i])
    #print(sciezkaDoPliku)
    dfX <- read.table(file=sciezkaDoPliku,header=TRUE, sep=",", na.strings=c("","NA"))
    lista[[i]] <- dfX
    #wynik <- mean(dfX[,nazwaKolumny],na.rm = TRUE)
    if( jakaFunkcja == "mean" ){
      wynik <- mean(lista[[i]][,nazwaKolumny],na.rm = TRUE)
      listaWynik[[i]] <- wynik
    }else  if( jakaFunkcja == "max" ){
      wynik <- max(lista[[i]][,nazwaKolumny],na.rm = TRUE)
      listaWynik[[i]] <- wynik
    }
    else  if( jakaFunkcja == "min" ){
      wynik <- min(lista[[i]][,nazwaKolumny],na.rm = TRUE)
      listaWynik[[i]] <- wynik
    }
    else  if( jakaFunkcja == "median" ){
      wynik <- median(lista[[i]][,nazwaKolumny],na.rm = TRUE)
      listaWynik[[i]] <- wynik
    }
    else {
      stop("error message - brak takiej funkcji")
    }
    print(paste("dla pliku", sciezkaDoPliku, "dla kolumny",  nazwaKolumny, "wartość funkcji", jakaFunkcja, "wynosi:" , wynik))

  }
  listaWynik
}

test_sciezka <- "smogKrakow"
test_nazwaKolumny = "3_pressure"

x5_1 <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="mean", DlaIluPlikow=3)
#x5_1
x5_2 <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="median", DlaIluPlikow=3)
#x5_2
x5_3 <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="max", DlaIluPlikow=3)
#x5_3
x5_4 <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="min", DlaIluPlikow=3)
#x5_4

test_nazwaKolumny = "147_temperature"
x5_1a <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="mean", DlaIluPlikow=3)
#x5_1a
x5_2a <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="median", DlaIluPlikow=3)
#x5_2a
x5_3a <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="max", DlaIluPlikow=3)
#x5_3a
x5_4a <-liczZplikow(test_sciezka, test_nazwaKolumny, jakaFunkcja="min", DlaIluPlikow=3)
#x5_4a




