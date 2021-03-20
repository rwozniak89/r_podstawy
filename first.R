#komentarz
print("hello")
x<-123

if(require(jsonlite) == FALSE){
  install.packages("httr")
  library(httr)
}
if(require(jsonlite)== FALSE){
  install.packages("jsonlite")
  require(jsonlite)
}
#w1 <- library(httr) zwraca bibliotekę albo bład
#w2 <- require(jsonlite) zawraca True albo false



endpoint <- "https://api.openweathermap.org/data/2.5/weather?q=Warszawa&appid=1765994b51ed366c506d5dc0d0b07b77"
# https://openweathermap.org/
getWeather <- GET(endpoint)
weatherText <- content(getWeather, as = "text")
weatherFromJson <- fromJSON(weatherText, flatten = FALSE)
weatherDF <- as.data.frame(weatherFromJson)
View(weatherDF)

is.vector(x)
is.vector(weatherDF)
is.vector(endpoint)
?vector
x2<-c(1,2,3,4,5,6,7,8,9,10)
x3<-c(1:6)
class(x2)
class(x3)

v1<- as.integer(x2)
class(v1)
v1 <- as.vector(v1,mode='integer')
class(v1)
v2<- as.logical(x2)
v3<- as.character(x2)

x4 <- 2*x2

v2 <-as.vector(c(3),mode="integer")
wynik <- v1*v2
class
wynik
wynik <- v1-v2
class(wynik)
wynik
wynik <- v1+v2
class(wynik)
wynik
wynik <- v1/v2
class(wynik)
wynik
wynik <- v1%/%v2 ## reszta cakowita z dzielenia
class(wynik)
wynik
wynik <- v1%%v2 #dzielenie modulo
class(wynik)
wynik

ls()## lista zmeinnych

v1
v3<-c("333","444","555", "abc")

wynikNowy <- as.numeric(c(v1,v3))
?NA

is.na(wynikNowy)

brakujace<-is.na(wynikNowy)

wynikNowy[!brakujace]

lista<-list(1,2,3,4,5)

lista<-list(c("1",2,3,4,5),v1,wynikNowy)
lista[[3]]
lista[3][1]

lista[[3]] [11]

wn <- lista[[3]] [ lista[[3]]>5]


plec <- c("men", "kob", "men")
plecf <- as.factor(plec)
str(plecf)
?factor
x <- c("Man", "Male", "Man", "Lady", "Female")
(xf <- factor(x, levels = c("Male", "Man" , "Lady",   "Female"),labels = c("Male", "Male", "Female", "Female")))
str(xf)


mojaMAcierz <- matrix(data=seq(1,90,1),nrow=10,ncol=9)
mojaMAcierz2 <- matrix(data=seq(1,90,1),nrow=10,ncol=9,byrow=TRUE)
mojaMAcierz3 <- matrix(data=seq(1,5,1),nrow=10,ncol=9)

mojaMAcierz[,3]
mojaMAcierz[3,]
mojaMAcierz[3,3]


wynik
v1
cbind(v1,wynik)
rbind(v1,wynik)

b=1

df <- data.frame(index=1:3,
                 imie=c("jan","alina","bartek"),
                 plec=c("men","kob","men")
                 ,stringsAsFactors = FALSE
                 )


df2<-read.table(file="dane.csv",header=TRUE, sep=";")
df2b<-read.csv2(file="dane.csv",header=TRUE, sep=";")


hello <- function(x=10){
  for(i in 1:x){
    print(i)
  }
}

hello(11)

dziel <- function(a,b){
  if(b == 0){
    print("nie dziel przez 0")
  }
  else{
    print(a/b)
  }
}

dziel(1,0)

dziel2 <- function(x,y){
  if(0 %in% y) {
    wynik <- "nie dziel przez 0"
  }
  else{
    wynik <-x/y
  }
  wynik
}

dziel2 <- function(x,y){
  y[y == 0] <- NA
  if(0 %in% y) {
    wynik <- "nie dziel przez 0"
  }
  else{
    wynik <-x/y
  }
  wynik
}

dziel3 <- function(x,y){
  y[y == 0] <- NA
  x/y
}

dziel(2,3)
dziel2(2,3)


dziel2(c(2,3,4),2)
dziel2(c(2,3,4),0)
dziel2(c(2,3,4),c(1,2,3))
dziel2(c(2,3,4),c(1,2,1))
dziel2(c(2,3,4),c(0,2,1))


dziel3(c(2,3,4),c(1,2,1))
dziel3(c(2,3,4),c(0,2,1))

dzielKlawiatura<-function(){
  komunikat<-"podaj 2 liczby oddzielone przecinkiem: "
  wektorOdp<-as.numeric(strsplit(readline(komunikat),",")[[1]])
  l1<- wektorOdp[1]
  l2<- wektorOdp[2]
  if(l2==0){
    v<-"nie dziel przez zero"
  }
  else{
    v<-l1/l2
  }
  v
}

dzielKlawiatura()

