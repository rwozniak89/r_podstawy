# praca domowa: pobranie danych z innego portalu (np. otomoto)

library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(rvest)


wektorLinkowOtoMoto<-c()
for(i in 1:10){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/?page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]/article[*]/div[2]/div[1]/div[1]/h2/a')
  print(result)
  wektorLinkowOtoMoto<-c(wektorLinkowOtoMoto,xml_attr(result,"href"))
  print(wektorLinkowOtoMoto)
}
wektorLinkowOtoMotoU<-wektorLinkowOtoMoto%>%unique()

zrobWierszRvestOtoMoto<-function(w,wektorLinkow,remDr){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".offer-price__number")%>%html_text()
  cena <- gsub('[A-Z]+','',cena) 
  cena <- str_trim(cena)
  cena <- str_replace_all(cena, "[\r\n]" , "")
  print(paste0("cena : ",cena ) )
  waluta<-html_node(page,".offer-price__currency")%>%html_text()
  print(paste0("waluta : ",waluta ) )
  v1 <-html_nodes(page, ".offer-params__label")%>%html_text()%>%na.omit()
  v1 <- str_replace_all(v1, "[\r\n]" , "")
  v2 <-html_nodes(page, ".offer-params__value")%>%html_text()%>%na.omit()
  v2 <- str_replace_all(v2, "[\r\n]" , "")
  v2 <- gsub(' +',' ',v2) 
  v2 <- str_trim(v2)
  nazwyKolumn <- v1 #[indexy%%2==1]
  #print('nazwyKolumn')
  #print(nazwyKolumn)
  wartosci <- v2 #[indexy%%2==0]
  #print('wartosci')
  #print(wartosci)
  df1<- data.frame (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
  df1<-cbind(waluta,df1)
  print(df1)

}

mieszkania3OtoMoto<-NULL
liczbaLinkowOtoMoto<-length(wektorLinkowOtoMotoU)
#for(w in 1: 3 ){
for(w in 1: liczbaLinkowOtoMoto ){
  print(paste0(w," / ",liczbaLinkowOtoMoto ) )
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvestOtoMoto(w,wektorLinkowOtoMotoU,remDr),error=function(e){skip<<-TRUE}
  )
  # print(df1)
  if(skip){next}
  if(is.null(mieszkania3OtoMoto)){
    mieszkania3OtoMoto<-df1
  }else{
    mieszkania3OtoMoto<-smartbind(mieszkania3OtoMoto,df1)
  }
}

mieszkania3OtoMoto